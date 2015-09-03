#server

# personal projects file

library(shiny)
library(ggplot2)
library(dplyr)
library(leaflet)
library(data.table)
library(DT)

options(xtable.include.rownames=F)

DF <- read.csv("geo.csv", stringsAsFactors=FALSE)

format_table <- function(DF){
  DF$outcome_and_type <- paste(DF$P1_outcome, DF$outcome_type)
  DF <- select(DF, sequence, P1_name, outcome_and_type, P2_name, location, date, comments)
  names(DF) <- c('Fight #', 'Boxer', 'Outcome', 'Opponent', 'Location', 'Date', 'Notes')
  DF$Notes[which(DF$Notes == 0)] <- ''
  return(DF)
}

## here we want to apply the sequnce to both Data, P2_name, and also create a new column for the sequence of fights
DF <- arrange(DF, P1_name, date)
DT <- data.table(DF)
DT[, .sequence := sequence(.N), by = "P1_name"]
DF$date_sequence <- paste(DF$date, '   (', DT$.sequence, ')', sep='')
DF$P2_with_sequence <- paste(DF$P2_name, '   (', DT$.sequence, ')', sep='')
DF$sequence <- DT$.sequence
DF <- arrange(DF, P1_name, desc(date))

icon_W <- makeIcon(
  iconUrl = "http://i58.tinypic.com/119m3r5_th.gif",
  iconWidth = 10, iconHeight = 23,
  iconAnchorX = 10, iconAnchorY =23 
)

icon_L <- makeIcon(
  iconUrl = "http://i62.tinypic.com/2dulcvq_th.jpg",
  iconWidth = 10, iconHeight = 23,
  iconAnchorX = 10, iconAnchorY = 23
)

icon_D <- makeIcon(
  iconUrl = "http://i58.tinypic.com/2zox2yf_th.gif",
  iconWidth = 10, iconHeight = 23,
  iconAnchorX = 10, iconAnchorY = 23
)

icon_N <- makeIcon(
  iconUrl = "http://i62.tinypic.com/339j7de_th.gif",
  iconWidth = 10, iconHeight = 23,
  iconAnchorX = 22, iconAnchorY = 94
)

icon_list <- iconList(W=icon_W,L=icon_L,D=icon_D,N=icon_N)

server <- function(input, output, session) {
  
  output$dates<-renderUI({
    selectInput('dates', 'by date', choices=DF[which(DF$P1_name == input$person), ]$date_sequence, selectize = FALSE)
  })
  
  output$opponents<-renderUI({
    selectInput('opponents', 'opponent name', choices=DF[which(DF$P1_name == input$person), ]$P2_with_sequence, selectize = FALSE)
  })
  
  output$map<-renderLeaflet({
    validate(
      need(!is.null(input$dates),""),
      need(!is.null(input$person),"")
    )
    
    if(input$radio=='all'){
      DF <- filter(DF, P1_name==input$person)
      zoom_num <- 2
      setzoom <- c(DF$lat[1], DF$lon[1])
      output$table <- DT::renderDataTable(format_table(DF), options = list(paging = FALSE, searching = FALSE), rownames= FALSE)}
    
    if(input$radio=='individual'){
      DF <- filter(DF, P1_name==input$person, P2_with_sequence==input$opponents)
      zoom_num <- 5
      setzoom <- c(DF$lat[1], DF$lon[1])
      output$table <- DT::renderDataTable(format_table(DF), options = list(paging = FALSE, searching = FALSE), rownames= FALSE)}
    
    if(input$radio=='by date'){
      DF <- filter(DF, P1_name==input$person, date_sequence==input$dates)
      zoom_num <- 5
      setzoom <- c(DF$lat, DF$lon) 
      output$table <- DT::renderDataTable(format_table(DF), options = list(paging = FALSE, searching = FALSE), rownames= FALSE)}     
                                               
    m <- leaflet(DF) %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      setView(lat=setzoom[1], lng=setzoom[2], zoom=zoom_num) %>%
      
      addMarkers(lat=DF$lat, lng=DF$lon,icon= ~icon_list[DF$P1_outcome])
  })  #<- end output$map
}     #<- end server function


