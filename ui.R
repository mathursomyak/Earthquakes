#ui
library(leaflet)
choices  <- unique(read.csv("geo.csv", stringsAsFactors=FALSE)$P1_name)

ui <- fluidPage(
  titlePanel("Boxing Career Explorer v1.1"),
  sidebarLayout (
    sidebarPanel(
      selectInput('person', 'Select boxer', choices=choices, selectize = FALSE),  #unique(DF$P1_name
      radioButtons('radio', 'Select opponents', choices=c('all', 'individual', 'by date'), selected = NULL, inline = TRUE),
      
      conditionalPanel(
        condition = "input.radio == 'all'"
      ),    
      
      conditionalPanel(
        condition = "input.radio == 'individual'",
        uiOutput('opponents')   
      ),
      
      conditionalPanel(
        condition = "input.radio == 'by date'",
        uiOutput('dates')   
      )
      
    ),
    mainPanel(
      leafletOutput('map'), 
      fluidRow(
        column(6, div(style = "height:10px;"))),
      fluidRow(column(4, DT::dataTableOutput('table')))
    )
  )  # <- end sidebarLayout
)  #<-  end ui
