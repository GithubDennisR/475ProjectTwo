library(shiny)
library(fpp3)
library(ggplot2)
library(shinyWidgets)
library(quantmod)
library(plotly)
library(shinydashboard)

SYMBOLS <- stockSymbols()


ui <-
  dashboardPage(
    dashboardHeader(title = "What if I invested?"),
    dashboardSidebar(),
    dashboardBody(
      selectInput("select",
                  label = h3("Select / Search A Stock Name "),
                  choices = names(table(SYMBOLS$Name)),
                  selected = 1),
      
      hr(),
      
      fluidRow(column(3, verbatimTextOutput("value")))
      
    )
    
  )

  
  


server <- function(input, output, session) {
output$value <- renderPrint({ SYMBOLS$Symbol[which(SYMBOLS$Name == input$select)] })
  
}

shinyApp(ui, server)