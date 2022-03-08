library(shiny)
library(fpp3)
library(ggplot2)
library(shinyWidgets)
library(quantmod)
library(plotly)
SYMBOLS <- stockSymbols()


ui <- fluidPage(
  titlePanel(title = h3("What if I invested?", 
                        align = "center")),
  selectInput("select", label = h3("Select A Stock Name"), 
           choices = names(table(SYMBOLS$Name)), selected = 1),
  hr(),
  fluidRow(column(3, verbatimTextOutput("value")))
)
  br()
  
 textInput("stock", label = h3("Input a stock that you want to consider"), value = ". . .")
  
  


server <- function(input, output, session) {
output$value <- renderPrint({ SYMBOLS$Symbol[which(SYMBOLS$Name == input$select)] })
  
}

shinyApp(ui, server)