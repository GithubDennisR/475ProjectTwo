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
      basicPage(
        h1("Stock Prices"),
        textInput("stocks", "pick stock", "AAPL"),   
        dateRangeInput("date", "date range ", start = "2013-01-01", end = "2022-01-01",min = "2007-01-01", max = "2022-02-01",format = "yyyy-mm-dd" ),
        checkboxInput(inputId = "log", label = "log y axis", value = FALSE),  
        plotOutput("plot")
      ) 
    )
    
  )

  
  


server <- function(input, output, session) {
output$value <- renderPrint({ SYMBOLS$Symbol[which(SYMBOLS$Name == input$select)] })
  output$plot <- renderPlot({
    data <- getSymbols(input$stocks,  
                       from = input$date[1],
                       to = input$date[2],
                       auto.assign = FALSE   
    )
    chartSeries(data, theme = chartTheme("white"),
                type = "line", log.scale = input$log, TA = NULL)
  })

shinyApp(ui, server)
