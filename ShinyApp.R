library(shiny)
library(fpp3)
library(ggplot2)
library(shinyWidgets)
library(quantmod)
library(plotly)
library(shinydashboard)
library(tidyquant)

SYMBOLS <- stockSymbols()

clean_names <- function(stocks) {
  split_names = strsplit(names(stocks), split = '.', fixed = TRUE)
  vapply(split_names, function(x) x[2], character(1))
}


ui <-
  dashboardPage( skin = "green",
                 dashboardHeader(title = "What if I invested?", titleWidth = 400),
                 dashboardSidebar(
                   sidebarMenu(
                     menuItem("Stock Selection", tabName = "stockselect", 
                              icon = icon("dashboard")),
                     menuItem("Stock Prices Graphic", tabName = "graph", 
                              icon = icon("table")),
                     menuItem("Volume Graphic", tabName = "feature3", 
                              icon = icon("th")),
                     menuItem("Feature 4", tabName = "feature4", 
                              icon = icon("th"))
                   )
                 ),
                 dashboardBody(
                   tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size: 24px;
      }
    '))),
                   
                   tabItems(
                     # First tab content
                     tabItem(tabName = "stockselect",
                             selectInput("select",
                                         label = h3("Select / Search A Stock Name "),
                                         choices = names(table(SYMBOLS$Name)),
                                         selected = names(table(SYMBOLS$Name))[2]),
                             
                             hr(),
                             
                             fluidRow(column(3, verbatimTextOutput("value")))
                     ),
                     
                     # Second tab content
                     tabItem(tabName = "graph",
                             h1("Graph My Stock Prices"),
                             basicPage(
                               h2("Stock Prices"),
                               textInput("stocks", "Input the ticker symbol of your choice", 
                                         "FLWS"),   
                               dateRangeInput("date", "Choose the desired date range for your stock of 
                       interest (Jan 2007 - Jan 2022)",
                                              start = "2013-01-01", 
                                              end = "2022-01-01",min = "2007-01-01", 
                                              max = "2022-02-01",
                                              format = "yyyy-mm-dd" ),
                               checkboxInput(inputId = "log", label = "log y axis", 
                                             value = FALSE),  
                               plotOutput("plot")
                             )
                     ), 
                     
                     # Third tab content
                     tabItem(tabName = "feature3",
                             h1("Plot the Volume of My Stock"), 
                             basicPage(
                               textInput("stocks1", "Input the ticker symbol of your choice", 
                                         "FLWS"),   
                               dateRangeInput("date1", "Choose the desired date range for your stock of 
                       interest (Jan 2007 - Jan 2022)",
                                              start = "2013-01-01", 
                                              end = "2022-01-01",min = "2007-01-01", 
                                              max = "2022-02-01",
                                              format = "yyyy-mm-dd" ),
                               plotlyOutput("volumeplot")
                             )
                     ),
                     
                     # Fourth tab content
                     tabItem(tabName = "feature4",
                             h2("Feature 4")
                             
                             
                     )
                     
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
  output$volumeplot <- renderPlotly({
    data1<- getSymbols(input$stocks1, from = input$date1[1],
                       to = input$date1[2], src = "yahoo",
                       auto.assign = FALSE)
    names(data1) <- clean_names(data1)
    autoplot(data1$Volume) %>% ggplotly() 
    
  })
}
shinyApp(ui, server)

