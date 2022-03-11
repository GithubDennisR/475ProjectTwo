library(shiny)
library(fpp3)
library(ggplot2)
library(shinyWidgets)
library(quantmod)
library(plotly)
library(dplyr)
library(shinydashboard)
library(tidyquant)
library(flexdashboard)
library(ggeasy)
library(ggthemes)


SYMBOLS <- stockSymbols()

clean_names <- function(stocks) {
  split_names = strsplit(names(stocks), split = '.', fixed = TRUE)
  vapply(split_names, function(x) x[2], character(1))
}


ui <-
  dashboardPage( skin = "green",
                 dashboardHeader(title = "What if I invested?", titleWidth = 400),
                 dashboardSidebar( width = 200,
                                   sidebarMenu(
                                     menuItem("Stock Selection", tabName = "stockselect", 
                                              icon = icon("chart-line")),
                                     menuItem("Stock Prices Graphic", tabName = "graph", 
                                              icon = icon("chart-line")),
                                     menuItem("Volume Graphic", tabName = "feature3", 
                                              icon = icon("chart-line")),
                                     menuItem("Stock Percent Change", tabName = "stockchange", 
                                              icon = icon("chart-line"))
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
                             h1("What is the ticker symbol of my stock?"),
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
                               verbatimTextOutput("percentchange"),
                               plotlyOutput("volumeplot")
                             )
                     ),
                     
                     # Fourth tab content
                     tabItem(tabName = "stockchange",
                             h2("Stock Percent Change"),
                             basicPage(
                               textInput("stocks2", "Input the ticker symbol of your choice", 
                                         "FLWS"),   
                               dateInput(inputId = "date2", label = "First Date (Minimum 2007-01-01) (Do Not Select Weekends as Market is Closed)",
                                         value = "2014-01-10", format = "yyyy-mm-dd" ),
                               dateInput(inputId = "date3",label = "Second Date",
                                         format = "yyyy-mm-dd" ),
                               h4("Change in $ Value over Time"),
                               hr(),
                               textOutput("DollarChange"),
                               hr(),
                               verbatimTextOutput("firstdatevalues"),
                               verbatimTextOutput("seconddatevalues"),
                               plotlyOutput("changeplot")
                  
                             )
                             
                             
                     )
                     
                   )  
                   
                 )
  )





server <- function(input, output, session) {
  output$value <- renderText({ SYMBOLS$Symbol[which(SYMBOLS$Name == input$select)] })
  output$plot <- renderPlot({
    data <- getSymbols(input$stocks,  
                       from = input$date[1],
                       to = input$date[2],
                       auto.assign = FALSE   
    )
    chartSeries(data, theme = chartTheme("black"),
                type = "line", log.scale = input$log, TA = NULL)
  })
  output$volumeplot <- renderPlotly({
    data1<- getSymbols(input$stocks1, from = input$date1[1],
                       to = input$date1[2], src = "yahoo",
                       auto.assign = FALSE)
    names(data1) <- clean_names(data1)
    p <- ggplot(data1, aes(Index, Volume)) + 
      geom_line(color = "green") + 
      theme_fivethirtyeight()+
      labs(title = "The Volume of My Stock", y = "Volume") +
      ggeasy::easy_center_title() +
      ggeasy::easy_all_text_color(color = "yellow") +
      theme(plot.background = element_rect(fill = "black"), 
            panel.background = element_rect(fill = "black"))
    ggplotly(p)
    
  })
  output$changeplot <- renderPlotly({
    data2<- getSymbols(input$stocks2, from = input$date2,
                       to = input$date3, src = "yahoo",
                       auto.assign = FALSE)
    names(data2) <- clean_names(data2)
    p1 <- ggplot(data2, aes(Index, Close)) + 
      geom_line(color = "green") + 
      theme_fivethirtyeight()+
      labs(title = "The Close of My Stock", y = "Close") +
      ggeasy::easy_center_title() +
      ggeasy::easy_all_text_color(color = "yellow") +
      theme(plot.background = element_rect(fill = "black"), 
            panel.background = element_rect(fill = "black"))
    ggplotly(p1)
    
  })
  output$firstdatevalues <- renderPrint({
    close1 <- getSymbols(input$stocks2, from=input$date2,
                         to = as.Date(input$date2) + 1, src = "yahoo",
                         auto.assign = FALSE)
    names(close1) <- clean_names(close1)
    close1
  })
  output$seconddatevalues <- renderPrint({
    close2 <- getSymbols(input$stocks2, from=input$date3,
                         to = as.Date(input$date3) + 1,src = "yahoo",
                         auto.assign = FALSE)
    names(close2) <- clean_names(close2) 
    close2
  })
  output$DollarChange <- renderText({
    close1 <- getSymbols(input$stocks2, from=input$date2,
                         to = as.Date(input$date2) + 1, src = "yahoo",
                         auto.assign = FALSE)
    names(close1) <- clean_names(close1)
    close2 <- getSymbols(input$stocks2, from=input$date3,
                         to = as.Date(input$date3) + 1,src = "yahoo",
                         auto.assign = FALSE)
    names(close2) <- clean_names(close2)
  increaseValue <- as.double(close2$Close)-as.double(close1$Close)
  increasePercent <- (as.double(close2$Close)-as.double(close1$Close))/as.double(close2$Close)*100
  increasePercent <- round(increasePercent, 2)
  noquote(paste(c("The difference in price between the start and end of the period selected is $", increaseValue, "(",increasePercent,"%)"), collapse = " "))
  })
}
shinyApp(ui, server)



