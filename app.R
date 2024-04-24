#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(MASS)
library(vcd)
library(vcdExtra)
library(ggplot2)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Interactive Charts for Q2"),
    sidebarLayout(
      sidebarPanel(
      dateRangeInput("dateRange", "Date range:", start = as.Date("2017-01-01"), end = as.Date("2018-01-01")),
  
      radioButtons("variable2", "Choose Box Plot Variable", choices=c("year", "month", "day","hour"))),
      mainPanel(
        plotOutput("lineplot"),
        plotOutput("boxplot")
      )
    )
)


server <- function(input, output) {
  #energy database
  energy<-read.csv("power_data.csv", header = TRUE, row.name="X")
  x <- as.POSIXct(energy$Datetime, format="%Y-%m-%d %H:%M:%S")
  year <- as.numeric(format(x, '%Y'))
  month <- as.numeric(format(x, '%m'))
  day <- as.numeric(format(x, '%d'))
  hour <- as.numeric(format(x, '%H'))
  realDate <- as.Date(x)
  energy<-cbind(energy, year, month, day, hour, realDate)
  energy <- energy %>% mutate(Datetime = as.POSIXct(Datetime, format="%Y-%m-%d %H:%M:%S"))
  energy<-energy[order(energy$Datetime), ]
  rownames(energy) <- NULL

    output$lineplot <- renderPlot({
        # draw the histogram with the specified number of bins
        dataSubset <- subset(energy, realDate >= input$dateRange[1] & realDate<=input$dateRange[2])
        
        ggplot(dataSubset, aes(x=realDate, y=Power_MWH)) + geom_line(colours="orange", size=2)+theme_minimal()
    })
    
    output$boxplot<-renderPlot({
      energy$year<-as.factor(energy$year)
      energy$month<-as.factor(energy$month)
      energy$day<-as.factor(energy$day)
      energy$hour<-as.factor(energy$hour)
      req(input$variable2)
      ggplot(energy, aes_string(x=input$variable2, y="Power_MWH"))+geom_boxplot()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
