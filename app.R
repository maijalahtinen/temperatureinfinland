#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(plotly)

data <-
  readRDS("miniproject_data.rds")

sidebar <- dashboardSidebar(
  menuItem("Utsjoki", tabName = "utsjoki", icon = icon("dashboard")),
  menuItem("Jyväskylä", tabName = "jyvaskyla", icon = icon("th"))
  #menuItem("Oulu", tabName = "oulu", icon = icon("dashboard")),
  #menuItem("Helsinki", tabName = "helsinki", icon = icon("th"))
)

body <- dashboardBody(# Boxes need to be put in a row (or column)
  tabItems(
    # First tab content
    tabItem(tabName = "utsjoki",
            fluidRow(
              box(
                title = "Select date range",
                dateRangeInput(
                  "daterange",
                  "Date range:",
                  start = min(data$time),
                  end   = max(data$time),
                  min = min(data$time),
                  max = max(data$time)
                )
              )
            ),
            fluidRow(box(
              plotOutput("plot1", height = 350)
            ))),
    # Second tab content
    tabItem(tabName = "jyvaskyla",
            h2("Jyväskylä tab content"))
))

ui <- dashboardPage(
  dashboardHeader(title = "Daily Temperatures"),
  sidebar,
  body
)
  


server <- function(input, output, session) {
  
 # updateDateRangeInput()
  
  output$plot1 <- renderPlot({
    df <- data %>% filter(time >= input$daterange[1] & time <= input$daterange[2])

    p <- ggplot(data = df) +
      geom_line(aes(x = time, y = value), color = "dark blue") +
      labs(title = "Daily average temperatures in Utsjoki",
           x = "Time", y = "Celsius") +
      theme_minimal() +
      #scale_x_continuous(breaks=c(1970, 1980, 1990, 2000, 2010, 2020)) +
      #scale_x_continuous(breaks=seq(1970, 2022, by = 5)) +
      theme(legend.position='none') 
    
    #plotly::ggplotly(p)
    p
  })
}

shinyApp(ui, server)
