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
library(ggplot2)

data <-
  readRDS("miniproject_data.rds")
forecast <- readRDS("miniproject_forecast.rds")
final_model <- readRDS("final_model.rds")

data$time <- lubridate::ymd(data$time)
forecast$time <- lubridate::ymd(forecast$ds)

# sidebar <- dashboardSidebar(
#   menuItem("Utsjoki", tabName = "utsjoki", icon = icon("dashboard"))
# )

ui <- dashboardPage(
  dashboardHeader(title = "Utsjoki temperatures"),
  dashboardSidebar(
    br(),
    br(),
    h5(
      "This is a webpage for checking how the temperature has changed from the 1970 to present moment in Utsjoki.",
      align = "center"
    ),
    br(),
    h5(
      "Plots are adjustible for shorter periors of time. Please, try them!",
      align = "center"
    ),
    br(),
    h5(
      "Date ranges from 1.1.1970 to 3.11.2023. And period between 4.11.2022 and 3.11.2023 is a forecast made by history data. You can distinguish them from the plot by color. Blue is real data, yellow is forecast and grey is upper and lower limits for the forecast.",
      align = "center"
    )
  ),
  dashboardBody(fluidRow(
    box(
      title = "Select date range between 1.1.1970 and 3.11.2023",
      dateRangeInput(
        "daterange",
        "Date range:",
        start = min(data$time),
        end   = max(forecast$time),
        min = min(data$time),
        max = max(forecast$time),
        format = "yyyy/mm/dd"
      ),
      width = 400
    )
  ),
  fluidRow(box(
    plotOutput("plot1", height = 350), width = 400
  )),
  fluidRow(box(
    plotOutput("plot2", height = 350), width = 400
  )))
)

server <- function(input, output, session) {
  # updateDateRangeInput()
  
  output$plot1 <- renderPlot({
    if (!isTruthy(input$daterange[1]) | !isTruthy(input$daterange[2])) {
      ggplot() +
        annotate(
          "text",
          x = 10,
          y = 10,
          size = 6,
          label = "O-ou!\nDate range is incorrect.
                          \nPlease select dates between \n1.1.1970 and 3.11.2023."
        ) +
        theme_void()
    } else {
      df <-
        data %>% filter(time >= input$daterange[1] &
                          time <= input$daterange[2])
      forecast1 <-
        forecast %>% filter(time >= input$daterange[1] &
                              time <= input$daterange[2])
      
      p <- ggplot() +
        geom_line(data = df, aes(x = time, y = value), color = "dark blue") +
        geom_line(
          data = forecast1 %>% filter(time >= '2022-11-4'),
          aes(x = time, y = yhat),
          color = "yellow"
        ) +
        geom_line(
          data = forecast1 %>% filter(time >= '2022-11-4'),
          aes(x = time, y = yhat_lower),
          color = "light grey"
        ) +
        geom_line(
          data = forecast1 %>% filter(time >= '2022-11-4'),
          aes(x = time, y = yhat_upper),
          color = "light grey"
        ) +
        theme_minimal() +
        theme(legend.position = 'none')
      
      if (input$daterange[2] < '2022-11-04' &
          input$daterange[1] >= '1970-01-01') {
        p <- p +
          labs(title = "Daily average temperatures in Utsjoki",
               x = "Time",
               y = "Celsius")
        p
      } else if (input$daterange[2] >= '2022-11-04' &
                 input$daterange[2] <= '2023-11-03' &
                 input$daterange[1] >= '1970-01-01') {
        p <- p +
          labs(title = "Daily average temperatures in Utsjoki and forecast",
               x = "Time",
               y = "Celsius")
        
        p
      }
    }
    
  })
  
  output$plot2 <- renderPlot({
    if (!isTruthy(input$daterange[1]) | !isTruthy(input$daterange[2])) {
      ggplot() +
        annotate(
          "text",
          x = 10,
          y = 10,
          size = 6,
          label = "O-ou!\nDate range is incorrect.
                          \nPlease select dates between \n1.1.1970 and 3.11.2023."
        ) +
        theme_void()
    } else {
      forecast1 <-
        forecast %>% filter(time >= input$daterange[1] &
                              time <= input$daterange[2])
      
      prophet_plot_components(final_model, forecast1, uncertainty = T)
      title(main = "Trend and yearly circle in Temperature",
            cex.main = 1,
            font.main = 4)
    }
    
  })
  
}

shinyApp(ui, server)
