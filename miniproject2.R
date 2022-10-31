#install.packages("remotes")
#remotes::install_github("rOpenGov/fmi2")
 
library(fmi2)
library(magrittr)
library(tidyr)
library(dplyr)
library(janitor)
library(ggplot2)
library(prophet)
library(forecast)
station_data <- fmi2::fmi_stations()

station_data %>%
  DT::datatable()

# Utsjoki fmisid = 102036
# Data starting from 1970-01-01

start <- '1970-01-01'
end <- '1970-12-31'

collect_data <- function(start, end) {
  weather <- data.frame()
  while(start <= "2022-01-01") {
    table <- 
      fmi2::obs_weather_daily(starttime = start,
                              endtime = end,
                              fmisid = 102036) %>% 
      filter(variable == "tday", !is.na(value))
    
    weather <- weather %>% rbind(table)
    
    start <- paste0(as.integer(substr(start, 1, 4)) + 1, "-01-01")
    end <- paste0(as.integer(substr(end, 1, 4)) + 1, "-12-31")
  }
  weather
}

data <- collect_data(start = start, end = end)

data$time <- lubridate::ymd_hms(paste(data$time, "00:00:00"))
data <- data %>% mutate(ds = time, y = value)

# drop geometry

sf::st_geometry(data) <- NULL 

saveRDS(data, "~/Documents/Introduction to Data Science/miniprojekti/miniproject_data.rds")

# Plot 

p <- ggplot(data = data) +
  geom_line(aes(x = ds, y = y), color = "dark blue") +
  labs(title = "Daily average temperatures in Utsjoki from 1970-",
       x = "Time", y = "Celsius") +
  theme_minimal() +
  #scale_x_continuous(breaks=c(1970, 1980, 1990, 2000, 2010, 2020)) +
  #scale_x_continuous(breaks=seq(1970, 2022, by = 5)) +
  theme(legend.position='none') 

plotly::ggplotly(p)
# OR simply

plot(y ~ ds, data, type = "l")

# The BoxCox.lambda() function will choose a value of lambda
lam <- BoxCox.lambda(data$value, method = "guerrero")
data$y = BoxCox(data$value, lam)
#data.m <- melt(data, measure.vars=c("value", "y"))

# Build a model
model <-
 prophet(data)
future <- make_future_dataframe(model, freq = 'day', periods = 1825)
forecast <- predict(model, future)

plot(model, forecast)
prophet_plot_components(model, forecast)

# Validate model
cutoffs <- as.Date(c('2013-02-15', '2013-08-15', '2014-02-15'))
model_cv2 <- cross_validation(m, initial = 3650, cutoffs = cutoffs, horizon = 365, units = 'days')

model_cv <- cross_validation(model, initial = 3650, period = 365, horizon = 365, units = "days")
metrics <- performance_metrics(model_cv2)

plot_cross_validation_metric(model_cv, metric = 'smape')
