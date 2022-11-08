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

# Data ----
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

# Plot the data

p <- ggplot(data = data %>% filter(time >= '2022-01-01')) +
  geom_line(aes(x = ds, y = y), color = "dark blue") +
  labs(title = "Daily average temperatures in Utsjoki from 1970-",
       x = "Time", y = "Celcius") +
  theme_minimal() +
  theme(legend.position='none') 

plotly::ggplotly(p)
# OR simply
plot(y ~ ds, data, type = "l")

# Building a model ----

# Let's build a model and predict values for the next year

## Simple model ----

model <-
  prophet(data)
future <- make_future_dataframe(model, freq = 'day', periods = 365)
forecast <- predict(model, future)
head(forecast)

### Validate simple model ----

# Making 25 forecasts with cutoffs between 2010-01-05 and 2021-11-03
# initial 40 * 365
model_cv <- cross_validation(model, initial = 14600, period = 180, horizon = 365, units = 'days')
metrics <- performance_metrics(model_cv)

#calculate MSE
mse <- mean((model_cv$y - model_cv$yhat)^2)
rmse <- sqrt(mse)
mse
rmse

# calculate MAPe, not good cause actual_values = 0
mean(abs((model_cv$y-model_cv$yhat)/ifelse(model_cv$y==0, model_cv$yhat, model_cv$y))) * 100

# Validate with cutoff (what does this really mean?)
cutoffs <- as.Date(c('2013-02-15', '2013-08-15', '2014-02-15'))
model_cv2 <- cross_validation(model, initial = 14600, cutoffs = cutoffs, horizon = 365, units = 'days')
metrics2 <- performance_metrics(model_cv2)
# MSE and RMSE
mse2 <- mean((model_cv2$y - model_cv2$yhat)^2)
rmse2 <- sqrt(mse2)
mse2
rmse2

## BoxCox model ----

data2 <- data
# The BoxCox.lambda() function will choose a value of lambda
lam <- BoxCox.lambda(data2$value, method = "guerrero")
data2$y = BoxCox(data2$value, lam)
#data.m <- melt(data, measure.vars=c("value", "y"))

# Build a model
model2 <-
 prophet(data2)
future <- make_future_dataframe(model2, freq = 'day', periods = 365)
forecast2 <- predict(model2, future)

### Validate BoxCox model
# Making 25 forecasts with cutoffs between 2010-01-05 and 2021-11-03
# initial 40 * 365
bc_model_cv <- cross_validation(model2, initial = 14600, period = 180, horizon = 365, units = 'days')
bc_metrics <- performance_metrics(bc_model_cv)

#calculate MSE
bc_mse <- mean((bc_model_cv$y - bc_model_cv$yhat)^2)
bc_rmse <- sqrt(bc_mse)
bc_mse
bc_rmse

# Returning original value (if this method chosen)
inverse_forecast <- forecast2
inverse_forecast <- tibble::column_to_rownames(inverse_forecast, var = "ds")
inverse_forecast$yhat_untransformed = InvBoxCox(forecast2$yhat, lam)


# R
plot(model, forecast) + add_changepoints_to_plot(model)
prophet_plot_components(model, forecast)

plot_cross_validation_metric(model_cv2, metric = 'rmse')

ggplot(data = data %>% filter(time >= '2013-02-14')) +
  geom_line(aes(x = ds, y = y), color = "dark blue") +
  labs(title = "Daily average temperatures in Utsjoki from 1970-",
       x = "Time", y = "Celcius") +
  theme_minimal() +
  theme(legend.position='none') 

ggplot() + 
  geom_line(data=data %>% filter(time >= '2013-02-16' & time <= '2015-02-15'), aes(x=ds, y=y), color='black') + 
  geom_line(data=model_cv2, aes(x=ds, y=y), color='red')


# Tuning the parameters ----

param_grid <- data.frame(
  'changepoint_prior_scale'=c(0.001, 0.01, 0.1, 0.5),
  'seasonality_prior_scale'=c(0.01, 0.1, 1.0, 10.0))

# Generate all combinations of parameters

all_params <- data.frame(matrix(nrow = 0, ncol = 2))
names(all_params) <- c('changepoint_prior_scale', 'seasonality_prior_scale')
for (i in 1:nrow(param_grid)) {
  for (j in 1:nrow(param_grid)) {
    all_params <- all_params %>% add_row(changepoint_prior_scale=param_grid[i, 1], seasonality_prior_scale=param_grid[j, 2]) 
  }
}
rmses <- c()  # Store the RMSEs for each params here

# Use cross validation to evaluate all parameters
for (row in 1:nrow(all_params)){
  m <- prophet(data, 
               changepoint.prior.scale = all_params[row, 1], 
               seasonality.prior.scale = all_params[row, 2])
  future <- make_future_dataframe(m, freq = 'day', periods = 365)
  forecast <- predict(model, future)
  df_cv = cross_validation(m, initial = 14600, horizon=365, units = 'days')
  df_p = performance_metrics(df_cv, rolling_window=1)
  rmses <- append(rmses, df_p["rmse"])
  
}


  #rmses.append(df_p['rmse'].values[0])

# Find the best parameters
rmses_vector <- unlist(rmses)

tuning_results <- all_params
tuning_results['rmse'] <- rmses_vector
tuning_results %>% arrange(rmse)

saveRDS(tuning_results, "tuning_results.rds")

# Final model ----

final_model <-
  prophet(data, changepoint.prior.scale = 0.001, seasonality.prior.scale = 10,
          weekly.seasonality = FALSE)
future <- make_future_dataframe(final_model, freq = 'day', periods = 365)
forecast <- predict(final_model, future)

model_cv <- cross_validation(final_model, initial = 14600, period = 180, horizon = 365, units = 'days')
metrics <- performance_metrics(model_cv)

#calculate MSE
mse <- mean((model_cv$y - model_cv$yhat)^2)
rmse <- sqrt(mse)
mse
rmse

# Form data for Shiny ----

saveRDS(data, "miniproject_data.rds")
saveRDS(forecast, "miniproject_forecast.rds")
saveRDS(final_model, "final_model.rds")

tail(forecast)
tail(data)
## plots for shiny ----

# library plots
plot(final_model, forecast) + add_changepoints_to_plot(final_model)
prophet_plot_components(final_model, forecast)

# My plot

p <- ggplot(data = data %>% filter(time >= '2022-01-01')) +
  geom_line(data = data, aes(x = ds, y = y), color = "dark blue") +
  geom_line(data = forecast %>% filter(ds >= '2022-11-4'), aes(x = ds, y = yhat), color = "red") +
  geom_line(data = forecast %>% filter(ds >= '2022-11-4'), aes(x = ds, y = yhat_lower), color = "light grey") +
  geom_line(data = forecast %>% filter(ds >= '2022-11-4'), aes(x = ds, y = yhat_upper), color = "light grey") +
  labs(title = "Daily average temperatures in Utsjoki from 1970-",
       x = "Time", y = "Celcius") +
  theme_minimal() +
  theme(legend.position='none') 

plotly::ggplotly(p)