## ----include=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------
rm(list=ls())

if (!require("pacman")) install.packages("pacman")
pacman::p_load("ggplot2", "dplyr", "GGally", "caret","glmnet","DAAG","lubridate",
               "reshape2","smooth","DT","rnaturalearth","caret","sf","sp","tidyverse",
               "maps","spatstat","forecast","readr","zoo","TTR","rgeos")

covid_data <- read.csv("covid_19_data.csv", header = TRUE)
# Convert the ObservationDate from "factor" to "Date" class to make plotting much easier
covid_data$ObservationDate <- as.Date(covid_data$ObservationDate, format='%m/%d/%Y')


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
head(covid_data)
summary(covid_data)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Filter data for March 11
covid_data_recent <- covid_data %>% filter(ObservationDate =="2020-03-11")
head(covid_data_recent)
# Show number of confirmed
sum(covid_data_recent$Confirmed)
# Show number of recovered
sum(covid_data_recent$Recovered)
# Show number of deaths
sum(covid_data_recent$Deaths)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Show number of observations over time
covid_observations <- covid_data %>% 
  group_by(ObservationDate) %>%
  summarize(count =n())
head(covid_observations %>% arrange(desc(count)))

# Plot the number of observations over time
covid_observations %>% ggplot(aes(x=ObservationDate, y=count, group=1)) +
  geom_point() + geom_line() +  labs(x = "Observation Dates", y = "Count of observations", 
              title = "Count of observations over time")


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Show number of confirmed cases over time
covid_confirmed <- covid_data %>% 
  group_by(ObservationDate) %>%
  summarize(total_confirmed = sum(Confirmed))
head(covid_confirmed %>% arrange(desc(total_confirmed)))

# Plot the number of confirmed cases over time
covid_confirmed %>% ggplot(aes(x=ObservationDate, y=total_confirmed, group=1)) +
  geom_point() + geom_line() +  labs(x = "Observation Dates", y = "Count of confirmed cases", 
              title = "Count of confirmed cases over time")


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Show number of deaths cases over time
covid_deaths <- covid_data %>% 
  group_by(ObservationDate) %>%
  summarize(total_deaths = sum(Deaths))
head(covid_deaths %>% arrange(desc(total_deaths)))

# Plot the number of deaths cases over time
covid_deaths %>% ggplot(aes(x=ObservationDate, y=total_deaths, group=1)) +
  geom_point() + geom_line() +  labs(x = "Observation Dates", y = "Count of deaths", 
              title = "Count of deaths over time")


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Show number of recovered cases over time
covid_recovered <- covid_data %>% 
  group_by(ObservationDate) %>%
  summarize(total_recovered = sum(Recovered))
head(covid_recovered %>% arrange(desc(total_recovered)))

# Plot the number of recovered cases over time
covid_recovered %>% ggplot(aes(x=ObservationDate, y=total_recovered, group=1)) +
  geom_point() + geom_line() +
  labs(x = "Observation Dates", y = "Count of recovered", 
              title = "Count of recovered over time")


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Show number of all cases over time
covid_all <- covid_data %>% 
  group_by(ObservationDate) %>%
  summarize(total_confirmed=sum(Confirmed), total_recovered=sum(Recovered), total_deaths=sum(Deaths))
head(covid_all)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
covid_all_melted <- melt(covid_all, id.var='ObservationDate')
head(covid_all_melted)

covid_all_melted %>% ggplot(aes(x=ObservationDate, y=value, col=variable)) + 
  geom_area(aes(fill=variable))


## ---- fig.width=8---------------------------------------------------------------------------------------------------------------------------------------------------
# Show top 10 Country.Region with cases
covid_country_top10 <- covid_data_recent %>% 
  group_by(Country.Region, Province.State, Confirmed) %>%
    summarize(total_confirmed = sum(Confirmed)) %>% arrange(desc(total_confirmed))
covid_country_top10 <- covid_country_top10[1:10,1:3]
covid_country_top10

# Plot the number of country cases over time
covid_country_top10 %>% ggplot(aes(Country.Region, Confirmed)) +
  geom_bar(stat="identity", aes(fill=Province.State)) + 
  theme(legend.position = "right")


## ---- fig.width=6, fig.height=12------------------------------------------------------------------------------------------------------------------------------------
# Load our time series data
covid_data_timeseries <- read_csv(str_c('time_series_covid_19_confirmed.csv'))
# Due to plotting issues of all 50 maps, we will only get 10% and 90% percentile of date points (i.e., Day 1 to 5, and Day 45 to 50)
covid_data_timeseries <- covid_data_timeseries[,-15:-44]
# Set the daily entries count
n_times <- ncol(covid_data_timeseries) - 4
n_times
# Prepare timeseries data, parse latitude longtiude data
covid_data_ts_latlong <- covid_data_timeseries
colnames(covid_data_ts_latlong) <- c(colnames(covid_data_ts_latlong)[1:4],
  str_c('Global Map: ', str_pad(as.character(1:n_times), 2, 'left', '0'),
        str_c(' - ', colnames(covid_data_ts_latlong)[5:(4 + n_times)])))
# Pivot our latitude longitude data for ggplot use
covid_data_ts_latlong_pivot <- covid_data_ts_latlong %>% pivot_longer(names_to = 'Confirmed.Time',
                                                  values_to = 'Confirmed',
                                                  cols = colnames(covid_data_ts_latlong)[5:(4 + n_times)])
head(covid_data_ts_latlong_pivot)
# Use ggplot map_data world, to display a global map
world <- map_data('world')
ggplot(legend = FALSE) +
  # Plot our map
  geom_polygon(data = world, aes(x = long, y = lat, group = group),
               color = 'black', fill = 'lightgreen') +  xlab('') + ylab('') + 
  # Plot points of our confirmed cases using latitude/longitude data
  geom_point(data = covid_data_ts_latlong_pivot, fill = 'red', color = 'black',
             shape = 21, alpha = 0.6,
             aes(x = Long, y = Lat, fill = Confirmed, size = Confirmed)) +
  theme_minimal() +
  # Set the scale from 0 to 30 for the shapes in the map
  scale_size_continuous(range = c(0, 15)) + ggtitle('Occurrences Map - COVID19') +
  theme(text = element_text(size = 5), legend.position = 'top',
        panel.background = element_rect(fill='lightblue', colour='black')) +
  facet_wrap(~Confirmed.Time, ncol = 2)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Since the default ts() function is not a good tool to use for daily sampled data,
# - we will use 'zoo' package for irregular time series data. This takes care of-
# indexing for time-series data. Create a daily Date object - helps our work on dates
inds <- seq(as.Date("2020-01-22"), as.Date("2020-03-11"), by = "day")
# We link the time-series with our confirmed cases
covid_confirmed_ts <- zoo(covid_confirmed$total_confirmed, inds)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Initialize our Naive model. Forecast for the next 50 days
model_naive <- naive(covid_confirmed_ts, h = 50)
naive_forecast_max <- max(model_naive$mean)
naive_forecast_max

# The plot though will cause an issue as the x-axis is in days since the epoch (1970-01-01)
# So we need to suppress the auto plotting of this axis and then plot our own
plot(model_naive, xaxt ="n")
Axis(inds, side = 1)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
# For simple exponential smoothing we also plug in our time series data and predict for the next 50 days
model_ses <- ses(covid_confirmed_ts, h = 50, alpha = 0.99, lambda="auto")
ses_forecast_max <- max(model_ses$mean)
ses_forecast_max

plot(model_ses, xaxt ="n")
Axis(inds, side = 1)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Initialize our HoltWinters model, the es() function constructs a model and returns forecast and fitted values.
# "AAM" denotes HoltWinters model, h is our forecast length, interval allows for prediction intervals
model_AAM <- es(covid_confirmed_ts, "AAM", h=50, interval=TRUE, silent = "none")
holtwinters_forecast_max <- max(model_AAM$forecast)
holtwinters_forecast_max


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Initialize our ARIMA model. It is straightforward as we only have to plug in our time series
model_arima <- auto.arima(covid_confirmed_ts)

# Forecast next 50 days for the ARIMA model
forecast_arima <- forecast::forecast(model_arima, h=50)
arima_forecast_max <- max(forecast_arima$mean)
arima_forecast_max
plot(forecast_arima, xaxt ="n")
Axis(inds, side = 1)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Look at the Naive model performance and maximum forecast
summary(model_naive)
# Residual sd: 2431.0251 
# Error measures:
#                    ME    RMSE      MAE      MPE     MAPE MASE      ACF1
# Training set 2557.347 3511.31 2557.347 9.697016 9.697016    1 0.3318278

# Look at the SES model performance and maximum forecast
summary(model_ses)
#     AIC     AICc      BIC 
# 674.1495 674.4048 677.9736 
# 
# Error measures:
#                    ME     RMSE      MAE     MPE     MAPE      MASE      ACF1
# Training set 2530.505 3499.967 2530.536 9.58068 9.586328 0.9895163 0.3540446

# Look at the HoltWinters model performance and maximum forecast
summary(model_AAM)
# Loss function type: MSE; Loss function value: 5235844.5326
# Error standard deviation: 2385.61
# Information criteria:
#      AIC     AICc      BIC     BICc 
# 923.4458 924.3347 931.0939 932.8326 

# Look at the ARIMA model performance and maximum forecast
summary(model_arima)
# AIC=901.81   AICc=902.34   BIC=907.48
# 
# Training set error measures:
#                   ME     RMSE      MAE       MPE     MAPE    MASE        ACF1
# Training set 21.2316 2231.458 1176.584 -13.61523 16.47069 0.46008 -0.01570852

