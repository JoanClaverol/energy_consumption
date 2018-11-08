require(pacman)
p_load(fpp2, tidyverse, lubridate)

data <- read_rds("Datasets/CleanWeatherData.rds")

data <- data %>% filter(year(date)!=2011)

ts <- ts(data$ActiveEnergy, start = 2007, frequency = 365.25)
h <- 90
# Average method
plot(meanf(ts, h)) # h is the forecast horizon, 90 days

# Naïve method
# set all forecasts to be the value of the last observation. This method works remarkably well for many economic and financial time series.
plot(naive(ts, h))
plot(rwf(ts, h))
# Because a naïve forecast is optimal when data follow a random walk (see Section 8.1), these are also called random walk forecasts.

# Seasonal naïve method
# A similar method is useful for highly seasonal data. In this case, we set each forecast to be equal to the last observed value from the same season of the year
plot(snaive(ts, h))
# very good predictions 

# Drift method
# A variation on the naïve method is to allow the forecasts to increase or decrease over time, where the amount of change over time (called the drift) is set to be the average change seen in the historical data. 
plot(rwf(ts, h, drift=TRUE))


# Plot some seasonal forecasts 
autoplot(ts) +
  autolayer(meanf(ts, h),
            series="Mean", PI=FALSE) +
  autolayer(naive(ts, h),
            series="Naïve", PI=FALSE) +
  autolayer(snaive(ts, h),
            series="Seasonal naïve", PI=FALSE) +
  ggtitle("Forecasts for quarterly beer production") +
  xlab("Year") + ylab("Megalitres") +
  guides(colour=guide_legend(title="Forecast"))

# Plot some non-seasonal forecasts 
autoplot(goog200) +
  autolayer(meanf(goog200, h=40),
            series="Mean", PI=FALSE) +
  autolayer(rwf(goog200, h=40),
            series="Naïve", PI=FALSE) +
  autolayer(rwf(goog200, drift=TRUE, h=40),
            series="Drift", PI=FALSE) +
  ggtitle("Google stock (daily ending 6 Dec 2013)") +
  xlab("Day") + ylab("Closing Price (US$)") +
  guides(colour=guide_legend(title="Forecast"))

# Calendar adjustments
# Some of the variation seen in seasonal data may be due to simple calendar effects. In such cases, it is usually much easier to remove the variation before fitting a forecasting model. The monthdays() function will compute the number of days in each month or quarter.

data %>% 
  mutate(year = year(date), month = month(date, label = T, locale = "us")) %>% group_by(year, month) %>% 
  summarise(sum = sum(ActiveEnergy)) -> data2

ts_month <- ts(data2[,3], start = 2007, frequency = 12)

ts_month_norm <- cbind(ts_month, 
                       DailyAverage = ts_month/monthdays(ts_month))
autoplot(ts_month_norm, facet=TRUE) +
  xlab("Years") + ylab("Watts/h") +
  ggtitle("Active energy")
# Population adjustments
# Inflation adjustments
# Mathematical transformations
lambda <- BoxCox.lambda(ts_month)
autoplot(BoxCox(ts_month, lambda))
