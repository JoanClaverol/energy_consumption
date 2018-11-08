# Energy consumption modalization

# Libraries and data ----
require(pacman)
p_load(tidyverse, lubridate, forecast)

# Uploading data
df3 <- read_rds("Datasets/CleanTotalData.rds")
df.weather <- read_rds("Datasets/CleanWeatherData.rds")

# Prediction with weathar dataset ----

# we are going to create a linear model to predict the active coume of the house, and will take into consideration two main variables, avg temperature. 

mod.lm <- lm(ActiveEnergy ~ Avg.Temp, data = df.weather)
plot(mod.lm)

# Prepare and analyse the data ----

# We are going to start the analysys from 2017-01-01
df3 <- df3 %>%
  year(year = year(DateTime),month = month(DateTime), week = week(DateTime)) %>% 
  group_by(year,month,week) %>%
  summarise(mean = mean(ActiveEnergy)) 

# Understanding the distribution of the results
diff(df3$mean, lag = 365) -> d
hist(d, probability = T, ylim = c(0,0.8), breaks = 30, main = "Energy consumption", col = "green")
lines(density(d), lwd=2)

# time series creation ----


df.ts <- ts(df3$mean, start = c(2007), frequency = 52)
df.decompose <- decompose(df.ts)

# Looking for the trend of the time series
df.season.adjusted <- df.ts - df.decompose$seasonal
plot(df.ts)
lines(df.season.adjusted, col = "blue", lwd = 3)


# Modalization ----

####    Forecasting   ####
# Doing a linear model with the tsml() function
df.ts.hw <- HoltWinters(df.ts, gamma = TRUE)
# Haremos la predicción a dos anyos vista con el método de holt winters
df.forecast.hw <- forecast(df.ts.hw, h = 24)
plot(df.forecast.hw, main = "Forecast from HoltWinters in months")

# Creación de modelos autorregresivo de media móvil. Con arima intentaremos crear un modelo que nos permita ajustar diferentes partes del mismo. Porque surt igual???
df.ts.arima <- auto.arima(df.ts)
df.forecast.arima <- forecast(df.ts.arima, h = 48)
plot(df.forecast.arima, main = "Forecast 2011: model Arima by month")

####    Forecasting methods   ####
# Linear model time series
df.ts.lm <- tslm(df.filter$Mean~season+trend,df.ts)
df.forecast.lm<- forecast(df.ts.lm,h=12)
autoplot(df.forecast.lm)
# Average method
plot(meanf(df.ts, h = 12))
# Na?ve method, forecasts the next values using the last values observed. The problem with whtis prediction is that is not taking into account the seasonality of the serie not the trend.
plot(naive(df.ts, h = 12))
# Seasonal na?ve method, it shows the seasonal pattern, whihc will take into account the values of the last cycle
df.forecast.naive <- snaive(df.ts, h = 12)
plot(df.forecast.naive)
# Drift method, which predictions will be the result of drawing a line between the first and the last line of the serie
plot(df.ts, h = 12, drift = TRUE) # it gives problems
# Holt winters method, which will combine the season na?ve and drift methods
plot(forecast(HoltWinters(df.ts), h = 12))

####    Forecasting vs 2010   ####
# Creation and preparation of the training and dataset
df.testing <- filter(.data = df, 
                     df$year >= 2010)
df.training <- filter(.data = df,
                      df$year >= 2007 & df$year < 2010)
df.training <- df.training %>% 
  group_by(year = year, month = month) %>%
  summarise(Mean = mean(Global_active_power))
df.testing <- df.testing %>% 
  group_by(year = year, month = month) %>%
  summarise(Mean = mean(Global_active_power))
# Creation of the time series
df.training.ts <- ts(df.training$Mean, start = c(2007), frequency = 12)
df.testing.ts <- ts(df.testing$Mean, start = c(2010), frequency = 12)
# Creation of a linaer model to predict
df.training.ts.lm <- tslm(df.training$Mean~season+trend,df.training.ts)
df.training.forecast.lm <- forecast(df.training.ts.ml, h = 12)
plot(df.training.forecast.model, main = "Forecast vs 2010: Linear model")
lines(df.testing.ts,col = "black", lwd = 2)
# Creation of an arima model to predict
df.training.ts.arima <- auto.arima(df.training.ts)
df.training.forecast.arima <- forecast(df.training.ts.arima, h = 12)
plot(df.training.forecast.model, main = "Mean by month forecast vs 2010: Arima model", cex.main = 2)
lines(df.testing.ts,col = "black", lwd = 2)
# Creation of an HoltWinters model to predict
df.training.ts.hw <- HoltWinters(df.training.ts)
df.training.forecast.hw <- forecast(df.training.ts.hw, h = 12)
plot(df.training.forecast.model, main = "Forecast active power_07/09 vs real active power 2010: HoltWinters model", cex.main = 2)
lines(df.testing.ts,col = "black", lwd = 2)

# Differences between the foreast with arima model and 2010
df.testing.results <- df.testing.ts - df.training.forecast.lm$mean
sum(df.testing.results)# the small result is for hw to predict 2010

