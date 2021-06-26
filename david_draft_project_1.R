library(ggplot2)
library(mice)
library(forecast)
library(seasonal)
library(urca)

data <- read.csv("https://raw.githubusercontent.com/dmoste/DATA624/main/raw_data.csv", header = TRUE)

s01_v01_ts <- ts(window(data$S01Var01, end = 1622))
s01_v02_ts <- ts(window(data$S01Var02, end = 1622))
s02_v02_ts <- ts(window(data$S02Var02, end = 1622))

# Check for NAs and general data summary
summary(s01_v01_ts)
summary(s01_v02_ts)
summary(s02_v02_ts)

# Impute missing values
s01_v01_ts <- forecast::na.interp(s01_v01_ts)

# Trying to get a visual of the data
autoplot(s01_v01_ts)
autoplot(s01_v02_ts)
autoplot(s02_v02_ts)

# Try a holt es model (since there is clearly a trend)
fc <- holt(s01_v01_ts, h = 140)
autoplot(fc)

#######################

# Checking for stationarity. It looks like after a single difference, the data is stationary (test statistic is much smaller than 1% of critical value)
s01_v02_ts %>% diff() %>% ggtsdisplay(main="")
s01_v02_ts %>% diff() %>% ur.kpss() %>% summary()

# Just trying an arima fit real quick to see how it looks (Since data is stationary)
(fit <- auto.arima(s01_v02_ts, seasonal = FALSE,
                   stepwise = FALSE, approximation = FALSE))

checkresiduals(fit)

fit %>% forecast(h = 140) %>% autoplot()

# What happens if I log the series?
log_s01_v02_ts <- log(s01_v02_ts)

log_s01_v02_ts %>% diff() %>% ggtsdisplay(main="")
log_s01_v02_ts %>% diff() %>% ur.kpss() %>% summary()

# Just trying an arima fit real quick to see how it looks (Since data is stationary)
(fit <- auto.arima(log_s01_v02_ts, seasonal = FALSE,
                    stepwise = FALSE, approximation = FALSE))

checkresiduals(fit)

fit %>% forecast(h = 140) %>% autoplot()

###################

# Checking for stationarity. It looks like after a single difference, the data is stationary (test statistic is much smaller than 1% of critical value)
s02_v02_ts %>% diff() %>% ggtsdisplay(main="")
s02_v02_ts %>% diff() %>% ur.kpss() %>% summary()

# Just trying an arima fit real quick to see how it looks (Since data is stationary)
(fit <- auto.arima(s02_v02_ts, seasonal = FALSE,
                   stepwise = FALSE, approximation = FALSE))

checkresiduals(fit)

fit %>% forecast(h = 140) %>% autoplot()

# What happens if I log the series?
log_s02_v02_ts <- log(s01_v02_ts)

log_s02_v02_ts %>% diff() %>% ggtsdisplay(main="")
log_s02_v02_ts %>% diff() %>% ur.kpss() %>% summary()

# Just trying an arima fit real quick to see how it looks (Since data is stationary)
(fit <- auto.arima(log_s02_v02_ts, seasonal = FALSE,
                   stepwise = FALSE, approximation = FALSE))

checkresiduals(fit)

fit %>% forecast(h = 140) %>% autoplot()
