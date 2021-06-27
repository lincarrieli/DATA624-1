library(ggplot2)
library(forecast)
library(urca)
library(tidyverse)

data <- read.csv("https://raw.githubusercontent.com/dmoste/DATA624/main/Project1/raw_data.csv", header = TRUE)

s01_v01_ts <- ts(window(data$S01Var01, end = 1622))
s01_v02_ts <- ts(window(data$S01Var02, end = 1622))
s02_v02_ts <- ts(window(data$S02Var02, end = 1622))

# Check for NAs and general data summary
summary(s01_v01_ts)
summary(s01_v02_ts)
summary(s02_v02_ts)

# Impute missing values from s01_v01
s01_v01_ts <- forecast::na.interp(s01_v01_ts)

# Trying to get a visual of the data
autoplot(s01_v01_ts)
autoplot(s01_v02_ts)
autoplot(s02_v02_ts)

# Checking if volume changes significantly after gaps
gaps <- diff(data$SeriesInd) > 1
gaps <- c(FALSE, gaps)
gaps_df <- data.frame("SeriesInd" = data$SeriesInd, "AfterGap" = gaps, "S01V02" = data$S01Var02)

after_gap <- gaps_df %>%
  filter(AfterGap) %>%
  drop_na()
all_others <- gaps_df %>%
  filter(AfterGap == FALSE) %>%
  drop_na()

gap_comparison <- data.frame("Situation" = c("After Gap", "Others"),
                             "Mean" = c(mean(after_gap$S01V02),mean(all_others$S01V02)),
                             "sd" = c(sd(after_gap$S01V02),sd(all_others$S01V02)),
                             "Variance" = c(var(after_gap$S01V02),var(all_others$S01V02)))

# Check performance of random walk
rwf_nodrift <- tsCV(s01_v01_ts, rwf, drift = FALSE, h = 1)
rmse_rwf_nodrift <- sqrt(mean(rwf_nodrift^2, na.rm = TRUE))
rwf_drift <- tsCV(s01_v01_ts, rwf, drift = TRUE, h = 1)
rmse_rwf_drift <- sqrt(mean(rwf_drift^2, na.rm = TRUE))
meanf <- tsCV(s01_v01_ts, meanf, h = 1)
rmse_meanf <- sqrt(mean(meanf^2, na.rm = TRUE))

# The random walk with no drift has the lowest rmse

# Try ses
s01_v01_ses <- ses(s01_v01_ts, h = 140)
summary(s01_v01_ses)

# The rmse is essentially the same as a random walk with no drift

# Try a holt model
s01_v01_holt <- holt(s01_v01_ts, h = 140)
summary(s01_v01_holt)

# The rmse is still the same

# Checking arima
s01_v01_ts %>% diff() %>% ggtsdisplay(main="")
s01_v01_ts %>% diff() %>% ur.kpss() %>% summary()

s01_v01_arima <- auto.arima(s01_v01_ts, seasonal = FALSE,
                            stepwise = FALSE, approximation = FALSE)

summary(s01_v01_arima)
checkresiduals(s01_v01_arima)

# The rmse is slightly better with the arima model.

#######################

# Check performance of random walk
rwf_nodrift <- tsCV(s01_v02_ts, rwf, drift = FALSE, h = 1)
rmse_rwf_nodrift <- sqrt(mean(rwf_nodrift^2, na.rm = TRUE))
rwf_drift <- tsCV(s01_v02_ts, rwf, drift = TRUE, h = 1)
rmse_rwf_drift <- sqrt(mean(rwf_drift^2, na.rm = TRUE))
meanf <- tsCV(s01_v02_ts, meanf, h = 1)
rmse_meanf <- sqrt(mean(meanf^2, na.rm = TRUE))

# The random walk with no drift has the lowest rmse

# Try ses
s01_v02_ses <- ses(s01_v02_ts, h = 140)
summary(s01_v02_ses)

# The rmse is much better with ses

# Try a holt model
s01_v02_holt <- holt(s01_v02_ts, h = 140)
summary(s01_v02_holt)

# The rmse is still better with ses

# Checking arima
s01_v02_ts %>% diff() %>% ggtsdisplay(main="")
s01_v02_ts %>% diff() %>% ur.kpss() %>% summary()

s01_v02_arima <- auto.arima(s01_v02_ts, seasonal = FALSE,
                            stepwise = FALSE, approximation = FALSE)

summary(s01_v02_arima)
checkresiduals(s01_v02_arima)

# This is the lowest rmse

#######################

# Check performance of random walk
rwf_nodrift <- tsCV(s02_v02_ts, rwf, drift = FALSE, h = 1)
rmse_rwf_nodrift <- sqrt(mean(rwf_nodrift^2, na.rm = TRUE))
rwf_drift <- tsCV(s02_v02_ts, rwf, drift = TRUE, h = 1)
rmse_rwf_drift <- sqrt(mean(rwf_drift^2, na.rm = TRUE))
meanf <- tsCV(s02_v02_ts, meanf, h = 1)
rmse_meanf <- sqrt(mean(meanf^2, na.rm = TRUE))

# The random walk with no drift has the lowest rmse

# Try ses
s02_v02_ses <- ses(s02_v02_ts, h = 140)
summary(s02_v02_ses)

# The rmse is much better with ses

# Try a holt model
s02_v02_holt <- holt(s02_v02_ts, h = 140)
summary(s02_v02_holt)

# The rmse is still better with ses

# Checking arima
s02_v02_ts %>% diff() %>% ggtsdisplay(main="")
s02_v02_ts %>% diff() %>% ur.kpss() %>% summary()

s02_v02_arima <- auto.arima(s02_v02_ts, seasonal = FALSE,
                            stepwise = FALSE, approximation = FALSE)

summary(s02_v02_arima)
checkresiduals(s02_v02_arima)

# This is the lowest rmse

# Forecasts
s01_v01_forecast <- rwf(s01_v01_ts, h = 140)
s01_v02_forecast <- forecast(s01_v02_arima, h = 140)
s02_v02_forecast <- forecast(s02_v02_arima, h = 140)

autoplot(s01_v01_ts) +
  autolayer(s01_v01_forecast)
autoplot(s01_v02_ts) +
  autolayer(s01_v02_forecast)
autoplot(s02_v02_ts) +
  autolayer(s02_v02_forecast)
