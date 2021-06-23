library(ggplot2)
library(mice)
library(forecast)
library(seasonal)
library(urca)

data <- read.csv("https://raw.githubusercontent.com/dmoste/DATA624/main/project_1_data.csv", header = TRUE)

s01 <- subset(data, group == "S01")
s02 <- subset(data, group == "S02")
s03 <- subset(data, group == "S03")
s04 <- subset(data, group == "S04")
s05 <- subset(data, group == "S05")
s06 <- subset(data, group == "S06")

# Just working with s01v02 due to lack of missing values
s01_v02_ts <- ts(s01[1:1622,4])

# Trying to get a visual of the data
autoplot(s01_v02_ts)
ggAcf(s01_v02_ts)

# Checking what computer software thinks about differencing
nsdiffs(s01_v02_ts)
ndiffs(s01_v02_ts)

# Checking for stationarity. It looks like after a single difference, the data is stationary (test statistic is much smaller than 1% of critical value)
s01_v02_ts %>% diff() %>% ggtsdisplay(main="")

s01_v02_ts %>% diff() %>% ur.kpss() %>% summary()

# Just trying an arima fit real quick to see how it looks (Since data is stationary)
(fit <- auto.arima(s01_v02_ts, seasonal = FALSE,
                    stepwise = FALSE, approximation = FALSE))

checkresiduals(fit)

fit %>% forecast(h = 140) %>% autoplot()