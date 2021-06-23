library(ggplot2)
library(mice)
library(forecast)

data <- read.csv("project_1_data.csv", header = TRUE)

s01 <- subset(data, group == "S01")
s02 <- subset(data, group == "S02")
s03 <- subset(data, group == "S03")
s04 <- subset(data, group == "S04")
s05 <- subset(data, group == "S05")
s06 <- subset(data, group == "S06")

ggplot(s01, aes(SeriesInd)) +
  geom_line(aes(y = Var01, color = "Var01")) +
  geom_line(aes(y = Var02/1000000, color = "Var02"))

frequency(s01$Var01)

ggAcf(s01$Var01)
ggPacf(s01$Var01)

ggplot(s02, aes(SeriesInd)) +
  geom_line(aes(y = Var02/10000000, color = "Var02")) +
  geom_line(aes(y = Var03, color = "Var03"))

ggplot(s03, aes(SeriesInd)) +
  geom_line(aes(y = Var05, color = "Var05")) +
  geom_line(aes(y = Var07, color = "Var07"))

ggplot(s04, aes(SeriesInd)) +
  geom_line(aes(y = Var01, color = "Var01")) +
  geom_line(aes(y = Var02/10000000, color = "Var02"))

ggplot(s05, aes(SeriesInd)) +
  geom_line(aes(y = Var02/1000000, color = "Var02")) +
  geom_line(aes(y = Var03, color = "Var03"))

ggplot(s06, aes(SeriesInd)) +
  geom_line(aes(y = Var05, color = "Var05")) +
  geom_line(aes(y = Var07, color = "Var07"))
