library(datasets)

data(iris)
iris
mean(iris$Sepal.Width)
mean(iris$Sepal.Width, trim = 0.1)
weighted.mean()
median(iris$Sepal.Width)
quantile(iris$Sepal.Width)
sd(iris$Sepal.Width)
IQR(iris$Sepal.Width)


library(ggplot2)
msleep
mean(msleep$sleep_total, trim = 0.1)
median(msleep$sleep_total)
quantile(na.omit(msleep)$sleep_rem))
IQR(msleep$sleep_total)
mad(msleep$sleep_total)
library(dplyr)
state <- read.csv('genero-nomes-censo2010.csv')
head(state)
mean(state$frequency_total)
head(arrange(state))
help("arrange")
