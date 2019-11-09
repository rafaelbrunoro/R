library(ggplot2)
library(dplyr)

state = read.csv('countries of the world.csv')
head(state)

state2 <- na.omit(state)
ggplot(na.omit(state2), aes(x = state2$GDP....per.capita.))+
  geom_histogram()

ggplot(state, aes(x = state$GDP....per.capita.))+
  geom_bar()+
  facet_wrap(~Region)
  