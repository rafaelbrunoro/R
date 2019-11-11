library(ggplot2)
library(dplyr)

state = read.csv('countries of the world.csv')
head(state)

state2 <- na.omit(state)
ggplot(na.omit(state2), aes(x = state2$GDP....per.capita.))+
  geom_histogram()

agregado <- aggregate(state2$GDP....per.capita.,by=list(Category=state2$Region), FUN=sum)


esquisse::esquisser()

ggplot(agregado) +
 aes(x = Category, weight = x) +
 geom_bar(fill = "#781c6d") +
 labs(x = "Região", y = "GDP", title = "GDP x Região") +
 coord_flip() +
 theme_dark()


state2 <- state2 %>%
  filter(!(Climate %in% ""))

ggplot(state2) +
  aes(x = Region, y = GDP....per.capita.) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

state3 <- state2[,3:20]

state3 = state3(decimal_mark = ",")

cor(, y = NULL, use = "everything",
    method = c("pearson", "kendall", "spearman"))



