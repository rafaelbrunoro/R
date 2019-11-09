state <- read.csv('state.csv')

quantile(state$Murder.Rate,p=c(0.05,0.25,0.5,0.75,0.95))
head(state)
boxplot(state$Population/1000000, ylab = "Population (millions)")

library(ggplot2)
ggplot(state, aes(x = Population, y = Murder.Rate))+
  geom_boxplot()
breaks <- seq(from=min(state[["Population"]]),
              to=max(state[["Population"]]), length=11)
pop_freq <- cut(state[["Population"]], breaks = breaks,
                right=TRUE, include.lowest=TRUE)
table(pop_freq)
hist(state$Population, breaks=breaks)
ggplot(state, aes(x = Population))+
  geom_histogram(bins = 5)

library(datasets)
data("iris")


ggplot(iris,aes(x=Sepal.Width,fill=Species))+
  geom_density(alpha=0.7)

barplot(iris$Sepal.Length, iris$Species)
