library(ggplot2)
library(dplyr)

state = read.csv("C:/Users/rafael.brunoro/Downloads/R-master/R-master/countries of the world.csv", dec = ",")
head(state)

state2 <- na.omit(state)
ggplot(state2, aes(x = state2$GDP....per.capita.))+
  geom_histogram()

agregado <- aggregate(state2$GDP....per.capita.,by=list(Category=state2$Region), FUN=sum)


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

C1 <- cor(state3, y = NULL, use = "everything",
          method = c("pearson", "kendall", "spearman")
          
          
          panel.lm <- function (x, y, col = par("col"), bg = NA, pch = par("pch"), 
                                cex = 1, col.line="red") {
            points(x, y, pch = pch, col = col, bg = bg, cex = cex)
            ok <- is.finite(x) & is.finite(y)
            if (any(ok)) {
              abline(lm(y[ok]~x[ok]), col = col.line)
            }
          }
          
          panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
          {
            usr <- par("usr"); on.exit(par(usr))
            par(usr = c(0, 1, 0, 1))
            r <- abs(cor(x, y))
            txt <- format(c(r, 0.123456789), digits = digits)[1]
            txt <- paste0(prefix, txt)
            if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
            text(0.5, 0.5, txt, cex = cex.cor * r)
          }
          
          panel.hist <- function(x, ...)
          {
            usr <- par("usr"); on.exit(par(usr))
            par(usr = c(usr[1:2], 0, 1.5) )
            h <- hist(x, plot = FALSE)
            breaks <- h$breaks; nB <- length(breaks)
            y <- h$counts; y <- y/max(y)
            rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
          }
          
          pairs(C1, diag.panel = panel.hist, upper.panel = panel.cor,
                lower.panel = panel.lm)