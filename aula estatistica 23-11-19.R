norm_samp <- rnorm(1000) # Gera 100 números 'normais'
qqnorm(norm_samp)
abline(a=0, b=1, col='grey')

dbinom(x=0,size=200,p=0.02)

poisson <- rpois(100,lambda=2)

bin <- 1- pbinom(6,size=10,0.5)

pexp(3,rate=1/4)

library(ggplot2)

session_times <- read.csv("C:/Users/aluno.laboratorio/Downloads/web_page_data.csv")
head(session_times)

ggplot(session_times, aes(x=Page,y=Time)) + 
  geom_boxplot()
mean_a <- mean(session_times[session_times['Page'] == 'Page A','Time'])
mean_b <- mean(session_times[session_times['Page'] == 'Page B','Time'])
mean_b - mean_a

perm_fun <- function(x, n1,n2){
  n <- n1+ n2
  idx_b <- sample(1:n,n1)
  idx_a <- setdiff(1:n,idx_b)
  mean_diff <- mean(x[idx_b]) - mean(x[idx_a])
  return (mean_diff)
}

perm_diffs <- rep(0,1000)
for(i in 1:1000)
  perm_diffs[i] = perm_fun(session_times[,"Time"],21,15)
perm_diffs

hist(perm_diffs)
abline(v=mean_b - mean_a)


obs_pct_diff <- 100*(200/23739 - 182/22588)
conversion <- c(rep(0,45945),rep(1,382))
for(i in 1:1000)
  perm_diffs[i] = 100*perm_fun(conversion,23739,22588)
hist(perm_diffs)
abline(v = obs_pct_diff)

mean(perm_diffs > obs_pct_diff)


prop.test(x=c(200,182), n=c(23739,22588), alternative="greater")


library(pwr)
pwr.2p.test(h=0.05,power = 0.8, sig.level = 0.05)

sessions <- read.csv("C:/Users/aluno.laboratorio/Downloads/four_sessions.csv")
names(sessions)
dim(sessions)
head(sessions)

library(ggplot2)

ggplot(data = sessions, aes(x=sessions$Page, y=sessions$Time))+
  geom_boxplot()
t.test(Time ~ Page, data=session_times, alternative='less')


library(lmPerm)
summary(aovp(Time ~ Page, data=sessions))
summary(aov(Time ~ Page, data=sessions))


chisq.test(sessions, simulate.p.value=TRUE)
