loan_data <- read.csv('C:/Users/aluno.laboratorio/Downloads/loan_data.csv')
loan_data$outcome <- ordered(loan_data$outcome, levels=c('paid off', 'default'))

library(klaR)
naive_model <- NaiveBayes(outcome ~ purpose_ + home_ + emp_len_, 
                          data = na.omit(loan_data))
naive_model$table

new_loan <- loan_data[147, c('purpose_', 'home_', 'emp_len_')]
row.names(new_loan) <- NULL
new_loan

predict(naive_model, new_loan)


less_naive <- NaiveBayes(outcome ~ borrower_score + payment_inc_ratio + 
                           purpose_ + home_ + emp_len_, data = loan_data)
less_naive$table[1:2]

stats <- less_naive$table[[1]]
library(ggplot2)
ggplot(data.frame(borrower_score=c(0,1)), aes(borrower_score)) +
  stat_function(fun = dnorm, color='blue', linetype=1, 
                args=list(mean=stats[1, 1], sd=stats[1, 2])) +
  stat_function(fun = dnorm, color='red', linetype=2, 
                args=list(mean=stats[2, 1], sd=stats[2, 2])) +
  labs(y='probability')


loan_lda <- lda(outcome ~ borrower_score + payment_inc_ratio,
                data=loan_data)
loan_lda$scaling

pred <- predict(loan_lda)
head(pred$posterior)


lda_df <- cbind(loan_data, prob_default=pred$posterior[,'default'])
x <- seq(from=.33, to=.73, length=100)
y <- seq(from=0, to=20, length=100)
newdata <- data.frame(borrower_score=x, payment_inc_ratio=y)
pred <- predict(loan_lda, newdata=newdata)
lda_df0 <- cbind(newdata, outcome=pred$class)

ggplot(data=lda_df, aes(x=borrower_score, y=payment_inc_ratio, color=prob_default)) +
  geom_point(alpha=.6) +
  scale_color_gradient2(low='white', high='blue') +
  scale_x_continuous(expand=c(0,0)) + 
  scale_y_continuous(expand=c(0,0), lim=c(0, 20)) + 
  geom_line(data=lda_df0, col='green', size=2, alpha=.8) +
  theme_bw()


p <- seq(from=0.01, to=.99, by=.01)
df <- data.frame(p = p ,
                 logit = log(p/(1-p)),
                 odds = p/(1-p))
library(ggplot2)
ggplot(data=df, aes(x=p, y=logit)) +
  geom_line() +
  labs(x = 'p', y='logit(p)') +
  theme_bw()





logistic_model <- glm(outcome ~ payment_inc_ratio + purpose_ + 
                        home_ + emp_len_ + borrower_score,
                      data=loan_data, family='binomial')

library(MASS)

loan_lda <- lda(outcome ~ purpose_ + home_ + emp_len_, data = na.omit(loan_data))
loan_lda$scaling

pred <- predict(loan_lda, new_loan)
head(pred$posterior)

loan_log <- glm(outcome ~ purpose_ + home_ + emp_len_, data = na.omit(loan_data), family='binomial')

pred <- predict(loan_log, new_loan)
prob <- 1/(1+exp(-pred))
prob


library(pROC)

pred_y <- predict(logistic_model, newdata = loan_data)

auc(loan_data$outcome, pred_y)

