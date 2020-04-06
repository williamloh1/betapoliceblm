rm(list = ls())

#Per slide 35
resume<-read.csv("http://politicaldatascience.com/PDS/Datasets/resume.csv")
summary(lm(call~race, data=resume))
low.race.coef <- 0.032033-qt(0.975, 4868)*0.007785
high.race.coef <- 0.032033+qt(0.975, 4868)*0.007785
paste("The 95% CI for the race treatment is between", low.race.coef, "and", high.race.coef)

#Group assignment
women <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")
model <- lm(water~reserved, data=women)
summary(model)
low.reserved.coef <- 9.252-qt(0.975, 320)*3.948
high.reserved.coef <- 9.252+qt(0.975, 320)*3.948
paste("The 95% CI for the reserved treatment is between", low.reserved.coef, "and", high.reserved.coef)

#using bootstrapped standard errors
set.seed(999)
bootstrapped.reserved <- list()
for (i in 1:1000){
  women.sample <- women[sample (1:nrow(women), nrow(women), replace=T),]
  bootstrapped.reserved[[i]] <- coef(lm (water~reserved, data=women.sample))[2]
}
mean(unlist(bootstrapped.reserved))
sd(unlist(bootstrapped.reserved))
low.boot.reserved.coef <- mean(unlist(bootstrapped.reserved))-qt(0.975, 320)*sd(unlist(bootstrapped.reserved))
high.boot.reserved.coef <-mean(unlist(bootstrapped.reserved))+qt(0.975, 320)*sd(unlist(bootstrapped.reserved))
paste("The 95% CI for the bootstrapeed reserved treatment is between", low.boot.reserved.coef, "and", high.boot.reserved.coef)



########  With controls


model.2 <- lm(water ~ reserved + irrigation, data=women)
summary(model.2)
low.reserved.coef.2 <- model.2$coefficients["reserved"] - qnorm(0.975)*sqrt(vcov(model.2)["reserved","reserved"])
high.reserved.coef.2 <- model.2$coefficients["reserved"] + qnorm(0.975)*sqrt(vcov(model.2)["reserved","reserved"])
paste("The 95% CI for the controlled reserved treatment is between", low.reserved.coef.2, "and", high.reserved.coef.2)

#using bootstrapped standard errors
set.seed(999)
bootstrapped.reserved.2 <- list()
for (i in 1:1000){
  women.sample <- women[sample (1:nrow(women), nrow(women), replace=T),]
  bootstrapped.reserved.2[[i]] <- coef(lm (water~reserved+irrigation, data=women.sample))[2]
}
mean(unlist(bootstrapped.reserved.2))
sd(unlist(bootstrapped.reserved.2))
low.boot.reserved.coef.2 <- mean(unlist(bootstrapped.reserved.2))-qt(0.975, 320)*sd(unlist(bootstrapped.reserved.2))
high.boot.reserved.coef.2 <-mean(unlist(bootstrapped.reserved.2))+qt(0.975, 320)*sd(unlist(bootstrapped.reserved.2))
paste("The 95% CI for the bootstrapeed reserved treatment is between", low.boot.reserved.coef.2, "and", high.boot.reserved.coef.2)

