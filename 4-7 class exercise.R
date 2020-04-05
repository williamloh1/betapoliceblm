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
  bootstrapped.reserved[[i]] <- coef (lm (water~reserved, data=women.sample))[2]
}
mean(unlist(bootstrapped.reserved))
sd(unlist(bootstrapped.reserved))
low.boot.reserved.coef <- mean(unlist(bootstrapped.reserved))-qt(0.975, 320)*sd(unlist(bootstrapped.reserved))
high.boot.reserved.coef <-mean(unlist(bootstrapped.reserved))+qt(0.975, 320)*sd(unlist(bootstrapped.reserved))
paste("The 95% CI for the bootstrapeed reserved treatment is between", low.boot.reserved.coef, "and", high.boot.reserved.coef)

