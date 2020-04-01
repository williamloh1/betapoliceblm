#class exercise 3-31
#per slide 10
#when income=2, age=4
lambda1 <- -2+1*2+0.7*4
logit1 <- exp(lambda1)/(1+exp(lambda1))
#when income=4, age=4
lambda2 <- -2+1*4+0.7*4
logit2 <- exp(lambda2)/(1+exp(lambda2))
#compare
table=as.data.frame(cbind(c(lambda1, logit1), c(lambda2, logit2)))
rownames(table)[1] <- c("lambda")
rownames(table)[2] <- c("probability")
table

#per slide 17
turnout<-read.csv("http://politicaldatascience.com/PDS/Datasets/SimpleTurnout2008.csv")
model <- glm(turnout~stt+eth+inc+age, family="binomial", data=turnout)
summary(model)
Modelpreds<-predict(model, type="response")
boxplot(Modelpreds~turnout$inc, xlab="Income", ylab="Predicted Probabilities")

#per slide 23
confusion.matrix <- matrix(c(4283, 18523, 3809, 46418), nrow=2)
precision <- 46418/(18523+46418)
recall <- 46418/(46418+3809)
accuracy <- (46418+4283)/(4283+18523+3809+46418)

#per slide 35
install.packages("rpart")
library(rpart)
equation<-as.formula("turnout~eth+inc+age")
tree_mod1<-rpart(equation, data=turnout)
tree_mod2<-rpart(equation, data=turnout, control=rpart.control(cp=.0002))
treePreds1<-predict(tree_mod1)
treePreds2<-predict(tree_mod2) 
table(treePreds1, turnout$turnout)
table(treePreds2, turnout$turnout)
brier1 <- sqrt(mean((turnout$turnout-treePreds1)^2))
brier2 <- sqrt(mean((turnout$turnout-treePreds2)^2))
#our own tree model with different cp
our_tree_mod <- rpart(turnout~eth+inc+age, data=turnout, control=rpart.control(cp=.005))
#confusion matrix
treePreds<-predict(our_tree_mod)
table(treePreds, turnout$turnout)
#brier score
brier<- sqrt(mean((turnout$turnout-treePreds)^2))


#per slide 41
install.packages("randomForest")
library(randomForest)

#per slide 45 (group assignment)





