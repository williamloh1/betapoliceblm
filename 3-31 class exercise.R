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
table((Modelpreds>0.5)*1, turnout$turnout)
boxplot(Modelpreds~turnout$inc, xlab="Income", ylab="Predicted Probabilities")

#per slide 23
confusion.matrix <- matrix(c(4283, 18523, 3809, 46418), nrow=2)
precision <- 46418/(18523+46418)
recall <- 46418/(46418+3809)
accuracy <- (46418+4283)/(4283+18523+3809+46418)
precision
recall
accuracy
#per slide 35
library(rpart)
equation<-as.formula("turnout~eth+inc+age")
tree_mod1<-rpart(equation, data=turnout)
tree_mod2<-rpart(equation, data=turnout, control=rpart.control(cp=.0002))
#confusion matrix for tree_mod1 and accuracy
treePreds1<-predict(tree_mod1)
table((treePreds1>0.5)*1, turnout$turnout)
accuracy.tree1 <- (5886+44582)/(5886+5645+16920+44582)
accuracy.tree1
#confusion matrix for tree_mod2
treePreds2<-predict(tree_mod2) 
table((treePreds2>0.5)*1, turnout$turnout)
accuracy.tree2 <- (3158+47973)/(3158+2254+19648+47973)
accuracy.tree2
#brier score
brier1 <- sqrt(mean((turnout$turnout-treePreds1)^2))
brier2 <- sqrt(mean((turnout$turnout-treePreds2)^2))
brier1
brier2
#build our own tree model with different cp
our_tree_mod <- rpart(turnout~eth+inc+age, data=turnout, control=rpart.control(cp=.00003))
#confusion matrix
treePreds<-predict(our_tree_mod)
table((treePreds>0.5)*1, turnout$turnout)
accuracy.tree <- (4686+46491)/(4686+3736+18120+46491)
accuracy.tree
#brier score
brier<- sqrt(mean((turnout$turnout-treePreds)^2))
brier

#per slide 41
library(randomForest)
turnout$turnout<-as.factor(turnout$turnout)
our_mod_forest<-randomForest(turnout~stt+eth+inc+age, data=turnout, 
                          ntree=101, mtry=3, maxnodes=4)
our_mod_forest$confusion

#per slide 45 (group assignment)

########  Grtting Ready
####

rm(list = ls())
senateData<-read.csv("http://politicaldatascience.com/PDS/Datasets/SenateForecast/CandidateLevel.csv")
library(tidyverse)

senate.summary <- senateData %>%
  group_by(RaceID) %>%
  mutate(winning = min_rank(desc(VotePercentage))) %>%
  filter(Incumbent == 1) %>%
  mutate(win = as.numeric(winning ==1))


senate.summary <- senate.summary %>%
  mutate(pvi.rep = pvi * Republican)



########  Subsetting Training-Test Datasets
####

senate.summary.test <- senate.summary[senate.summary$year==2016, ] 
senate.summary.training <- senate.summary[senate.summary$year!=2016, ] 



########  Model 1: Linear
####

model.1 <- glm(win ~ pvi * Republican + weightexperience + PercentageRaised, family="binomial", data=senate.summary.training)

model.1.preds <-predict(model.1, type="response", newdata = senate.summary.test)
table((model.1.preds > 0.5)*1, senate.summary.test$win) 
#Confusion matrix for linear model for 2016:


########  Model 2: Random Forest
####
library(randomForest)
senate.summary.training$win<-as.factor(senate.summary.training$win)
model.2<-randomForest(win ~ pvi * Republican + weightexperience + PercentageRaised, data=senate.summary.training, 
                             ntree=201, mtry=3, maxnodes=4)

#Confusion matrix for Random Forest model for 2016:
model.2.preds <- as.numeric(predict(model.2, newdata=senate.summary.test))
table((model.2.preds > 0.5)*1, senate.summary.test$win) 

########  Model 3: KNN
####

library(class)
X.train<-senate.summary.training[,c("pvi", "Republican", "pvi.rep", "weightexperience", "PercentageRaised")]
X.test <- senate.summary.test[,c("pvi", "Republican", "pvi.rep", "weightexperience", "PercentageRaised")]
model.3<-knn(train=X.train, test=X.test, cl=senate.summary.training$win, k=10)
#Confusion matrix for KNN model for 2016:
table((as.numeric(model.3)>0.5)*1, senate.summary.test$win)




