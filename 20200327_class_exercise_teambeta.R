##########################################################################################
#################################||  2020, March 31  ||###################################
#################################||     Team Beta     ||##################################
##########################################################################################




########  Getting Ready:
####
rm(list = ls())
senateData<-read.csv("http://politicaldatascience.com/PDS/Datasets/SenateForecast/CandidateLevel.csv")
candidates2018 <- read.csv("http://politicaldatascience.com/PDS/Datasets/SenateForecast/CandidateLevel2018.csv")
library(tidyverse)



########  Making Datasets Comparable:
####
candidates2018 <- candidates2018 %>%
  rename(weightexperience=Weightedexperience) %>%
  rename(PercentageRaised=Percentage.Raised)



########  Let's Split the Data
####
library(rsample)
set.seed(10000)
split_senateData<-initial_split(senateData, prop=.8)
senate_train<-training(split_senateData)
senate_test<-testing(split_senateData)



########  Running the Simple Model as a Baseline:
####
SimpleModelTrain <- lm(VotePercentage~ pvi * Republican + Incumbent, data=senate_train)
SimpleModelPredictions <- predict(SimpleModelTrain, newdata=senate_test)
sqrt(mean((SimpleModelPredictions-senate_test$VotePercentage)^2))
#---  See that the cross-validated RMSE of the baseline model is 7.58.



########  The Kitchen-Sink Model:
####
KitchenSinkTrain <- lm(VotePercentage ~ pvi * Republican + Incumbent + weightexperience + GenericBallotSept + PercentageRaised, data=senate_train)
KitchenSinkPredictions <- predict(KitchenSinkTrain, newdata=senate_test)
sqrt(mean((KitchenSinkPredictions-senate_test$VotePercentage)^2))
#---  See that the cross-validated RMSE of the KitchenSinkTrain model is 6.6.
#---    It is much less it is neither parsimonious nor elegant.

summary(KitchenSinkTrain)
#---  See that GenericBallotSept is not statistically significant.
#---    Let's remove it.

KitchenSinkTrain2 <- lm(VotePercentage ~ pvi * Republican + Incumbent + weightexperience + PercentageRaised, data=senate_train)
KitchenSinkPredictions2 <- predict(KitchenSinkTrain2, newdata=senate_test)
sqrt(mean((KitchenSinkPredictions2-senate_test$VotePercentage)^2))
#---  See that the cross-validated RMSE of the KitchenSinkTrain2 model is still 6.6.
#---    Let's keep it away. 

summary(KitchenSinkTrain2)  
#---  What if we get rid of interaction:

KitchenSinkTrain3 <- lm(VotePercentage ~ pvi + Republican + Incumbent + weightexperience + PercentageRaised, data=senate_train)
KitchenSinkPredictions3 <- predict(KitchenSinkTrain3, newdata=senate_test)
sqrt(mean((KitchenSinkPredictions3-senate_test$VotePercentage)^2))
#---  See that the cross-validated RMSE of the KitchenSinkTrain3 model is now 7.1.
#---    Let's retain the interaction. 




########  What about functional form of the model? Let's play a little game. 
####
#---  We'll be calculating the yhat from training data and add its quadratic 
#---  forms to the regressions. If the coefficients have statistical significance,
#---  it means that there might need for further 
KitchenSinkPrediction4 <- predict(KitchenSinkTrain2, newdata=senate_train)
yhat2 <- KitchenSinkPrediction4^2
yhat3 <- KitchenSinkPrediction4^3

#---  Now let's see:
summary(lm(VotePercentage ~ yhat2 + yhat3 + pvi * Republican + Incumbent + weightexperience + PercentageRaised, data=senate_train))
#---  Great! Neither of them are statistically significant. Probably have the sufficent model!
summary(lm(VotePercentage ~ yhat2 + pvi * Republican + Incumbent + weightexperience + PercentageRaised, data=senate_train))
#---  Cool! Not significant. The last version of our model is "ComplexModelTrain":

#---  What about quadratic forms? Let's try our continueous "PercentageRaised" in quadratic form:
summary(lm(VotePercentage ~ pvi * Republican + Incumbent + weightexperience + poly(PercentageRaised, 2), data=senate_train))
KitchenSinkTrain5 <- lm(VotePercentage ~ pvi * Republican + Incumbent + weightexperience + poly(PercentageRaised,2), data=senate_train)
KitchenSinkPredictions5 <- predict(KitchenSinkTrain5, newdata=senate_test)
sqrt(mean((KitchenSinkPredictions5-senate_test$VotePercentage)^2))
#---  See that the cross-validated RMSE of the KitchenSinkTrain3 model is now 6.7, larger than 6.6.
#---    So let's drop the polynomial.




########  The Complex Model:
####
ComplexModelTrain <- lm(VotePercentage ~ pvi * Republican + weightexperience + Incumbent + PercentageRaised, data=senate_train)
summary(ComplexModelTrain)
ComplexModelPredictions<-predict(ComplexModelTrain, newdata=senate_test)
sqrt(mean((ComplexModelPredictions-senate_test$VotePercentage)^2))




########  Now Predict 2018 Elections:
####
candidates2018.new <- candidates2018[which(candidates2018$Candidateidentifier %in% c("2018OHBrown", "2018MOMcCaskill", "2018WVManchin")), ]

candidates2018.new <- cbind(candidates2018.new, predict(ComplexModelTrain, 
        newdata=candidates2018.new))
candidates2018.new[,c(3, 15)]
#---  We predict that:
#---    2018OHBrown       60.97786
#---    2018MOMcCaskill   56.97754
#---    2018WVManchin     52.07193