senateData<-read.csv("http://politicaldatascience.com/PDS/Datasets/SenateForecast/CandidateLevel.csv")
candidates2018 <- read.csv("http://politicaldatascience.com/PDS/Datasets/SenateForecast/CandidateLevel2018.csv")
library(tidyverse)
candidates2018 <- candidates2018 %>%
  rename(weightexperience=Weightedexperience) %>%
  rename(PercentageRaised=Percentage.Raised)

#split data
library(rsample)
set.seed(10000)
split_senateData<-initial_split(senateData, prop=.8)
senate_train<-training(split_senateData)
senate_test<-testing(split_senateData)
#the simple model
SimpleModelTrain<-lm(VotePercentage~pvi*Republican+Incumbent, data=senate_train)
SimpleModelPredictions<-predict(SimpleModelTrain, newdata=senate_test)
sqrt(mean((SimpleModelPredictions-senate_test$VotePercentage)^2))
#the complex model
ComplexModelTrain <- lm(VotePercentage~pvi*Republican+weightexperience+Incumbent+PercentageRaised, data=senate_train)
summary(ComplexModelTrain)
ComplexModelPredictions<-predict(ComplexModelTrain, newdata=senate_test)
sqrt(mean((ComplexModelPredictions-senate_test$VotePercentage)^2))
#predict 2018 candidates
vote <- predict(ComplexModelTrain, 
        newdata=candidates2018[which(candidates2018$Candidateidentifier %in% c("2018MOMcCaskill", "2018OHBrown", "2018WVManchin")), ])
vote <- as.data.frame(vote)
rownames(vote)[1] <- "2018OHBrown"
rownames(vote)[2] <- "2018MOMcCaskill"
rownames(vote)[3] <- "2018WVManchin"
vote
