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

#per slide 35
install.packages("rpart")
library(rpart)
equation<-as.formula("turnout~eth+inc+age")
tree_mod1<-rpart(equation, data=turnout)
tree_mod2<-rpart(equation, data=turnout, control=rpart.control(cp=.0002))
#confusion matrix for tree_mod1
treePreds1<-predict(tree_mod1)
table((treePreds1>0.5)*1, turnout$turnout)
#confusion matrix for tree_mod2
treePreds2<-predict(tree_mod2) 
table((treePreds2>0.5)*1, turnout$turnout)
#brier score
brier1 <- sqrt(mean((turnout$turnout-treePreds1)^2))
brier2 <- sqrt(mean((turnout$turnout-treePreds2)^2))
brier1
brier2
#build our own tree model with different cp
our_tree_mod <- rpart(turnout~eth+inc+age, data=turnout, control=rpart.control(cp=.005))
#confusion matrix
treePreds<-predict(our_tree_mod)
table((treePreds>0.5)*1, turnout$turnout)
#brier score
brier<- sqrt(mean((turnout$turnout-treePreds)^2))
brier

#per slide 41
install.packages("randomForest")
library(randomForest)
turnout$turnout<-as.factor(turnout$turnout)
our_mod_forest<-randomForest(turnout~stt+eth+inc+age, data=turnout, 
                          ntree=101, mtry=3, maxnodes=4)
our_mod_forest$confusion

#per slide 45 (group assignment)
senateData<-read.csv("http://politicaldatascience.com/PDS/Datasets/SenateForecast/CandidateLevel.csv")
election <- rep(0,nrow(senateData))
incumbent_win <- rep(0,nrow(senateData))
senateData <- cbind(senateData,election,incumbent_win)
for (i in seq_along(senateData$election)) {
  senateData$election[i] = paste(senateData$cycle[i],senateData$state[i],sep="")
}
senateData <- senateData[order(senateData$election),]
for (i in seq_along(senateData$election)) {
  temp <- c(i)
  for (j in i:nrow(senateData)) {
    if (senateData$election[i] == senateData$election[j]) {
      temp <- c(temp,j)
    }
  }
  val <- 0
  save <- 0
  for (x in temp) {
    if (senateData$election[x] > val) {
      val <- senateData$election[x]
      save <- x
    }
  }
  senateData$incumbent_win[save] <- 1
  for (x in temp) {
    if (x != save) {
      senateData <- senateData[-x,]
    }
  }
}


