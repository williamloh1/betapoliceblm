#class exercise 3-31
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
