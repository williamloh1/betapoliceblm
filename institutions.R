VSG<-read_csv("~/Downloads/VOTER_Survey_Jan217_Release1-csv.csv")
VSG.inst <-VSG %>% 
  select(starts_with("inst"))
for(i in seq_along(VSG.inst)){
  VSG.inst[,i]= na_if(VSG.inst[[i]],VSG.inst[[i]]>4)
}

mean.inst <- map(VSG.inst, mean, na.rm = TRUE)
median.inst <-map(VSG.inst, median, na.rm = TRUE)
mean.inst <- sort(mean.inst[[]], decreasing = TRUE)
mean.inst <- mean.inst[order(sapply(mean.inst,'[[',1))]
median.inst <- median.inst[order(sapply(median.inst,'[[',1))]
mean.plot <- plot(1:length(mean.inst),mean.inst,pch=18, col="blue",xlab="Institutions",ylab="Mean Trust")
mean.plot <- text(1:length(mean.inst),mean.inst,names(mean.inst),cex=0.5, col="red")
median.plot <- plot(1:length(median.inst),median.inst,pch=18, col="blue",xlab="Institutions",ylab="Median Trust")
median.plot <- text(1:length(median.inst),median.inst,names(median.inst),cex=0.5, col="red")

my.fun <- function(x, na){
  x[x>na]<-NA
  return(x)
}


VSG.inst2 <- apply(VSG.inst, 2, my.fun, na = 4)
mean <- apply(VSG.inst2, 2, mean, na.rm=T )
mean <- sort(mean, decreasing = F)
?mean
median <- apply(VSG.inst2, 2, median, na.rm=T )
mean.plot <- plot(1:length(mean),mean,pch=18, col="blue",xlab="Institutions",ylab="Mean Trust")
mean.plot <- text(1:length(mean),mean,names(mean),cex=0.5, col="red")
median.plot <- plot(1:length(median.inst),median.inst,pch=18, col="blue",xlab="Institutions",ylab="Median Trust")
median.plot <- text(1:length(median.inst),median.inst,names(median.inst),cex=0.5, col="red")

