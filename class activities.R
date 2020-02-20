vap<-voting.age.population<-c(3481823, 496387, 4582842, 2120139,26955438,
                              3617942,2673154,652189,472143,14085749,6915512,
                              995937,1073799,9600372,4732010,2265860,2068253,
                              3213141,3188765,1033632,4242214,4997677,7620982,
                              3908159,2139918,4426278,731365,1321923,1870315,1012033,
                              6598368,1452962,14838076,6752018,494923,8697456,2697855,
                              2850525,9612380,824854,3303593,594599,4636679,
                              17038979,1797941,487900,5841335,4876661,1421717,
                              4257230,392344) 
total.votes<-tv<-c(NA, 238307, 1553032, 780409,8899059,1586105, 
                   1162391,258053, 122356,4884544, 2143845,348988, 
                   458927,3586292, 1719351,1071509, 864083,1370062, 
                   954896,NA, 1809237, 2243835,3852008, 2217552,NA, 
                   2178278, 411061,610499, 586274,418550, 2315643,568597, 
                   4703830,2036451, 220479,4184072, NA,1399650, NA,392882, 
                   1117311,341105, 1868363,NA, 582561, 263025,2398589, 
                   2085074,473014, 2183155, 196217)
#activity 3
nums<-seq(1,6,by=1)
varnames<-paste0("Var",nums)
varnames<-gsub("Var","",varnames)
as.numeric(varnames)
wrong <- c(1,"one",TRUE)
class(wrong)
gettysburg<-read.delim("http://politicaldatascience.com/PDS/ClassActivities/Activity3/Gettysburg")
gettysburg<-as.character(gettysburg)
gettysburg<- gsub("???","",gettysburg)
gettysburg<-strsplit(gettysburg,split=" ")
unlist(gettysburg)

#activity 4
x <- sample(-100:100, size=100)
x[x%%2==1]<--1*x[x%%2==1]
y<-x[x>=20&x<=93]
catX<-x
catX[x> -30 & x<4]<-1
catX[x<= -30]<-0
catX[x>3]<-2
subCatX<-x[catX==1 | catX==2]
subCatXOdd<-x[(catX==1 | catX==2) & x%%2==1]

#Jan 23
m1<- cbind(vap,tv)
m2<- rbind(vap,tv)
m1[1,2] #first row, second column
m1[,1] #the ith column
m1[1:5,1:2] #rows 1 to 5, columns 1 to 2
attributes(m1)
colnames(m1) <- c("Voting age pop.","Total Votes")

list.a <- list(matrix=m1, vap=vap, three=3)
list.a$vap
turnout<-tv/vap
voting.data <- data.frame(tv, vap, turnout)

#activity 5
rm(list=ls())
example(lm)
newLM<-unclass(lm.D90)

#Jan 28
x = 10
if (x > 2) {
  print("X is larger than 2")
} else {
  print("X is 2 or smaller")
}

#activity 6
for(i in seq(from=1,to=100,by=3)) {
  print(i)
}
for(i in 1:20) {
  print(1:i)
}
for(i in 19:1) {
  print(1:i)
}
trackH <- 0
trackT <- 0
while (trackH<3 & trackT<3) {
  flip <- sample(0:1,1)
  if (flip==1) {
    print("H")
    trackH<-trackH+1
    trackT<-0
  } else {
    print("T")
    trackT<-trackT+1
    trackH<-0
  }
}

#Jan 30
my.function = function(x) {
  return(x)
}

#activity 7
num = seq(1,1000,by=67)

sig.digit = function(x){
  char.vec <- as.character(x)
  first.char <- substr(char.vec, 1, 1)
  return(as.numeric(first.char))
}

freq.fun = function(x){
  test.freq = table(x)
  test.freq = unclass(test.freq)
  test.freq = test.freq/length(x)
  return(test.freq)
}

leemis <- function(x){
  max = 0
  for (i in 1:9) {
    m = x[i] - log(1 + 1/i, base=10)
    if(m > max){
      max = m
    }
  }
  return(max)
}

choGains = function(x){
  sum.cho = 0
  for (i in 1:9){
    m = (x[i] - log(1 + 1/i, base=10))^2
    sum.cho = sum.cho + m
  }
  return(sqrt(sum.cho))
}
calc.stat = function(x,y,z){
  if (y==TRUE) {
    print(paste("Leemis stat: ",leemis(x)))
    print(paste("Leemis is ",crit.leemis(leemis(x))))
  }
  if (z==TRUE) {
    print(paste("Cho-Gains stat: ",choGains(x)))
    print(paste("Cho-Gains is ",crit.choGains(choGains(x))))
  }
}
crit.leemis <- function(leemis.val) {
  val <- ""
  if(leemis.val < .851) {
    val <- "not significant"
  }
  if(leemis.val >= .851 & leemis.val < .967) {
    val <- "significant at 90%"
  }
  if(leemis.val >= .967 & leemis.val < 1.212) {
    val <- "significant at 95%"
  }
  if(leemis.val >= 1.212) {
    val <- "significant at 99%"
  }
  return(val)
}
crit.choGains <- function(choGains.val) {
  val <- ""
  if(choGains.val < 1.212) {
    val <- "not significant"
  }
  if(choGains.val >= 1.212 & choGains.val < 1.330) {
    val <- "significant at 90%"
  }
  if(choGains.val >= 1.330 & choGains.val < 1.569) {
    val <- "significant at 95%"
  }
  if(choGains.val >= 1.569) {
    val <- "significant at 99%"
  }
  return(val)
}

test = sig.digit(num)
freq.digit = freq.fun(test)
calc.stat(freq.digit,TRUE,TRUE) #first TRUE is for leemis, second TRUE is for cho-gains

#Feb 6
webData<-url("http://pages.wustl.edu/montgomery/incumbents2.txt")
OOS <- read.table(webData)
summary(OOS)
by.var.lm<-function(by.var, formula, data, coef.num){
  output.vector<-NULL
  classes<-unique(data[by.var])
  for (i in classes){
    output.vector[which(classes == i)]<- 
      lm(formula, data[data$by.var==i,])$coefficients[coef.num]
  }
  return(output.vector)
}
plot(by.var.lm("year", voteshare~inparty, OOS, 2), type="l")
website<-url("http://pages.wustl.edu/montgomery/titanic")
titanic<-read.delim(website)
by.var.lm("Class", (as.numeric(Survived)-1) ~ Gender, titanic, 2)
unique(titanic["Class"])

#Feb 11
library(ggplot2)
primaryPolls<-read.csv('https://jmontgomery.github.io/PDS/Datasets/president_primary_polls_feb2020.csv', stringsAsFactors = F)
primaryPolls$start_date<-as.Date(primaryPolls$start_date, "%m/%d/%Y")
primaryPolls<-primaryPolls[primaryPolls$state=="Iowa",]
primaryPolls<-primaryPolls[primaryPolls$candidate_name%in%c("Amy Klobuchar", "Bernard Sanders", "Elizabeth Warren", "Joseph R. Biden Jr.", "Michael Bloomberg", "Pete Buttigieg","Tom Steyer"),]
ggplot(data=primaryPolls)+geom_point(mapping = aes(x=start_date, y=pct, shape=candidate_name, color=candidate_name))
ggplot(data=primaryPolls)+
  geom_smooth(mapping = aes(x=start_date, y=pct, color=candidate_name)) + 
  geom_point(mapping = aes(x=start_date, y=pct, color=candidate_name), alpha=.4) +
  facet_wrap(~ candidate_name, nrow=2)
ggplot(data=primaryPolls)+geom_smooth(mapping = aes(x=start_date, y=pct, color=candidate_name, linetype=candidate_name))

#class activity on super tuesday
primaryPolls<-read.csv('https://jmontgomery.github.io/PDS/Datasets/president_primary_polls_feb2020.csv', stringsAsFactors = F)
primaryPolls$start_date<-as.Date(primaryPolls$start_date, "%m/%d/%Y")
primaryPolls<-primaryPolls[primaryPolls$state%in%c("Alabama","Arkansas","California","Colorado","Maine","Massachusetts","Minnesota","North Carolina","Oklahoma","Tennessee","Texas","Utah","Vermont","Virginia"),]
primaryPolls<-primaryPolls[primaryPolls$candidate_name%in%c("Amy Klobuchar", "Bernard Sanders", "Elizabeth Warren", "Joseph R. Biden Jr.", "Michael Bloomberg", "Pete Buttigieg","Tom Steyer"),]
ggplot(data=primaryPolls)+
  geom_smooth(mapping = aes(x=start_date, y=pct, color=candidate_name)) + 
  geom_point(mapping = aes(x=start_date, y=pct, color=candidate_name), alpha=.4) +
  facet_wrap(~ state, nrow=4) +xlim("%m/%d/%Y")

#Feb 13
library(dplyr)
library(tidyr)
library(readr)
primaryPolls<-read_csv('https://jmontgomery.github.io/PDS/Datasets/president_primary_polls_feb2020.csv')
primaryPolls$start_date<-as.Date(primaryPolls$start_date, "%m/%d/%y")
basicPolls<-select(primaryPolls, state, candidate_name, start_date, pct)
rename(basicPolls, candidate = candidate_name)
mutate(basicPolls, proportion=pct/100)
transmute(primaryPolls, numberRespondents=round((pct/100)*sample_size))
summarise(basicPolls, average_candidate=mean(pct), count=n())
primaryPolls %>% 
  filter(start_date>="2019-12-01") %>%
  select(start_date, pct, state, candidate_name) %>%
  mutate(pct=pct/100)
primaryPolls %>%
  group_by(candidate_name, state) %>%
  summarise(average_candidate=mean(pct), count=n()) %>%
  filter(count>="5")

#Feb 18
library(tidyverse)
mayors<-read_csv(file="https://raw.githubusercontent.com/jmontgomery/jmontgomery.github.io/master/PDS/Datasets/Mayors.csv")
tweets<-read_csv("~/Downloads/Tweets.csv")
print(object.size(mayors), units="auto")
print(object.size(tweets), units="auto")  
tweets <- rename(tweets, TwitterHandle=ScreenName)
#show no unique identifiers
mayors %>% 
  count(TwitterHandle) %>%
  filter(n>1)
tweets %>% 
  left_join(select(mayors, TwitterHandle, LastElectionDate, YearsCurrentPosition, CityName), 
            by="TwitterHandle")
numTweets <- tweets %>%
  count(TwitterHandle)
mayors %>%
  full_join(numTweets,
            by="TwitterHandle")
mayors %>%
  filter(TwitterHandle%in%c("robertgarcialb","rodhiggins2017"))
tweets %>%
  filter(TwitterHandle%in%c("robertgarcialb","rodhiggins2017"))
tweets %>%
  full_join(select(mayors,TwitterHandle,LastName,FirstName),
            by="TwitterHandle")
mentions <-read_csv(file="https://raw.githubusercontent.com/jmontgomery/jmontgomery.github.io/master/PDS/Datasets/TwitterMentions.csv")
mentions <- rename(mentions, TwitterHandle=ScreenName)
mentions %>%
  count(TwitterHandle)
mentions %>%
  left_join(numTweets,by="MayorHandle")
tweetsbymayors<- mentions %>%
  semi_join(select(tweets,TwitterHandle),by="TwitterHandle")

#Feb 20
aTweet<-tweets[1,]$Text
aTweet
str_length(aTweet)
words<-str_split(aTweet, pattern = " ")
words
str_c(words)
x <- c("\"", "\\nice")
writeLines(x)
mayors$TwitterHandle[mayors$FullName == "Lyda Krewson"]
krewson <- subset(tweets, TwitterHandle=="lydakrewson")
krewsonwords<-str_split(krewson$Text, pattern=" ")
unlistkrewson <- unlist(krewsonwords)
length(unlistkrewson) / length(krewson$Text)
length(unique(unlistkrewson))
fivekrewson<-str_sub(unlistkrewson, 1, 5)
length(fivekrewson) / length(krewson$Text)
length(unique(fivekrewson))
summary(as.factor(fivekrewson),decreasing=T)
count<-0
for (i in krewson$Text) {
  if(str_detect(i,"polic")==TRUE) {
    count<-count+1
  }
}
count<-0
for (i in unlistkrewson) {
  if(str_detect(i,"^police$")==TRUE) {
    count<-count+1
  }
}
count<-0
for (i in unlistkrewson) {
  if(str_detect(i,"^http")==TRUE) {
    count<-count+1
  }
}
blm<-c()
for (i in krewson$Text) {
  if(str_detect(i,"Black Lives")==TRUE) {
    blm <- c(blm,i)
  }
}


