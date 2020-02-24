mayors<-read_csv(file="https://raw.githubusercontent.com/jmontgomery/jmontgomery.github.io/master/PDS/Datasets/Mayors.csv")
tweets<-read_csv("~/Downloads/Tweets.csv")
tweets <- rename(tweets, TwitterHandle=ScreenName)

#look at lyda krewson first
krewson <- subset(tweets, TwitterHandle=="lydakrewson")
krewsonwords<-str_split(krewson$Text, pattern=" ")
unlistkrewson <- unlist(krewsonwords)
policewords <- c()
blmwords <- c("BLM","blacklivesmatter")
str_view(tweets$Text,"\\#michaelbrown",match=TRUE)
str_view(tweets$Text,"Police",match=TRUE)
str_view(tweets$Text," cop ",match=TRUE)
str_view(tweets$Text," cops ",match=TRUE)
str_detect(tweets$Text," detective ")
krewsontweets <- krewson$Text
krewsontweets <- krewsontweets %>%
  filter(str_detect(krewson$Text," detective ")==TRUE) %>%
  mutate(policewords=1)
sum(str_count(krewsontweets," cop "))


mayors <- subset(mayors,TwitterHandle!=NaN)
tweets$Text <- str_to_lower(tweets$Text)
policementions <- rep(0,nrow(mayors))
blmmentions <- rep(0,nrow(mayors))
cbind(mayors,policementions,blmmentions)
for(i in unique(mayors$TwitterHandle)) {
  mayortweets <- subset(tweets, TwitterHandle == i)
  policecount <- 0
  blmcount <- 0
  for(j in mayortweets$Text) {
    if(str_detect(j," cop ")==TRUE | str_detect(j,"police")==TRUE | str_detect(j,"detective")==TRUE) {
      policecount <- policecount + 1
    }
    if(str_detect(j,"blacklivesmatter")==TRUE | str_detect(j,"\\#racism")==TRUE | str_detect(j,"\\#blm")==TRUE) {
      blmcount <- blmcount + 1
    }
  }
  mayors$policementions[mayors$TwitterHandle == i] <- policecount
  mayors$blmmentions[mayors$TwitterHandle == i] <- blmcount
}
