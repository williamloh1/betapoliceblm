####  Getting Ready

rm(list = ls())
mayors<-read_csv(file="https://raw.githubusercontent.com/jmontgomery/jmontgomery.github.io/master/PDS/Datasets/Mayors.csv")
tweets<-read_csv("G:/My Drive/_WashU_courses/2020_Spring/Coding/Tweets.csv/Tweets.csv")
tweets <- rename(tweets, TwitterHandle=ScreenName)




####  Given the enormous size of tweets dataset, let's do our exploration on mayor Krewson:

krewson <- subset(tweets, TwitterHandle=="lydakrewson")
krewsonwords<-str_split(krewson$Text, pattern=" ")
unlistkrewson <- unlist(krewsonwords)
policewords <- c(" cop ", "police", "detective")  # Potential words for the cop and police part. 
blmwords <- c("blm","blacklivesmatter")     # Potential words for the black lives matter part.
krewsonwords <- str_to_lower(krewsonwords)  # Given that regex is case-letters, let's make all lower case.
tweets$Text <- str_to_lower(tweets$Text)  # Same for all dataset!
str_view(tweets$Text, "\\#michaelbrown", match=TRUE)  # No solutions
str_view(tweets$Text,"\\#racism",match=TRUE)  # Not a good one
str_view(tweets$Text, "\\sjuly\\s13\\s", match=TRUE)  # Not a good one
str_view(tweets$Text, "brutality", match=TRUE)  # Not a good one
str_view(tweets$Text, "\\#blacklivesmatter", match=TRUE)  # OK
str_view(tweets$Text, "\\#georgezimmerman", match=TRUE)  # Not a good one
str_view(tweets$Text, "\\#ferguson$", match=TRUE)  # Good one!
str_view(tweets$Text,"\\#michaelbrown", match=TRUE)  # Not a good one
str_view(tweets$Text,"\\#blacklivesmatter", match=TRUE)  # Not a good one
str_view(tweets$Text,"\\#icantbreathe", match=TRUE)  # Not a good one
str_view(tweets$Text,"\\#jonathansanders", match=TRUE)  # OK
str_view(tweets$Text,"\\#blacktwitter", match=TRUE)  # Not a good one
str_view(tweets$Text,"police", match=TRUE)  # OK
str_view(tweets$Text," cop ", match=TRUE)  # OK
str_view(tweets$Text," cops ", match=TRUE)  # OK
str_view(tweets$Text," detective ", match=TRUE)  # OK




####  Let's write a function take takes these words and counts them by mayor:

mayors <- subset(mayors, TwitterHandle!=NaN)
policementions <- rep(0,nrow(mayors))
blmmentions <- rep(0,nrow(mayors))
mayors <- cbind(mayors,policementions,blmmentions)
for(i in unique(mayors$TwitterHandle)) {
  mayortweets <- subset(tweets, TwitterHandle == i)
  policecount <- 0
  blmcount <- 0
  for(j in mayortweets$Text) {
    if(str_detect(j," cop ")==TRUE | str_detect(j," cops ")==TRUE |  str_detect(j,"police")==TRUE | str_detect(j,"detective")==TRUE) {
      policecount <- policecount + 1
    }
    if(str_detect(j,"blacklivesmatter")==TRUE | str_detect(j, "\\#ferguson$") == TRUE | str_detect(j,"\\#blm")==TRUE | str_detect(j,"\\#jonathansanders")==TRUE) {
      blmcount <- blmcount + 1
    }
  }
  mayors$policementions[mayors$TwitterHandle == i] <- policecount
  mayors$blmmentions[mayors$TwitterHandle == i] <- blmcount
}




####  Let's run a diagnostic:

sum(mayors$policementions)  # 12482 is great result
sum(mayors$blmmentions)     # 50 is much less than we expected. Maybe mayors refrain from using #blm.





####  Let's prepare the plots:




police.data <- mayors[mayors$policementions !=0, ]
police.data <- police.data[order (police.data$policementions, decreasing = TRUE), ]
blm.data <- mayors[mayors$blmmentions !=0, ]
blm.data <- blm.data[order (blm.data$blmmentions, decreasing = TRUE), ]
plot <- ggplot(data=police.data[1:25,])+
  scale_shape_manual(values = 1:25)+
  geom_point(mapping = aes(x=LastName, y=policementions,  color="red"), alpha=.8)+
  geom_point(data=blm.data[1:22,],mapping=aes(x=LastName,y=blmmentions,color="blue"),alpha=.8)+
  labs(y="Twitter Mentions",x="Mayors")
  
#ggplot(data=blm.data[1:22,])+
#  scale_shape_manual(values = 1:22)+
#  geom_point(mapping = aes(x=LastName, y=blmmentions,  color="blue"), alpha=.8)


