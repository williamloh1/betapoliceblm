library(tidyverse)
library(rvest)

#per slide 20
PDS_url<-"http://politicaldatascience.com/"
PDS_page<-read_html(PDS_url)
lecture.text <- PDS_page %>%
  html_nodes('#lecture-slides a') %>%
  html_text()
lecture.text
lecture.link <- PDS_page %>%
  html_nodes('#lecture-slides a') %>%
  html_attr('href')
lecture.link

#per slide 28
#webscrape
arkansas_url<-"https://ballotpedia.org/United_States_Senate_elections_in_Arkansas,_2014"
arkansas_page<-read_html(arkansas_url)
stateLinks<-arkansas_page %>%
  html_nodes('small center a') %>%
  html_attr('href')
stateLinks[3] <- c("/United_States_Senate_elections_in_Arkansas,_2014")
stateLinks
mainText <- list()  
for (i in c(1:33)){
  thisUrl<-paste0("https://ballotpedia.org", stateLinks[i])
  mainText[[i]]<-read_html(thisUrl) %>%
    html_nodes('p') %>%
    html_text()
  }
#detect the words (fundraising, immigration) in each page
word.fundraising <- c()
for (i in 1:33){
  word.fundraising[i] <- sum(str_detect(mainText[[i]], regex("fundrais", ignore_case = T)))
    }
word.immigration <- c()
for (i in 1:33){
  word.immigration[i] <- sum(str_detect(mainText[[i]], regex("immigr", ignore_case = T)))
    }


State <- c("Alabama", "Alaska", "Arkansas", "Colorado", "Delaware", 
           "Georgia", "Idaho", "Illinois", "Iowa", "Kansas", "Kentucky", 
           "Louisiana", "Maine", "Massachusetts", "Michigan", "Minnesota", 
           "Mississippi", "Montana", "Nebraska", "New Hampshire", "New Jersey",
           "New Mexico", "North Carolina", "Oklahoma", "Oregon",
           "Rhode Island", "South Carolina", "South Dakota", "Tennessee", 
           "Texas", "Virginia", "West Virginia", "Wyoming")
data <- data.frame(cbind(State, word.fundraising, word.immigration))
#plot
ggplot(data=data, mapping=aes(x=State, y=word.fundraising))+ geom_boxplot()+coord_flip()
ggplot(data=data, mapping=aes(x=State, y=word.immigration))+ geom_boxplot()+coord_flip()


