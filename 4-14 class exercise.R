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


#group assignment
gs_url1<-"https://scholar.google.com/scholar?hl=en&as_sdt=7,26&q=political+parties&btnG="
pg1<-gs_url1%>%
  read_html()%>%
  html_nodes(".gs_or_cit+ a , .gs_a , .gs_rt") %>% 
  html_text()
pg1 <- list(pg1)

gs_page1<-read_html(gs_url1)
pageLinks<-gs_page1 %>%
  html_nodes('#gs_n a') %>%
  html_attr('href')
pageLinks

results <- c()
for (i in c(1:9)){
  thisUrl<-paste0("https://scholar.google.com/", pageLinks[i])
  results[[i]]<-read_html(thisUrl) %>%
    html_nodes('.gs_or_cit+ a , .gs_a , .gs_rt') %>%
    html_text()
}

results <- c(pg1, results)
df <- data.frame(matrix(unlist(results), nrow = 100, ncol=3, byrow = T))
library(readr)
citations <- as.character(df[,3])
citations.log <- log(parse_number(citations))

