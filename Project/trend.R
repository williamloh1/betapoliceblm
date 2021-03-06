#################################################################################
#############################||   Trend Data    ||###############################
#################################################################################





########  Package:  'gtrendsR'
####
rm(list = ls())

#---  Installing the package
##install.packages("gtrendsR")
library(gtrendsR)

#---  Categories dataset composed of exisiting research categories (not vital)
data("categories")
str(categories)

#---  Getting Data on State Abbreviations
data('state')
state.df <- as.data.frame(cbind(state.x77, state.abb, state.area, state.division, state.name, state.region))
rm(list=setdiff(ls(), c("state.df", "categories")))


#---  Creating a function that takes three arguments
#---    1)  a string vector of keywords     (default: covid19)
#---    2)  a state abbreviation            (default: whole US)
#---    2)  a boolean of percentage change  (default: FALSE)



my.fun <- function(vector.keyword = "covid19",  state.abbr = "US", change = FALSE){
  library(gtrendsR)
  library(tidyverse)
  result <- gtrends(keyword = vector.keyword, geo = as.character(state.abbr), time = "2020-03-01 2020-04-15",
          gprop = c("web"),
          category = 0, hl = "en-US", low_search_volume = FALSE,
          cookie_url = "http://trends.google.com/Cookies/NID", tz = 0,
          onlyInterest = T)
  result <- as.data.frame(result[1]) %>%
    mutate(interest_over_time.hits.change = (interest_over_time.hits/lead(interest_over_time.hits) - 1) * 100)
  lockdown <- read.csv("~/Documents/GitHub/teambeta/Project/datasets/lockdown_dates.csv")
  USlockdown <- lockdown[which(lockdown$Country=="United States"), ]
  USlockdown$Place <- state.abb[match(USlockdown$Place, state.name)]
  
  if(change == FALSE){if(is.element(state.abbr, USlockdown$Place)==T){
    statelock <- USlockdown[USlockdown$Place==state.abbr, ]
    ggplot(result, aes(x=as.Date(interest_over_time.date), y=interest_over_time.hits)) +
      geom_line( color="#69b3a2") + 
      ylab("Hits") +
      ggtitle(paste0("Search trend of ", vector.keyword, " over time")) +
      xlab("") + 
      ylim(0,100) +
      theme_light() +
      theme(axis.text.x=element_text(angle=60, hjust=1))+
      geom_vline(xintercept = as.Date(statelock$Start.date))+
      geom_text(aes(x=as.Date(statelock$Start.date), label="\nafter lockdown", y=mean(range(interest_over_time.hits))), size=4, colour="grey", angle=90) +
      geom_text(aes(x=as.Date(statelock$Start.date), label="before lockdown\n", y=mean(range(interest_over_time.hits))), size=4, colour="grey", angle=90)
  } else{ggplot(result, aes(x=as.Date(interest_over_time.date), y=interest_over_time.hits)) +
      geom_line( color="#69b3a2") + 
      ylab("Hits") +
      ggtitle(paste0("Search trend of ", vector.keyword, " over time")) +
      xlab("") + 
      ylim(0,100) +
      theme_light() +
      theme(axis.text.x=element_text(angle=60, hjust=1))}
  } else {if(is.element(state.abbr, USlockdown$Place)==T){
    statelock <- USlockdown[USlockdown$Place==state.abbr, ]
    ggplot(result, aes(x=as.Date(interest_over_time.date), y=interest_over_time.hits.change)) +
      geom_line( color="#69b3a2") + 
      ylab("Hits") +
      ggtitle(paste0("Search trend of ", vector.keyword, " over time")) +
      xlab("") +
      theme_light() +
      theme(axis.text.x=element_text(angle=60, hjust=1))+
      geom_vline(xintercept = as.Date(statelock$Start.date))+
      geom_text(aes(x=as.Date(statelock$Start.date), label="\nafter lockdown", y=mean(range(interest_over_time.hits.change))), size=4, colour="grey", angle=90) +
      geom_text(aes(x=as.Date(statelock$Start.date), label="before lockdown\n", y=mean(range(interest_over_time.hits.change))), size=4, colour="grey", angle=90)
  } else{
    ggplot(result, aes(x=as.Date(interest_over_time.date), y=interest_over_time.hits.change)) +
      geom_line( color="#69b3a2") + 
      ylab("Hits") +
      ggtitle(paste0("Search trend of ", vector.keyword, " over time")) +
      xlab("") +
      theme_light() +
      theme(axis.text.x=element_text(angle=60, hjust=1))
  }
}
}

#test the function in a state has lockdown
my.fun(vector.keyword = "sourdough", state.abbr = "CA", F)
my.fun(vector.keyword = "sourdough", state.abbr = "CA", T)

#test the function in a state never lockdown
my.fun(vector.keyword = "puzzle", state.abbr = "AR", F)
my.fun(vector.keyword = "puzzle", state.abbr = "AR", T)


