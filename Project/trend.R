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
  if(change == FALSE){
    ggplot(result, aes(x=interest_over_time.date, y=interest_over_time.hits)) +
      geom_line( color="#69b3a2") + 
      ylab("Hits") +
      ggtitle(paste0("Search trend of ", vector.keyword, " over time")) +
      xlab("") +
      theme_light() +
      theme(axis.text.x=element_text(angle=60, hjust=1)) 
  } else {
    ggplot(result, aes(x=interest_over_time.date, y=interest_over_time.hits.change)) +
      geom_line( color="#69b3a2") + 
      ylab("Hits") +
      ggtitle(paste0("Search trend of ", vector.keyword, " over time")) +
      xlab("") +
      theme_light() +
      theme(axis.text.x=element_text(angle=60, hjust=1))
  } 
}



my.fun(vector.keyword = "puzzle", state.abbr = "CA", F)
my.fun(vector.keyword = "lockdown", state.abbr = "MO", F)
my.fun(vector.keyword = "sourdough", state.abbr = "CA", F)
my.fun(vector.keyword = "puzzle", state.abbr = "MO", F)
