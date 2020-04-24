#################################################################################
#############################||   Trend Data    ||###############################
#################################################################################





########  Package:  'gtrendsR'
####
rm(list = ls())

#---  Installing the package
install.packages("gtrendsR")
library(gtrendsR)

#---  Categories dataset composed of exisiting research categories (not vital)
data("categories")
str(categories)

#---  Getting Data on State Abbreviations
data('state')
state.df <- as.data.frame(cbind(state.x77, state.abb, state.area, state.division, state.name, state.region))
rm(list=setdiff(ls(), c("state.df", "categories")))


#---  Creating a function that takes as the first argument as a vector of keywords and second argument as state abbreviation:
my.fun <- function(vector.keyword = NA,  state.abbr = "US"){
  gtrends(keyword = c("firearms", "gun"), geo = as.character(state.abbr), time = "today 3-m",
          gprop = c("web"),
          category = 0, hl = "en-US", low_search_volume = FALSE,
          cookie_url = "http://trends.google.com/Cookies/NID", tz = 0,
          onlyInterest = T)
}


#---  Example:
my.fun(vector.keyword = c("gun", "firearms"), state.abbr = "CA")

#---  For all states:
list.gun.firearms <- list(NULL)
for(i in state.df$state.abb){
  list.gun[[i]] <- my.fun(vector.keyword = c("gun", "firearms"), state.abbr = i)
}


