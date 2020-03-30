##########################################################################################
#################################||  2020, 5 March   ||###################################
##########################################################################################

############  GETTING READY

rm(list = ls())
library(tidyverse)
VSG<-read_csv("G:/My Drive/_WashU_courses/2020_Spring/Coding/Week7/VOTER_Survey_Jan217_Release1-csv.csv")

VSG<-VSG %>% 
  mutate(fav_biden_2019=na_if(fav_biden_2019, 8)) %>%
  mutate(fav_biden_2019=na_if(fav_biden_2019, 98))
with(VSG, table(fav_biden_2019))

mean(VSG$fav_biden_2019, na.rm=TRUE)

set.seed(201)
sample_100= VSG %>%
  sample_n(size=100)
mean(sample_100$fav_biden_2019, na.rm=TRUE)

sample_400= VSG %>%
  sample_n(size=400)
se_400 <- sd(sample_400$fav_biden_2019, na.rm=TRUE)/sqrt(400)


se_500obs_400sz <- sd(sample_means)
sd_pop <- sd(VSG$fav_biden_2019, na.rm = T)



##  Calculate CI with sample 300
sample_300= VSG %>%
  sample_n(size=300)
se_300 <- sd(sample_300$fav_biden_2019, na.rm=TRUE)/sqrt(300)

mean(sample_300$fav_biden_2019, na.rm = T) + c(-1,1) * se_300 * qnorm(0.975)
hist(sample_300$fav_biden_2019)

sample_means <- NULL
for(i in 1:1000){
  sample_means<-c(mean(sample(sample_300$fav_biden_2019, size=300, replace = TRUE), na.rm = T), sample_means)
}
se_bootstrap <- sd(sample_means, na.rm=TRUE)

mean(sample_300$fav_biden_2019, na.rm = T) + c(-1,1) * sd_bootstrap * qnorm(0.975)
hist(sample_300$fav_biden_2019)


##  Jacob continues

VSG<-VSG %>% 
  mutate(Democrats_2019=na_if(Democrats_2019, 997)) %>%
  mutate(Democrats_2019=na_if(Democrats_2019, 998))
plot(density(VSG$Democrats_2019, na.rm=TRUE))

therm_plot<-ggplot(data=VSG, aes(x=Democrats_2019, y=fav_biden_2019)) +
  geom_point() + geom_smooth(method="lm") +
  geom_jitter() +
  ylab("Biden disapproval") +
  xlab("Democratic thermometer") 
therm_plot

Therm_model<-lm(fav_biden_2019 ~ Democrats_2019, data=VSG)
summary(Therm_model)

VSG<-VSG %>% 
  mutate(imiss_y_2019 =na_if(imiss_y_2019 , 8))
with(VSG, table(imiss_y_2019))

Therm_model2<-lm(fav_biden_2019 ~ Democrats_2019 + imiss_y_2019, data=VSG)
summary(Therm_model2)
