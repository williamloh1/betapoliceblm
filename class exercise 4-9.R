#Per slide 11
congress <- read.csv("https://jmontgomery.github.io/ProblemSets/incumbents.csv")
lm(voteshare~incspend, data=congress)
lm(voteshare~incspend+chalspend+presvote+chalquality, data=congress)
library(tidyverse)
congress %>% group_by(incspend>mean(incspend, na.rm=T)) %>%
  summarize(Chalspend = mean(chalspend, na.rm=TRUE), 
            Presvote=mean(presvote, na.rm=TRUE),
            Chalquality = mean(chalquality, na.rm=TRUE))

#Per slide 21
library(AER)
data("Fatalities")
Fatalities$fatal_rate <- Fatalities$fatal / Fatalities$pop * 10000
summary(lm(fatal_rate ~ beertax, data=Fatalities))

AL <- Fatalities[Fatalities$state=="al", ]
AZ <- Fatalities[Fatalities$state=="az", ]
AR <- Fatalities[Fatalities$state=="ar", ]
lm(fatal_rate ~ beertax, data=AL)$coefficients
lm(fatal_rate ~ beertax, data=AZ)$coefficients
lm(fatal_rate ~ beertax, data=AR)$coefficients


########  Put into slack what you think explains the flip in sign. 
####        (You don't need to put up the lines, but drawing them may help you think about it.)

model2<-lm(fatal_rate ~ beertax + factor(state), data=Fatalities)
head(model2$coefficients)

#---  It is probably because of the unobserved state-fixed factors, such
#---    as driving regulations, drinking culture, or other socio-economic
#---    factors, such as median income, education, unemployment etc. Another
#---    reasonable explanation is that the states with grater drinking habits
#---    are likely to have higher fatality rates, which make these states 
#---    more restrictive about tax regulations. Recall the hospitalization-
#---    average health comparison. Beer tax is likely to be assigned states
#---    with much more drinking habit.

#group assignment
palestinians<- read.csv("https://jmontgomery.github.io/ProblemSets/longo.csv")
my.model <- lm(militancy~ZA+Sample2009+Sample2009*ZA, data = palestinians)
library(sjPlot)
plot_model(my.model, type="int")
summary(my.model)
