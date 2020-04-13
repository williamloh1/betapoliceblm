#Per slide 11
congress <- read.csv("https://jmontgomery.github.io/ProblemSets/incumbents.csv")
summary(lm(voteshare~incspend, data=congress))
summary(lm(voteshare~incspend+chalspend+presvote+chalquality, data=congress))
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


#group assignment
palestinians<- read.csv("https://jmontgomery.github.io/ProblemSets/longo.csv")
my.model <- lm(militancy~ZA+Sample2009+Sample2009*ZA, data = palestinians)
library(sjPlot)
plot_model(my.model, type="int")
summary(my.model)
