senate_race <- read.csv("http://politicaldatascience.com/PDS/Datasets/SenateForecast/CandidateLevel.csv")
senate_race2018 <- read.csv("http://politicaldatascience.com/PDS/Datasets/SenateForecast/CandidateLevel2018.csv")
SimpleModelFull <- lm(VotePercentage~pvi*Republican+Incumbent, data=senate_race)

senate_race2018[senate_race2018$Candidateidentifier=="2018MOMcCaskill",]
