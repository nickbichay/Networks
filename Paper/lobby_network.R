

setwd('/Users/nickbichay/Desktop/ /aPLS 900/Paper/')

data_raw  <- read.csv('all_foreign_principals_by_country__as_of_03_13_2018.csv', stringsAsFactor=FALSE)


library(countrycode)
library(igraph)
library(scales)
library(RColorBrewer)
library(dplyr)
library(stargazer)


##### CLEAN DATA

# years, country
data_raw$startyear <- as.numeric(substr(data_raw$RegistrationDate,7,11))
data_raw$endyear <- as.numeric(substr(data_raw$TerminationDate,7,11))
data_raw$endyear[is.na(data_raw$endyear)] <- 2019

data_raw$country <- tools::toTitleCase(tolower(data_raw$Country.LocationRepresented))

#fariss
fariss <- read.csv('HumanRightsProtectionScores_v2.04.csv', stringsAsFactor=FALSE)
fariss$year <- fariss$YEAR
fariss <- fariss[names(fariss) %in% c("year","COW","latentmean")]



##### CALL NETWORK FUNCTION (networker.R FILE) & CREATE STATS FOR 3 TIME PERIODS
# creates and plots network, and calculates network stats for specified time period

source('networker.R')

stats_old <- networker(data_raw, 1949, 1959)
stats_median <- networker(data_raw, 1976, 1986)
stats_modern <- networker(data_raw, 2003, 2013)



##### TABLE RESULTS


# firm centrality
stargazer(rbind(stats_old[[1]], stats_median[[1]], stats_modern[[1]]))

# country centrality
stargazer(rbind(stats_old[[2]], stats_median[[2]], stats_modern[[2]]))

# worst offenders
stargazer(rbind(stats_old[[3]], stats_median[[3]], stats_modern[[3]]), summary=FALSE)


