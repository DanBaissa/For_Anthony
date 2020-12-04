library(tidyverse)
library(ggplot2)
library(shiny)
library(ggforce)
library(readr)
library(dplyr, warn.conflicts = FALSE)

# Reading in .csv files
# Premier_League_Transfer_Activity is the transfer balances of 
# every Premier League team from the 16/17 season to the 20/21 season.


Premier_League_Dataset <- read_csv("Premier League Dataset.csv",
                                   col_types = 
                                     cols(
                                       .default = col_double(),
                                       Date = col_character(),
                                       HomeTeam = col_character(),
                                       AwayTeam = col_character(),
                                       FTR = col_character(),
                                       HM1 = col_character(),
                                       HM2 = col_character(),
                                       HM3 = col_character(),
                                       HM4 = col_character(),
                                       HM5 = col_character(),
                                       AM1 = col_character(),
                                       AM2 = col_character(),
                                       AM3 = col_character(),
                                       AM4 = col_character(),
                                       AM5 = col_character(),
                                       HTFormPtsStr = col_character(),
                                       ATFormPtsStr = col_character())) 




# Function  -----------------------------------------------------------

datamaker <- function(csv, s, year){
  season <- read.csv(csv)
  
  teamsinseason1 <- season %>%
    select(HomeTeam) 
  
  teamsinpremierleague <- unique(teamsinseason1)
  vectorofteamsinpremierleague <- teamsinpremierleague$HomeTeam
  
  # Setting the number of total goals in the Premier League from 
  # the 00/01 season to 0.
  
  teamsinpremierleague$totalgoals <- 0
  teamsinpremierleague$totalgoals_conceded <- 0
  teamsinpremierleague$total_away_goals <- 0
  teamsinpremierleague$total_homegoals_goals <- 0
  teamsinpremierleague$total_away_goals_conceded <- 0 #How many goals they conceed when they are the away team
  teamsinpremierleague$total_home_goals_conceded <- 0 #How many goals they conceed when they are the home team
  
  # Calculating the total number of goals scored by
  # each Premier League team in the 00/01 season.
  
  for (i in 1:length(vectorofteamsinpremierleague)) {
    homegoals <- season %>%
      select(HomeTeam, FTHG) %>%
      filter(HomeTeam == vectorofteamsinpremierleague[i])
    teamsinpremierleague$total_homegoals_goals[i] <- sum(homegoals$FTHG)
    
    awaygoals <- season %>%
      select(AwayTeam, FTAG) %>%
      filter(AwayTeam == vectorofteamsinpremierleague[i])
    teamsinpremierleague$total_away_goals[i] <- sum(awaygoals$FTAG)
    
    totalgoals = sum(homegoals$FTHG) + sum(awaygoals$FTAG)
    teamsinpremierleague$totalgoals[i] <- totalgoals
    
  }
  
  for (i in 1:length(vectorofteamsinpremierleague)) {
    homegoals_conceded <- season %>%
      select(HomeTeam, FTAG) %>%
      filter(HomeTeam == vectorofteamsinpremierleague[i])
    teamsinpremierleague$total_home_goals_conceded[i] <- sum(homegoals_conceded$FTAG)
    
    awaygoals_conceded <- season %>%
      select(AwayTeam, FTHG) %>%
      filter(AwayTeam == vectorofteamsinpremierleague[i])
    teamsinpremierleague$total_away_goals_conceded[i] <- sum(awaygoals_conceded$FTHG)
    
    totalgoals_conceded = sum(homegoals_conceded$FTAG) + sum(awaygoals_conceded$FTHG)
    teamsinpremierleague$totalgoals_conceded[i] <- totalgoals_conceded
    
  }
  
  
  teamsinpremierleague$Season <- s
  teamsinpremierleague$Year <- year
  teamsinpremierleague$goal_difference <- teamsinpremierleague$totalgoals - teamsinpremierleague$totalgoals_conceded
  return(teamsinpremierleague)
}

#testing to see if it works 

D1 <- datamaker(csv = "Premier League Datasets/2000-01.csv", s = "00/01", year = 2000)    
D2 <- datamaker(csv = "Premier League Datasets/2001-02.csv", s = "01/02", year = 2001)    
D3 <- datamaker(csv = "Premier League Datasets/2002-03.csv", s = "02/03", year = 2002)    



# Setting up the code -----------------------------------------------------

## Setting up the year format for the csv and season
y1 <- 2000:2019 
y2 <- 1:20

y2 <- sprintf("%02d",y2)

season_years <- paste0(y1,"-",y2) # setting the season year
season_years

## Setting up a vector of csvs to read in
data_to_read <- paste0("Premier League Datasets/",season_years, ".csv") # you can remove the "Premier League Datasets/" if that is not needed
data_to_read

# Testing to make sure it works
D5 <- datamaker(csv = data_to_read[5], s = season_years[5], year = y1[5])    



# Making the data ---------------------------------------------------------


Prem_data <- c()

for (i in 1:length(data_to_read)) {
  d <- datamaker(csv = data_to_read[i], s = season_years[i], year = y1[i])
  
  Prem_data <- rbind(Prem_data, d)
}

view(Prem_data)