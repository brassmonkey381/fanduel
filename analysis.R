setwd("F:/FANDUEL/NBA")
library(rvest)
library(dplyr)
library(data.table)
library(xgboost)
library(lubridate)

player_data <- fread("player_data_WIDE.csv")
player_home_away_data <- fread("player_home_away_data_WIDE.csv")
rosters <- fread("rosters.csv")
rosters$HT <- sapply(rosters$HT, function(x) ifelse(nchar(x) == 3, as.numeric(substr(x,1,1)) + as.numeric(substr(x,3,3)) / 12,
                                                    as.numeric(substr(x,1,1)) + as.numeric(substr(x,3,4)) / 12))
game_data <- fread("game_data.csv")
todays_games <- fread("file:///C:/Users/Brian/Downloads/FanDuel-NBA-2017-01-08-17592-players-list.csv")
todays_games$NAME <- paste0(todays_games$`First Name`, " ", todays_games$`Last Name`)

gd1 <- game_data %>%
  filter(ymd(Date) >= ymd("2017-01-01"),
         NAME %in% unique(todays_games$NAME))
gd1$V1 <- NULL
todays_games$V14 <- NULL
todays_games$V15 <- NULL

# gd1$NAME[gd1$NAME == "Louis Williams"] <- "Lou Williams"

today <- left_join(todays_games, gd1)
today <- today[complete.cases(today),]
# colnames(today) <- sapply(colnames(today), function(x) paste0("today_", x))
today <- today %>%
  filter(MIN != 0) %>% 
  group_by(NAME) %>%
  mutate(count = n(),
         Salary = as.numeric(Salary),
         Fantasy_PTS = 1.5*AST + 1.2*REB + 1*PTS + 2 * BLK + 2 * STL - 1 * TO,
         multiplier = Fantasy_PTS / Salary * 1000,
         running_multiplier = sum(Fantasy_PTS) / sum(Salary) * 1000)

today_summary <- today %>%
  group_by(NAME, Position, Salary, Team) %>% 
  summarise(running_multiplier = sum(Fantasy_PTS) / sum(Salary) * 1000)
today_summary$Expected_PTS <- round(today_summary$running_multiplier * today_summary$Salary/1000,2)



