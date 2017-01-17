http://insider.espn.com/nba/hollinger/statistics/_/page/3


setwd("F:/FANDUEL/NBA")
library(rvest)
library(dplyr)
library(data.table)
library(readr)
library(xgboost)
library(lubridate)


# Initialize Master Table -------------------------------------------------
url <- ("http://insider.espn.com/nba/hollinger/statistics")

dat <- read_html(url)
datx <- dat %>% html_table() %>% .[[1]]
colnames(datx) <- datx[2,]
datx <- datx[-1,]
datx <- datx[-1,]
datx <- datx[datx$RK != "RK",]

master_table <- datx[1,]
master_table <- master_table[-1,]

# Get All Data ------------------------------------------------------------
dates <- as.character(rev(seq(ymd("2016-10-25"), ymd("2017-01-08"), "day")))
url <- ("http://rotoguru1.com/cgi-bin/hoopstat-daterange0.pl?startdate=20170108&date=20170108&saldate=20170108&g=0&ha=&min=&tmptmin=0&tmptmax=999&opptmin=0&opptmax=999&gmptmin=0&gmptmax=999&gameday=&sd=0")


pages <- c("/_/page/2","/_/page/3","/_/page/4","/_/page/5","/_/page/6", "/_/page/7")
for (p in pages){
  dat <- read_html(paste0(url, p))
  datx <- dat %>% html_table() %>% .[[1]]
  colnames(datx) <- datx[2,]
  datx <- datx[-1,]
  datx <- datx[-1,]
  datx <- datx[datx$RK != "RK",]
  master_table <- rbind(master_table, datx)
}

write_csv(master_table, "season_player_efficiencies.csv")
team_pace <- fread("team_pace.csv")


team_pace
today1<- master_table[grep("ORL", master_table$PLAYER),]
t1 <- team_pace[team_pace$TEAM == "Orlando",]
today1$
today2 <- master_table[grep("UTA", master_table$PLAYER),]
t2 <- team_pace[team_pace$TEAM == "Utah",]
today <- rbind(today1,today2)
