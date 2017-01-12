setwd("F:/FANDUEL/NBA")
library(rvest)
library(dplyr)
library(data.table)
library(xgboost)

# Initialize Roster table format ------------------------------------------
dat <- read_html("http://www.espn.com/nba/team/roster/_/name/bos/boston-celtics")
tbl <- dat %>% html_nodes("table")
coach <- (tbl %>% html_table() %>% .[[2]])[1]
coach <- gsub("Coach: ", "", coach[1,])
tbl <- tbl %>% html_table() %>% .[[1]] %>% mutate(X9 = coach)
tbl <- tbl[-1,]
tbl$X9[1] <- "Coach"
colnames(tbl) <- tbl[1,]
tbl <- tbl[-1,]

# Get URLS for team rosters -----------------------------------------------
html <- read_html("http://www.espn.com/nba/players")
a <- html %>% html_nodes("#my-players-table a")
b <- a %>% html_attrs() %>% unlist
urls <- c()
for(i in 1:length(b)){
  if(i %% 2 == 1){
    next()
  }
  urls <- c(urls, b[i])
}
urls2 <- c()
for(i in 1:(length(b)/2)){
  if(i %% 2 == 1){
    next()
  }
  urls2 <- c(urls2, urls[i-1])
}
urls2 <- gsub("team/", "team/roster/", urls2)

# Loop to get complete rosters --------------------------------------------
rosters <- tbl[1,]
rosters <- rosters[-1,]
all_player_urls <- c()
for (url in urls2){
  dat <- read_html(url)
  tbl <- dat %>% html_nodes("table")
  coach <- (tbl %>% html_table() %>% .[[2]])[1]
  coach <- gsub("Coach: ", "", coach[1,])
  tbl <- tbl %>% html_table() %>% .[[1]] %>% mutate(X9 = coach, X10 = (dat %>% html_nodes("b") %>% html_text())[1])
  tbl <- tbl[-1,]
  tbl$X9[1] <- "Coach"
  tbl$X10[1] <- "Team"
  colnames(tbl) <- tbl[1,]
  tbl <- tbl[-1,]
  rosters <- rbind(rosters, tbl)

  player_urls <- dat %>% html_nodes('a') %>% html_attr('href')
  player_urls <- player_urls[grepl("player/_/id", player_urls)]
  all_player_urls <- c(all_player_urls, player_urls)
}
all_player_urls <- gsub("player/", "player/splits/", all_player_urls)

# Initialize Game History format ------------------------------------------
dat2 <- read_html("http://www.espn.com/nba/player/gamelog/_/id/4240/avery-bradley")
tbl2 <- dat2 %>% html_nodes("table")
tbl2 <- tbl2 %>% html_table(fill=TRUE) %>% .[[2]]
tbl2 <- tbl2[min(which(tbl2[,1] == "DATE")):(which(tbl2[,1] == "REGULAR SEASON STATS")-1),]
colnames(tbl2) <- tbl2[1,]
tbl2 <- tbl2[-1,]
tbl2 <- tbl2[,colSums(is.na(tbl2))<nrow(tbl2)]
tbl2$NAME <- dat2 %>% html_nodes("h1") %>% html_text %>% .[1]

# Loop to get complete Player data -------------------------------------------------
player_data <- tbl2[1,]
player_data <- player_data[-1,]
all_player_urls <- gsub("splits", "gamelog", all_player_urls)

for (url in all_player_urls[1:442]){
  dat2 <- read_html(url)
  tbl2 <- dat2 %>% html_nodes("table")
  if(length(tbl2) < 2){
    next()
  }
  tbl2 <- tbl2 %>% html_table(fill=TRUE) %>% .[[2]]
  tbl2 <- tbl2[min(which(tbl2[,1] == "DATE")):(which(tbl2[,1] == "REGULAR SEASON STATS")-1),]
  colnames(tbl2) <- tbl2[1,]
  tbl2 <- tbl2[-1,]
  tbl2 <- tbl2[,colSums(is.na(tbl2))<(nrow(tbl2)-3)]
  tbl2$NAME <- dat2 %>% html_nodes("h1") %>% html_text %>% .[1]
  player_data <- rbind(player_data, tbl2)
}

write_csv(player_data, "game_data_raw.csv")

