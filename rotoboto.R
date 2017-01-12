setwd("F:/FANDUEL/NBA")
library(rvest)
library(dplyr)
library(data.table)
library(xgboost)
library(lubridate)

# Functions ---------------------------------------------------------------
format_game_data <- function(game_data_raw){
  game_data_raw <- game_data_raw[grepl("\\w+\\s\\d{1,2}", game_data_raw$DATE),]
  game_data_raw$FMG <- sapply(game_data_raw$`FGM-FGA`, function(x) strsplit(x, "-")[[1]][1])
  game_data_raw$FMA <- sapply(game_data_raw$`FGM-FGA`, function(x) strsplit(x, "-")[[1]][2])
  game_data_raw$PM3 <- sapply(game_data_raw$`3PM-3PA`, function(x) strsplit(x, "-")[[1]][1])
  game_data_raw$PA3 <- sapply(game_data_raw$`3PM-3PA`, function(x) strsplit(x, "-")[[1]][2])
  game_data_raw$FTM <- sapply(game_data_raw$`FTM-FTA`, function(x) strsplit(x, "-")[[1]][1])
  game_data_raw$FTA <- sapply(game_data_raw$`FTM-FTA`, function(x) strsplit(x, "-")[[1]][2])
  game_data_raw$`FGM-FGA` <- NULL
  game_data_raw$`3PM-3PA` <- NULL
  game_data_raw$`FTM-FTA` <- NULL
  game_data_raw$Team_Score <- sapply(game_data_raw$SCORE, function(x) strsplit(strsplit(x, "-")[[1]][1], " ")[[1]][2])
  game_data_raw$Opp_Score <- sapply(game_data_raw$SCORE, function(x) strsplit(x, "-")[[1]][2])
  game_data_raw$Win_Lose <- sapply(game_data_raw$SCORE, function(x) strsplit(x, " ")[[1]][1])
  game_data_raw$SCORE <- NULL
  game_data_raw$Home_Away <- sapply(game_data_raw$OP, function(x) ifelse(length(grep("vs", x))==1, 'Home', 'Away'))
  game_data_raw$OPP <- sapply(game_data_raw$OP, function(x) gsub("vs|@","",x))
  game_data_raw$WDay <- sapply(game_data_raw$DATE, function(x) regmatches(x, gregexpr("^\\w+", x)))
  game_data_raw$Date <- sapply(game_data_raw$DATE, function(x) gsub("^\\w+", "", x))
  game_data_raw$DATE <- NULL
  game_data_raw$Date <- sapply(game_data_raw$Date, function(x) ifelse(length(grep("\\s1/", x))==1, paste0(x, "/2017"), paste0(x, "/2016")))
  game_data_raw$Date <- mdy(game_data_raw$Date)
  game_data_raw$WDay_CHECK <- weekdays(game_data_raw$Date, abbreviate = T)
  min(which(game_data_raw$WDay != game_data_raw$WDay_CHECK))
  bad_names <- unique(game_data_raw$NAME[(which(game_data_raw$WDay != game_data_raw$WDay_CHECK))])
  game_data_raw$WDay <- NULL
  game_data_raw <- game_data_raw %>%
    filter(!NAME %in% bad_names)
  cols = c(2:12,14:21)
  game_data_raw <- data.frame(game_data_raw)
  game_data_raw[,cols] = apply(game_data_raw[,cols], 2, function(x) as.numeric(as.character(x)))
  game_data_raw <- game_data_raw[,colSums(is.na(game_data_raw))<nrow(game_data_raw)]
  return(game_data_raw)
}

# # Initialize Roster table format ------------------------------------------
# dat <- read_html("http://www.espn.com/nba/team/roster/_/name/bos/boston-celtics")
# tbl <- dat %>% html_nodes("table")
# coach <- (tbl %>% html_table() %>% .[[2]])[1]
# coach <- gsub("Coach: ", "", coach[1,])
# tbl <- tbl %>% html_table() %>% .[[1]] %>% mutate(X9 = coach)
# tbl <- tbl[-1,]
# tbl$X9[1] <- "Coach"
# colnames(tbl) <- tbl[1,]
# tbl <- tbl[-1,]
# 
# # Get URLS for team rosters -----------------------------------------------
# html <- read_html("http://www.espn.com/nba/players")
# a <- html %>% html_nodes("#my-players-table a")
# b <- a %>% html_attrs() %>% unlist
# urls <- c()
# for(i in 1:length(b)){
#   if(i %% 2 == 1){
#     next()
#   }
#   urls <- c(urls, b[i])
# }
# urls2 <- c()
# for(i in 1:(length(b)/2)){
#   if(i %% 2 == 1){
#     next()
#   }
#   urls2 <- c(urls2, urls[i-1])
# }
# urls2 <- gsub("team/", "team/roster/", urls2)
# 
# # Loop to get complete rosters --------------------------------------------
# rosters <- tbl[1,]
# rosters <- rosters[-1,]
# all_player_urls <- c()
# for (url in urls2){
#   dat <- read_html(url)
#   tbl <- dat %>% html_nodes("table")
#   coach <- (tbl %>% html_table() %>% .[[2]])[1]
#   coach <- gsub("Coach: ", "", coach[1,])
#   tbl <- tbl %>% html_table() %>% .[[1]] %>% mutate(X9 = coach, X10 = (dat %>% html_nodes("b") %>% html_text())[1])
#   tbl <- tbl[-1,]
#   tbl$X9[1] <- "Coach"
#   tbl$X10[1] <- "Team"
#   colnames(tbl) <- tbl[1,]
#   tbl <- tbl[-1,]
#   rosters <- rbind(rosters, tbl)
#   
#   player_urls <- dat %>% html_nodes('a') %>% html_attr('href')
#   player_urls <- player_urls[grepl("player/_/id", player_urls)]
#   all_player_urls <- c(all_player_urls, player_urls)
# }
# all_player_urls <- gsub("player/", "player/splits/", all_player_urls)
# 
# # Initialize Player table format ------------------------------------------
# dat2 <- read_html("http://www.espn.com/nba/player/splits/_/id/4240/avery-bradley")
# tbl2 <- dat2 %>% html_nodes("table")
# tbl2 <- tbl2 %>% html_table(fill=TRUE) %>% .[[2]]
# tbl2 <- tbl2[which(tbl2[,1] == "By Opponent"):(which(tbl2[,1] == "By Arena")-1),]
# colnames(tbl2) <- tbl2[1,]
# tbl2 <- tbl2[-1,]
# tbl2 <- tbl2[,colSums(is.na(tbl2))<nrow(tbl2)]
# tbl2$NAME <- dat2 %>% html_nodes("h1") %>% html_text %>% .[1]
# 
# # Loop to get complete Player data -------------------------------------------------
# player _data <- tbl2
# player_data <- player_data[-1,]
# 
# for (url in all_player_urls){
#   dat2 <- read_html(url)
#   tbl2 <- dat2 %>% html_nodes("table")
#   if(length(tbl2) < 2){
#     next()
#   }
#   tbl2 <- tbl2 %>% html_table(fill=TRUE) %>% .[[2]]
#   tbl2 <- tbl2[which(tbl2[,1] == "By Opponent"):(which(tbl2[,1] == "By Arena")-1),]
#   colnames(tbl2) <- tbl2[1,]
#   tbl2 <- tbl2[-1,]
#   tbl2 <- tbl2[,colSums(is.na(tbl2))<nrow(tbl2)]
#   tbl2$NAME <- dat2 %>% html_nodes("h1") %>% html_text %>% .[1]
#   player_data <- rbind(player_data, tbl2)
# }
# 
# player_data$FMG <- sapply(player_data$`FGM-FGA`, function(x) strsplit(x, "-")[[1]][1])
# player_data$FMA <- sapply(player_data$`FGM-FGA`, function(x) strsplit(x, "-")[[1]][2])
# player_data$PM3 <- sapply(player_data$`3PM-3PA`, function(x) strsplit(x, "-")[[1]][1])
# player_data$PA3 <- sapply(player_data$`3PM-3PA`, function(x) strsplit(x, "-")[[1]][2])
# player_data$FTM <- sapply(player_data$`FTM-FTA`, function(x) strsplit(x, "-")[[1]][1])
# player_data$FTA <- sapply(player_data$`FTM-FTA`, function(x) strsplit(x, "-")[[1]][2])
# cols = c(2:18,20:25)
# player_data <- data.frame(player_data)
# player_data[,cols] = apply(player_data[,cols], 2, function(x) as.numeric(as.character(x)))
# player_data <- player_data[,colSums(is.na(player_data))<nrow(player_data)]
# 
# x <- player_data %>% melt(id.vars = c("NAME", "By.Opponent"))
# y <- dcast(x, NAME ~ By.Opponent + variable, fun.aggregate = mean)
# write_csv(y, "player_data_WIDE.csv")


# # Initialize Player HOME/AWAY format ------------------------------------------
# dat3 <- read_html("http://www.espn.com/nba/player/splits/_/id/4240/avery-bradley")
# tbl3 <- dat3 %>% html_nodes("table")
# tbl3 <- tbl3 %>% html_table(fill=TRUE) %>% .[[2]]
# tbl3 <- tbl3[which(tbl3[,1] == "By Arena"):(nrow(tbl3)-1),]
# colnames(tbl3) <- tbl3[1,]
# tbl3 <- tbl3[-1,]
# tbl3 <- tbl3[,colSums(is.na(tbl3))<nrow(tbl3)]
# tbl3$NAME <- dat3 %>% html_nodes("h1") %>% html_text %>% .[1]
# tbl3$FMG <- sapply(tbl3$`FGM-FGA`, function(x) strsplit(x, "-")[[1]][1])
# tbl3$FMA <- sapply(tbl3$`FGM-FGA`, function(x) strsplit(x, "-")[[1]][2])
# tbl3$PM3 <- sapply(tbl3$`3PM-3PA`, function(x) strsplit(x, "-")[[1]][1])
# tbl3$PA3 <- sapply(tbl3$`3PM-3PA`, function(x) strsplit(x, "-")[[1]][2])
# tbl3$FTM <- sapply(tbl3$`FTM-FTA`, function(x) strsplit(x, "-")[[1]][1])
# tbl3$FTA <- sapply(tbl3$`FTM-FTA`, function(x) strsplit(x, "-")[[1]][2])
# cols = c(2:18,20:25)  
# tbl3[,cols] = apply(tbl3[,cols], 2, function(x) as.numeric(as.character(x)))
# tbl3 <- tbl3[,colSums(is.na(tbl3))<nrow(tbl3)]
# 
# tbl3 <- tbl3 %>%
#   mutate(GP = as.numeric(GP),
#          Arena = ifelse(GP == max(GP), "Home", "Away")) %>% 
#   group_by(Arena, NAME) %>%
#   select(-`By Arena`, -GP) %>%
#   summarise_all(mean) %>%
#   left_join(tbl3 %>%
#               mutate(GP = as.numeric(GP),
#                      Arena = ifelse(GP == max(GP), "Home", "Away")) %>% 
#               group_by(Arena, NAME) %>%
#               summarise(GP = sum(GP)), by=c("Arena", "NAME"))
# 
# # Loop to get complete Player HOME/AWAY data -------------------------------------------------
# player_home_away_data <- tbl3[1,]
# player_home_away_data <- player_home_away_data[-1,]
# 
# for (url in all_player_urls){
#   dat3 <- read_html(url)
#   tbl3 <- dat3 %>% html_nodes("table")
#   if(length(tbl3) < 2){
#     next()
#   }
#   tbl3 <- tbl3 %>% html_table(fill=TRUE) %>% .[[2]]
#   tbl3 <- tbl3[which(tbl3[,1] == "By Arena"):(nrow(tbl3)-1),]
#   colnames(tbl3) <- tbl3[1,]
#   tbl3 <- tbl3[-1,]
#   tbl3 <- tbl3[,colSums(is.na(tbl3))<nrow(tbl3)]
#   tbl3$NAME <- dat3 %>% html_nodes("h1") %>% html_text %>% .[1]
#   tbl3$FMG <- sapply(tbl3$`FGM-FGA`, function(x) strsplit(x, "-")[[1]][1])
#   tbl3$FMA <- sapply(tbl3$`FGM-FGA`, function(x) strsplit(x, "-")[[1]][2])
#   tbl3$PM3 <- sapply(tbl3$`3PM-3PA`, function(x) strsplit(x, "-")[[1]][1])
#   tbl3$PA3 <- sapply(tbl3$`3PM-3PA`, function(x) strsplit(x, "-")[[1]][2])
#   tbl3$FTM <- sapply(tbl3$`FTM-FTA`, function(x) strsplit(x, "-")[[1]][1])
#   tbl3$FTA <- sapply(tbl3$`FTM-FTA`, function(x) strsplit(x, "-")[[1]][2])
#   cols = c(2:18,20:25)  
#   tbl3[,cols] = apply(tbl3[,cols], 2, function(x) as.numeric(as.character(x)))
#   tbl3 <- tbl3[,colSums(is.na(tbl3))<nrow(tbl3)]
#   
#   tbl3 <- tbl3 %>%
#     mutate(GP = as.numeric(GP),
#            Arena = ifelse(GP == max(GP), "Home", "Away")) %>% 
#     group_by(Arena, NAME) %>%
#     select(-`By Arena`, -GP) %>%
#     summarise_all(mean) %>%
#     left_join(tbl3 %>%
#                 mutate(GP = as.numeric(GP),
#                        Arena = ifelse(GP == max(GP), "Home", "Away")) %>% 
#                 group_by(Arena, NAME) %>%
#                 summarise(GP = sum(GP)), by=c("Arena", "NAME"))
#   player_home_away_data <- rbind(player_home_away_data, tbl3)
# }
# player_home_away_data <- player_home_away_data[(!duplicated(player_home_away_data)),]

# xgboost -----------------------------------------------------------------
player_data <- fread("player_data_WIDE.csv")
player_home_away_data <- fread("player_home_away_data_WIDE.csv")
rosters <- fread("rosters.csv")
rosters$HT <- sapply(rosters$HT, function(x) ifelse(nchar(x) == 3, as.numeric(substr(x,1,1)) + as.numeric(substr(x,3,3)) / 12,
                                                as.numeric(substr(x,1,1)) + as.numeric(substr(x,3,4)) / 12))
# game_data_raw <- fread("game_data_raw.csv")
# game_data <- format_game_data(game_data_raw)
# write.csv(game_data, "game_data.csv")
game_data <- fread("game_data.csv")

all_data <- left_join(rosters, player_home_away_data)
all_data <- left_join(all_data, player_data)
all_data <- left_join(all_data, game_data)
all_data$V1 <- NULL
# write_csv(all_data, "all_data.csv")

train <- all_data
train <- train %>%
  arrange(Date) %>%
  mutate(NAME = as.numeric(as.factor(NAME)),
         POS = as.numeric(as.factor(POS)),
         COLLEGE = as.numeric(as.factor(COLLEGE)),
         Coach = as.numeric(as.factor(Coach)),
         Team = as.numeric(as.factor(Team)),
         `2016-2017 SALARY` = log1p(as.numeric(gsub("\\$|,", "", `2016-2017 SALARY`))),
         Win_Lose = as.numeric(as.factor(Win_Lose)),
         Home_Away = as.numeric(as.factor(Home_Away)),
         Date = as.numeric(as.factor(Date)),
         WDay_CHECK = as.numeric(as.factor(WDay_CHECK)),
         OPP = as.numeric(as.factor(OPP)))

train <- train[complete.cases(train),]
set.seed(312)
model_rows <- sample(nrow(train), floor(nrow(train) * .8))
valid_rows <- setdiff(1:nrow(train), model_rows)
model <- train[model_rows,]
valid <- train[valid_rows,]
m1 <- model %>% ungroup() %>% select(-(MIN:Win_Lose), PTS)
v1 <- valid %>% ungroup() %>% select(-(MIN:Win_Lose), PTS)

X_train <- xgb.DMatrix(m1 %>% select(-PTS) %>% as.matrix(), label = model$PTS, missing = NaN)
X_valid <- xgb.DMatrix(v1 %>% select(-PTS) %>% as.matrix(), label = valid$PTS, missing = NaN)
watchlist <- list(test=X_valid, train=X_train)

param <- list(objective = "reg:linear",
              eval_metric = "rmse",
              eta = 0.03,
              booster = "gbtree",
              max_depth=7, 
              subsample=0.85,
              col_sample = 0.85) 

xgb <- xgb.train(X_train, params = param, nfold = 2, nrounds = 50000, watchlist = watchlist, early.stop.round = 50)
set.seed(312)
xgb <- xgb.train(X_train, params = param, nfold = 2, nrounds = xgb$bestInd)

## Evaluation
valid$pred <- predict(xgb, X_valid)
mae <- function(x, y){
  mean(abs(x-y))
}
with(valid, mae(PTS,pred))
result <- all_data[valid_rows,] %>% select(Team, NAME, Date, PTS) %>% mutate(pred = valid$pred, err = pred - PTS)
summary(result$err)
summary(result$PTS)
sd(result$err, na.rm = T)

library(ggplot2)
m <- xgb.dump(xgb, with.stats = T)
m[1:10] #This statement prints top 10 nodes of the model

# Feature Importance ------------------------------------------------------
# Get the feature real names
names <- dimnames(data.matrix((m1)))[[2]]

# Compute feature importance matrix
importance_matrix <- xgb.importance(names, model = xgb)
# Nice graph
importance_matrix <- importance_matrix[order(-importance_matrix$Frequence),]
ggplot(importance_matrix, aes(x = reorder(Feature, Frequence), Frequence, fill="red")) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position="none")


