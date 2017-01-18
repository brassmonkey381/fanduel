setwd("F:/FANDUEL/NBA")
library(rvest)
library(tidyr)
library(dplyr)
library(data.table)
library(readr)
library(xgboost)
library(lubridate)
library(FeatureHashing)
library(Matrix)

# Functions ---------------------------------------------------------------
plot_cor <- function(team_data, metric){
  dat <- team_data %>% select(`First  Last`, Date, which(colnames(team_data) == metric)) %>% melt(id.vars = c("First  Last", "Date"))
  dat <- dcast(dat, `Date` ~  `First  Last` + variable)
  # dat[is.na(dat)] <- 0
  
  a <- cor(dat, use = 'pairwise.complete.obs')
  qplot(x=Var1, y=Var2, data=melt(a), fill=value, geom="tile") + 
    scale_fill_distiller(palette = "Spectral") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}
# Matthews correlation coefficient
mcc <- function(actual, predicted) {
  
  tp <- as.numeric(sum(actual == 1 & predicted == 1))
  tn <- as.numeric(sum(actual == 0 & predicted == 0))
  fp <- as.numeric(sum(actual == 0 & predicted == 1))
  fn <- as.numeric(sum(actual == 1 & predicted == 0))
  
  numer <- (tp * tn) - (fp * fn)
  denom <- ((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn)) ^ 0.5
  
  if (denom == 0) return(0)
  else return(numer / denom)
}
# Aggregator by Game
aggregate_last_x_games <- function(game_date, player, columns_to_summarise, n, sum_fun){
  game_date <- ymd(game_date)
  filtered_data <- datx %>% 
    filter(first_last == player, date < game_date) %>%
    top_n(n, date)
  vars = c("last_first", columns_to_summarise)
  summary <- filtered_data %>%
    group_by(last_first) %>%
    select(one_of(vars)) %>%
    summarise_all(funs_(sum_fun))
  colnames(summary)[2:ncol(summary)] <- sapply(colnames(summary)[2:ncol(summary)], function(x)
    paste0("last_", n, "_games_", sum_fun, "_", x))
  if(nrow(summary) != 0){
    summary <- cbind(data.frame(date = game_date), summary)
  }
  return(summary)
}
# Create Aggregated table by game
create_aggregae_last_x_games <- function(n, sum_fun){
  game_date <- ymd(datx$date[1])
  player <- datx$first_last[1]
  columns_to_summarise <- colnames(datx)[sapply(datx,is.numeric)]
  columns_to_summarise <- columns_to_summarise[!grepl("gid|pos|gtime_et_", columns_to_summarise)]
  # n <- 1
  # sum_fun <- "mean"
  template <- aggregate_last_x_games(game_date, player, columns_to_summarise, n, sum_fun)[1,]
  template <- template[-1,]
  
  for(row in 1:nrow(datx)){
    temp <- aggregate_last_x_games(datx$date[row], datx$first_last[row], columns_to_summarise, n, sum_fun)
    if(nrow(temp) == 0){next()}
    template <- bind_rows(template, temp)
  }
  return(template)
}
# Add Efficiencies
add_efficiencies <- function(datx){
  top_eff <- datx %>%
    filter(fd_pos != 5) %>% 
    group_by(date, fd_pos) %>%
    top_n(3, multiplier) %>%
    ungroup() %>%
    select(date, fd_pos, first_last) %>%
    mutate(top_by_position_day = 1)
  top_eff_centers <- datx %>%
    filter(fd_pos == 5) %>% 
    group_by(date, fd_pos) %>%
    top_n(2, multiplier) %>%
    ungroup() %>%
    select(date, fd_pos, first_last) %>%
    mutate(top_by_position_day = 1) 
  top_eff <- rbind(top_eff, top_eff_centers)
  final <- data.frame(left_join(datx, top_eff))
  final$top_by_position_day[is.na(final$top_by_position_day)] <- 0
  return(final)
}
# CLEAN VEGAS ODDS DATA
clean_vegas <- function(fin){
  fin <- fin[,-1]
  fin <- fin[,-1]
  fin <- fin[,-3]
  
  for (i in 1:ncol(fin)){
    if(class(fin[,i]) == "factor"){
      fin[,i] <- as.character(fin[,i])
    }
  }
  
  # half_char <- substr(fin[3,][5], 3,3)
  half_char <- "½"
  fin <- data.frame(apply(fin, c(1,2), function(x) gsub(half_char, ".5", x)))
  for (i in 1:ncol(fin)){
    if(class(fin[,i]) == "factor"){
      fin[,i] <- as.character(fin[,i])
    }
  }
  
  fin2 <- data.frame(apply(fin, c(1,2), function(x) gsub("-1\\d{2}|\\+1\\d{2}", "", x)))
  for (i in 1:ncol(fin2)){
    if(class(fin2[,i]) == "factor"){
      fin2[,i] <- as.character(fin2[,i])
    }
  }
  
  # fin2 <- data.frame(apply(fin2, c(1,2), function(x) gsub("\\d+%", "", x)))
  # for (i in 1:ncol(fin2)){
  #   if(class(fin2[,i]) == "factor"){
  #     fin2[,i] <- as.character(fin2[,i])
  #   }
  # }
  fin2[,2] <- sapply(fin2[,2], function(x) gsub("Options","",x))
  fin2[,4] <- sapply(fin2[,4], function(x) gsub("%","% ",x))
  fin2[,4] <- sapply(fin2[,4], function(x) gsub("%\\s$","%",x))
  
  x <- separate(fin2, X6, c("a", "b"), sep = "\\s")
  x <- separate(x, X7, c("a", "b"), sep = "\\s")
  x[,5] <- as.numeric(gsub("%", "", x[,5]))
  x[,6] <- as.numeric(gsub("%", "", x[,6]))
  colnames(x)[5:6] <- c("pct1", "pct2")
  x <- separate(x, X8, c("a", "b"), sep = "\\s")
  x <- separate(x, X9, c("a", "b"), sep = "\\s")
  x <- separate(x, X10, c("a", "b"), sep = "\\s")
  x <- separate(x, X11, c("a", "b"), sep = "\\s")
  x <- separate(x, X12, c("a", "b"), sep = "\\s")
  x <- separate(x, X13, c("a", "b"), sep = "\\s")
  x <- separate(x, X14, c("a", "b"), sep = "\\s")
  x <- separate(x, X15, c("a", "b"), sep = "\\s")
  x <- separate(x, X16, c("a", "b"), sep = "\\s")
  x <- separate(x, X17, c("a", "b"), sep = "\\s")
  colnames(x)[23:24] <- c('a.9', 'b.9')
  colnames(x)[25:26] <- c('a.10', 'b.10')
  
  x[,3:26] <- apply(x[,3:26], 2, as.numeric)
  
  pfg <- x %>%
    select(starts_with("a"), starts_with("b"))
  for(k in 1:nrow(pfg)){
    for(m in 1:11){
      if(is.na(pfg[k,][m]) | pfg[k,][m] < 150){
        temp = pfg[k,][m]
        pfg[k,][m] = pfg[k,][m+11]
        pfg[k,][m+11] = temp
      }
    }
  }
  
  y <- pfg %>%
    select(starts_with("a"))
  z <- apply(y, 1, function(x) mean(x, na.rm=T))
  xyz <- cbind(y,z)
  
  u <- pfg %>%
    select(starts_with("b"))
  u[u >= 30] <- NA
  v <- apply(u, 1, function(x) mean(x, na.rm=T))
  tuv <- cbind(u,v)
  odds <- data.frame(overunder = xyz$z, margin = tuv$v)
  
  x <- cbind(x[,c(1,2)], odds)
  teams <- unique(datx$Team)[order(unique(datx$Team))]
  x$X4 <- gsub("Atlanta", paste0(" ", teams[1]), x$X4)
  x$X4 <- gsub("Boston", paste0(" ", teams[2]), x$X4)
  x$X4 <- gsub("Brooklyn", paste0(" ", teams[3]), x$X4)
  x$X4 <- gsub("Charlotte", paste0(" ", teams[4]), x$X4)
  x$X4 <- gsub("Chicago", paste0(" ", teams[5]), x$X4)
  x$X4 <- gsub("Cleveland", paste0(" ", teams[6]), x$X4)
  x$X4 <- gsub("Dallas", paste0(" ", teams[7]), x$X4)
  x$X4 <- gsub("Denver", paste0(" ", teams[8]), x$X4)
  x$X4 <- gsub("Detroit", paste0(" ", teams[9]), x$X4)
  x$X4 <- gsub("Golden State", paste0(" ", teams[10]), x$X4)
  x$X4 <- gsub("Houston", paste0(" ", teams[11]), x$X4)
  x$X4 <- gsub("Indiana", paste0(" ", teams[12]), x$X4)
  x$X4 <- gsub("L.A. Clippers", paste0(" ", teams[13]), x$X4)
  x$X4 <- gsub("L.A. Lakers", paste0(" ", teams[14]), x$X4)
  x$X4 <- gsub("Memphis", paste0(" ", teams[15]), x$X4)
  x$X4 <- gsub("Miami", paste0(" ", teams[16]), x$X4)
  x$X4 <- gsub("Milwaukee", paste0(" ", teams[17]), x$X4)
  x$X4 <- gsub("Minnesota", paste0(" ", teams[18]), x$X4)
  x$X4 <- gsub("New Orleans", paste0(" ", teams[19]), x$X4)
  x$X4 <- gsub("New York", paste0(" ", teams[20]), x$X4)
  x$X4 <- gsub("Oklahoma City", paste0(" ", teams[21]), x$X4)
  x$X4 <- gsub("Orlando", paste0(" ", teams[22]), x$X4)
  x$X4 <- gsub("Philadelphia", paste0(" ", teams[23]), x$X4)
  x$X4 <- gsub("Phoenix", paste0(" ", teams[24]), x$X4)
  x$X4 <- gsub("Portland", paste0(" ", teams[25]), x$X4)
  x$X4 <- gsub("Sacramento", paste0(" ", teams[26]), x$X4)
  x$X4 <- gsub("San Antonio", paste0(" ", teams[27]), x$X4)
  x$X4 <- gsub("Toronto", paste0(" ", teams[28]), x$X4)
  x$X4 <- gsub("Utah", paste0(" ", teams[29]), x$X4)
  x$X4 <- gsub("Washington", paste0(" ", teams[30]), x$X4)
  x$X4 <- gsub("\\s$|^\\s", "", x$X4)
  x <- separate(x, X4, c("team", "opp"))
  # x$team[1] <- "tor"; x$opp[1] <- "bkn"
  colnames(x)[1] <- 'time'
  return(x)
}

# Load Data ---------------------------------------------------------------
data <- fread("RG_all_games_2.csv")
data <- data %>%
  mutate(date = ymd(as.character(Date)),
         multiplier = FDP / `FD Sal` * 1000)
data <- data[complete.cases(data),]

top_eff <- data %>%
  filter(`FD pos` != 5) %>% 
  group_by(Date, `FD pos`) %>%
  top_n(3, multiplier) %>%
  ungroup() %>%
  select(Date, `FD pos`, `First  Last`) %>%
  mutate(top_by_position_day = 1)
top_eff_centers <- data %>%
  filter(`FD pos` == 5) %>% 
  group_by(Date, `FD pos`) %>%
  top_n(3, multiplier) %>%
  ungroup() %>%
  select(Date, `FD pos`, `First  Last`) %>%
  mutate(top_by_position_day = 1) 
top_eff <- rbind(top_eff, top_eff_centers)
# top_eff <- top_eff_centers

toStr <- function(x) {paste(x, collapse = ",")}
daily_info <- data %>%
  group_by(Date, `FD pos`) %>%
  summarise(pool = toStr(`First  Last`)) %>%
  ungroup() %>%
  group_by(Date) %>%
  mutate(pool_day = toStr(pool))
vegas_odds <- fread('vegas_odds.csv')

# colnames(datx) <- tolower(gsub("\\.+", "_", colnames(datx)))
# datx$date <- ymd(datx$date)
# datx$multiplier <- datx$fdp / datx$fd_sal * 1000
# datx$ppm <- datx$fdp / datx$minutes
# datx <- add_efficiencies(datx)
## Team minutes
data <- data %>%
  mutate(w_l = ifelse(`Team pts` > `Opp pts`, "W", "L"))
datx <- data
colnames(datx)[7] <- 'h_a'
min_sum <- datx %>%
  group_by(date, Team, Start, w_l, h_a) %>%
  summarise(team_minutes = sum(Minutes))
team_min_sum <- min_sum %>% 
  group_by(Team, Start, w_l, h_a) %>%
  summarise(Minutes = sum(team_minutes)) %>%
  group_by(Team, w_l, h_a) %>%
  summarise(start_to_bench_minutes = Minutes[Start == 1] / Minutes[Start == 0])

# # Load Data 2 (all newest) ---------------------------------------------------------------
# # NEWEST DATA?
# url <- ("http://rotoguru1.com/cgi-bin/nba-dhd-2017.pl")
# 
# dat <- read_html(url)
# text <- dat %>% html_text
# 
# a <- gregexpr("GID", text)[[1]][1]
# b <- gregexpr("A data legend", text)[[1]][1]-1
# t <- substr(text, a, b)
# 
# fileConn<-file("output.txt")
# writeLines(t, fileConn)
# close(fileConn)
# 
# datx <- read.table("output.txt", sep=":", header=T, fill = TRUE)
# datx <- datx[,colSums(is.na(datx)) != nrow(datx)]
# datx <- datx[complete.cases(datx),]
# for (i in 1:ncol(datx)){
#   if(class(datx[,i]) == "factor"){
#     datx[,i] <- as.character(datx[,i])
#   }
# }
# colnames(datx) <- tolower(gsub("\\.+", "_", colnames(datx)))
# datx$date <- ymd(datx$date)
# datx$multiplier <- datx$fdp / datx$fd_sal * 1000
# datx$ppm <- datx$fdp / datx$minutes
# datx <- add_efficiencies(datx)

# Combine All The Data ----------------------------------------------------
labels <- left_join(daily_info, top_eff, by=c("Date", "FD pos"))
data <- left_join(data, team_min_sum, by=c("Team", "w_l", "H/A" = "h_a"))
train <- left_join(data, labels %>% select(Date, `FD pos`, pool) %>% ungroup() %>% mutate(Date = ymd(Date)) %>% distinct(.keep_all=T), by=c("date" = "Date", "FD pos"))
train <- left_join(train, labels %>% select(-`FD pos`, -pool), by=c("Date", "First  Last"))
train$top_by_position_day[is.na(train$top_by_position_day)] <- 0
train <- train %>%
  arrange(date)
train$Date <- NULL
colnames(train)[3] <- "First_Last"
colnames(train) <- gsub("\\s+", "_", colnames(train))
colnames(train)[6] <- "H_A"
# train <- train %>% filter(FD_pos == 1)
# train <- train %>% filter(Minutes >= 10)
colnames(train) <- tolower(colnames(train))
colnames(train) <- gsub(",|\\(|\\)", "", colnames(train))
vegas_odds$date <- ymd(vegas_odds$date)
train2 <- left_join(train, vegas_odds, by=c("date", "team" = "team", "opp" = "opp"))
train2 <- left_join(train2, vegas_odds, by=c("date", "team" = "opp", "opp" = "team"))
train2 <- train2 %>%
  mutate(time = ifelse(is.na(time.x), time.y, time.x),
         overunder = ifelse(is.na(overunder.x), overunder.y, overunder.x),
         margin = ifelse(is.na(margin.x), margin.y, margin.x))
train2$time.x <- NULL
train2$time.y <- NULL
train2$margin.x <- NULL
train2$margin.y <- NULL
train2$overunder.x <- NULL
train2$overunder.y <- NULL
train <- train2

### Using latest stuffs
# past <- past[,-c(24,47,70,93)]
# past <- past[,-c(24,46,68,90)]
# datx <- left_join(datx, past, by=c("last_first", "date"))
# colnames(daily_info) <- c("date", "fd_pos", "pool", "pool_day")
# daily_info$date <- ymd(daily_info$date)

# train <- left_join(datx, daily_info)

# Feature Hashing Model ---------------------------------------------------
b <- 2 ^ 10
f <- ~ first_last + fd_sal + team + opp + h_a + time + overunder + margin + minutes + fd_pos + split(pool, delim = ",") + split(pool_day, delim = ",") - 1
X_train <- hashed.model.matrix(f, train2, b)
# X_train <- cBind(X_train, train %>% select(starts_with("last_10")) %>% as.matrix)
# X_train <- cBind(X_train, train %>% select(starts_with("last_5")) %>% as.matrix)
# X_train <- cBind(X_train, train %>% select(starts_with("last_3")) %>% as.matrix)
# X_train <- cBind(X_train, train %>% select(starts_with("last_1")) %>% as.matrix)
# X_train <- cBind(X_train, train %>% select(starts_with("seasonal")) %>% as.matrix)
# X_test  <- hashed.model.matrix(f, users_test,  b)

# Validate xgboost model
# Y_key <- sort(unique(train$top_by_position_day))
# Y     <- match(train$top_by_position_day, Y_key) - 1
Y <- train2$multiplier
Y[Y <= 0] <- 0

model <- 1:14500
valid <- (1:length(Y))[-model]
model <- train2 %>% filter(date <= "2017-01-01")
valid <- train2 %>% filter(date > "2017-01-01")
valid <- (nrow(model) + 1):(nrow(train2))
model <- 1:nrow(model)


dmodel <- xgb.DMatrix(X_train[model,], label = Y[model])
dvalid <- xgb.DMatrix(X_train[valid,], label = Y[valid])
dtrain <- xgb.DMatrix(X_train, label = Y)
watch  <- list(valid = dvalid, model = dmodel)

param <- list(objective = "reg:linear",
              booster = "gbtree", eta = 0.1,
              eval_metric = "rmse",
              colsample_bytree = .7,
              subsample = .7,
              max_depth = 3
)
set.seed(381)
m1 <- xgb.train(data = dmodel, param, nrounds = 2000,
                watchlist = watch
                , early.stop.round = 200
)
m1$bestScore

set.seed(381)
m2 <- xgb.cv(data = dtrain, param, nrounds = m1$bestInd * 1.05,
                watchlist = watch
                # , early.stop.round = 200
             , nfold = 2
)

# best_ix <- 0
# best_score <- 0
# for (i in 1:100){
#   result <- cbind(train[valid,], predict(m1, dvalid))
#   result$`predict(m1, dvalid)`[result$`predict(m1, dvalid)` >= i/100] <- 1
#   result$`predict(m1, dvalid)`[result$`predict(m1, dvalid)` < i/100] <- 0
#   score <- with(result, mcc(top_by_position_day,  `predict(m1, dvalid)`))
#   if(score >= best_score){
#     best_ix <- i
#     best_score <- score
#   }
# }

dvalid2 <- xgb.DMatrix(hashed.model.matrix(f, train2 %>% mutate(minutes = 28), b)[valid,] , label = Y[valid])
r3 <- predict(m1, dvalid2)
result <- cbind(train[valid,], predict(m1, dvalid))
colnames(result)[ncol(result)] <- 'pred'
result <- cbind(result, r3)
colnames(result)[ncol(result)] <- 'pred28'

# colnames(result)
# result$`predict(m1, dvalid)`[result$`predict(m1, dvalid)` >= best_ix/100] <- 1
# result$`predict(m1, dvalid)`[result$`predict(m1, dvalid)` < best_ix/100] <- 0

result %>% select(first_last, date, multiplier, pred, pred28, team, minutes, fd_pos, fd_sal) %>% 
  mutate(minutes=round(minutes, 2)) %>% filter(date == "2017-01-02") %>% arrange(fd_pos, -pred28) %>% View

# sum(result[,37])
# sum(result[,38])
# sum(result[,38] == 1 & result[,37])
# sum(result[,38] == 1 & result[,37]) / sum(result[,38])

# Use Hash Model to Predict Today -----------------------------------------
dtrain <- xgb.DMatrix(X_train, label = Y)
m2 <- xgb.train(data = dtrain, param, nrounds = floor(m1$bestInd * 1.05))

test <- fread("today_FD_0112.csv")
test$`First  Last` <- mapply(function(x,y) paste(x,y," "), test$`First Name`, test$`Last Name`)
test$`First  Last` <- gsub("\\s+$", "", test$`First  Last`)
colnames(test) <- gsub("\\s+", "_", colnames(test))
colnames(test)[8] <- "FD_Sal"
colnames(test)[1] <- "Opp"
test <- test %>%
  group_by(Position) %>%
  mutate(pool = toStr(First_Last)) %>%
  ungroup() %>%
  mutate(pool_day = toStr(unique(pool)))

X_test <- hashed.model.matrix(f, test, b)
pred <- predict(m2, X_test)

test_results <- cbind(test, pred = pred)

# Predicting Today (UPDATED DAILY) ----------------------------------------------
url <- 'http://www.sportsbookreview.com/betting-odds/nba-basketball/merged/'
dat <- read_html(url)
# text <- dat %>% html_nodes(".status-complete .el-div") %>% html_text
text <- dat %>% html_nodes(".status-scheduled .el-div") %>% html_text
table <- data.frame(t(matrix(text, nrow = 17)))
vegas <- clean_vegas(table)
vegas$date <- lubridate::today()
vegas$opp[4] <- "bos"
vegas$overunder[4] <- 208
vegas$margin[4] <- -4
vegas$overunder[5] <- 199
vegas$margin[5] <- -7
vegas$overunder[6] <- 200
vegas$margin[6] <- -2.5
  
write_csv(vegas, "vegas_0118.csv")
vegas <- fread('vegas_0118.csv', stringsAsFactors = F)
today <- fread("0118.csv") # Download live from fanduel!!
colnames(today) <- tolower(colnames(today))
colnames(today) <- gsub("\\s+", "_", colnames(today))
colnames(today)[11] <- "opp"
colnames(today)[8] <- "fd_sal"
colnames(today)[2] <- "fd_pos"
today$first_last <- paste0(today$first_name, " ", today$last_name)
today <- today %>% 
  mutate(h_a = ifelse(substr(game,1,3) == team, "H", "A"))

unique(today$first_last)[!unique(today$first_last) %in% unique(train$first_last)]


## Map some names
today$first_last[today$first_last == "Wesley Matthews"] <- "Wes Matthews"
today$first_last[today$first_last == "J.J. Barea"] <- "Jose Barea"
today$first_last[today$first_last == "Lou Williams"] <- "Louis Williams"
today$first_last[today$first_last == "Larry Nance Jr."] <- "Larry Nance"
today$first_last[today$first_last == "John Lucas III"] <- "John Lucas"
today$first_last[today$first_last == "Juancho Hernangomez"] <- "Juan Hernangomez"
#
today$first_last[today$first_last =="Chasson Randle"]<-"Chasson Randle"
today$first_last[today$first_last =="DeAndre' Bembry"]<-"DeAndre Bembry"
today$first_last[today$first_last =="Donatas Motiejunas"]<-"Donatas Motiejunas"
today$first_last[today$first_last =="Edy Tavares"]<-"Edy Tavares"
today$first_last[today$first_last =="Ish Smith"]<-"Ishmael Smith"
today$first_last[today$first_last =="James Ennis III"]<-"James Ennis"
today$first_last[today$first_last =="Joe Young"]<-"Joseph Young"
today$first_last[today$first_last =="Jordan Farmar"]<-"Jordan Farmar"
today$first_last[today$first_last =="Kelly Oubre Jr."]<-"Kelly Oubre"
today$first_last[today$first_last =="Khris Middleton"]<-"Khris Middleton"
today$first_last[today$first_last =="Maurice Ndour"]<-"Maurice N'dour"
today$first_last[today$first_last =="Stephen Zimmerman Jr."]<-"Stephen Zimmerman"
today$first_last[today$first_last =="Timothe Luwawu-Cabarrot"]<-"Timothe Luwawu"
today$first_last[today$first_last =="Wade Baldwin IV"]<-"Wade Baldwin"

daily_test_info <- today %>%
  group_by(fd_pos) %>%
  summarise(pool = toStr(first_last)) %>%
  mutate(pool_day = toStr(pool))

today$team <- tolower(today$team)
today$opp <- tolower(today$opp)
today$team <- gsub("sa", "sas", today$team)
today$opp  <- gsub("sa", "sas", today$opp)
today$team <- gsub("gs", "gsw", today$team)
today$opp  <- gsub("gs", "gsw", today$opp)
today$team <- gsub("no", "nor", today$team)
today$opp  <- gsub("no", "nor", today$opp)
today$team <- gsub("ny", "nyk", today$team)
today$opp  <- gsub("ny", "nyk", today$opp)

test <- left_join(today, daily_test_info)
test <- left_join(test, vegas, by = c("team", "opp"))
test <- left_join(test, vegas, by = c("team" = "opp", "opp" = "team"))
test <- test %>%
  mutate(time = ifelse(is.na(time.x), time.y, time.x),
         overunder = ifelse(is.na(overunder.x), overunder.y, overunder.x),
         margin = ifelse(is.na(margin.x), margin.y, margin.x)
         # ,
         # date = ifelse(is.na(date.x), date.y, date.x)
         )
test$time.x <- NULL
test$time.y <- NULL
test$margin.x <- NULL
test$margin.y <- NULL
test$overunder.x <- NULL
test$overunder.y <- NULL
test$date.x <- NULL
test$date.y <- NULL

test$minutes <- 28

X_test <- hashed.model.matrix(f, test, b)
today_results <- cbind(test, predict(m1, X_test))
colnames(today_results)[ncol(today_results)] <- "prediction"
view <- today_results %>% select(first_last, prediction)
view2 <- today_results %>% select(first_last, fd_pos, prediction, fd_sal) %>% arrange(fd_pos, -prediction)

# Feature Importance ------------------------------------------------------
library(ggplot2)
m <- xgb.dump(m1, with.stats = T)
m[1:10] #This statement prints top 10 nodes of the model
# Get the feature real names

names <- dimnames(data.matrix((X_train)))[[2]]

# Compute feature importance matrix
importance_matrix <- xgb.importance(names, model = m1)
# Nice graph
importance_matrix <- importance_matrix[order(-importance_matrix$Frequence),]
ggplot(importance_matrix, aes(x = reorder(Feature, Frequence), Frequence, fill="red")) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position="none")

# Predicting Yesterday (UPDATED/BROKEN) ----------------------------------------------
url <- 'http://www.sportsbookreview.com/betting-odds/nba-basketball/?date=20170116'
dat <- read_html(url)
text <- dat %>% html_nodes(".el-div") %>% html_text
table <- data.frame(t(matrix(text, nrow = 17)))
vegas <- clean_vegas(table)
vegas$date <- lubridate::today()
# today <- fread("0117.csv")
# colnames(today) <- tolower(colnames(today))
# colnames(today) <- gsub("\\s+", "_", colnames(today))
# colnames(today)[11] <- "opp"
# colnames(today)[8] <- "fd_sal"
# colnames(today)[2] <- "fd_pos"
# today$first_last <- paste0(today$first_name, " ", today$last_name)
# today <- today %>% 
#   mutate(h_a = ifelse(substr(game,1,3) == team, "H", "A"))

# today$first_last[today$first_last == "Wesley Matthews"] <- "Wes Matthews"
# today$first_last[today$first_last == "J.J. Barea"] <- "Jose Barea"
# today$first_last[today$first_last == "Lou Williams"] <- "Louis Williams"
# today$first_last[today$first_last == "Larry Nance Jr."] <- "Larry Nance"
# today$first_last[today$first_last == "John Lucas III"] <- "John Lucas"
# today$first_last[today$first_last == "Juancho Hernangomez"] <- "Juan Hernangomez"

yesterday <- fread("0116_guru.csv")
colnames(yesterday) <- tolower(colnames(yesterday))
colnames(yesterday) <- gsub("\\s+", "_", colnames(yesterday))
yesterday <- yesterday %>% 
  mutate(multiplier = fanduel_pts / fanduel_salary * 1000) %>% 
  select(espn_name, fanduel_salary, team, fanduel_position, multiplier) %>%
  rename(first_last = espn_name,
         fd_sal = fanduel_salary,
         fd_pos = fanduel_position)
yesterday$team <- tolower(yesterday$team)
yesterday$opp <- " "
yesterday$h_a <- NaN

daily_test_info <- yesterday %>%
  group_by(fd_pos) %>%
  summarise(pool = toStr(first_last)) %>%
  mutate(pool_day = toStr(pool))


# yesterday$opp <- tolower(yesterday$opp)
# yesterday$team <- gsub("sa", "sas", yesterday$team)
# yesterday$opp <- gsub("sa", "sas", yesterday$opp)

test2 <- left_join(yesterday, daily_test_info)
test2 <- left_join(test2, vegas, by = c("team" = "team"))
test2 <- left_join(test2, vegas, by = c("team" = "opp"))
test2 <- test2 %>%
  mutate(time = ifelse(is.na(time.x), time.y, time.x),
         overunder = ifelse(is.na(overunder.x), overunder.y, overunder.x),
         margin = ifelse(is.na(margin.x), margin.y, margin.x),
         date = ifelse(is.na(date.x), date.y, date.x))
test2$time.x <- NULL
test2$time.y <- NULL
test2$margin.x <- NULL
test2$margin.y <- NULL
test2$overunder.x <- NULL
test2$overunder.y <- NULL
test2$date.x <- NULL
test2$date.y <- NULL
test2$team.x <- NULL
test2$team.y <- NULL
test2$opp.x <- NULL
test2$opp.y <- NULL
test2$opp <- " "

X_test <- hashed.model.matrix(f, test2 %>% mutate(minutes = 28), b)
dtest <- xgb.DMatrix(X_test, label = Y[1:nrow(test2)])
yesterday_results <- cbind(test2, predict(m2, dtest))
colnames(yesterday_results)[ncol(yesterday_results)] <- "prediction"
view <- yesterday_results %>% select(first_last, prediction, multiplier)
view2 <- yesterday_results %>% select(first_last, fd_pos, fd_sal, prediction, multiplier) %>% arrange(fd_pos, -prediction)
