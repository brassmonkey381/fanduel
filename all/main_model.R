setwd("F:/FANDUEL/NBA/all")
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
# Clean vegas data
clean_vegas <- function(fin){
  temp_date <- fin$date
  fin$date <- NULL
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
  swap_vector <- c()
  for(k in 1:nrow(pfg)){
    for(m in 1:11){
      if(is.na(pfg[k,][m]) | pfg[k,][m] < 150){
        temp = pfg[k,][m]
        pfg[k,][m] = pfg[k,][m+11]
        pfg[k,][m+11] = temp
        swap_vector <- c(swap_vector, 1)
      } else{
        swap_vector <- c(swap_vector, 0)
      }
    }
  }
  swap_vector <- swap_vector[seq(1,length(swap_vector),11)]
  
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
  teams <- c("atl", "bkn", "bos", "cha", "chi", "cle", "dal", "den", "det", "gsw", "hou", "ind", "lac", "lal", "mem", "mia", 
             "mil", "min", "nor", "nyk", "okc", "orl", "phi", "pho", "por", "sac", "sas", "tor", "uta", "was")
  x$X4 <- gsub("Atlanta", paste0(" ", teams[1]), x$X4)
  x$X4 <- gsub("Boston", paste0(" ", teams[3]), x$X4)
  x$X4 <- gsub("Brooklyn", paste0(" ", teams[2]), x$X4)
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
  
  for( j in 1:length(swap_vector)){
    if(swap_vector[j] == 1){
      temp <- x$team[j]
      x$team[j] <- x$opp[j]
      x$opp[j] <- temp
    }
  }
  x$date <- temp_date
  
  return(x)
}
# Clean Vegas score per quarter or half data
clean_vegas_qh <- function(fin){
  temp_date <- fin$date
  fin <- fin[,-1]
  fin <- fin[,-1]
  fin <- fin[,-3]
  fin$date <- NULL
  
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
  
  fin2[,2] <- sapply(fin2[,2], function(x) gsub("Options","",x))
  fin2[,4] <- sapply(fin2[,4], function(x) gsub("%","% ",x))
  fin2[,4] <- sapply(fin2[,4], function(x) gsub("%\\s$","%",x))
  fin2 <- apply(fin2, 2, function(x) gsub("PKPK","0 0",x))
  fin2 <- data.frame(fin2)
  
  x <- separate(fin2, X6, c("a", "b"), sep = "\\s")
  x <- separate(x, X7, c("a", "b"), sep = "\\s")
  x[,5] <- as.numeric(gsub("%", "", x[,5]))
  x[,6] <- as.numeric(gsub("%", "", x[,6]))
  colnames(x)[5:6] <- c("pct1", "pct2")
  x$X14 <- NULL
  x <- separate(x, X8, c("a", "b"), sep = "\\s")
  x <- separate(x, X9, c("a", "b"), sep = "\\s")
  x <- separate(x, X10, c("a", "b"), sep = "\\s")
  x <- separate(x, X11, c("a", "b"), sep = "\\s")
  x <- separate(x, X12, c("a", "b"), sep = "\\s")
  x <- separate(x, X13, c("a", "b"), sep = "\\s")
  x <- separate(x, X15, c("a", "b"), sep = "\\s")
  x <- separate(x, X16, c("a", "b"), sep = "\\s")
  x <- separate(x, X17, c("a", "b"), sep = "\\s")
  colnames(x)[21:22] <- c('a.8', 'b.8')
  colnames(x)[23:24] <- c('a.9', 'b.9')
  
  x[,3:24] <- apply(x[,3:24], 2, as.numeric)
  
  pfg <- x %>%
    select(starts_with("a"), starts_with("b"))
  # swap_vector <- c()
  # for(k in 1:nrow(pfg)){
  #   for(m in 1:11){
  #     if(is.na(pfg[k,][m]) | pfg[k,][m] < 150){
  #       temp = pfg[k,][m]
  #       pfg[k,][m] = pfg[k,][m+11]
  #       pfg[k,][m+11] = temp
  #       swap_vector <- c(swap_vector, 1)
  #     } else{
  #       swap_vector <- c(swap_vector, 0)
  #     }
  #   }
  # }
  # swap_vector <- swap_vector[seq(1,length(swap_vector),11)]
  
  y <- pfg %>%
    select(starts_with("a"))
  z <- apply(y, 1, function(x) mean(x, na.rm=T))
  xyz <- cbind(y,z)
  
  u <- pfg %>%
    select(starts_with("b"))
  # u[u >= 30] <- NA
  v <- apply(u, 1, function(x) mean(x, na.rm=T))
  tuv <- cbind(u,v)
  odds <- data.frame(overunder = xyz$z, margin = tuv$v)
  
  x <- cbind(x[,c(1,2)], odds)
  teams <- c("atl", "bkn", "bos", "cha", "chi", "cle", "dal", "den", "det", "gsw", "hou", "ind", "lac", "lal", "mem", "mia", 
             "mil", "min", "nor", "nyk", "okc", "orl", "phi", "pho", "por", "sac", "sas", "tor", "uta", "was")
  x$X4 <- gsub("Atlanta", paste0(" ", teams[1]), x$X4)
  x$X4 <- gsub("Boston", paste0(" ", teams[3]), x$X4)
  x$X4 <- gsub("Brooklyn", paste0(" ", teams[2]), x$X4)
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
  colnames(x)[4:5] <- c("team_margin", "opp_margin")
  x$date <- temp_date
  
  return(x)
  
}
# Paste to 1 string with delim ','
toStr <- function(x) {paste(x, collapse = ",")}
# Gets and cleans all vegas data for a specified date
get_vegas_data <- function(d){
  d <- ymd(d)
  url <- ("http://www.sportsbookreview.com/betting-odds/nba-basketball/merged/")
  url <- paste0(url, "?date=", gsub("-", "", d))
  dat <- read_html(url)
  if(d == today()){
    text <- dat %>% html_nodes("#byDateleagueData-2017-01-22+ .eventLines .el-div") %>% html_text
  } else{
    text <- dat %>% html_nodes(".el-div") %>% html_text
  }
  table <- data.frame(t(matrix(text, nrow = 17)))
  
  url <- ("http://www.sportsbookreview.com/betting-odds/nba-basketball/1st-quarter/")
  url <- paste0(url, "?date=", gsub("-", "", d))
  dat <- read_html(url)
  if(d == today()){
    text <- dat %>% html_nodes("#byDateleagueData-2017-01-22+ .eventLines .el-div") %>% html_text
  } else{
    text <- dat %>% html_nodes(".el-div") %>% html_text
  }
  table2 <- data.frame(t(matrix(text, nrow = 17)))
  
  url <- ("http://www.sportsbookreview.com/betting-odds/nba-basketball/1st-half/")
  url <- paste0(url, "?date=", gsub("-", "", d))
  dat <- read_html(url)
  if(d == today()){
    text <- dat %>% html_nodes("#byDateleagueData-2017-01-22+ .eventLines .el-div") %>% html_text
  } else{
    text <- dat %>% html_nodes(".el-div") %>% html_text
  }
  table3 <- data.frame(t(matrix(text, nrow = 17)))
  
  if(nrow(table) == 0){return(0)}
  if(nrow(table2) == 0){return(0)}
  if(nrow(table3) == 0){return(0)}
  
  x <- clean_vegas(table)
  y <- clean_vegas_qh(table2)
  z <- clean_vegas_qh(table3)
  x$date <- d
  y$date <- d
  z$date <- d
  
  colnames(x)[4:5] <- sapply(colnames(x)[4:5], function(x) paste0("full_game_", x))
  colnames(y)[4:5] <- sapply(colnames(y)[4:5], function(x) paste0("q1_", x))
  colnames(z)[4:5] <- sapply(colnames(z)[4:5], function(x) paste0("h1_", x))
  
  all_vegas_odds <- cbind(x %>% select(1,2,3,6,4,5), y %>% select(4,5), z %>% select(4,5))
  for (i in 1:nrow(all_vegas_odds)){
    if(all_vegas_odds[i,]$q1_team_margin < all_vegas_odds[i,]$q1_opp_margin){
      temp <- all_vegas_odds[i,]$q1_team_margin
      all_vegas_odds[i,]$q1_team_margin <- all_vegas_odds[i,]$q1_opp_margin
      all_vegas_odds[i,]$q1_opp_margin <- temp
    }
    if(all_vegas_odds[i,]$h1_team_margin < all_vegas_odds[i,]$h1_opp_margin){
      temp <- all_vegas_odds[i,]$h1_team_margin
      all_vegas_odds[i,]$h1_team_margin <- all_vegas_odds[i,]$h1_opp_margin
      all_vegas_odds[i,]$h1_opp_margin <- temp
    }
  }
  
  return(all_vegas_odds)
  
}

# Load Data ---------------------------------------------------------------
data <- fread("rotoguru_0120.csv")
data <- data %>%
  mutate(date = ymd(as.character(date)))
# data <- data[complete.cases(data),]
data$illness <- 0
data$illness[data$minutes == 0] <- 1

daily_info <- data %>%
  group_by(date, fd_pos) %>%
  summarise(pool = toStr(first_last)) %>%
  ungroup() %>%
  group_by(date) %>%
  mutate(pool_day = toStr(pool))
daily_illness <- data %>%
  filter(illness == 1) %>% 
  group_by(date, fd_pos) %>%
  summarise(illness_pool = toStr(first_last)) %>%
  ungroup() %>%
  group_by(date) %>%
  mutate(illness_pool_day = toStr(illness_pool))
daily_info <- left_join(daily_info, daily_illness)

# vegas_odds <- fread('vegas_odds_swapped.csv') # 1-18 max
# # ADD 1-19
# url <- 'http://www.sportsbookreview.com/betting-odds/nba-basketball/merged/?date=20170119'
# dat <- read_html(url)
# text <- dat %>% html_nodes(".el-div") %>% html_text
# table <- data.frame(t(matrix(text, nrow = 17)))
# vegas <- clean_vegas(table)
# vegas$date <- as.character(ymd(lubridate::today())-1)
# vegas_odds <- rbind(vegas_odds, vegas)
# # ADD 1-20
# url <- 'http://www.sportsbookreview.com/betting-odds/nba-basketball/merged/?date=20170120'
# dat <- read_html(url)
# text <- dat %>% html_nodes(".el-div") %>% html_text
# table <- data.frame(t(matrix(text, nrow = 17)))
# vegas <- clean_vegas(table)
# vegas$date <- as.character(ymd(lubridate::today())-1)
# vegas_odds <- rbind(vegas_odds, vegas)
# vegas_odds$date <- ymd(vegas_odds$date)
#
vegas_odds <- fread("all_vegas_odds.csv", stringsAsFactors = F)
vegas_odds$full_game_team_margin <- (-1) * vegas_odds$full_game_margin
vegas_odds$full_game_opp_margin <- vegas_odds$full_game_margin
vegas_odds$full_game_margin <- NULL
vegas_odds$date <- ymd(vegas_odds$date)
vegas_odds_flipped <- data.frame(vegas_odds)
vegas_odds_flipped[,c(6:10)] <- apply(vegas_odds_flipped[,c(6:10)], c(1,2), function(x) x*(-1))

# Combine All The Data ----------------------------------------------------
train <- left_join(data, daily_info)
train <- left_join(train, vegas_odds, by=c("date", "team" = "team", "opp" = "opp"))
train <- left_join(train, vegas_odds, by=c("date", "team" = "opp", "opp" = "team"))

train <- train %>%
  mutate(time = ifelse(is.na(time.x), time.y, time.x),
         full_game_overunder = ifelse(is.na(full_game_overunder.x), full_game_overunder.y, full_game_overunder.x),
         full_game_team_margin = ifelse(is.na(full_game_team_margin.x), full_game_team_margin.y, full_game_team_margin.x),
         full_game_opp_margin = ifelse(is.na(full_game_opp_margin.x), full_game_opp_margin.y, full_game_opp_margin.x),
         q1_team_margin = ifelse(is.na(q1_team_margin.x), q1_team_margin.y, q1_team_margin.x),
         q1_opp_margin = ifelse(is.na(q1_opp_margin.x), q1_opp_margin.y, q1_opp_margin.x),
         h1_team_margin = ifelse(is.na(h1_team_margin.x), h1_team_margin.y, h1_team_margin.x),
         h1_opp_margin = ifelse(is.na(h1_opp_margin.x), h1_opp_margin.y, h1_opp_margin.x)) %>%
  arrange(date)
train$time.x <- NULL
train$time.y <- NULL
train$full_game_team_margin.x <- NULL
train$full_game_team_margin.y <- NULL
train$full_game_opp_margin.x <- NULL
train$full_game_opp_margin.y <- NULL
train$full_game_overunder.x <- NULL
train$full_game_overunder.y <- NULL
train$q1_team_margin.x <- NULL
train$q1_team_margin.y <- NULL
train$q1_opp_margin.x <- NULL
train$q1_opp_margin.y <- NULL
train$h1_team_margin.x <- NULL
train$h1_team_margin.y <- NULL
train$h1_opp_margin.x <- NULL
train$h1_opp_margin.y <- NULL

# train <- train %>% filter(minutes >= 10)
# train <- train %>% filter(date <= ymd("2017-01-08"))

# Feature Hashing Model ---------------------------------------------------
# asd <- with(train, mapply(function(x, y) paste0(x,",",y), team, opp))
# train$asd <- asd
# past_5 <- create_aggregae_last_x_games(5, "mean")
# past_3 <- create_aggregae_last_x_games(3, "mean")

# past_10_max <- create_aggregae_last_x_games(10, "max")
# past_1 <- create_aggregae_last_x_games(1, "mean")
# 
# train <- cbind(train, past_5 %>% select(-first_last, -date))
# train <- cbind(train, past_3 %>% select(-first_last, -date))
# train <- cbind(train, past_1 %>% select(-first_last, -date))

b <- 2 ^ 12
# f <- ~ first_last + fd_sal + minutes + team + opp + h_a + time + full_game_overunder + full_game_team_margin + full_game_opp_margin + q1_team_margin + q1_opp_margin + h1_team_margin + h1_opp_margin - 1 
f <- ~ first_last + fd_sal + team + opp + h_a + time + full_game_overunder + full_game_team_margin + full_game_opp_margin + q1_team_margin + q1_opp_margin + h1_team_margin + h1_opp_margin - 1

# f <- ~ first_last + fd_sal + minutes + team + opp + h_a + time + margin + overunder- 1

# f <- ~ first_last + fd_sal + dk_sal + dd_sal + yh_sal + fd_change + dk_change + dd_change + yh_change + team + opp + h_a + minutes + illness + time + overunder +  + margin - 1
# last_3_games_mean_minutes + last_3_games_mean_start + last_3_games_mean_win + last_3_games_mean_start + last_3_games_mean_illness + last_3_games_mean_ppm - 1
X_train <- hashed.model.matrix(f, train, b)

# X_train <- cBind(X_train, train %>% select(starts_with("last_10")) %>% as.matrix)
# X_train <- cBind(X_train, train %>% select(starts_with("last_5")) %>% as.matrix)
# X_train <- cBind(X_train, train %>% select(starts_with("last_3")) %>% as.matrix)
# X_train <- cBind(X_train, train %>% select(starts_with("last_1")) %>% as.matrix)
# X_train <- cBind(X_train, train %>% select(starts_with("seasonal")) %>% as.matrix)
# X_train  <- hashed.model.matrix(f, users_train,  b)

# Validate xgboost model
# Y_key <- sort(unique(train$top_by_position_day))
# Y     <- match(train$top_by_position_day, Y_key) - 1
Y <- train$multiplier
Y[Y <= 0] <- 0

model <- 1:14500
valid <- (1:length(Y))[-model]
model <- train %>% filter(date < "2017-01-01")
valid <- train %>% filter(date >= "2017-01-01")
valid <- (nrow(model) + 1):(nrow(train))
model <- 1:nrow(model)

dmodel <- xgb.DMatrix(X_train[model,], label = Y[model])
dvalid <- xgb.DMatrix(X_train[valid,], label = Y[valid])
dtrain <- xgb.DMatrix(X_train, label = Y)
watch  <- list(valid = dvalid, model = dmodel)

param <- list(objective = "reg:linear",
              booster = "gbtree", eta = 0.03,
              eval_metric = "rmse",
              colsample_bytree = 1,
              subsample = .85,
              max_depth = 3
)
set.seed(381)
m1 <- xgb.train(data = dmodel, param, nrounds = 10000,
                watchlist = watch
                , early.stop.round = 100
)
m1$bestScore

# xgb.save(m1, "m1_with_minutes")
# xgb.save(m1, "m1_without_minutes")
m1_with_minutes <- xgb.load('m1_with_minutes')
m1_without_minutes <- xgb.load('m1_without_minutes')

set.seed(381)
m2 <- xgb.train(data = dtrain, param, nrounds = m1$bestInd * 1.05
             , nfold = 2
)
# xgb.save(m2, "m2_with_minutes")
# xgb.save(m2, "m2_without_minutes")

result_with_minutes <- cbind(train[valid,], predict(m1_with_minutes, dvalid))
colnames(result_with_minutes)[ncol(result_with_minutes)] <- 'pred'
view_with_minutes <- result_with_minutes %>% select(first_last, fd_pos, date, multiplier, pred, fd_sal, minutes, fdp) %>% mutate(proj_pts = fd_sal/1000 * pred) %>% filter(date == "2017-01-04")

result_without_minutes <- cbind(train[valid,], predict(m1_without_minutes, dvalid))
colnames(result_without_minutes)[ncol(result_without_minutes)] <- 'pred'
view_without_minutes <- result_without_minutes %>% select(first_last, fd_pos, date, multiplier, pred, fd_sal, minutes, fdp) %>% mutate(proj_pts = fd_sal/1000 * pred) %>% filter(date == "2017-01-04")

cbind(view_with_minutes, view_without_minutes %>% select(pred, proj_pts))  %>% View 

# Predicting Today (UPDATED DAILY) ----------------------------------------------
# url <- 'http://www.sportsbookreview.com/betting-odds/nba-basketball/merged/'
# dat <- read_html(url)
# # text <- dat %>% html_nodes(".status-complete .el-div") %>% html_text
# # text <- dat %>% html_nodes(".status-scheduled .el-div") %>% html_text
# text <- dat %>% html_nodes("#byDateleagueData-2017-01-22+ .eventLines .el-div") %>% html_text
# 
# table <- data.frame(t(matrix(text, nrow = 17)))
# vegas <- clean_vegas(table)
# vegas$date <- lubridate::today()
vegas <- get_vegas_data(today())
vegas$full_game_team_margin <- (-1) * vegas$full_game_margin
vegas$full_game_opp_margin <- vegas$full_game_margin
vegas$full_game_margin <- NULL
vegas$date <- ymd(vegas$date)
vegas_flipped <- data.frame(vegas)
vegas_flipped[,c(6:10)] <- apply(vegas_flipped[,c(6:10)], c(1,2), function(x) x*(-1))
write_csv(vegas, "vegas_0122.csv")

today <- fread("0122.csv") # Download live from fanduel!!
colnames(today) <- tolower(colnames(today))
colnames(today) <- gsub("\\s+", "_", colnames(today))
colnames(today)[11] <- "opp"
colnames(today)[8] <- "fd_sal"
colnames(today)[2] <- "fd_pos"
today$first_last <- paste0(today$first_name, " ", today$last_name)
today <- today %>% 
  mutate(h_a = ifelse(substr(game,1,3) == team, "H", "A"))

# unique(today$first_last)[!unique(today$first_last) %in% unique(train$first_last)]

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
today$team <- gsub("^sa$", "sas", today$team)
today$opp  <- gsub("^sa$", "sas", today$opp)
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
         date = ifelse(is.na(date.x), date.y, date.x),
         full_game_overunder = ifelse(is.na(full_game_overunder.x), full_game_overunder.y, full_game_overunder.x),
         full_game_team_margin = ifelse(is.na(full_game_team_margin.x), full_game_team_margin.y, full_game_team_margin.x),
         full_game_opp_margin = ifelse(is.na(full_game_opp_margin.x), full_game_opp_margin.y, full_game_opp_margin.x),
         q1_team_margin = ifelse(is.na(q1_team_margin.x), q1_team_margin.y, q1_team_margin.x),
         q1_opp_margin = ifelse(is.na(q1_opp_margin.x), q1_opp_margin.y, q1_opp_margin.x),
         h1_team_margin = ifelse(is.na(h1_team_margin.x), h1_team_margin.y, h1_team_margin.x),
         h1_opp_margin = ifelse(is.na(h1_opp_margin.x), h1_opp_margin.y, h1_opp_margin.x)
         ) %>%
  arrange(date)
test$time.x <- NULL
test$time.y <- NULL
test$date.x <- NULL
test$date.y <- NULL
test$full_game_team_margin.x <- NULL
test$full_game_team_margin.y <- NULL
test$full_game_opp_margin.x <- NULL
test$full_game_opp_margin.y <- NULL
test$full_game_overunder.x <- NULL
test$full_game_overunder.y <- NULL
test$q1_team_margin.x <- NULL
test$q1_team_margin.y <- NULL
test$q1_opp_margin.x <- NULL
test$q1_opp_margin.y <- NULL
test$h1_team_margin.x <- NULL
test$h1_team_margin.y <- NULL
test$h1_opp_margin.x <- NULL
test$h1_opp_margin.y <- NULL

test$illness <- 0
test$illness[test$injury_indicator == "O"] <- 1
test$minutes <- 32
test$fd_sal <- as.numeric(test$fd_sal)
test$date <- ymd(today())

# test <- test %>% filter(team %in% c("sac", "chi", "ind", "uta", "lac", "den"))

b <- 2 ^ 12
f <- ~ first_last + fd_sal + minutes + team + opp + h_a + time + full_game_overunder + full_game_team_margin + full_game_opp_margin + q1_team_margin + q1_opp_margin + h1_team_margin + h1_opp_margin - 1 
X_test <- hashed.model.matrix(f, test, b)
m2 <- xgb.load('m2_with_minutes')
today_results <- cbind(test, predict(m2, X_test))
colnames(today_results)[ncol(today_results)] <- "prediction"
view <- today_results %>% select(first_last, prediction)
view2 <- today_results %>% select(first_last, team, fd_pos, fd_sal, prediction) %>% mutate(proj_pts = fd_sal*prediction/1000) %>% arrange(fd_pos, -prediction)
write_csv(view2, 'results_with_minutes_0122.csv')


b <- 2 ^ 12
f <- ~ first_last + fd_sal + team + opp + h_a + time + full_game_overunder + full_game_team_margin + full_game_opp_margin + q1_team_margin + q1_opp_margin + h1_team_margin + h1_opp_margin - 1 
X_test <- hashed.model.matrix(f, test, b)
m2 <- xgb.load('m2_without_minutes')
today_results <- cbind(test, predict(m2, X_test))
colnames(today_results)[ncol(today_results)] <- "prediction"
view <- today_results %>% select(first_last, prediction)
view3 <- today_results %>% select(first_last, team, fd_pos, fd_sal, prediction) %>% mutate(proj_pts = fd_sal*prediction/1000) %>% arrange(fd_pos, -prediction)
write_csv(view3, 'results_without_minutes_0122.csv')

view4 <- cbind(view2 %>% arrange(first_last), view3 %>% arrange(first_last) %>% select(prediction, proj_pts) %>% rename(withoutmins_proj_pts = proj_pts, withoutmins_prediction = prediction))
view4$mean_proj_pts <- mapply(function(x,y) (x + y) / 2, view4$proj_pts, view4$withoutmins_proj_pts)
view4$mean_prediction <- mapply(function(x,y) (x + y) / 2, view4$prediction, view4$withoutmins_prediction)

get_player_results <- function(player){
  x <- view2 %>% filter(first_last == player) 
  y <- view3 %>% filter(first_last == player) %>% select(prediction, proj_pts) %>% rename(withoutmins_proj_pts = proj_pts, withoutmins_prediction = prediction)
  return(cbind(x, y))
}
player <- "Montrezl Harrell"
get_player_results(player)

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

# Predicting Yesterday (UPDATED) ----------------------------------------------
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

X_test <- hashed.model.matrix(f, test2, b)
yesterday_results <- cbind(test2, predict(m1, X_test))
colnames(yesterday_results)[ncol(yesterday_results)] <- "prediction"
view <- yesterday_results %>% select(first_last, prediction, multiplier)
view2 <- yesterday_results %>% select(first_last, fd_pos, fd_sal, prediction, multiplier) %>% arrange(fd_pos, -prediction)

# With rotofire or whatever minutes ---------------------------------------
url <- "https://www.numberfire.com/nba/daily-fantasy/daily-football-projections"
dat <- read_html(url)
minutes <- dat %>% html_nodes("table")
stats <- minutes[[5]] %>% html_table()
colnames(stats) <- stats[1,]
stats <- stats[-1,]
names <- dat %>% html_nodes(".full") %>% html_text()
names <- gsub("\\n\\s+", "", names)
names <- gsub("\\s+$", "", names)
proj_minutes <- cbind(names, stats)

test2 <- left_join(test, proj_minutes, by=c("first_last" = "names"))
test2$minutes <- as.numeric(test2$Min)
test2$minutes[is.na(test2$minutes)] <- 0

b <- 2 ^ 12
f <- ~ first_last + fd_sal + minutes + team + opp + h_a + time + full_game_overunder + full_game_team_margin + full_game_opp_margin + q1_team_margin + q1_opp_margin + h1_team_margin + h1_opp_margin - 1 
X_test <- hashed.model.matrix(f, test2, b)
m2 <- xgb.load('m2_with_minutes')
today_results <- cbind(test2, predict(m2, X_test))
colnames(today_results)[ncol(today_results)] <- "prediction"
view <- today_results %>% select(first_last, prediction)
view5 <- today_results %>% select(first_last, team, fd_pos, fd_sal, prediction) %>% mutate(proj_pts = fd_sal*prediction/1000) %>% arrange(fd_pos, -prediction)
write_csv(view5, 'results_with_minutes_0122_numberfire.csv')

