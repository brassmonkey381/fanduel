setwd("F:/FANDUEL/NBA/all")
library(rvest)
library(data.table)
library(readr)
library(xgboost)
library(lubridate)
library(FeatureHashing)
library(Matrix)
library(dplyr)
library(tidyr)

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
# Gets and cleans all vegas data for a specified date
get_vegas_data <- function(d){
  d <- ymd(d)
  url <- ("http://www.sportsbookreview.com/betting-odds/nba-basketball/merged/")
  url <- paste0(url, "?date=", gsub("-", "", d))
  dat <- read_html(url)
  text <- dat %>% html_nodes(".el-div") %>% html_text
  table <- data.frame(t(matrix(text, nrow = 17)))
  
  url <- ("http://www.sportsbookreview.com/betting-odds/nba-basketball/1st-quarter/")
  url <- paste0(url, "?date=", gsub("-", "", d))
  dat <- read_html(url)
  text <- dat %>% html_nodes(".el-div") %>% html_text
  table2 <- data.frame(t(matrix(text, nrow = 17)))
  
  url <- ("http://www.sportsbookreview.com/betting-odds/nba-basketball/1st-half/")
  url <- paste0(url, "?date=", gsub("-", "", d))
  dat <- read_html(url)
  text <- dat %>% html_nodes(".el-div") %>% html_text
  table3 <- data.frame(t(matrix(text, nrow = 17)))
  
  if(nrow(table) == 0){return(0)}
  if(nrow(table2) == 0){return(0)}
  if(nrow(table3) == 0){return(0)}
  
  x <- clean_vegas(table)
  y <- clean_vegas_qh(table2)
  z <- clean_vegas_qh(table3)
  
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

# Load Data (Takes 5 Seconds) ---------------------------------------------------------------
# NEWEST DATA?
url <- ("http://rotoguru1.com/cgi-bin/nba-dhd-2017.pl")

dat <- read_html(url)
text <- dat %>% html_text

a <- gregexpr("GID", text)[[1]][1]
b <- gregexpr("A data legend", text)[[1]][1]-1
t <- substr(text, a, b)

fileConn<-file("output.txt")
writeLines(t, fileConn)
close(fileConn)

datx <- read.table("output.txt", sep=":", header=T, fill = TRUE)
datx <- datx[,colSums(is.na(datx)) != nrow(datx)]
datx <- datx[complete.cases(datx),]
for (i in 1:ncol(datx)){
  if(class(datx[,i]) == "factor"){
    datx[,i] <- as.character(datx[,i])
  }
}
colnames(datx) <- tolower(gsub("\\.+", "_", colnames(datx)))
datx$date <- ymd(datx$date)
datx$multiplier <- datx$fdp / datx$fd_sal * 1000
datx$ppm <- datx$fdp / datx$minutes
datx <- add_efficiencies(datx)

## MORE FEATURES
datx <- datx %>%
  mutate(w_l = ifelse(team_pts > opp_pts, "W", "L"),
         home_win = ifelse(w_l == "W", ifelse(h_a == "H", 1, 0), 0),
         away_win = ifelse(w_l == "W", ifelse(h_a == "A", 1, 0), 0),
         win = ifelse(team_pts > opp_pts, 1, 0))
write_csv(datx, 'rotoguru_0120.csv')

# Powerful Aggregate Loops (Takes 5 minutes each) ------------------------------------------------
# Set X HIGH to get running season aggregates (only 76 games this season! Will work backwards)
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))
datx[is.nan(datx)] <- 0
seasonal <- create_aggregae_last_x_games(100, "mean")
past_10_sum <- create_aggregae_last_x_games(10, "sum")
past_10 <- create_aggregae_last_x_games(10, "mean")
past_5_sum <- create_aggregae_last_x_games(5, "sum")
past_5 <- create_aggregae_last_x_games(5, "mean")
past_3 <- create_aggregae_last_x_games(3, "mean") 
past_1 <- create_aggregae_last_x_games(1, "mean")
extra <- cbind(seasonal, 
               past_10_sum %>% select(-last_first, -date),
               past_10%>% select(-last_first, -date),
               past_5_sum%>% select(-last_first, -date),
               past_5%>% select(-last_first, -date),
               past_3%>% select(-last_first, -date), 
               past_1%>% select(-last_first, -date))

train3 <- left_join(datx, extra)

# Vegas Odds ---------------------------------------------------------------
fin <- table[1,]; fin <- fin[-1,]
fin2 <- table2[1,]; fin2 <- fin2[-1,]
fin3 <- table2[1,]; fin3 <- fin3[-1,]

dates <- as.character(rev(seq(ymd("2016-10-25"), ymd("2017-01-20"), "day")))
for (d in dates){
  url <- ("http://www.sportsbookreview.com/betting-odds/nba-basketball/merged/")
  url <- paste0(url, "?date=", gsub("-", "", d))
  dat <- read_html(url)
  text <- dat %>% html_nodes(".el-div") %>% html_text
  table <- data.frame(t(matrix(text, nrow = 17)))
  
  url <- ("http://www.sportsbookreview.com/betting-odds/nba-basketball/1st-quarter/")
  url <- paste0(url, "?date=", gsub("-", "", d))
  dat <- read_html(url)
  text <- dat %>% html_nodes(".el-div") %>% html_text
  table2 <- data.frame(t(matrix(text, nrow = 17)))
  
  url <- ("http://www.sportsbookreview.com/betting-odds/nba-basketball/1st-half/")
  url <- paste0(url, "?date=", gsub("-", "", d))
  dat <- read_html(url)
  text <- dat %>% html_nodes(".el-div") %>% html_text
  table3 <- data.frame(t(matrix(text, nrow = 17)))
  
  if(nrow(table) == 0){next()}
  table$date <- ymd(d)
  fin <- rbind(fin, table)
  
  if(nrow(table2) == 0){next()}
  table2$date <- ymd(d)
  fin2 <- rbind(fin2, table2)
  
  if(nrow(table3) == 0){next()}
  table3$date <- ymd(d)
  fin3 <- rbind(fin3, table3)
}

x <- clean_vegas(fin)
y <- clean_vegas_qh(fin2)
z <- clean_vegas_qh(fin3)

colnames(x)[4:5] <- sapply(colnames(x)[4:5], function(x) paste0("full_game_", x))
colnames(y)[4:5] <- sapply(colnames(y)[4:5], function(x) paste0("q1_", x))
colnames(z)[4:5] <- sapply(colnames(z)[4:5], function(x) paste0("h1_", x))

write_csv(x, "vegas_odds_full_game_swapped_0120.csv")
x <- fread('vegas_odds_full_game_swapped_0120.csv')
write_csv(y, "vegas_odds_1st_quarter_swapped_0120.csv")
y <- fread('vegas_odds_1st_quarter_swapped_0120.csv')
write_csv(z, "vegas_odds_1st_half_swapped_0120.csv")
z <- fread('vegas_odds_1st_half_swapped_0120.csv')

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

write_csv(all_vegas_odds, "all_vegas_odds.csv")
