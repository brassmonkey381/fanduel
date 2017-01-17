## The purpose of this file is to maximize the signal of rotoguru master data
setwd("F:/FANDUEL/NBA")
library(rvest)
library(data.table)
library(readr)
library(xgboost)
library(lubridate)
library(FeatureHashing)
library(Matrix)
library(dplyr)

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

# Load Data ---------------------------------------------------------------
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

# Powerful Aggregate Loops ------------------------------------------------
# Set X HIGH to get running season aggregates (only 76 games this season! Will work backwards)
seasonal <- create_aggregae_last_x_games(100, "mean")
past_10 <- create_aggregae_last_x_games(10, "mean")
past_5 <- create_aggregae_last_x_games(5, "mean")
past_3 <- create_aggregae_last_x_games(3, "mean") 
past_1 <- create_aggregae_last_x_games(1, "mean")

# Future Useful Data ------------------------------------------------------
# DATA 2016-10-25 to 2017-01-10
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
  top_n(2, multiplier) %>%
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

datx <- datx %>%
  mutate(w_l = ifelse(team_pts > opp_pts, "W", "L"))

min_sum <- datx %>%
  group_by(date, team, start, w_l, h_a) %>%
  summarise(minutes = sum(minutes))
team_min_sum <- min_sum %>% 
  group_by(team, start, w_l, h_a) %>%
  summarise(minutes = sum(minutes)) %>%
  group_by(team, w_l, h_a) %>%
  summarise(start_to_bench_minutes = minutes[start == 1] / minutes[start == 0])

# Vegas Odds ---------------------------------------------------------------
fin <- table[1,]; fin <- fin[-1,]
dates <- as.character(rev(seq(ymd("2016-10-25"), ymd("2017-01-15"), "day")))
for (d in dates){
  url <- ("http://www.sportsbookreview.com/betting-odds/nba-basketball/merged/")
  url <- paste0(url, "?date=", gsub("-", "", d))
  
  dat <- read_html(url)
  text <- dat %>% html_nodes(".el-div") %>% html_text
  table <- data.frame(t(matrix(text, nrow = 17)))
  if(nrow(table) == 0){next()}
  table$date <- ymd(d)
  fin <- rbind(fin, table)
}

fin <- fin[,-1]
fin <- fin[,-1]
fin <- fin[,-3]

for (i in 1:ncol(fin)){
  if(class(fin[,i]) == "factor"){
    fin[,i] <- as.character(fin[,i])
  }
}

half_char <- substr(fin[3,][3], 4,4)
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
colnames(x)[25:26] <- c('a.10', 'b.10', sep = "\\s")

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

x <- cbind(x[,c(27,1,2)], odds)
teams <- unique(datx$team)[order(unique(datx$team))]
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
x <- separate(x, X4, c("team", "opp"), sep = "\\s")
colnames(x)[2] <- 'time'
write_csv(x, "vegas_odds.csv")
   

# Example FD Daily Download -----------------------------------------------
example <- fread("today_FD_0112.csv")
