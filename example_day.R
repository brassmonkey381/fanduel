setwd("F:/FANDUEL/NBA")
library(dplyr)
library(data.table)
library(readr)
library(lubridate)

data <- fread("example_day.csv")
# data <- data[data$Date == 20170107,]
data <- data[complete.cases(data),]
data$multiplier <- as.numeric(data$multiplier)

top_eff <- data %>%
  group_by(Date, `FD pos`) %>%
  top_n(5, multiplier) %>%
  # select(`First  Last`, Team, Opp, `Team pts`, `Opp pts`, `FD Sal`, FDP, multiplier) %>%
  # arrange(`FD pos`, -multiplier)
  ungroup() %>%
  select(Date, `First  Last`) %>%
  mutate(top_5_by_position_day = 1)

data <- left_join(data, top_eff, by=c("Date", "First  Last"))
data$top_5_by_position_day[is.na(data$top_5_by_position_day)] <- 0
s1 <- data %>%
  group_by(`First  Last`, `FD pos`) %>%
  summarise(games = n(), games_active = sum(active), num_top_5 = sum(top_5_by_position_day),
            pct_top_5_of_active = num_top_5 / games_active, avg_salary_when_top_5 = mean(`FD Sal`)) %>%
    arrange(-pct_top_5_of_active)

filter <- fread("today_FD_0112.csv")
filter$`First  Last` <- mapply(function(x,y) paste(x,y," "), filter$`First Name`, filter$`Last Name`)
filter$`First  Last` <- gsub("\\s+$", "", filter$`First  Last`)
# filter <- fread("today0111.csv")
# f1 <- function(x){
#   x = unlist(strsplit(x, ","))
#   x = gsub("^\\s", "", x)
#   x = paste(x[2], x[1], collapse = " ")
#   return(x)
# }
# filter$`First  Last` <- sapply(filter$name, f1)

filtered_today <- s1 %>%
  filter(`First  Last` %in% filter$`First  Last`)
filtered_today <- left_join(filtered_today, filter %>% select(`First  Last`, Salary, `Injury Indicator`, `Injury Details`, Team)) %>%
  mutate(Sal_Diff = (Salary - avg_salary_when_top_5)/avg_salary_when_top_5)

# # Select Top Performers ---------------------------------------------------
# ### Efficiency
# top_eff <- data %>%
#   group_by(`FD pos`) %>%
#   top_n(5, multiplier) %>%
#   select(`First  Last`, Team, Opp, `Team pts`, `Opp pts`, `FD Sal`, FDP, multiplier) %>%
#   arrange(`FD pos`, -multiplier)
# 
# most_eff <- top_eff %>%
#   group_by(`FD pos`) %>%
#   top_n(2, multiplier) %>%
#   arrange(`FD pos`) %>%
#   .[1:9,]
# 
# goal <- most_eff %>%
#   ungroup() %>%
#   summarise(sal = sum(`FD Sal`), pts = sum(FDP),
#             multiplier = pts / sal * 1000, ideal = pts * 50000/sal)
# 
# ### Points
# top_pts <- data %>%
#   group_by(`FD pos`) %>%
#   top_n(5, FDP) %>%
#   select(`First  Last`, Team, Opp, `Team pts`, `Opp pts`, `FD Sal`, FDP, multiplier)
# 
# most_pts <- top_pts %>% 
#   group_by(`FD pos`) %>%
#   top_n(2, FDP) %>%
#   arrange(`FD pos`) %>%
#   .[1:9,]
# 
# nice <- most_pts %>%
#   ungroup() %>% 
#   summarise(sal = sum(`FD Sal`), pts = sum(FDP),
#             multiplier = pts / sal * 1000, ideal = pts * 50000/sal)
# 
# # Optimize ----------------------------------------------------------------
# best <- top_pts %>%
#   group_by(`FD pos`) %>%
#   top_n(2, multiplier) %>%
#   arrange(`FD pos`) %>%
#   .[1:9,]
# sum(best$`FD Sal`);sum(best$`FDP`);sum(best$`FDP`)/sum(best$`FD Sal`)*1000
# 
# with(data, length(unique(Team)))
# with(top_eff, length(unique(Team)))
# with(top_pts, length(unique(Team)))
# with(most_eff, length(unique(Team)))
# with(most_pts, length(unique(Team)))
# with(best, length(unique(Team)))
# 
# View(cbind(best, most_eff %>% select(multiplier, Team, `FD pos`, `First  Last`,  `FD Sal`, FDP)))
# 
# # Really Optimize ---------------------------------------------------------
# a <- top_eff[1:5,]
# b <- top_eff[1:5+5,]
# c <- top_eff[1:5+10,]
# d <- top_eff[1:5+15,]
# e <- top_eff[1:5+20,]
# 
# a[combn(1:5, 2),]
# b[combn(1:5, 2),]
# c[combn(1:5, 2),]
# d[combn(1:5, 2),]
# e[combn(1:5, 1),]
# 
# master_df <- best[1,]; master_df$lineup_ID <- 0; master_df <- master_df[-1,]
# temp <- best
# temp$lineup_ID <- 0
# lineup_ID <- 0
# # v_counter <- 0
# for(v in 1:20){
#   if (v %% 2 == 0){next()}
#   temp[1:2,] <- a[v:(v+1),]
#   # w_counter <- 0
#   for(w in 1:20){
#     if (w %% 2 == 0){next()}
#     temp[3:4,] <- b[w:(w+1),]
#     # x_counter <- 0
#     for(x in 1:20){
#       if (x %% 2 == 0){next()}
#       temp[5:6,] <- c[x:(x+1),]
#       # y_counter <- 0
#       for(y in 1:20){
#         if (y %% 2 == 0){next()}
#         temp[7:8,] <- d[y:(y+1),]
#         # z_counter <- 0
#         for(z in 1:5){
#           lineup_ID <- lineup_ID + 1
#           temp[9,] <- e[z:(z),]
#           temp$lineup_ID <- lineup_ID
#           master_df <- rbind(master_df, temp)
#         }
#       }
#     }
#   }
# }
# 
# final <- master_df %>%
#   group_by(lineup_ID) %>%
#   summarise(sal = sum(`FD Sal`), pts = sum(FDP),
#             multiplier = pts / sal * 1000) %>%
#   filter(sal <= 50000) %>%
#   arrange(-pts)
# 
# write_csv(master_df %>% filter(lineup_ID == final$lineup_ID[1]), "best_team_20170107.csv")


