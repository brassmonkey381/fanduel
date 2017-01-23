setwd("F:/FANDUEL/NBA/all")
library(dplyr)
library(data.table)
library(readr)
library(lubridate)

data <- fread("results_with_minutes_0122_numberfire.csv")

# Label Top Performers ---------------------------------------------------
### Efficiency
top_eff <- data %>%
  group_by(fd_pos) %>%
  top_n(5, prediction) %>%
  arrange(fd_pos)

### Points
top_pts <- data %>%
  group_by(fd_pos) %>%
  top_n(5, proj_pts)

# Optimize ---------------------------------------------------------
a <- top_eff[1:5,]
b <- top_eff[1:5+5,]
c <- top_eff[1:5+10,]
d <- top_eff[1:5+15,]
e <- top_eff[1:5+20,]

temp <- e
e <- a
a <- temp

a[combn(1:5, 2),]
b[combn(1:5, 2),]
c[combn(1:5, 2),]
d[combn(1:5, 2),]
e[combn(1:5, 1),]

best <- rbind(a[combn(1:5, 2),][1:2,],
              b[combn(1:5, 2),][1:2,],
              c[combn(1:5, 2),][1:2,],
              d[combn(1:5, 2),][1:2,],
              e[combn(1:5, 1),][1,])

master_df <- best[1,]; master_df$lineup_ID <- 0; master_df <- master_df[-1,]
temp <- best
temp$lineup_ID <- 0
lineup_ID <- 0
# v_counter <- 0

curr <- Sys.time()
for(v in 1:20){
  if (v %% 2 == 0){next()}
  temp[1:2,] <- a[v:(v+1),]
  # w_counter <- 0
  for(w in 1:20){
    if (w %% 2 == 0){next()}
    temp[3:4,] <- b[w:(w+1),]
    # x_counter <- 0
    for(x in 1:20){
      if (x %% 2 == 0){next()}
      temp[5:6,] <- c[x:(x+1),]
      # y_counter <- 0
      for(y in 1:20){
        if (y %% 2 == 0){next()}
        temp[7:8,] <- d[y:(y+1),]
        # z_counter <- 0
        for(z in 1:5){
          lineup_ID <- lineup_ID + 1
          temp[9,] <- e[z:(z),]
          temp$lineup_ID <- lineup_ID
          master_df <- rbind(master_df, temp)
        }
      }
    }
  }
}

final <- master_df %>%
  group_by(lineup_ID) %>%
  summarise(fd_sal = sum(fd_sal), proj_pts = sum(proj_pts),
            multiplier = proj_pts / fd_sal * 1000) %>%
  filter(fd_sal <= 60000) %>%
  arrange(-proj_pts)
final2 <- final %>% filter(fd_sal >= 55000)
Sys.time() - curr

write_csv(master_df %>% filter(lineup_ID == final$lineup_ID[1]), "best_team_20170107.csv")

finals <- master_df %>% filter(lineup_ID %in% final2$lineup_ID)
players <- finals %>% group_by(first_last, fd_pos) %>% summarise(count = n()) %>% arrange(-count)
