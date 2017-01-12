setwd("F:/FANDUEL/NBA")
library(rvest)
library(dplyr)
library(data.table)
library(readr)
library(xgboost)
library(lubridate)


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

list.files()

data <- fread("RG_all_games_2.csv")
data <- data %>%
  mutate(date = ymd(as.character(Date)))

# Feature Engineering -----------------------------------------------------
##
past_5 <- data %>%
  group_by(`First  Last`) %>%
  top_n(5, date)

past_3 <- data %>%
  group_by(`First  Last`) %>%
  top_n(5, date)

past_1 <- data %>%
  group_by(`First  Last`) %>%
  top_n(5, date)

season <- data %>%
  group_by(`First  Last`)

colnames(past_1) <- sapply(colnames(past_1), function(x) paste0("past1_", x))
colnames(past_3) <- sapply(colnames(past_3), function(x) paste0("past3_", x))
colnames(past_5) <- sapply(colnames(past_5), function(x) paste0("past5_", x))
colnames(season) <- sapply(colnames(season), function(x) paste0("season_", x))

##
past_5 <- data %>%
  group_by(`First  Last`) %>%
  top_n(5, date)

past_3 <- data %>%
  group_by(`First  Last`) %>%
  top_n(5, date)

past_1 <- data %>%
  group_by(`First  Last`) %>%
  top_n(5, date)


# Oneoff-days -------------------------------------------------------------
data <- data %>%
  filter(Date == 20170108)
write_csv(data, "today0108.csv")
          


# Correlation of pts/minutes ----------------------------------------------


plot_cor(data %>% filter(Team == 'sas'
                         # , `First  Last` %in% c("Bradley Beal", "Otto Porter", "Markieff Morris")
                         ), "Minutes")

rowSums(dat[,2:16])
fdat <- melt(dat[,1:6], id.vars = "Date")
fdat$Date <- ymd(fdat$Date)
ggplot(fdat, aes(Date, value, color = variable)) + geom_line()
