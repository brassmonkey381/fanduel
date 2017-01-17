setwd("F:/FANDUEL/NBA")
library(rvest)
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
top_eff <- top_eff_centers

toStr <- function(x) {paste(x, collapse = ",")}
daily_info <- data %>%
  group_by(Date, `FD pos`) %>%
  summarise(pool = toStr(`First  Last`))

# Format Data for Modelling -----------------------------------------------
labels <- left_join(daily_info, top_eff, by=c("Date", "FD pos"))
train <- left_join(data, labels %>% select(Date, `FD pos`, pool) %>% ungroup() %>% mutate(Date = ymd(Date)) %>% distinct(.keep_all=T), by=c("date" = "Date", "FD pos"))
train <- left_join(train, labels %>% select(-`FD pos`, -pool), by=c("Date", "First  Last"))
train$top_by_position_day[is.na(train$top_by_position_day)] <- 0
train <- train %>%
  arrange(date)
train$Date <- NULL
colnames(train)[3] <- "First_Last"
colnames(train) <- gsub("\\s+", "_", colnames(train))
colnames(train)[6] <- "H_A"
train <- train %>% filter(FD_pos == 5)

# Feature Hashing Model ---------------------------------------------------
b <- 2 ^ 9
f <- ~ First_Last + FD_Sal + Team + Opp + H_A + split(pool, delim = ",") - 1
X_train <- hashed.model.matrix(f, train, b)
# X_test  <- hashed.model.matrix(f, users_test,  b)

# Validate xgboost model
Y_key <- sort(unique(train$top_by_position_day))
Y     <- match(train$top_by_position_day, Y_key) - 1

model <- 1:14500
valid <- (1:length(Y))[-model]
model <- train %>% filter(date <= "2017-01-01")
valid <- train %>% filter(date > "2017-01-01")
valid <- (nrow(model) + 1):(nrow(train))
model <- 1:nrow(model)


param <- list(objective = "reg:linear",
              booster = "gbtree", eta = 0.01,
              eval_metric = "auc",
              colsample_bytree = .9,
              subsample = .9,
              max_depth = 6
              )

dmodel <- xgb.DMatrix(X_train[model,], label = Y[model])
dvalid <- xgb.DMatrix(X_train[valid,], label = Y[valid])
watch  <- list(valid = dvalid, model = dmodel)

set.seed(381)
m1 <- xgb.train(data = dmodel, param, nrounds = 1000,
                watchlist = watch
                , early.stop.round = 20
                )

best_ix <- 0
best_score <- 0
for (i in 1:100){
  result <- cbind(train[valid,], predict(m1, dvalid))
  result$`predict(m1, dvalid)`[result$`predict(m1, dvalid)` >= i/100] <- 1
  result$`predict(m1, dvalid)`[result$`predict(m1, dvalid)` < i/100] <- 0
  score <- with(result, mcc(top_by_position_day,  `predict(m1, dvalid)`))
  if(score >= best_score){
    best_ix <- i
    best_score <- score
  }
}

result <- cbind(train[valid,], predict(m1, dvalid))
# result$`predict(m1, dvalid)`[result$`predict(m1, dvalid)` >= best_ix/100] <- 1
# result$`predict(m1, dvalid)`[result$`predict(m1, dvalid)` < best_ix/100] <- 0

sum(result[,37])
sum(result[,38])
sum(result[,38] == 1 & result[,37])
sum(result[,38] == 1 & result[,37]) / sum(result[,38])

# Use Hash Model to Predict Today -----------------------------------------
dtrain <- xgb.DMatrix(X_train, label = Y)
m2 <- xgb.train(data = dtrain, param, nrounds = floor(m1$bestInd * 1.1))

test <- fread("today_FD_0112.csv")
test$`First  Last` <- mapply(function(x,y) paste(x,y," "), test$`First Name`, test$`Last Name`)
test$`First  Last` <- gsub("\\s+$", "", test$`First  Last`)
colnames(test) <- gsub("\\s+", "_", colnames(test))
colnames(test)[8] <- "FD_Sal"
test$pool <- toStr(test$First_Last)
X_test <- hashed.model.matrix(f, test, b)
pred <- predict(m2, X_test)

test_results <- cbind(test, pred = pred)

# xgboost -----------------------------------------------------------------
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

X_train <- xgb.DMatrix(m1 %>% select(-PTS) %>% as.matrix(), label = model$top_by_position_day, missing = NaN)
X_valid <- xgb.DMatrix(v1 %>% select(-PTS) %>% as.matrix(), label = valid$top_by_position_day, missing = NaN)
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




























O



# Oneoff-days -------------------------------------------------------------
data <- data %>%
  filter(Date == 20170108)
write_csv(data, "today0108.csv")
          

# Correlation of pts/minutes ----------------------------------------------
plot_cor(data %>% filter(Team == 'dal'
                         # , `First  Last` %in% c("Bradley Beal", "Otto Porter", "Markieff Morris")
                         ), "Minutes")

rowSums(dat[,2:16])
fdat <- melt(dat[,1:6], id.vars = "Date")
fdat$Date <- ymd(fdat$Date)
ggplot(fdat, aes(Date, value, color = variable)) + geom_line()
