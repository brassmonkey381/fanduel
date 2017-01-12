setwd("F:/FANDUEL/NBA")
library(rvest)
library(dplyr)
library(data.table)
library(readr)
library(xgboost)
library(lubridate)


# Initialize Master Table -------------------------------------------------
url <- ("http://rotoguru1.com/cgi-bin/hoopstat-daterange0.pl?startdate=20170108&date=20170108&saldate=20170108&g=0&ha=&min=&tmptmin=0&tmptmax=999&opptmin=0&opptmax=999&gmptmin=0&gmptmax=999&gameday=&sd=0")

dat <- read_html(url)
text <- dat %>% html_text

a <- gregexpr("gid", text)[[1]][1]
b <- gregexpr("Statistical", text)[[1]][1]-1
t <- substr(text, a, b)

fileConn<-file("output.txt")
writeLines(t, fileConn)
close(fileConn)

datx <- read.table("output.txt", sep=";", header=T, fill = TRUE)
datx$date <- ymd("2017-01-08")

master_table <- datx[1,]
master_table <- master_table[-1,]

# Get All Data ------------------------------------------------------------
dates <- as.character(rev(seq(ymd("2016-10-25"), ymd("2017-01-08"), "day")))
url <- ("http://rotoguru1.com/cgi-bin/hoopstat-daterange0.pl?startdate=20170108&date=20170108&saldate=20170108&g=0&ha=&min=&tmptmin=0&tmptmax=999&opptmin=0&opptmax=999&gmptmin=0&gmptmax=999&gameday=&sd=0")

for (d in dates){
  datex = gsub("-", "", d)
  url <- gsub("date=\\d+", paste0("date=", datex), url)
  dat <- read_html(url)
  text <- dat %>% html_text
  
  a <- gregexpr("gid", text)[[1]][1]
  b <- gregexpr("Statistical", text)[[1]][1]-1
  t <- substr(text, a, b)
  
  if(nchar(t) <= 314){
    next()
  }
  
  fileConn<-file("output.txt")
  writeLines(t, fileConn)
  close(fileConn)
  
  datx <- read.table("output.txt", sep=";", header=T, fill = TRUE)
  datx <- data.frame(datx)
  datx$date <- d
  master_table <- rbind(master_table, datx)
}
master_table <- master_table[complete.cases(master_table),]
master_table$multiplier <- master_table$fanduel.pts / master_table$fanduel.salary * 1000

write_csv(master_table, "RG_all_games.csv")

# Set up Features ---------------------------------------------------------
features <- c("name", "team", "fanduel.salary", "draftkings.salary",
              "draftday.salary", 'yahoo.salary')

# xgboost -----------------------------------------------------------------
train <- master_table[,features]
train <- train %>%
  mutate(name = as.numeric(as.factor(name)),
         team = as.numeric(as.factor(team)))

master_table <- master_table[complete.cases(train),]
master_table$multiplier <- master_table$fanduel.pts/master_table$fanduel.salary*1000
train <- train[complete.cases(train),]
# orig <- master_table$fanduel.pts
# master_table$fanduel.pts <- master_table$fanduel.pts * master_table$fanduel.pts
# master_table$fanduel.pts <- orig

set.seed(312)
valid_rows <- master_table$date == ymd("2017-01-08")
model_rows <- master_table$date != ymd("2017-01-08")
# model_rows <- sample(nrow(train), floor(nrow(train) * .8))
# valid_rows <- setdiff(1:nrow(train), model_rows)
model <- train[model_rows,]
valid <- train[valid_rows,]

X_train <- xgb.DMatrix(model %>% as.matrix(), label = master_table$multiplier[model_rows], missing = NaN)
X_valid <- xgb.DMatrix(valid %>% as.matrix(), label = master_table$multiplier[valid_rows], missing = NaN)
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
result <- valid
result$pred <- predict(xgb, X_valid)
mae <- function(x, y){
  mean(abs(x-y))
}
result <- result %>% 
  mutate(multiplier = master_table$multiplier[valid_rows],
         err = pred - multiplier)
with(result, mae(multiplier,pred))
summary(result$err)
summary(result$multiplier)
sd(result$err, na.rm = T)


# Feature Importance ------------------------------------------------------
library(ggplot2)
m <- xgb.dump(xgb, with.stats = T)
m[1:10] #This statement prints top 10 nodes of the model
# Get the feature real names

names <- dimnames(data.matrix((model)))[[2]]

# Compute feature importance matrix
importance_matrix <- xgb.importance(names, model = xgb)
# Nice graph
importance_matrix <- importance_matrix[order(-importance_matrix$Frequence),]
ggplot(importance_matrix, aes(x = reorder(Feature, Frequence), Frequence, fill="red")) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(legend.position="none")


