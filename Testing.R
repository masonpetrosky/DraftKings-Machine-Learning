setwd("~/DK011422")
library(data.table)
train <- fread("NBA.csv")
test <- fread("Test.csv")
k <- 2
size <- nrow(train)
partition <- size/k
for (i in 1:k){
  beg_int <- 1
  end_int <- partition
  sub_train <- train[beg_int:end_int]
  lm <- lm(Actual ~ NF + RW + Salary, data = sub_train)
  
  beg_int <- end_int
  end_int <- beg_int + partition
  }
