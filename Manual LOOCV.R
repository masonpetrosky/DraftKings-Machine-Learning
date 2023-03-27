library(data.table)
setwd("~/DK011422")
fitted_value <- NULL
squaredError <- 0
  for(i in 1:nrow(train)){
    validation <- train[i,]
    training <- train[-i,]
    lm <- lm(Actual ~ NF + RW + Salary, data = training)
    fitted_value[i] <- predict(lm, validation)
    squaredError <- squaredError + (fitted_value[i] - validation$Actual)^2
  }
sqrt(squaredError/nrow(train))

#CV KNN
#1st Time
setwd("~/DK011422")
library(data.table)
train <- fread("NHL.csv")
train$NFScaled <- train$NF/mean(train$NF)
train$RWScaled <- train$RW/mean(train$RW)
train$SalaryScaled <- train$Salary/mean(train$Salary)

#All Times - NBA = 47, NHL = 71
fitted_value <- NULL
error_value <- NULL
for (j in 1:300){
  squaredError <- 0
  for(i in 1:nrow(train)){
    validation <- train[i,]
    training <- train[-i,]
    k <- j
    training$Distance <- sqrt((validation$NFScaled - training$NFScaled)^2 + (validation$RWScaled - training$RWScaled)^2 + (validation$SalaryScaled - training$SalaryScaled)^2)
    training <- training[order(Distance),]
    training <- training[1:k,]
    fitted_value[i] <- mean(training$Actual)
    squaredError <- squaredError + (fitted_value[i] - validation$Actual)^2
  }
error_value[j] <- sqrt(squaredError/nrow(train))
}
which.min(error_value)

#Predict KNN NBA = 47, NHL = 71
setwd("~/DK011422")
library(data.table)
train <- fread("NBA.csv")
train$NFScaled <- train$NF/mean(train$NF)
train$RWScaled <- train$RW/mean(train$RW)
train$SalaryScaled <- train$Salary/mean(train$Salary)
test <- fread("Test.csv")
test$NFScaled <- test$NF/mean(train$NF)
test$RWScaled <- test$RW/mean(train$RW)
test$SalaryScaled <- test$Salary/mean(train$Salary)
fitted_value <- NULL
for (i in 1:nrow(test)){
  k <- 47
  testing <- test[i]
  training <- train
  training$Distance <- sqrt((testing$NFScaled - training$NFScaled)^2 + (testing$RWScaled - training$RWScaled)^2 + (testing$SalaryScaled - training$SalaryScaled)^2)
  training <- training[order(Distance),]
  training <- training[1:k,]
  fitted_value[i] <- mean(training$Actual)
}
test$Pred <- fitted_value
write.csv(test, "testResults.csv")

#LM UFC
fitted_value <- NULL
squaredError <- 0
  for(i in 1:nrow(train)){
    validation <- train[i,]
    training <- train[-i,]
    lm <- lm(Actual ~ Round1 + Round2 + Round3 + Round4 + Round5 + Decision + Draw, data = training)
    fitted_value[i] <- predict(lm, validation)
    #fitted_value[i] <- mean(training$Actual)
    #fitted_value[i] <- 0
    squaredError <- squaredError + (fitted_value[i] - validation$Actual)^2
  }
sqrt(squaredError/nrow(train))

#CV KNN UFC = 30
#1st Time
setwd("~/DK011422")
library(data.table)
train <- fread("UFC.csv")
train$Round1Scaled <- train$Round1/mean(train$Round1)
train$Round2Scaled <- train$Round2/mean(train$Round2)
train$Round3Scaled <- train$Round3/mean(train$Round3)
train$Round4Scaled <- train$Round4/mean(train$Round4)
train$Round5Scaled <- train$Round5/mean(train$Round5)
train$DecisionScaled <- train$Decision/mean(train$Decision)
train$DrawScaled <- train$Draw/mean(train$Draw)
train$SalaryScaled <- train$Salary/mean(train$Salary)

#All Times
fitted_value <- NULL
error_value <- NULL
for (j in 1:132){
  squaredError <- 0
  for(i in 1:nrow(train)){
    validation <- train[i,]
    training <- train[-i,]
    k <- j
    training$Distance <- sqrt((validation$Round1Scaled - training$Round1Scaled)^2 + (validation$Round2Scaled - training$Round2Scaled)^2 + (validation$Round3Scaled - training$Round3Scaled)^2 + (validation$Round4Scaled - training$Round4Scaled)^2 + (validation$Round5Scaled - training$Round5Scaled)^2 + (validation$DecisionScaled - training$DecisionScaled)^2 + (validation$DrawScaled - training$DrawScaled)^2 + (validation$SalaryScaled - training$SalaryScaled)^2)
    training <- training[order(Distance),]
    training <- training[1:k,]
    fitted_value[i] <- mean(training$Actual)
    squaredError <- squaredError + (fitted_value[i] - validation$Actual)^2
  }
  error_value[j] <- sqrt(squaredError/nrow(train))
}
which.min(error_value)

#Predict KNN UFC = 30
setwd("~/DK011422")
library(data.table)
train <- fread("UFC.csv")
train$Round1Scaled <- train$Round1/mean(train$Round1)
train$Round2Scaled <- train$Round2/mean(train$Round2)
train$Round3Scaled <- train$Round3/mean(train$Round3)
train$Round4Scaled <- train$Round4/mean(train$Round4)
train$Round5Scaled <- train$Round5/mean(train$Round5)
train$DecisionScaled <- train$Decision/mean(train$Decision)
train$DrawScaled <- train$Draw/mean(train$Draw)
train$SalaryScaled <- train$Salary/mean(train$Salary)
test <- fread("TestUFC.csv")
test$Round1Scaled <- test$Round1/mean(train$Round1)
test$Round2Scaled <- test$Round2/mean(train$Round2)
test$Round3Scaled <- test$Round3/mean(train$Round3)
test$Round4Scaled <- test$Round4/mean(train$Round4)
test$Round5Scaled <- test$Round5/mean(train$Round5)
test$DecisionScaled <- test$Decision/mean(train$Decision)
test$DrawScaled <- test$Draw/mean(train$Draw)
test$SalaryScaled <- test$Salary/mean(train$Salary)
fitted_value <- NULL
for (i in 1:nrow(test)){
  k <- 30
  testing <- test[i]
  training <- train
  training$Distance <- sqrt((testing$Round1Scaled - training$Round1Scaled)^2 + (testing$Round2Scaled - training$Round2Scaled)^2 + (testing$Round3Scaled - training$Round3Scaled)^2 + (testing$Round4Scaled - training$Round4Scaled)^2 + (testing$Round5Scaled - training$Round5Scaled)^2 + (testing$DecisionScaled - training$DecisionScaled)^2 + (testing$DrawScaled - training$DrawScaled)^2 + (testing$SalaryScaled - training$SalaryScaled)^2)
  training <- training[order(Distance),]
  training <- training[1:k,]
  fitted_value[i] <- mean(training$Actual)
}
test$Pred <- fitted_value
write.csv(test, "testResults.csv")

# Cubic Regression
setwd("~/DK011422")
library(data.table)
train <- fread("NBA.csv")
train$NF2 <- train$NF^2
train$NF3 <- train$NF^3
train$RW2 <- train$RW^2
train$RW3 <- train$RW^3
train$Salary2 <- train$Salary^2
train$Salary3 <- train$Salary^3
lm <- lm(Actual ~ NF + NF2 + NF3 + RW + RW2 + RW3 + Salary + Salary2 + Salary3, data = train)
options(scipen=999)
summary(lm)

#CV KNN
#1st Time
setwd("~/DK011422")
library(data.table)
train <- fread("NBA.csv")
train$NFScaled <- train$NF/mean(train$NF)
train$RWScaled <- train$RW/mean(train$RW)
train$SalaryScaled <- train$RW/mean(train$Salary)

#All Times - NBA = 33, NHL = 95
fitted_value <- NULL
error_value <- NULL
for (j in 1:100){
  squaredError <- 0
  for(i in 1:nrow(train)){
    validation <- train[i,]
    training <- train[-i,]
    k <- j
    training$Distance <- sqrt((validation$RWScaled - training$RWScaled)^2 + (validation$SalaryScaled - training$SalaryScaled)^2)
    training <- training[order(Distance),]
    training <- training[1:k,]
    fitted_value[i] <- mean(training$Actual)
    squaredError <- squaredError + (fitted_value[i] - validation$Actual)^2
  }
error_value[j] <- sqrt(squaredError/nrow(train))
}
which.min(error_value)

library(data.table)
setwd("~/DK011422")
train$NF2 <- train$NF^2
train$NF3 <- train$NF^3
train$RW2 <- train$RW^2
train$RW3 <- train$RW^3
train$Salary2 <- train$Salary^2
train$Salary3 <- train$Salary^3
# All
fitted_value <- NULL
squaredError <- 0
  for(i in 1:nrow(train)){
    validation <- train[i,]
    training <- train[-i,]
    lm <- lm(Actual ~ NF + NF2 + NF3 + RW + RW2 + RW3 + Salary + Salary2 + Salary3, data = training)
    fitted_value[i] <- predict(lm, validation)
    #fitted_value[i] <- mean(training$Actual)
    #fitted_value[i] <- 0
    squaredError <- squaredError + (fitted_value[i] - validation$Actual)^2
  }
sqrt(squaredError/nrow(train))

setwd("~/DK011422")
library(data.table)
train <- fread("NBA.csv")
train$NFScaled <- train$NF/mean(train$NF)
#train$RWScaled <- train$RW/mean(train$RW)
test <- fread("Test.csv")
test$NFScaled <- test$NF/mean(train$NF)
#test$RWScaled <- test$RW/mean(train$RW)
fitted_value <- NULL
for (i in 1:nrow(test)){
  k <- 42
  testing <- test[i]
  training <- train
  training$Distance <- sqrt((testing$NFScaled - training$NFScaled)^2)
  training <- training[order(Distance),]
  training <- training[1:k,]
  fitted_value[i] <- mean(training$Actual)
}
test$Pred <- fitted_value
write.csv(test, "testResults.csv")


library(data.table)
train <- fread("ArtistStartYearStreams.csv")
train$YearScaled <- train$Year/mean(train$Year)
fitted_value <- NULL
error_value <- NULL
for (j in 1:nrow(train)){
  squaredError <- 0
  for(i in 1:nrow(train)){
    validation <- train[i,]
    training <- train[-i,]
    k <- j
    training$Distance <- sqrt((validation$YearScaled - training$YearScaled)^2)
    training <- training[order(Distance),]
    training <- training[1:k,]
    fitted_value[i] <- mean(training$Streams)
    squaredError <- squaredError + (fitted_value[i] - validation$Streams)^2
  }
error_value[j] <- sqrt(squaredError/nrow(train))
}
which.min(error_value)

library(data.table)
train <- fread("ArtistStartYearStreams.csv")
train$YearScaled <- train$Year/mean(train$Year)
test <- fread("StreamsTest.csv")
test$YearScaled <- test$Year/mean(train$Year)
fitted_value <- NULL
for (i in 1:nrow(test)){
  k <- 12
  testing <- test[i]
  training <- train
  training$Distance <- sqrt((testing$YearScaled - training$YearScaled)^2)
  training <- training[order(Distance),]
  training <- training[1:k,]
  fitted_value[i] <- mean(training$Streams)
}
test$Pred <- fitted_value
write.csv(test, "testResults.csv")