setwd("~/DK011422")
library(data.table)
train <- fread("NBA.csv")
k <- 1
training$NFScaled <- training$NF/mean(training$NF)
training$RWScaled <- training$RW/mean(training$RW)
training$SalaryScaled <- training$Salary/mean(training$Salary)
training$Distance <- (validation$NFScaled - training$NFScaled)^2 + (validation$RWScaled - training$RWScaled)^2 + (validation$SalaryScaled - training$SalaryScaled)^2
training <- training[order(Distance),]
