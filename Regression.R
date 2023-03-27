# Cubic Cross-Validation
library(data.table)
setwd("~/DK011422")
fitted_value <- NULL
squaredError <- 0
  for(i in 1:nrow(train)){
    validation <- train[i,]
    training <- train[-i,]
    lm <- lm(Actual ~ NF2 + RW + RW2, data = training)
    fitted_value[i] <- predict(lm, validation)
    squaredError <- squaredError + (fitted_value[i] - validation$Actual)^2
  }
sqrt(squaredError/nrow(train))

# Cubic Regression
setwd("~/DK011422")
library(data.table)
train <- fread("NHL.csv")
train$NF2 <- train$NF^2
train$NF3 <- train$NF^3
train$RW2 <- train$RW^2
train$RW3 <- train$RW^3
train$Salary2 <- train$Salary^2
train$Salary3 <- train$Salary^3
lm <- lm(Actual ~ NF + NF2 + NF3 + RW + RW2 + RW3 + Salary + Salary2 + Salary3, data = train)
options(scipen=999)
summary(lm)

# Cubic Cross-Validation UFC
library(data.table)
setwd("~/DK011422")
fitted_value <- NULL
squaredError <- 0
  for(i in 1:nrow(train)){
    validation <- train[i,]
    training <- train[-i,]
    lm <- lm(Actual ~ 0 + Round1_3 + Round3_2 + Round4_3 + Round5 + Decision + Draw + Salary + Salary_2 + Salary_3, data = training)
    fitted_value[i] <- predict(lm, validation)
    squaredError <- squaredError + (fitted_value[i] - validation$Actual)^2
  }
sqrt(squaredError/nrow(train))

# Cubic Regression UFC
setwd("~/DK011422")
library(data.table)
train <- fread("UFC.csv")
train$Round1_2 <- train$Round1^2
train$Round1_3 <- train$Round1^3
train$Round2_2 <- train$Round2^2
train$Round2_3 <- train$Round2^3
train$Round3_2 <- train$Round3^2
train$Round3_3 <- train$Round3^3
train$Round4_2 <- train$Round4^2
train$Round4_3 <- train$Round4^3
train$Round5_2 <- train$Round5^2
train$Round5_3 <- train$Round5^3
train$Decision_2 <- train$Decision^2
train$Decision_3 <- train$Decision^3
train$Draw_2 <- train$Draw^2
train$Draw_3 <- train$Draw^3
train$Salary_2 <- train$Salary^2
train$Salary_3 <- train$Salary^3
lm <- lm(Actual ~ Round1 + Round1_2 + Round1_3 + Round2 + Round2_2 + Round2_3 + Round3 + Round3_2 + Round3_3 + Round4 + Round4_2 + Round4_3 + Round5 + Round5_2 + Round5_3 + Decision + Decision_2 + Decision_3 + Draw + Draw_2 + Draw_3 + Salary + Salary_2 + Salary_3, data = train)
options(scipen=999)
summary(lm)