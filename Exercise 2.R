install.packages("tidyverse")
install.packages("caret")
install.packages("randomForest")

library(tidyverse)
library(caret)
library(randomForest)

data <- read_csv("rome_weekends.csv")
summary(data)

set.seed(123)
trainIndex <- createDataPartition(data$price, p = 0.8, list = FALSE)
train <- data[trainIndex, ]
test <- data[-trainIndex, ]

logistic_model <- glm(price ~ ., data = train, family = "binomial")
summary(logistic_model)

logistic_predictions <- predict(logistic_model, newdata = test, type = "response")

set.seed(123)
randomForest_model <- randomForest(price ~ ., data = train, ntree = 100)
summary(randomForest_model)

randomForest_predictions <- predict(randomForest_model, newdata = test)

logistic_MAE <- mean(abs(test$price - logistic_predictions))
randomForest_MAE <- mean(abs(test$price - randomForest_predictions))

cat("Logistic Regression MAE:", logistic_MAE, "\n")
cat("Random Forest MAE:", randomForest_MAE, "\n")