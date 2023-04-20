library(tidyverse)
library(caret)
library(rpart)
library(rpart.plot)

# Read the data
data <- read_csv('data/rome_weekends.csv', show_col_types = FALSE)

colnames(data)

# Pre-process the data
data <- data %>%
  select(-c()) %>%
  mutate_if(is.character, as.factor)

summary(data$realSum)
mean(data$realSum)
median(data$realSum)
range(data$realSum)
sd(data$realSum)
var(data$realSum)


# Split the data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(data$realSum, p = .7, list = FALSE, times = 1)
train <- data[trainIndex, ]
test <- data[-trainIndex, ]

