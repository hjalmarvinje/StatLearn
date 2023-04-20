library(keras)
library(dplyr)
library(tidyverse)
library(caret)

# Read the dataset
data <- read_csv("data/rome_weekends.csv")

# Convert non-numeric variables into numeric variables
data$room_type <- as.numeric(factor(data$room_type))
data$room_shared <- as.numeric(data$room_shared)
data$room_private <- as.numeric(data$room_private)
data$host_is_superhost <- as.numeric(data$host_is_superhost)

# Split the dataset into training and test sets
set.seed(42)
sample_size <- floor(0.8 * nrow(data))
train_index <- sample(seq_len(nrow(data)), size = sample_size)
train <- data[train_index, ]
test <- data[-train_index, ]

# Set up the model architecture
model <- keras_model_sequential() %>%
  layer_dense(units = 60, activation = 'relu', input_shape = ncol(train) - 1) %>%
  layer_dense(units = 100, activation = 'relu') %>%
  layer_dense(units = 20, activation = 'relu') %>%
  layer_dense(units = 20, activation = 'relu') %>%
  layer_dense(units = 1)

# Compile the model
model %>% compile(
  loss = "mean_squared_error",
  optimizer = optimizer_adam(learning_rate = 0.0001),
  metrics = c("mean_absolute_error")
)

# Train the model
history <- model %>% fit(
  as.matrix(train %>% select(-realSum)), as.matrix(train[["realSum"]]),
  epochs = 50,
  batch_size = 32,
  validation_split = 0.2,
  verbose = 1
)

# Evaluate the model on the test set
model %>% evaluate(as.matrix(test %>% select(-realSum)), as.matrix(test[["realSum"]]))

# Make predictions using the trained model
predictions <- model %>% predict(as.matrix(test %>% select(-realSum)))

# Add predictions to the test dataset
test$predicted_realSum <- predictions


library(ggplot2)

# Find the maximum value of realSum and predicted_realSum
max_val <- max(max(test$realSum), max(test$predicted_realSum))

# Create a scatter plot of predicted realValue vs actual realValue
ggplot(test, aes(x = realSum, y = predicted_realSum)) +
  geom_point() +
  geom_smooth(method = "loess", span = 0.5, se = FALSE, color = "blue") +
  xlab("Actual realValue") +
  ylab("Predicted realValue") +
  theme_minimal() +
  xlim(0, max_val) +
  ylim(0, max_val) +
  coord_fixed()

