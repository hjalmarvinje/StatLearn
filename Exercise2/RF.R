library(randomForest)


# Read the dataset
data <- read_csv("Exercise2/data/rome_weekends.csv")

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

# Train the random forest model
rf_model <- randomForest(realSum ~ ., data = train, ntree = 800, mtry = 4, importance = TRUE)

# Make predictions using the trained random forest model
predictions <- predict(rf_model, newdata = test)

# Calculate the mean squared error
mae <- mean(abs(test$realSum - predictions))
print(paste("Mean Absolute Error:", mae))

# Add the predicted realValue to the test dataset
test$predicted_realSum <- predictions

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
