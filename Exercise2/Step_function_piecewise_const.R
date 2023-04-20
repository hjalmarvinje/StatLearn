

library(keras)
library(dplyr)
library(ggplot2)

# Read the dataset
data <- read_csv("data/rome_weekends.csv")


# Choose an input variable and target variable
input_var <- as.numeric(factor(data$room_type))
target_var <- data$realSum

# Define the number of intervals for the step function
n_intervals <- 3


# Fit a piecewise constant function using the lm() function
fit <- lm(target_var ~ cut(input_var, n_intervals))

# Create a new data frame with the input variable, intervals, and the corresponding step function values
piecewise_constant_data <- data.frame(
  room_type = factor(data$room_type),
  actual_real_value = target_var,
  predicted_real_value = predict(fit, newdata = data)
)

# Plot the actual observations and the step function predictions
ggplot(piecewise_constant_data) +
  geom_point(aes(x = factor(room_type), y = actual_real_value), color = "blue", alpha = 0.4) +
  geom_line(aes(x = factor(room_type), y = predicted_real_value, group = 1), color = "red", size = 1) +
  labs(x = "Room Type", y = "Real Value", title = "Actual Observations vs. Piecewise Constant Function Predictions") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "red")) +
  theme(legend.position = "none")



# # Create a piecewise constant function
# piecewise_constant_fun <- function(input_var, n_intervals) {
#   output_var <- cut(input_var, breaks = n_intervals, labels = FALSE)
#   return(output_var)
# }
# 
# # Apply the piecewise constant function to the realSum variable
# data$predicted_realSum <- piecewise_constant_fun(data$realSum, n_intervals)
# 
# # Create a scatter plot comparing the actual realSum values to the predicted realSum values
# library(ggplot2)
# 
# ggplot(data, aes(x = predicted_realSum, y = realSum)) +
#   geom_point() +
#   ggtitle("Scatter plot of Actual RealSum vs. Predicted RealSum")




# # Divide the input variable into intervals
# intervals <- cut(input_var, breaks = n_intervals, labels = FALSE)
# 
# # Calculate the mean of the target variable within each interval
# step_function_values <- tapply(target_var, intervals, mean)
# 
# # Create a new data frame with the input variable, intervals, and the corresponding step function values
# piecewise_constant_data <- data.frame(
#   room_type = factor(data$room_type),
#   actual_real_value = target_var,
#   intervals = intervals,
#   predicted_real_value = step_function_values[intervals]
# )
# 
# # Plot the actual observations and the step function predictions
# ggplot(piecewise_constant_data) +
#   geom_point(aes(x = factor(room_type), y = actual_real_value), color = "blue", alpha = 0.4) +
#   geom_line(aes(x = factor(room_type), y = predicted_real_value, group = 1), color = "red", size = 1) +
#   labs(x = "Room Type", y = "Real Value", title = "Actual Observations vs. Piecewise Constant Step Function Predictions") +
#   theme_minimal() +
#   scale_color_manual(values = c("blue", "red")) +
#   theme(legend.position = "none")

