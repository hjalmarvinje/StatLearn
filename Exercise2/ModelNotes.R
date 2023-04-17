
### Some example for models below here ####



## RANDOM FORREST
rf_model <- train(realSum ~ ., data = train, method = "rf")
# Make predictions on the test data
rf_pred <- predict(rf_model, newdata = test)
# Evaluate the performance of the random forest model
test$realSum <- factor(test$realSum, levels = levels(train$realSum))
confusionMatrix(rf_pred, test$realSum)


## GRADIENT BOOSTING
# Train the GBM model
gbm_model <- train(realSum ~ ., data = train, method = "gbm")
# Make predictions on the test data
gbm_pred <- predict(gbm_model, newdata = test)
# Evaluate the performance of the GBM model
confusionMatrix(gbm_pred, test$realSum)


## LOGISTIC REGRESSION
# Make predictions on the test data with logistic regression
log_reg_model <- train(realSum ~ ., data = train, method = "glm", family = "binomial")
log_reg_pred <- predict(log_reg_model, newdata = test)
# Evaluate the performance of the logistic regression model
confusionMatrix(log_reg_pred, test$realSum)


## TREE-BASED MODEL
# Train the decision tree model
tree_model <- rpart(realSum ~ ., data = train, method = "class")
# Plot the decision tree
rpart.plot(tree_model)
# Make predictions on the test data
tree_pred <- predict(tree_model, newdata = test, type = "class")
# Evaluate the performance of the decision tree model
confusionMatrix(tree_pred, test$realSum)
