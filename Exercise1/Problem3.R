bigfoot_original <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-13/bigfoot.csv")

library(dplyr)

# Prepare the data:
bigfoot <- bigfoot_original %>%
  # Select the relevant covariates:
  dplyr::select(classification, observed, longitude, latitude, visibility) %>%
  # Remove observations of class C (these are second- or third hand accounts):
  dplyr::filter(classification != "Class C") %>%
  # Turn into 0/1, 1 = Class A, 0 = Class B:
  dplyr::mutate(class = ifelse(classification == "Class A", 1, 0)) %>%
  # Create new indicator variables for some words from the description:
  dplyr::mutate(fur = grepl("fur", observed),
                howl = grepl("howl", observed),
                saw = grepl("saw", observed),
                heard = grepl("heard", observed)) %>%
  # Remove unnecessary variables:
  dplyr::select(-c("classification", "observed")) %>%
  # Remove any rows that contain missing values:
  tidyr::drop_na()

set.seed(2023)
# 70% of the sample size for training set
training_set_size <- floor(0.7 * nrow(bigfoot))
train_ind <- sample(seq_len(nrow(bigfoot)), size = training_set_size)
train <- bigfoot[train_ind, ]
test <- bigfoot[-train_ind, ]

## Task a) (i): Fit a logistic regression model using the training set, and perform the classification on the test
## set, using a 0.5 cutoff. How many of the reports were classified as clear sightings (class A, category 1)?

?glm()

model <- glm(class~longitude+latitude+visibility+fur+howl+saw+heard, family="binomial", data=train)

glm_probabilities <- predict(model, test, type="response")
no_classified = sum(glm_probabilities >= 0.5)
no_classified # Number of reports classified as clear sightings: 441
print('Number of clear sightings: 441')

## Task a) (ii): Single choice: According to this model, how would the odds that an observation is classified as
## Class A change if the report contains the word “saw”, compared to if it does not? (all other covariates stay the same)

summary(model)
print("The coefficients for sawTRUE is 1.29, which means that the average change in log odds with one unit increase of the value")
change <- exp(1.29)
print(change)
print("The answer is therefore D) Multiply by 3.64")


## Task b) (i): Fit a QDA model using the training set, and perform the classification on the test set, using a 0.5
## cutoff. How many of the reports were classified as class A?

require(MASS)
qda_model <- qda(class~longitude+latitude+visibility+fur+howl+saw+heard, data=train)
qda_predicted <- predict(qda_model, test)
qda_predicted
table(qda_predicted$class) 
print('Number of clear sightings: 626')

## Task b) (ii): Which statements about linear discriminant analysis and quadratic discriminant analysis are true
## and which are false? Say for each of them if it is true or false.

print('1): True')
print('2): False')
print('3): False')
print('4): False')


## Task c) (i): Fit a KNN model using the training set, and perform the classification on the test set, with
## k = 25 (use the knn function from the class package).

require(class)
?knn()
knn_model <- knn(train=train, test=test, cl=train$class, k=25, prob=TRUE)
knn_model

table(knn_model)
print('Number of clear sightings: 441')

## Task c) (ii):  Explain how you could choose the tuning parameter k in a better way. How does k relate to the
## bias-variance trade-off in this context?

print('Trade-off between bias and variance, higher k -> less variance and more bias, could create
      plots for different k-values and choose the k-value with the lowest error.')


## Task d) We now wish to compare the performance of the three models (logistic regression, QDA and KNN) on this
## dataset, in order to report back to BFRO on our results

# (i) In this case, are we interested in prediction or inference? What implications would it have for the
#model choice if we wanted to do prediction, and what implications would it have if we wanted to do
#inference? In conclusion, does the question of whether our aim is prediction or inference exclude any of
#the three candidate models in this case? (comment briefly)

print('Prediction, because we use existing data for creating a model that will classify a new instance correctly as
      often as possible. With inference, we are more interested in evaluating the relationship between the response
      variables and the predictor, i.e. the interepretability of the model. All models are interesting with predicting,
      but KNN and QDA would not been as relevant for inference.')


# (ii): Make confusion matrices for the three predictions performed on the test set in a) - c), and report
#the confusion matrices, along with sensitivity and specificity. Explain briefly: What does sensitivity and
#specificity mean? Feel free to explain using the bigfoot example. Make sure it is clear in the confusion
#matrix which numbers show the predictions and which show the true values.

print('Sensitivity: True positive value, probability of a positive test result, given that instance truly is positive')
print('Specificity: True negative value, probability of a negative test result, given that instance tryly is negative')

print('For all confusion matrices: rows show prediction values and columns show true values')

# Confusion matrix Glm
glm_predicted <- rep(0, 912)
glm_predicted[glm_probabilities > 0.5] <- 1
table(glm_predicted, test$class)
glm_sensitivity <- 299/(299+148)
glm_specificity <- 323/(323+142)
glm_sensitivity
glm_specificity
print('Glm sensitivity is 66,9 % and specificity is 69,5 %'  )

# Confusion matrix QDA
table(qda_predicted$class, test$class) 
qda_sensitivity <- 389/(389+58)
qda_specificity <- 228/(228+237)
qda_sensitivity
qda_specificity
print('QDA sensitivity is 87,0 % and specificity is 49,0 %'  )

# Confusion matrix KNN
table(knn_model, test$class) 
knn_sensitivity <- 362/(362+85)
knn_specificity <- 386/(386+79)
knn_sensitivity
knn_specificity
print('KNN sensitivity is 81,0 % and specificity is 83,0 %'  )

# (iii):Present a plot of the ROC curves and calculate the area under the curve (AUC) for each of the classifiers.

library(pROC)
?roc()
?plot()

glm_roc <- roc(response = test$class, predictor = glm_probabilities)
plot(glm_roc, col="pink", lwd=4, print.auc=TRUE, main="ROC-curve for glm-model")

qda_roc <- roc(response = test$class, predictor = qda_predicted$posterior[,"1"])
plot(qda_roc, col="purple", lwd=4, print.auc=TRUE, main="ROC-curve for qda-model")

knn_probabilities <- ifelse(knn_model == 0, 1 - attributes(knn_model)$prob,attributes(knn_model)$prob)
knn_roc <- roc(response = test$class, predictor = knn_probabilities)
plot(knn_roc, col="turquoise", lwd=4, print.auc=TRUE, main="ROC curve knn-model")



# (iv): Summarize the performance of the three classifiers with words, based on the evaluations above. 
# Which one would you choose? Justify briefly


print('Glm and QDA performs similar for ROC, while KKN performs significantly better. Would therefore choose this classifier
      for this problem.')

