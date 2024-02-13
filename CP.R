# Load required packages
library(xgboost)
library(tidyverse)
# Load the dataset
dataSepsis <- read.csv("D:\\MySpace\\MyWorkspace-Year2\\SEM-4\\DS\\CP\\compiled_data.csv")

str(dataSepsis)

missing_percentage <- round(100*(colSums(is.na(dataSepsis))/nrow(dataSepsis)), 2)
print(missing_percentage)
barplot(missing_percentage, main="Percentage of missing values in each column", 
        ylab="Percentage of missing values", las=2, cex.names=0.7)

df <- data.frame(dataSepsis)
df <- select(df, -EtCO2)
df <- select(df, -Unit1)
df <- select(df, -Unit2)

# Define columns to drop
columns_drop <- c('Unnamed: 0', 'SBP', 'DBP', 'EtCO2', 'BaseExcess', 'HCO3', 'pH', 'PaCO2', 'Alkalinephos', 'Calcium', 'Magnesium', 'Phosphate', 'Potassium', 'PTT', 'Fibrinogen', 'Unit1', 'Unit2')

# Select only columns that are not in the columns_drop vector
df_new <- df[, setdiff(colnames(df), columns_drop)]


colnames(df_new)
dataSepsis <- data.frame(df)
missing_percentage <- round(100*(colSums(is.na(df))/nrow(df)), 2)
print(missing_percentage)
barplot(missing_percentage, main="Percentage of missing values in each column", 
        ylab="Percentage of missing values", las=2, cex.names=0.7)

library(caret)
library(ggplot2)
library(gridExtra)

#data visualization
# Define the vital signs
vital_signs <- c("HR", "O2Sat", "Temp", "SBP", "MAP", "DBP", "Resp")

histograms_vital <- lapply(vital_signs, function(x) {
  ggplot(data = df, mapping = aes(x = .data[[x]], fill = factor(SepsisLabel))) +
    geom_density(alpha = 0.5) +
    xlab(paste("Value of", x)) +
    ylab("Density") +
    scale_fill_manual(values = c("#ff1b1f", "#1f77b4")) +
    theme(legend.position = "top", legend.title = element_blank())
})

grid.arrange(grobs = histograms_vital, ncol = 4)

#lab values
lab_values <- c('BaseExcess', 'HCO3', 'FiO2', 'pH', 'PaCO2', 'SaO2', 'AST', 'BUN',
                'Alkalinephos', 'Calcium', 'Chloride', 'Creatinine', 'Bilirubin_direct',
                'Glucose', 'Lactate', 'Magnesium', 'Phosphate', 'Potassium',
                'Bilirubin_total', 'TroponinI', 'Hct', 'Hgb', 'PTT', 'WBC',
                'Fibrinogen', 'Platelets')

histograms_lab <- lapply(lab_values, function(x) {
  ggplot(data = df, mapping = aes(x = .data[[x]], fill = factor(SepsisLabel))) +
    geom_density(alpha = 0.5) +
    xlab(paste("Value of", x)) +
    ylab("Density") +
    scale_fill_manual(values = c("#ff110e", "#1f77b4")) +
    theme(legend.position = "top", legend.title = element_blank())
})

grid.arrange(grobs = histograms_lab, ncol = 8)

str(df)

# Select the variables you want to include in the correlation matrix
vars <- c("HR", "O2Sat", "Temp", "SBP", "MAP", "DBP", "Resp")

# Create a subset of the data frame with the selected variables
df_subset <- df[vars]

# Calculate the correlation matrix
cor_matrix <- cor(df_subset)

# Print the correlation matrix
print(cor_matrix)

heatmap(cor_matrix)

#building model
library(foreach)
library(doParallel)
library(caret)
library(mlr)
library(ranger)
library(xgboost)

# Set the seed for reproducibility
set.seed(123)

# Define the number of cores to use
cores <- detectCores()

# Register the parallel backend
cl <- makeCluster(cores)
registerDoParallel(cl)

# Create the training and testing data
trainIndex <- createDataPartition(df$SepsisLabel, p = 0.8, list = FALSE)
training_data <- df[trainIndex, ]
testing_data <- df[-trainIndex, ]
dim(testing_data)
dim(training_data)
library(caret)
library(mlr)
library(ranger)
library(xgboost)

library(caret)
library(randomForest)
set.seed(1234)

# create a formula for logistic regression
formula <- as.formula("SepsisLabel ~ .")

# train the logistic regression model using the training data
glm_model <- glm(formula = formula, data = training_data, family = "binomial")

# use the model to make predictions on the testing data
glm_pred <- predict(glm_model, newdata = testing_data, type = "response")

# convert the probabilities to binary predictions (0 or 1)
glm_pred_binary <- ifelse(glm_pred > 0.5, 1, 0)

# evaluate the model using confusion matrix
confusionMatrix(table(glm_pred_binary, testing_data$SepsisLabel))

# Result
# Confusion Matrix and Statistics
# 
# 
# glm_pred_binary     0     1
# 0 19536   389
# 1    33    41
# 
# Accuracy : 0.9789          
# 95% CI : (0.9768, 0.9808)
# No Information Rate : 0.9785          
# P-Value [Acc > NIR] : 0.3598          
# 
# Kappa : 0.1574          
# 
# Mcnemar's Test P-Value : <2e-16          
#                                           
#             Sensitivity : 0.99831         
#             Specificity : 0.09535         
#          Pos Pred Value : 0.98048         
#          Neg Pred Value : 0.55405         
#              Prevalence : 0.97850         
#          Detection Rate : 0.97685         
#    Detection Prevalence : 0.99630         
#       Balanced Accuracy : 0.54683         
#                                           
#        'Positive' Class : 0    

library(caret)

# Set the seed for reproducibility
set.seed(123)

# Split the data into 60% training, 20% validation, and 20% test
trainIndex <- createDataPartition(df$SepsisLabel, p = 0.6, list = FALSE)
validIndex <- createDataPartition(df[-trainIndex, ]$SepsisLabel, p = 0.5, list = FALSE)
testIndex <- setdiff(1:nrow(df), c(trainIndex, validIndex))

training_data <- df[trainIndex, ]
validation_data <- df[validIndex, ]
testing_data <- df[testIndex, ]

# Fit the logistic regression model on the training data
glm_model <- glm(formula = formula, data = training_data, family = "binomial")


# Evaluate model performance on the validation set
valid_pred <- predict(glm_model, newdata = validation_data, type = "response")

# convert the probabilities to binary predictions (0 or 1)
valid_pred_binary <- ifelse(valid_pred > 0.5, 1, 0)

confusionMatrix(table(valid_pred_binary, validation_data$SepsisLabel))

# testing the model on testing_data
# Generate predicted values based on testing data
lr_pred <- predict(glm_model, newdata = testing_data)
lr_pred_binary <- ifelse(lr_pred > 0.5, 1, 0)
# Create confusion matrix
cm <- confusionMatrix(table(lr_pred_binary, testing_data$SepsisLabel))

# Print confusion matrix
print(cm)


# Explainer Model
library(lime)
library(MASS)
library(caret)
library(e1071)
# Create the training and testing data
trainIndex <- createDataPartition(df$SepsisLabel, p = 0.8, list = FALSE)
training_data <- df[trainIndex, ]
testing_data <- df[-trainIndex, ]
dim(testing_data)
dim(training_data)
library(caret)
library(mlr)
library(ranger)
library(xgboost)

library(caret)
library(randomForest)
set.seed(1234)

# create a formula for logistic regression
formula <- as.formula("SepsisLabel ~ .")

# train the logistic regression model using the training data
glm_model <- glm(formula = formula, data = training_data, family = "binomial")

# use the model to make predictions on the testing data
glm_pred <- predict(glm_model, newdata = testing_data, type = "response")

# convert the probabilities to binary predictions (0 or 1)
glm_pred_binary <- ifelse(glm_pred > 0.5, 1, 0)

# evaluate the model using confusion matrix
confusionMatrix(table(glm_pred_binary, testing_data$SepsisLabel))

explainer <- lime(training_data, glm_model)

sample_index <- sample(1:nrow(testing_data), 5)
explanation <- explain(testing_data[sample_index, -8], explainer, n_labels = 1, n_features = 5, type = "classification")



# Load required libraries
library(caret)
library(xgboost)
library(lime)

# Load the dataset (assuming it's stored in a CSV file)
data <- data.frame(df)

# Split the data into training and test sets
set.seed(123)
trainIndex <- createDataPartition(data$SepsisLabel, p = 0.8, list = FALSE)
train <- data[trainIndex, ]
test <- data[-trainIndex, ]

# Define the predictors and response variable
predictors <- colnames(data)[2:39]
response <- "SepsisLabel"

# Train the XGBoost model
xgb_model <- xgboost(
  data = as.matrix(train[, predictors]),
  label = train[, response],
  nrounds = 100,
  objective = "binary:logistic",
  eta = 0.1,
  max_depth = 3,
  lambda = 1,
  alpha = 0
)

# Use the trained model to make predictions on the test set
pred <- predict(xgb_model, as.matrix(test[, predictors]))

# Calculate the performance metrics (e.g., AUC) on the test set
confusionMatrix(table(test[, response], round(pred)), positive = "1")

# Use LIME to explain the model's decision on a single instance
explainer <- lime(train[, predictors], model = xgb_model, bin_continuous = TRUE)
explanation <- explain(test[1, predictors], explainer,n_labels = 1, n_features = 39)
explanation
plot_features(explanation)

#SMOTE
library(smotefamily)

# separate the target variable from the features
x <- df_new[, !colnames(df_new) %in% c("SepsisLabel")]
y <- df_new$SepsisLabel

# apply SMOTE
data_smote <- SMOTE(x, y)
smote_df <- data_smote$data
data <- data.frame(smote_df)
data$class <- as.numeric(data$class)

# Get column names except "class"
cols <- names(data)[names(data) != "class"]

# Scale all columns except "class"
data[cols] <- scale(data[cols])


set.seed(123)
trainIndex <- createDataPartition(data$class, p = 0.8, list = FALSE)
train <- data[trainIndex, ]
test <- data[-trainIndex, ]

# Define the predictors and response variable
predictors <- colnames(data)[2:24]
response <- "class"

# Train the XGBoost model
xgb_model <- xgboost(
  data = as.matrix(train[, predictors]),
  label = train[, response],
  nrounds = 100,
  objective = "binary:logistic",
  eta = 0.1,
  max_depth = 3,
  lambda = 1,
  alpha = 0
)

# Use the trained model to make predictions on the test set
pred <- predict(xgb_model, as.matrix(test[, predictors]))

# Calculate the performance metrics (e.g., AUC) on the test set
confusionMatrix(table(test[, response], round(pred)), positive = "1")

# Use LIME to explain the model's decision on a single instance
explainer <- lime(train[, predictors], model = xgb_model, bin_continuous = TRUE)
explanation <- explain(test[1, predictors], explainer,n_labels = 1, n_features = 39)
explanation
plot_features(explanation)

