df = read.csv("D:\\MySpace\\MyWorkspace-Year2\\SEM-4\\DS\\CP\\Dataset.csv")

head(df)
tail(df)
sum(is.na(df))
str(df)

# Calculate the percentage of null values in each column
null_percentage <- colMeans(is.na(df)) * 100

# Print the null percentage for each column
print(null_percentage)

missing_percentage <- round(100*(colSums(is.na(df))/nrow(df)), 2)
print(missing_percentage)
barplot(missing_percentage, main="Percentage of missing values in each column", 
        ylab="Percentage of missing values", las=2, cex.names=0.7)

library(caret)

set.seed(123) # for reproducibility
trainIndex <- createDataPartition(df$SepsisLabel, p = 0.8, list = FALSE, times = 1)
df_train <- df[trainIndex,]
df_test <- df[-trainIndex,]

library(dplyr)

df_train <- df_train %>% 
  mutate(Unit = Unit1 + Unit2)

head(df_train)


columns_drop <- c('Unnamed: 0','SBP','DBP','EtCO2','BaseExcess', 'HCO3','pH','PaCO2','Alkalinephos', 'Calcium','Magnesium', 'Phosphate','Potassium','PTT','Fibrinogen','Unit1','Unit2')

df_train <- df_train[, !(names(df_train) %in% columns_drop)]

library(dplyr)
library(zoo)

str(df_train)

null_percentage <- colMeans(is.na(df_train)) * 100
print(null_percentage)

df_train_impute <- data.frame(df_train)

library(mice)



# Set seed for reproducibility
set.seed(123)

# Create a mice object with default settings
df_train_impute <- mice(df_train_impute)

# Run the imputation process
imputed_data <- mice::complete(df_train_impute)
View(imputed_data)

setwd("D:\\MySpace\\MyWorkspace-Year2\\SEM-4\\DS\\CP")

# Export the data frame to a CSV file named "my_data.csv"
write.csv(imputed_data, file = "imputed_data.csv", row.names = FALSE)

sum(is.na(imputed_data))

null_percentage <- colMeans(is.na(imputed_data)) * 100
print(null_percentage)
str(imputed_data)

imputed_data <- imputed_data[, -which(names(imputed_data) == "Unit")]


# Split the data into training and test sets
set.seed(123)
trainIndex <- createDataPartition(imputed_data$SepsisLabel, p = 0.8, list = FALSE)
train <- imputed_data[trainIndex, ]
test <- imputed_data[-trainIndex, ]

# Define the predictors and response variable
str(imputed_data)

predictors <- colnames(imputed_data)[2:28]
response <- "SepsisLabel"

library(caret)
library(xgboost)
library(lime)
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

sum(is.na(imputed_data))

# Use the trained model to make predictions on the test set
pred <- predict(xgb_model, as.matrix(test[, predictors]))

# Calculate the performance metrics (e.g., AUC) on the test set
confusionMatrix(table(test[, response], round(pred)), positive = "1")

# Use LIME to explain the model's decision on a single instance
explainer <- lime(train[, predictors], model = xgb_model, bin_continuous = TRUE)

explanation <- explain(test[1, predictors], explainer,n_labels = 1, n_features = 39)
explanation
plot_features(explanation)

library(tidyverse)
df_imputed = read.csv("D:\\MySpace\\MyWorkspace-Year2\\SEM-4\\DS\\CP\\imputed_data.csv")
dim(df_imputed)
head(df_imputed)
df_imputed <- select(df_imputed, -Patient_ID)
df_imputed <- select(df_imputed, -Unit)
#df_imputed <- select(df_imputed, -)
# Load required libraries
library(caret)
library(xgboost)
library(lime)

# Load the dataset (assuming it's stored in a CSV file)
data <- data.frame(df_imputed)

# Split the data into training and test sets
set.seed(123)
trainIndex <- createDataPartition(data$SepsisLabel, p = 0.8, list = FALSE)
train <- data[trainIndex, ]
test <- data[-trainIndex, ]

# Define the predictors and response variable
predictors <- colnames(data)[2:27]
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

# Confusion Matrix and Statistics
# 
# 
# 0      1
# 0 243893      0
# 1      0   4460
# 
# Accuracy : 1        
# 95% CI : (1, 1)   
# No Information Rate : 0.982    
# P-Value [Acc > NIR] : < 2.2e-16
# 
# Kappa : 1        
# 
# Mcnemar's Test P-Value : NA       
#                                    
#             Sensitivity : 1.00000  
#             Specificity : 1.00000  
#          Pos Pred Value : 1.00000  
#          Neg Pred Value : 1.00000  
#              Prevalence : 0.01796  
#          Detection Rate : 0.01796  
#    Detection Prevalence : 0.01796  
#       Balanced Accuracy : 1.00000  
#                                    
#        'Positive' Class : 1        
                             

# Use LIME to explain the model's decision on a single instance
explainer <- lime(train[, predictors], model = xgb_model, bin_continuous = TRUE)
explanation <- explain(test[1, predictors], explainer,n_labels = 1, n_features = 39)
explanation
plot_features(explanation)

# Load the necessary package
library(ggplot2)

# Create a histogram of age
ggplot(data = df_imputed, aes(x = Age)) + 
  geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
  labs(title = "Distribution of Age", x = "Age", y = "Count")




# Create a data frame with the count of each gender
gender_count <- data.frame(table(df_imputed$Gender))

# Create a bar plot
ggplot(gender_count, aes(x = factor(Var1), y = Freq)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Gender", y = "Count", title = "Distribution of Gender")


library(ggplot2)

ggplot(df_imputed, aes(x = factor(Gender), fill = factor(SepsisLabel))) +
  geom_bar(position = "dodge") +
  labs(x = "Gender", y = "Count", title = "Distribution of SepsisLabel by Gender") +
  scale_fill_discrete(name = "SepsisLabel", labels = c("No", "Yes"))


sepsis_count <- table(df_imputed$SepsisLabel)
View(sepsis_count)
ggplot(data.frame(sepsis_count), aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "SepsisLabel", y = "Count", title = "Distribution of SepsisLabel")

sum(df_imputed$SepsisLabel==1)

# Load necessary libraries
library(dplyr)

# Split the dataset based on the SepsisLabel
df_positive <- df_imputed %>% filter(SepsisLabel == 1) # Subset of positive sepsis cases
df_negative <- df_imputed %>% filter(SepsisLabel == 0) # Subset of negative sepsis cases

# Determine the number of positive cases and set it as the size of the new dataset
new_size <- nrow(df_positive)

# Sample the same number of negative cases as the positive cases
df_negative_sampled <- df_negative %>% sample_n(new_size)

# Combine the positive and negative cases into a new dataset
df_balanced <- rbind(df_positive, df_negative_sampled)
summary(df_balanced)
str(df_balanced)
dim(df_balanced)
# Load the dataset (assuming it's stored in a CSV file)
data <- data.frame(df_balanced)
data <- select(data, -HospAdmTime)
data <- select(data, -Hour)

# Split the data into training and test sets
set.seed(123)
trainIndex <- createDataPartition(data$SepsisLabel, p = 0.8, list = FALSE)
train <- data[trainIndex, ]
test <- data[-trainIndex, ]

# Define the predictors and response variable
predictors <- colnames(data)[2:24]
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



install.packages("DMwR")

library(smotefamily)

# assume your dataset is stored in a variable called 'mydata'
# where the target variable is named 'SepsisLabel'
df_imputed <- select(df_imputed, -HospAdmTime)
df_imputed <- select(df_imputed, -Hour)
# separate the target variable from the features
x <- df_imputed[, !colnames(df_imputed) %in% c("SepsisLabel")]
y <- df_imputed$SepsisLabel

# apply SMOTE
balanced_data <- SMOTE(x, y)
balanced_data_smote <- balanced_data$data# the perc.over and perc.under parameters control the degree of oversampling and undersampling, respectively
data <- data.frame(balanced_data_smote)

data[, cols] <- scale(data[, cols])
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


