# Load required packages
library(tidyverse)

# Read in the dataset
dataSepsis <- read.csv("D:\\MySpace\\MyWorkspace-Year2\\SEM-4\\DS\\CP\\Dataset.csv")

head(dataSepsis,15)
View(dataSepsis)
colnames(dataSepsis)

length(unique(dataSepsis$Patient_ID))

str(dataSepsis)

# Finding the Missing Values
library(ggplot2)

missing_percentage <- round(100*(colSums(is.na(dataSepsis))/nrow(dataSepsis)), 2)
print(missing_percentage)
barplot(missing_percentage, main="Percentage of missing values in each column", 
         ylab="Percentage of missing values", las=2, cex.names=0.7)

#finding relevent features
df <- data.frame(dataSepsis)

view(summary(df))

library(caret)
library(ggplot2)

# Define the vital signs
vital_signs <- c("HR", "O2Sat", "Temp", "SBP", "MAP", "DBP", "Resp", "EtCO2")

# Create a function to plot the histograms for each vital sign
plot_histogram <- function(df, column) {
  ggplot(df, aes(x = .data[[column]], fill = as.factor(SepsisLabel))) + 
    geom_histogram(binwidth = 1, alpha = 0.5) +
    xlab(paste("Value of", column)) +
    ylab("Frequency") +
    theme(legend.position = "top", legend.title = element_blank())
}

# Create a list of histograms for each vital sign
histograms <- lapply(vital_signs, function(x) plot_histogram(df, x))

# Combine the histograms into a grid
gridExtra::grid.arrange(grobs = histograms, ncol = 4)


histograms <- lapply(vital_signs, function(x) {
  ggplot(data = df, mapping = aes(x = .data[[x]], fill = factor(SepsisLabel))) +
    geom_density(alpha = 0.5) +
    xlab(paste("Value of", x)) +
    ylab("Density") +
    scale_fill_manual(values = c("#00ff00", "#377eb8")) +
    theme(legend.position = "top", legend.title = element_blank())
})

grid.arrange(grobs = histograms, ncol = 4)

df <- data.frame(dataSepsis)
library(ggplot2)

lab_values <- c('BaseExcess', 'HCO3', 'FiO2', 'pH', 'PaCO2', 'SaO2', 'AST', 'BUN',
                'Alkalinephos', 'Calcium', 'Chloride', 'Creatinine', 'Bilirubin_direct',
                'Glucose', 'Lactate', 'Magnesium', 'Phosphate', 'Potassium',
                'Bilirubin_total', 'TroponinI', 'Hct', 'Hgb', 'PTT', 'WBC',
                'Fibrinogen', 'Platelets')
# Create a list of histograms for each lab value
histograms <- lapply(lab_values, function(x) {
  
  # Create a ggplot object with the data and mapping
  ggplot(data = df, mapping = aes(x = .data[[x]], fill = factor(SepsisLabel))) +
    
    # Add a histogram layer with specified binwidth, alpha, and density y-axis
    geom_histogram(binwidth = 1, alpha = 0.5, aes(y = ..density..)) +
    
    # Add x and y axis labels
    xlab(paste("Value of", x)) +
    ylab("Density") +
    scale_x_continuous(limits = c(0, 1)) +
    
    # Add a manual color scale
    scale_fill_manual(values = c("#E69F00", "#0072B2"), name = "SepsisLabel") +
    
    # Customize theme and legend
    theme_bw() +
    theme(legend.position = "top", legend.title = element_blank())
})

grid.arrange(grobs = histograms, ncol = 8)

df <- data.frame(dataSepsis)
# Convert all columns in the data frame df to numeric
df[] <- lapply(df, as.numeric)
cor_matrix <- cor(df)
colnames(df)

# Create a new column named "Unit"
df$Unit <- ""

# Fill in the "Unit" column based on the values in "Unit1" and "Unit2"
df$Unit[df$Unit1 == 1] <- "MICU"
df$Unit[df$Unit2 == 1] <- "SICU"
df$Unit[(is.na(df$Unit1) & is.na(df$Unit2))] <- "Other ICU"

df <- df[c("HR", "O2Sat", "Temp", "MAP", "Resp", "BaseExcess", "HCO3", "pH", "PaCO2", 
           "SaO2", "AST", "BUN", "Alkalinephos", "Calcium", "Chloride", "Creatinine", 
           "Glucose", "Lactate", "Magnesium", "Phosphate", "Potassium", "Bilirubin_total", 
           "Hgb", "PTT", "WBC", "Fibrinogen", "Platelets", "Age", "Gender", "Unit", 
           "ICULOS", "SepsisLabel", "Patient_ID")]

library(caret)



str(df)

# load the required packages
library(readr)
library(randomForest)
library(caret)

#packages

library(tidyr)
library(dplyr)
library(caret)
library(ranger)
library(gbm)
library(xgboost)
library(ROCR)
library(DALEX)
library(pdp)
library(randomForestExplainer)
dataSepsis <- read.csv("D:\\MySpace\\MyWorkspace-Year2\\SEM-4\\DS\\CP\\Dataset.csv")
df <- data.frame(dataSepsis)
df$Unit <- ""

# Fill in the "Unit" column based on the values in "Unit1" and "Unit2"
df$Unit[df$Unit1 == 1] <- "MICU"
df$Unit[df$Unit2 == 1] <- "SICU"
df$Unit[(is.na(df$Unit1) & is.na(df$Unit2))] <- "Other ICU"

df <- df[c("HR", "O2Sat", "Temp", "MAP", "Resp", "BaseExcess", "HCO3", "pH", "PaCO2", 
           "SaO2", "AST", "BUN", "Alkalinephos", "Calcium", "Chloride", "Creatinine", 
           "Glucose", "Lactate", "Magnesium", "Phosphate", "Potassium", "Bilirubin_total", 
           "Hgb", "PTT", "WBC", "Fibrinogen", "Platelets", "Age", "Gender", "Unit", 
           "ICULOS", "SepsisLabel", "Patient_ID")]
# remove irrelevant columns
df <- df[, !(names(df) %in% c("Patient_ID", "Unit", "ICULOS"))]
# Load the mice package
library(mice)

# Impute missing values using MICE
#df_imputed <- mice(df, m = 5, method = "rf")
# split data into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(df$SepsisLabel, p = 0.7, list = FALSE)
train <- df[trainIndex, ]
test <- df[-trainIndex, ]
train[, 1:29] <- lapply(train[, 1:29], function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x))
test[, 1:29] <- lapply(test[, 1:29], function(x) ifelse(is.na(x), median(x, na.rm = TRUE), x))
# train a random forest model
model <- randomForest(SepsisLabel ~ ., data = train, ntree = 100, importance = TRUE)

# make predictions on the testing data
predictions <- predict(model, test[, !(names(test) %in% "SepsisLabel")])

# evaluate the model's performance
confusionMatrix(predictions, test$SepsisLabel)

# get variable importance
varImpPlot(model)

# generate a partial dependence plot
library(pdp)
pdp::partial(model, pred.var = "HR", plot = TRUE)


