# Load required packages
library(tidyverse)

# Read in the dataset
df <- read.csv("D:\\MySpace\\MyWorkspace-Year2\\SEM-4\\DS\\CP\\Paitients_Files_Train.csv")

head(df,5)
View(df)
colnames(df)


patients <- unique(df$ID)
length(patients)
nrow(df)

#missing_values = data.frame()
for (x in colnames(df)){
  print(x)
  print(sum(df[x]==0))
  #missing_values <- cbind(missing_values,x = c(as.numeric(sum(df[x]==0)),))
  # append(missing_values,as.integer(sum(df[x]==0)))
}
str(df)

# Load required packages
library(ggplot2)
library(dplyr)


# Check the structure of the data
str(df)

# Summary statistics for each variable
summary(df)

# Distribution of each variable
hist(df$PL)
hist(df$PR)
hist(df$SK)
hist(df$TS)
hist(df$M11)
hist(df$BD2)
hist(df$Age)
hist(df$Insurance)

# Scatter plot of two variables
ggplot(df, aes(x = Age, y = M11)) +
  geom_point()

# Boxplot of a variable
ggplot(df, aes(y = PL)) +
  geom_boxplot()

# Bar chart of a categorical variable
ggplot(df, aes(x = Sepssis)) +
  geom_bar()

# Correlation matrix of numeric variables
cor_matrix <- cor(df[, c("PL", "PR", "SK", "TS", "M11", "BD2", "Age")])

# Create a heatmap
ggplot(data = reshape2::melt(cor_matrix)) +
  geom_tile(aes(x = Var2, y = Var1, fill = value)) +
  scale_fill_gradient(low = "#FFFFFF", high = "#2F4B7C") +
  theme_minimal()

sum(df$Sepssis=='Positive')





