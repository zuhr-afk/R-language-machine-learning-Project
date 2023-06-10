install.packages("dplyr")
install.packages("caret")
install.packages("tidyverse")
install.packages("readr")
install.packages("e1071")
install.packages("randomForest")
library(dplyr)
library(caret)
library(tidyverse)
library(readr)
library(e1071)
library(randomForest)
library(ggplot2)
# load the dataset
SouthAfricaCrimes <- read.csv("~/SouthAfricaCrimeStats_v2.csv", header=FALSE)

# Sample 10000 rows from the dataset
set.seed(123)
data_sampled <- SouthAfricaCrimes %>% sample_n(30000, replace = TRUE)

# Convert relevant columns to numeric
data_sampled[, 4:ncol(data_sampled)] <- lapply(data_sampled[, 4:ncol(data_sampled)], as.numeric)

# Sum up the number of crimes across all categories for each row
total_crime <- rowSums(data_sampled[, 4:ncol(data_sampled)], na.rm = TRUE)

# Create a binary outcome variable based on '2005-2006'
outcome <-factor(ifelse(total_crime > 0, 1, 0),
                  levels = c(0, 1),
                  labels = c('non criminal activity', 'criminal activity'))

# Remove variables with low variance
vars_to_remove <- nearZeroVar(data_sampled, saveMetrics = TRUE)
data_sampled <- data_sampled[, !vars_to_remove$nzv]

# Check if any variables are left in the data set
if (ncol(data_sampled) == 0) {
  stop("No variables left in the data set after removing low variance variables.")
}


# Split the data into training and testing datasets
trainIndex <- createDataPartition(outcome, p = 0.7, list = FALSE, times = 1)
train <- data_sampled[trainIndex, ]
train$outcome <- outcome[trainIndex] # add outcome as a column to train
test <- data_sampled[-trainIndex, ]
test$outcome <- outcome[-trainIndex]


# Build the random forest model
set.seed(123)
rf_model <- randomForest(outcome ~ ., data = train, ntree = 500, importance = TRUE)

# Make predictions using the model
model_training <- predict(rf_model, train) # making predictions for training set
model_testing <- predict(rf_model, test) # making predictions for testing set
# Model performance (display confusion matrix and stats)
model_training_confusion <- confusionMatrix(model_training, train$outcome)
model_testing_confusion <- confusionMatrix(model_testing, test$outcome)
#print result
print(model_training_confusion)
print(model_testing_confusion)
# Get the confusion matrix for the testing set
conf_mat <- model_testing_confusion$table

# Convert the confusion matrix to a data frame
conf_mat_df <- as.data.frame.matrix(conf_mat)

# Add row and column names to the data frame
conf_mat_df <- cbind(Class = rownames(conf_mat_df), conf_mat_df)
rownames(conf_mat_df) <- NULL
# Reshape the data frame for plotting
conf_mat_df_long <- tidyr::gather(conf_mat_df, key = "Prediction", value = "Count", -Class)

# Plot the heatmap
ggplot(conf_mat_df_long, aes(x = Prediction, y = Class, fill = Count)) +
  geom_tile() +
  geom_text(aes(label = Count)) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(x = "Predicted", y = "Actual", title = "Confusion Matrix Heatmap for Testing Set") +
  theme_minimal()
# Calculate accuracy and f1_score for training set
train_accuracy <- confusionMatrix(model_training, train$outcome)$overall['Accuracy']
train_f1_score <- confusionMatrix(model_training, train$outcome)$byClass['F1']
# Calculate accuracy and f1_score for testing set
test_accuracy <- confusionMatrix(model_testing, test$outcome)$overall['Accuracy']
test_f1_score <- confusionMatrix(model_testing, test$outcome)$byClass['F1']
# Create a data frame with the metrics
df_metrics <- data.frame(
  Metric = c('Accuracy', 'F1 Score'),
  Training = c(train_accuracy, train_f1_score),
  Testing = c(test_accuracy, test_f1_score)
)
# Reshape data to long format for plotting
df_metrics_long <- df_metrics %>% pivot_longer(cols = c('Training', 'Testing'), names_to = 'Set', values_to = 'Value')

# Plot bar chart
ggplot(df_metrics_long, aes(Metric, Value, fill = Set)) +
  geom_col(position = position_dodge()) +
  scale_fill_manual(values = c('blue', 'red')) +
  ggtitle('Model Performance Metrics') +
  theme_minimal()
