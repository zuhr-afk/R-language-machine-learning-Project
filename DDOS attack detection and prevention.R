library(tidyverse)
library(readr)
library(mltools)
library(caret)
library(kernlab)
library(e1071)
library(tictoc)
library(lattice)
library(randomForest)
set.seed(123)

# Load the dataset
data <- read_csv("C:/Users/user/Downloads/data_bal.csv")

# Sample 10000 rows from the dataset
data_sampled <- data %>% sample_n(10000, replace = TRUE)

data_sampled$label <- factor(data_sampled$label, levels=c(1,0), labels=c('benign', 'malicious'))

# Remove variables with zero variance
nzv <- nearZeroVar(data_sampled)
data_sampled <- data_sampled[, -nzv]

# Visualize the number of benign and malicious rows in dataset
ggplot(data_sampled, aes(label, fill=label1)) + geom_bar() + ggtitle('Number of benign and malicious rows in dataset')

# Split the data into training and testing sets
set.seed(123)
train_index <- createDataPartition(data_sampled$label, p=0.8, list=FALSE)
train_data <- data_sampled[train_index,]
test_data <- data_sampled[-train_index,]

# Set up the tuning grid
#tuning_grid <- data.frame(degree = 1:80, scale = c(0.1, 1, 10), C = 2^(-2:2))

# Train the model
model <- train(label~., data = train_data,
               method = "svmPoly",
               na.action = na.omit,
               preProcess = c("scale", "center"),
               trControl = trainControl(method = "none",
               tuning_grid <- expand.grid(degree = 1:3, scale = c(0.1, 1, 10), C = 2^(-2:2))

               )
)


# Make predictions using the models
model_training <- predict(model, train_data) # making predictions for training set
model_testing <- predict(model, test_data) # making predictions for testing set


# Model performance (display confusion matrix and stats)
model_training_confusion <- confusionMatrix(model_training, train_data$label)
model_testing_confusion <- confusionMatrix(model_testing, test_data$label)


#print result
print(model_training_confusion)
print(model_testing_confusion)

#Plot the confusion matrix heatmap for the testing set


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
train_accuracy <- confusionMatrix(model_training, train_data$label)$overall['Accuracy']
train_f1_score <- confusionMatrix(model_training, train_data$label)$byClass['F1']

# Calculate accuracy and f1_score for testing set
test_accuracy <- confusionMatrix(model_testing, test_data$label)$overall['Accuracy']
test_f1_score <- confusionMatrix(model_testing, test_data$label)$byClass['F1']

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
  scale_fill_manual(values = c('blue', 'orange')) +
  ggtitle('Model Performance Metrics') +
  theme_minimal()
library(pROC)
library(pROC)

# Fit the model on the full dataset (not just the training set)
model_full <- ksvm(label~., data = data_sampled,
                   kernel = "poly",
                   degree = 1:3,
                   scale = c(0.1, 1, 10),
                   C = 2^(-2:2),
                   prob.model = TRUE, # enable probability estimates
                   na.action = na.omit,
                   preProcess = c("scale", "center")
)

# Obtain predicted probabilities for training and testing sets
model_training_probs <- predict(model_full, train_data, type = "prob")[, "malicious"]
model_testing_probs <- predict(model_full, test_data, type = "prob")[, "malicious"]

# Plot the ROC curve
roc_train <- roc(train_data$label, model_training_probs)
roc_test <- roc(test_data$label, model_testing_probs)

plot(roc_train, col = "blue", main = "ROC Curve", sub = "Training Set", print.auc = TRUE)
lines(roc_test, col = "red", print.auc = TRUE)
legend("bottomright", legend = c("Training Set", "Testing Set"),
       col = c("blue", "red"), lwd = 2)
