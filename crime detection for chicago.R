library(tidyverse)
library(caret)
library(readr)
library(kernlab)
# Set up a Shiny dashboard for real-time monitoring and response
library(shiny)
library(shinydashboard)

crime_data <- read_csv("C:/Users/user/Downloads/archive (5)/crime_data_w_population_and_crime_rate.csv")
view(crime_data)
# Sample 3000 rows from the dataset
set.seed(123)
data_sampled <- crime_data %>%
  sample_n(3000, replace = TRUE) %>%
  mutate(county_name = as.factor(county_name))

# Remove variables with zero variance
nzv <- nearZeroVar(data_sampled)
data_sampled <- data_sampled[, -nzv]

# Convert the 'crime_rate_per_100000' variable to a factor with levels '0' and '1'
data_sampled$crime_rate_per_100000 <- factor(ifelse(data_sampled$crime_rate_per_100000 > 0, 1, 0), levels = c(0, 1),labels=c( 'non criminal activity','criminal activity'))

# Split the data into training and testing datasets
trainIndex <- createDataPartition(data_sampled$crime_rate_per_100000, p = 0.7, list = FALSE, times = 1)
train <- data_sampled[trainIndex, ]
test <- data_sampled[-trainIndex, ]

# Train the model
model <- train(crime_rate_per_100000 ~ .,
               data = train,
               method = "svmPoly",
               preProcess = c("scale", "center"),
               trControl = trainControl(method = "none"))


# Make predictions using the model
model_training <- predict(model, train) # making predictions for training set
model_testing <- predict(model, test) # making predictions for testing set
# Model performance (display confusion matrix and stats)
model_training_confusion <- confusionMatrix(model_training, train$crime_rate_per_100000)
model_testing_confusion <- confusionMatrix(model_testing, test$crime_rate_per_100000)
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
train_accuracy <- confusionMatrix(model_training, train$crime_rate_per_100000)$overall['Accuracy']
train_f1_score <- confusionMatrix(model_training, train$crime_rate_per_100000)$byClass['F1']
# Calculate accuracy and f1_score for testing set
test_accuracy <- confusionMatrix(model_testing, test$crime_rate_per_100000)$overall['Accuracy']
test_f1_score <- confusionMatrix(model_testing, test$crime_rate_per_100000)$byClass['F1']
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
library(pROC)

# Fit the model on the full dataset (not just the training set)
model_full <- ksvm(crime_rate_per_100000~., data = data_sampled,
                   kernel = "poly",
                   degree = 1:3,
                   scale = c(0.1, 1, 10),
                   C = 2^(-2:2),
                   prob.model = TRUE, # enable probability estimates
                   na.action = na.omit,
                   preProcess = c("scale", "center")
)
# Obtain predicted probabilities for training and testing sets
model_training_probs <- predict(model_full, train, type = "prob")[, "criminal activity"]
model_testing_probs <- predict(model_full, test, type = "prob")[, "criminal activity"]

# Plot the ROC curve
roc_train <- roc(train$crime_rate_per_100000, model_training_probs)
roc_test <- roc(test$crime_rate_per_100000, model_testing_probs)

plot(roc_train, col = "blue", main = "ROC Curve", sub = "Training Set", print.auc = TRUE)
lines(roc_test, col = "red", print.auc = TRUE)
legend("bottomright", legend = c("Training Set", "Testing Set"),
       col = c("blue", "red"), lwd = 2)

