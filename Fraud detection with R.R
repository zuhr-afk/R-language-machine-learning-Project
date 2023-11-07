library(randomForest)
library(readr)
library(caret)
datafraud<- read_csv("C:/Users/turningpointKS/Downloads/archive (6)/fraud detection dataset.csv")
data$isFradulent<-factor(datafraud$isFradulent, levels=c(1,0), labels=c('non-fraudulent', 'fraudulent'))
# Convert relevant columns to numeric
datafraud[, 4:ncol(datafraud)] <- lapply(datafraud[, 4:ncol(datafraud)], as.numeric)

total_crime <- rowSums(datafraud[, 4:ncol(datafraud)], na.rm = TRUE)
# Create a binary outcome variable based on '2005-2006'
outcome <-factor(ifelse(total_crime > 0, 1, 0),
                 levels = c(0, 1),
                 labels = c('non criminal activity', 'criminal activity'))


# Remove variables with low variance
vars_to_remove <- nearZeroVar(datafraud, saveMetrics = TRUE)
data_sampled <- datafraud[, !vars_to_remove$nzv]

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
rf_model <- randomForest(outcome ~ ., data = train, ntree = 2001, importance = TRUE)

# Make predictions using the model
model_training <- predict(rf_model, train) # making predictions for training set
model_testing <- predict(rf_model, test) # making predictions for testing set

# Model performance (display confusion matrix and stats)
model_training_confusion <- confusionMatrix(model_training, train$outcome)
model_testing_confusion <- confusionMatrix(model_testing, test$outcome)
#print result
print(model_training_confusion)
print(model_testing_confusion)

# Hyperparameter Tuning
tune_results <- tuneRF(train[, -ncol(train)], train$outcome, ntreeTry = 4, stepFactor = 1.5)


# Find the best ntree value based on the minimum OOB error
best_ntree <- tune_results$ntreeTry[which.min(tune_results$err.rate[, 1]),]


# Build the final random forest model with the best ntree value
final_rf_model <- randomForest(outcome ~ ., data = train, ntree = tune_r, importance = TRUE)

# Make predictions using the final model
final_model_testing <- predict(final_rf_model, test)

# Model performance for the final model
final_model_testing_confusion <- confusionMatrix(final_model_testing, test$outcome)
print(final_model_testing_confusion)
