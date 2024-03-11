# Load libraries
library(e1071)  # for SVM
library(neuralnet)  # for ANN
library(ggplot2)  # for visualization
library(caret)

# Read the comprehensive dataset (replace 'data.csv' with your dataset path)
data <-  read.csv("C:/Users/turningpointKS/Downloads/archive (2)/train_6BJx641.csv")

# Data partitioning
set.seed(123)
train_index <- createDataPartition(data$electricity_consumption, p=0.7, list=FALSE)
train_data <- data[train_index,]
test_data <- data[-train_index,]
# Convert electricity_consumption to numeric
train_data$electricity_consumption <- as.numeric(as.character(train_data$electricity_consumption))
test_data$electricity_consumption <- as.numeric(as.character(test_data$electricity_consumption))
# Convert datetime column to POSIXct format
#train_data$datetime <- as.POSIXct(train_data$datetime, format="%m/%d/%Y %H:%M")



# Support Vector Machine (SVM) model
svm_model <- svm(electricity_consumption ~ temperature + var1 + pressure + windspeed + var2, data=train_data)
# Define the formula for ANN
ann_formula <- as.formula("electricity_consumption ~ temperature + var1 + pressure + windspeed")

# Train the ANN model
ann_model <- neuralnet(ann_formula, data = train_data, hidden = c(5, 3), linear.output = TRUE)


# Predictions
svm_predictions <- predict(svm_model, test_data)
ann_predictions <- predict(ann_model, test_data)


# Combine predictions
hybrid_predictions <- (svm_predictions + ann_predictions) / 2
hybrid_predictions


# Evaluate SVM model
svm_mae <- mean(abs(svm_predictions - test_data$electricity_consumption))
svm_rmse <- sqrt(mean((svm_predictions - test_data$electricity_consumption)^2))

# Evaluate ANN model
ann_mae <- mean(abs(ann_predictions - test_data$electricity_consumption))
ann_rmse <- sqrt(mean((ann_predictions - test_data$electricity_consumption)^2))

# Print evaluation results
print("SVM Model Evaluation:")
print(paste("MAE:", svm_mae))
print(paste("RMSE:", svm_rmse))

print("ANN Model Evaluation:")
print(paste("MAE:", ann_mae))
print(paste("RMSE:", ann_rmse))





# Calculate the mean absolute error for each model
svm_mae <- mean(abs(svm_predictions - test_data$electricity_consumption))
ann_mae <- mean(abs(ann_predictions - test_data$electricity_consumption))
hybrid_mae <- mean(abs(hybrid_predictions - test_data$electricity_consumption))

# Calculate R-squared for SVM
svm_test_rsq <- 1 - sum((svm_predictions - test_data$electricity_consumption)^2) / sum((mean(test_data$electricity_consumption) - test_data$electricity_consumption)^2)


# Calculate R-squared for ANN
ann_test_rsq <- 1 - sum((ann_predictions - test_data$electricity_consumption)^2) / sum((mean(test_data$electricity_consumption) - test_data$electricity_consumption)^2)


# Calculate R-squared for Hybrid

hybrid_test_rsq <- 1 - sum((hybrid_predictions - test_data$electricity_consumption)^2) / sum((mean(test_data$electricity_consumption) - test_data$electricity_consumption)^2)


# Comparative study
comparison_df <- data.frame(
  Method = c("SVM", "ANN", "Hybrid"),
  MAE = c(svm_mae, ann_mae, hybrid_mae),
  Test_R_Squared = c(svm_test_rsq, ann_test_rsq, hybrid_test_rsq)
)

# Print MAE and R-squared values
print("Comparative Study:")
print(comparison_df)

# Create a heatmap for predictions
prediction_df <- data.frame(
  Actual = test_data$electricity_consumption,
  Hybrid_Predicted = hybrid_predictions
)

# Plot heatmap
ggplot(prediction_df, aes(x = Actual, y = Hybrid_Predicted)) +
  geom_bin2d() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(x = "Actual Demand", y = "Hybrid Predicted Demand", title = "Hybrid Model Predictions Heatmap")

# Plot comparative study
ggplot(comparison_df, aes(x = Method, y = MAE, fill = Method)) +
  geom_bar(stat = "identity") +
  labs(x = "Method", y = "Mean Absolute Error", title = "Comparative Study of Methods") +
  theme_minimal()


