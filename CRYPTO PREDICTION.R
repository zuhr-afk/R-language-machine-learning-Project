# Loading required libraries
library(tidyverse)
library(caret)
library(randomForest)
library(rpart)
library(class)
library(Metrics)
# Reading the datasets
#data_1 <- read.csv("C:/Users/turningpointKS/Downloads/archive/dataset.csv")
#head(data_1)
#unique(data_1$new_coins)

dataset2 <- read.csv("C:/Users/turningpointKS/Downloads/archive/dataset.csv")
head(dataset2)
dataset2 <- dataset2[, -1] # Removing the first column
unique(dataset2$crypto_name)
view(dataset2)

# Sampling the dataframe for the main coins
selected_coins <- c("Bitcoin", "Dogecoin", "XRP", "Tether")
new_data <- dataset2[dataset2$crypto_name %in% selected_coins, ]
head(new_data)

# Checking for missing values
colSums(is.na(new_data))

# Visualizing data for each coin
# Bitcoin
btc <- subset(new_data, crypto_name == "Bitcoin")
btc$date <- as.Date(btc$date)
head(btc)

# Visualizing BTC close by year
plot(btc$date, btc$close, type = "l", xlab = "Year", ylab = "Close market price", main = "BTC close by year")

library(ggplot2)

btc$year <- as.numeric(format(btc$date, "%Y"))
btc$date <- as.Date(btc$date, format = "%d-%b-%Y")


# Plotting with ggplot2
ggplot(btc, aes(x = date, y = close)) +
  geom_line(color = "blue") +
  labs(x = "Year", y = "Close market price", title = "BTC close by year") +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  theme_minimal()

# Dogecoin
doge <- subset(new_data, crypto_name == "Dogecoin")
doge$date <- as.Date(doge$date)
head(doge)

# Visualizing Dogecoin close by year
plot(doge$date, doge$close, type = "l", xlab = "Year", ylab = "Close market price", main = "Dogecoin close by year")

# XRP
xrp <- subset(new_data, crypto_name == "XRP")
xrp$date <- as.Date(xrp$date)
head(xrp)

# Visualizing XRP close by year
plot(xrp$date, xrp$close, type = "l", xlab = "Year", ylab = "Close market price", main = "XRP close by year")

# Tether
usdt <- subset(new_data, crypto_name == "Tether")
usdt$date <- as.Date(usdt$date)
head(usdt)

# Visualizing Tether close by year
plot(usdt$date, usdt$close, type = "l", xlab = "Year", ylab = "Close market price", main = "USDT close by year")

# Distribution of data for each coin
# Select only numeric columns
numeric_data <- btc[, sapply(btc, is.numeric)]

# Plot histograms for each numeric column
par(mfrow=c(3,3)) # Adjust the layout if needed
for (col in colnames(numeric_data)) {
  hist(numeric_data[[col]], main = col)
}
# For Tether (usdt)
# Select only numeric columns
numeric_data_usdt <- usdt[, sapply(usdt, is.numeric)]

# Plot histograms for each numeric column
par(mfrow=c(3,3)) # Adjust the layout if needed
for (col in colnames(numeric_data_usdt)) {
  hist(numeric_data_usdt[[col]], main = col)
}

# For Dogecoin (doge)
# Select only numeric columns
numeric_data_doge <- doge[, sapply(doge, is.numeric)]

# Plot histograms for each numeric column
par(mfrow=c(3,3)) # Adjust the layout if needed
for (col in colnames(numeric_data_doge)) {
  hist(numeric_data_doge[[col]], main = col)
}

# For XRP (xrp)
# Select only numeric columns
numeric_data_xrp <- xrp[, sapply(xrp, is.numeric)]

# Plot histograms for each numeric column
par(mfrow=c(3,3)) # Adjust the layout if needed
for (col in colnames(numeric_data_xrp)) {
  hist(numeric_data_xrp[[col]], main = col)
}


# Correlation matrix for the four coins
# Select only numeric columns in btc
numeric_btc <- btc[, sapply(btc, is.numeric)]
correlation_1 <- cor(numeric_btc)
# Select only numeric columns in usdt
numeric_usdt <- usdt[, sapply(usdt, is.numeric)]
# Compute correlation matrix
correlation_usdt <- cor(numeric_usdt)
# Select only numeric columns in doge
numeric_doge <- doge[, sapply(doge, is.numeric)]

# Compute correlation matrix
correlation_doge <- cor(numeric_doge)
# Select only numeric columns in xrp
numeric_xrp <- xrp[, sapply(xrp, is.numeric)]

# Compute correlation matrix
correlation_xrp <- cor(numeric_xrp)

# Plot heatmap for BTC
heatmap(correlation_1, 
        main = "Correlation Heatmap - BTC",
        xlab = "Variables",
        ylab = "Variables")

# Plot heatmap for USDT
heatmap(correlation_usdt, 
        main = "Correlation Heatmap - USDT",
        xlab = "Variables",
        ylab = "Variables")
# Plot heatmap for DOGE
heatmap(correlation_doge, 
        main = "Correlation Heatmap - DOGE",
        xlab = "Variables",
        ylab = "Variables")
# Plot heatmap for XRP
heatmap(correlation_xrp, 
        main = "Correlation Heatmap - XRP",
        xlab = "Variables",
        ylab = "Variables")


# Data splitting and normalization
# Processing each of the coins for model building
# BTC
btc <- btc[, -c(1, 9)]
# Assuming you have a CSV file, adjust the file path accordingly
btc <- read.csv("C:/Users/turningpointKS/Downloads/archive/dataset.csv")

# Check the structure of btc
str(btc)

# Assuming marketCap contains the names of cryptocurrencies
selected_coins <- c("Bitcoin", "Dogecoin", "XRP", "Tether")

# Add the crypto_name column based on selected_coins
btc$crypto_name <- ifelse(btc$marketCap %in% selected_coins, btc$marketCap, NA)

# Convert crypto_name to numeric if it's a factor
if (is.factor(btc$crypto_name)) {
  btc$crypto_name <- as.numeric(as.factor(btc$crypto_name))
}


# Dogecoin
doge <- doge[, -c(1, 9)]
doge$crypto_name <- as.numeric(doge$crypto_name)

# XRP
xrp <- xrp[, -c(1, 9)]
xrp$crypto_name <- as.numeric(xrp$crypto_name)

# Tether
usdt <- usdt[, -c(1, 9)]
usdt$crypto_name <- as.numeric(usdt$crypto_name)

# FILTER COINS
coins <- c("Bitcoin", "Dogecoin", "XRP", "Tether") 
new_data <- filter(dataset2, crypto_name %in% coins)

# BTC
btc <- filter(new_data, crypto_name == "Bitcoin") %>%
  select(-timestamp) %>%
  mutate(date = as.Date(date))

# DOGE
doge <- filter(new_data, crypto_name == "Dogecoin") %>%
  select(-timestamp) %>% 
  mutate(date = as.Date(date))

# XRP  
xrp <- filter(new_data, crypto_name == "XRP") %>%
  select(-timestamp) %>%
  mutate(date = as.Date(date))

# USDT
usdt <- filter(new_data, crypto_name == "Tether") %>%
  select(-timestamp) %>%
  mutate(date = as.Date(date))

# ENCODE CRYPTO NAME
btc$crypto_name <- as.factor(btc$crypto_name) 
doge$crypto_name <- as.factor(doge$crypto_name)
xrp$crypto_name <- as.factor(xrp$crypto_name)
usdt$crypto_name <- as.factor(usdt$crypto_name)

# SPLIT DATA
set.seed(123)

btc_train <- sample(1:nrow(btc), 0.7*nrow(btc))
btc_test <- setdiff(1:nrow(btc), btc_train)

doge_train <- sample(1:nrow(doge), 0.7*nrow(doge))
doge_test <- setdiff(1:nrow(doge), doge_train)

xrp_train <- sample(1:nrow(xrp), 0.7*nrow(xrp))
xrp_test <- setdiff(1:nrow(xrp), xrp_train)

usdt_train <- sample(1:nrow(usdt), 0.7*nrow(usdt)) 
usdt_test <- setdiff(1:nrow(usdt), usdt_train)

# doge MODELS
X_train <- doge[doge_train, -4]
y_train <- doge[doge_train, 4]
X_test <- doge[doge_train, -4]
y_test <- doge[doge_train, 4]

# Rabdom forest:
rf_model <- randomForest(y_train ~ ., data = X_train, ntree = 300)
rf_pred <- predict(rf_model, X_test)
print("Random Forest Metrics for doge:")
print(mae(y_test, rf_pred))
print(mse(y_test, rf_pred))
print(R2(y_test, rf_pred))

# Linear Regression Model
# Check for factors with only one level
summary(X_train)

# Remove factors with only one level or convert them to numeric if appropriate
X_train <- X_train[, sapply(X_train, function(x) length(unique(x)) > 1)]

lm_model <- lm(y_train ~ ., data = X_train)
lm_pred <- predict(lm_model, X_test)
print("Linear Regression Metrics for doge:")
print(mae(y_test, lm_pred))
print(mse(y_test, lm_pred))
print(R2(y_test, lm_pred))

# Decision Tree
tree_model <- rpart(y_train ~ ., data = X_train)
tree_pred <- predict(tree_model, X_test)
print(mae(y_test, tree_pred))
print(mse(y_test, tree_pred))
print(R2(y_test, tree_pred))

##KNN 
# Split data into train/test
set.seed(123)
#train_index <- createDataPartition(doge$close, p=0.7, list=FALSE)
#train <- data[train_index, ] 
#test <- data[-train_index, ]
normalize<- function(x){
  return((x-min(x))/ (max(x)-min(x)))
}

# Ensure that all columns contain numeric data
dataset2[, 2:8] <- lapply(dataset2[, 2:8], as.numeric)

# Check for non-numeric columns
non_numeric_columns <- sapply(dataset2[, 2:8], function(x) any(!is.numeric(x)))

# If there are non-numeric columns, you need to handle them appropriately
if (any(non_numeric_columns)) {
  print("There are non-numeric columns in the dataset.")
  # Handle non-numeric columns based on your data characteristics
} else {
  # Normalize numeric columns
  normalize <- function(x) {
    return((x - min(x)) / (max(x) - min(x)))
  }
  
  datasub <- as.data.frame(lapply(dataset2[, 2:8], normalize))
}
# Combine predictors and response variable into one data frame
train_data <- cbind(X_train, y_train)

# Define the column names for the predictors and the response variable
colnames(train_data) <- c(colnames(X_train), "close")
train_data <- train_data[, !names(train_data) %in% c("crypto_name")]


# Train the KNN model
knn_model <- train(close ~ ., data = train_data, method = "knn")

# Print the model
print(knn_model)

knn_pred <- predict(knn_model, X_test)
print(mae(y_test, knn_pred))
print(mse(y_test, knn_pred))
print(R2(y_test, knn_pred))


# BTC MODELS
X_train <- btc[btc_train, -4]
y_train <- btc[btc_train, 4]
X_test <- btc[btc_train, -4]
y_test <- btc[btc_train, 4]

# Rabdom forest:
rf_model <- randomForest(y_train ~ ., data = X_train, ntree = 300)
rf_pred <- predict(rf_model, X_test)
print("Random Forest Metrics for btc:")
print(mae(y_test, rf_pred))
print(mse(y_test, rf_pred))
print(R2(y_test, rf_pred))

# Linear Regression Model
# Check for factors with only one level
summary(X_train)

# Remove factors with only one level or convert them to numeric if appropriate
X_train <- X_train[, sapply(X_train, function(x) length(unique(x)) > 1)]

lm_model <- lm(y_train ~ ., data = X_train)
lm_pred <- predict(lm_model, X_test)
print("Linear Regression Metrics for doge:")
print(mae(y_test, lm_pred))
print(mse(y_test, lm_pred))
print(R2(y_test, lm_pred))

# Decision Tree
tree_model <- rpart(y_train ~ ., data = X_train)
tree_pred <- predict(tree_model, X_test)
print(mae(y_test, tree_pred))
print(mse(y_test, tree_pred))
print(R2(y_test, tree_pred))

##KNN 

normalize<- function(x){
  return((x-min(x))/ (max(x)-min(x)))
}

# Ensure that all columns contain numeric data
dataset2[, 2:8] <- lapply(dataset2[, 2:8], as.numeric)

# Check for non-numeric columns
non_numeric_columns <- sapply(dataset2[, 2:8], function(x) any(!is.numeric(x)))

# If there are non-numeric columns, you need to handle them appropriately
if (any(non_numeric_columns)) {
  print("There are non-numeric columns in the dataset.")
  # Handle non-numeric columns based on your data characteristics
} else {
  # Normalize numeric columns
  normalize <- function(x) {
    return((x - min(x)) / (max(x) - min(x)))
  }
  
  datasub <- as.data.frame(lapply(dataset2[, 2:8], normalize))
}
# Combine predictors and response variable into one data frame
train_data <- cbind(X_train, y_train)

# Define the column names for the predictors and the response variable
colnames(train_data) <- c(colnames(X_train), "close")
train_data <- train_data[, !names(train_data) %in% c("crypto_name")]


# Train the KNN model
knn_model <- train(close ~ ., data = train_data, method = "knn")

# Print the model
print(knn_model)

knn_pred <- predict(knn_model, X_test)
print(mae(y_test, knn_pred))
print(mse(y_test, knn_pred))
print(R2(y_test, knn_pred))


## USDT 

# usdt MODELS
X_train <- usdt[usdt_train, -4]
y_train <- usdt[usdt_train, 4]
X_test <- usdt[usdt_train, -4]
y_test <- usdt[usdt_train, 4]

# Rabdom forest:
rf_model <- randomForest(y_train ~ ., data = X_train, ntree = 300)
rf_pred <- predict(rf_model, X_test)
print("Random Forest Metrics for usdt:")
print(mae(y_test, rf_pred))
print(mse(y_test, rf_pred))
print(R2(y_test, rf_pred))

# Linear Regression Model
# Check for factors with only one level
summary(X_train)

# Remove factors with only one level or convert them to numeric if appropriate
X_train <- X_train[, sapply(X_train, function(x) length(unique(x)) > 1)]

lm_model <- lm(y_train ~ ., data = X_train)
lm_pred <- predict(lm_model, X_test)
print("Linear Regression Metrics for usdt:")
print(mae(y_test, lm_pred))
print(mse(y_test, lm_pred))
print(R2(y_test, lm_pred))

# Decision Tree
tree_model <- rpart(y_train ~ ., data = X_train)
tree_pred <- predict(tree_model, X_test)
print(mae(y_test, tree_pred))
print(mse(y_test, tree_pred))
print(R2(y_test, tree_pred))


##KNN 

normalize<- function(x){
  return((x-min(x))/ (max(x)-min(x)))
}

# Ensure that all columns contain numeric data
dataset2[, 2:8] <- lapply(dataset2[, 2:8], as.numeric)

# Check for non-numeric columns
non_numeric_columns <- sapply(dataset2[, 2:8], function(x) any(!is.numeric(x)))

# If there are non-numeric columns, you need to handle them appropriately
if (any(non_numeric_columns)) {
  print("There are non-numeric columns in the dataset.")
  # Handle non-numeric columns based on your data characteristics
} else {
  # Normalize numeric columns
  normalize <- function(x) {
    return((x - min(x)) / (max(x) - min(x)))
  }
  
  datasub <- as.data.frame(lapply(dataset2[, 2:8], normalize))
}
# Combine predictors and response variable into one data frame
train_data <- cbind(X_train, y_train)

# Define the column names for the predictors and the response variable
colnames(train_data) <- c(colnames(X_train), "close")
train_data <- train_data[, !names(train_data) %in% c("crypto_name")]


# Train the KNN model
knn_model <- train(close ~ ., data = train_data, method = "knn")

# Print the model
print(knn_model)

knn_pred <- predict(knn_model, X_test)
print(mae(y_test, knn_pred))
print(mse(y_test, knn_pred))
print(R2(y_test, knn_pred))


## xrp
# xrp MODELS
X_train <- xrp[xrp_train, -4]
y_train <- xrp[xrp_train, 4]
X_test <- xrp[xrp_train, -4]
y_test <- xrp[xrp_train, 4]

# Rabdom forest:
rf_model <- randomForest(y_train ~ ., data = X_train, ntree = 300)
rf_pred <- predict(rf_model, X_test)
print("Random Forest Metrics for xrp:")
print(mae(y_test, rf_pred))
print(mse(y_test, rf_pred))
print(R2(y_test, rf_pred))

# Linear Regression Model
# Check for factors with only one level
summary(X_train)

# Remove factors with only one level or convert them to numeric if appropriate
X_train <- X_train[, sapply(X_train, function(x) length(unique(x)) > 1)]

lm_model <- lm(y_train ~ ., data = X_train)
lm_pred <- predict(lm_model, X_test)
print("Linear Regression Metrics for xrp:")
print(mae(y_test, lm_pred))
print(mse(y_test, lm_pred))
print(R2(y_test, lm_pred))

# Decision Tree
tree_model <- rpart(y_train ~ ., data = X_train)
tree_pred <- predict(tree_model, X_test)
print(mae(y_test, tree_pred))
print(mse(y_test, tree_pred))
print(R2(y_test, tree_pred))

##KNN 

normalize<- function(x){
  return((x-min(x))/ (max(x)-min(x)))
}

# Ensure that all columns contain numeric data
dataset2[, 2:8] <- lapply(dataset2[, 2:8], as.numeric)

# Check for non-numeric columns
non_numeric_columns <- sapply(dataset2[, 2:8], function(x) any(!is.numeric(x)))

# If there are non-numeric columns, you need to handle them appropriately
if (any(non_numeric_columns)) {
  print("There are non-numeric columns in the dataset.")
  # Handle non-numeric columns based on your data characteristics
} else {
  # Normalize numeric columns
  normalize <- function(x) {
    return((x - min(x)) / (max(x) - min(x)))
  }
  
  datasub <- as.data.frame(lapply(dataset2[, 2:8], normalize))
}
# Combine predictors and response variable into one data frame
train_data <- cbind(X_train, y_train)

# Define the column names for the predictors and the response variable
colnames(train_data) <- c(colnames(X_train), "close")
train_data <- train_data[, !names(train_data) %in% c("crypto_name")]


# Train the KNN model
knn_model <- train(close ~ ., data = train_data, method = "knn")

# Print the model
print(knn_model)

knn_pred <- predict(knn_model, X_test)
print(mae(y_test, knn_pred))
print(mse(y_test, knn_pred))
print(R2(y_test, knn_pred))

