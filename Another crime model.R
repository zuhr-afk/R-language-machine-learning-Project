library(tidyverse)
library(caret)
library(readr)
library(kernlab)
library(cluster)
library(corrplot)

# Load the Nigerian crime dataset from Kaggle
data_sample<-read_excel("~/new crime.xlsx")
View(new_crime)
# Check the structure of the dataset
str(crime_data)
set.seed(123)
data_sample <- crime_data %>%
  mutate(...2= as.factor(...2))
# Remove variables with zero variance
nz <- nearZeroVar(data_sampled)
data_sample<- data_sample[, -nz]

# Convert the 'crime_rate_per_100000' variable to a factor with levels '0' and '1'
data_sample$`OFFENCES AGAINST PROPERTY`<- factor(ifelse(data_sample$`OFFENCES AGAINST PROPERTY` >0, 1, 0), levels = c(0, 1),labels=c( 'non criminal activity','criminal activity'))
# Split the data into training and testing datasets
trainIndexe <- createDataPartition(data_sample$`OFFENCES AGAINST PROPERTY`, p = 0.7, list = FALSE, times = 1)
train_ <- data_sample[trainIndexe, ]
test_ <- data_sample[-trainIndexe, ]
# Train the model
model <- train(...2 ~ .,
               data = train_,
               method = "svmPoly",
               preProcess = c("scale", "center"),
               trControl = trainControl(method = "none"))

m
