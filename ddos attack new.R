library(tidyverse)
library(readr)
library(ggplot2)
library (pROC)
library(mltools)
library(rpart)
library(caret)
library(kernlab)
library(e1071)
set.seed(123)
# Load the dataset
data <- read_csv("C:/Users/user/Downloads/data_bal.csv")
data$label <- factor(data$label, levels=c(1,0), labels=c('benign', 'malicious'))

# Visualize the number of benign and malicious rows in dataset
ggplot(data, aes(label, fill=label)) + geom_bar() + ggtitle('Number of benign and malicious rows in dataset')


# Split the data into training and testing sets
set.seed(12)
train_index <- createDataPartition(data$label, p=0.7, list=FALSE)
train_data <- data[train_index,]
test_data <- data[-train_index,]


# Build trainig model
c<- c(0.1,1,10,100)
degree<- c(1,2,3)
scale<-1
sigma<-c(0.0001,0.001,0.01,0.1,1)
gr.poly<-expand.grid(c=c,degree=degree,scale=scale)
model<-train(label~.,data = train_data,
             method="svmPoly",
             na.action = na.omit,
             preProcess= c("scale","center"),
             trcontrol=trainControl(method ="none"),
             tuneGrid=gr.poly

)
#build cv model
c<- c(0.1,1,10,100)
degree<- c(1,2,3)
scale<-1
sigma<-c(0.0001,0.001,0.01,0.1,1)
gr.poly<-expand.grid(c=c,degree=degree,scale=scale)
model.cv<-train(label~.,data = train_data,
                method="svmPoly",
                na.action = na.omit,
                preProcess= c("scale","center"),
                trcontrol=trainControl(method ="cv",number = 10),
                tuneGrid=gr.poly)
                model.cv
    # test model kernel
        svm_model<- svm(label~., data=train_data, type="C-classification", cost=1,kernel="linear",scale=FALSE)
        svm_model

#APPLY for prediction
model.trainig<- predict(model, train_data) # making predictions for trainig set
model.testing<- predict(model,test_data)# making predictions for testing set
model.cv<- predict(model.cv, test_data)

# model performance(display confusion matrix and stat)
model.trainig.confusion<-confusionMatrix(model.trainig, train_data$label)
model.testing.comfusion<-confusionMatrix(model.testing,test_data$label)
model.cv.confusion<-confusionMatrix(model.cv, train_data$label)
#print result
print(model.trainig.confusion)
print(model.testing.comfusion)
print(model.cv.confusion)

