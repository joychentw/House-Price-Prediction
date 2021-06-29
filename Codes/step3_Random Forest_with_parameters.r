# work by Ellie

library(randomForest)
library('tidyverse')
#library(ggplot2)
#library(purrr)
library(stringr)
#install.packages('tibble')
#library(tibble)
library(forcats)
library(caret)
library(Metrics)

#### reading shortlisted attributes 
train_raw <- read.csv("train_bck_Selected.csv")
test_raw <- read.csv("test_bck_Selected.csv")

data <- rbind(train_raw,test_raw)

set.seed(1234)
test_num <- sample(1:nrow(data), floor(nrow(data)/2))
train<- data[-test_num,]
test<- data[test_num,]

rf_model <- randomForest(SalePrice ~ . ,
                         data = train,
                         keep.forest=TRUE,
                         importance=TRUE,
                         mtry = 6,
                         ntree = 9)

rf_model
varImp(rf_model)
importance(rf_model)
varImpPlot(rf_model)

y_hat <- predict(rf_model, train)

TRAIN.rf_scored <- as_tibble(cbind(train, y_hat))
#glimpse(TRAIN.rf_scored)

#RMSE is 12410.51 for training
library(Metrics)
rmse(train$SalePrice, y_hat)

#on test data
library(randomForest)
y_hat1 <- predict(rf_model, test)

test.rf_scored <- as_tibble(cbind(test, y_hat1))
#glimpse(test.rf_scored)

#rmse is 38866.15
rmse(train$SalePrice, y_hat)
rmse(test$SalePrice, y_hat1)
rmse(test$SalePrice, y_hat1) -rmse(train$SalePrice, y_hat)