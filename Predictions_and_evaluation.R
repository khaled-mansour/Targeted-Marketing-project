##################################################################################################################################
############### Predictions and evaluation #######################################################################################
##################################################################################################################################

##########################################
##### Final Predictions ##################
##########################################

if(!require("caret")) install.packages("caret"); library("caret") # load the package
if(!require("randomForest")) install.packages("randomForest"); library("randomForest") # load the package
if(!require("xgboost")) install.packages("xgboost"); library("xgboost") # load the package
if(!require("pROC")) install.packages("pROC"); library("pROC") # load the package
library(plyr); library(dplyr)
idx.train <- createDataPartition(y = train$return_customer, p = 0.6, list = FALSE) 
train08 <- train[idx.train, ] # training set
test02 <-  train[-idx.train, ] # testing set
# Setup the options for model selection
model.control<- trainControl(
  method = "cv", # 'cv' for cross validation
  number = 20, # number of folds in cross validation
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  allowParallel = TRUE, # Enable parallelization if available
  returnData = FALSE 
)

xgb.parms <- expand.grid(nrounds = 4000, 
                         max_depth = 15, 
                         eta = 0.0001, 
                         gamma = 10,
                         colsample_bytree = 0.8,
                         min_child_weight = 1,
                         subsample = 0.8)
# Train model
xgb <- train(return_customer~., data = train08,  
             method = "xgbTree",
             tuneGrid = xgb.parms, 
             metric = "ROC", trControl = model.control)

# Make prediction on test02 set
xgb.pred <- predict(xgb, newdata = test02, type = "prob")[,2]
xgb4000.ROC <- roc(predictor=xgb.pred,
                   response=test02$return_customer,
                   levels=rev(levels(test02$return_customer)))
xgb4000.ROC$auc
                               
# Make prediction on test set
yhat.xgb <- predict(xgb, newdata = test, type = "prob")[,2]
print(yhat.xgb)

FinalPredictions6_final <- data.frame(Customer_ID = test$ID, 
                               EstimatedreturnProbability = yhat.xgb)

head(FinalPredictions6_final)
write.csv(FinalPredictions6_final,file='/Users/Barischnikow/Desktop/chloe/FinalPredictions6_final.csv')
################################################################################################################
############ Predicted Data preparation for Cost Function and AUC #################################
################################################################################################################


FinalPredictions_test02 <- data.frame(return_customer = test02$return_customer, 
                                EstimatedreturnProbability = xgb.pred)

write.csv(FinalPredictions_test02_ver3,file='/Users/Barischnikow/Desktop/chloe/FinalPredictions_test02_ver3.csv')



####### Cost Function ##########

costs = matrix(c(0, 0, 10, -3), 2)
colnames(costs) = rownames(costs) = c('returning.customer','non.returning.customer')

FinalPredictions_test02$return_customer=as.factor(FinalPredictions_test02$return_customer)
str(FinalPredictions_test02)

th<-costs[2,1] - costs[2,2]/(costs[2,1] - costs[1,1] + costs[1,2] -costs[2,2])

predicted.outcome1 <- factor(FinalPredictions6_final$EstimatedreturnProbability > th, labels = c("non.returning.customer", "returning.customer"))
predicted.outcome1<-as.character(predicted.outcome1)
predicted.outcome1[predicted.outcome1=='returning.customer']<-1
predicted.outcome1[predicted.outcome1=='non.returning.customer']<-0

FinalPredictions1 <- data.frame(ID = class_onlyID_, return_customer=predicted.outcome1)
write.csv(FinalPredictions1,file='~/Desktop/51.csv',row.names = FALSE)
