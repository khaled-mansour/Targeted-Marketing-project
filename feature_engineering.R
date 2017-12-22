# WOE Replacing/Random Forest Variable importanc/ Wrapper / Partial Dependence plots / XGBOOST VARIABLE IMPORTANCE

###########################################
############ WOE Replacing ################
###########################################


train <- comdata[c(1:51884),]
test <- comdata[-c(1:51884),]
if(!require("klaR")) install.packages("klaR"); library("klaR")
if(!require("caret")) install.packages("caret"); library("caret")
str(train$return_customer)
train$return_customer<-as.factor(train$return_customer)
str(train$email_domain)
train$email_domain<-as.factor(train$email_domain)
tapply(train$email_domain, train$return_customer, summary)
str(train$postcode_invoice)
train$postcode_invoice<-as.factor(train$postcode_invoice)
tapply(train$postcode_invoice, train$return_customer, summary)

woe.object <- woe(return_customer ~ email_domain + postcode_invoice, data = train, zeroadj = 0.5)
woe.object$woe

summary(woe.object$xnew)

### changed to 
train[, c("email_domain", "postcode_invoice")]<- woe.object$xnew

## predict and insert in TEST
test.woe <- predict(woe.object, newdata = test, replace = TRUE)
summary(test.woe)
test[,c("email_domain")] <- test.woe$woe.email_domain
test[,c("postcode_invoice")] <- test.woe$woe.postcode_invoice
str(test)
## Information Value
# A rule of thumb : <0.02 -> not predictive , 0.02 - 0.1 -> weak, 0.1-0.3 -> Medium, >0.3 -> Strong
woe.object1 <- woe(x = train[, !(colnames(train) %in% "return_customer") & sapply(train, is.factor)], # select non-target factor vars
                   grouping = train$return_customer, zeroadj = 0.5)
woe.object1$IV

###########################################
## 4. Random Forest Variable importance####
###########################################

#### IMPORTANT
train$return_customer <- factor(train$return_customer, labels = c("non.returning.customer", "returning.customer"))
levels(train$return_customer)
make.names(levels(train$return_customer))

#### spliting train data 60% and 40%
idx.train <- createDataPartition(y = train$return_customer, p = 0.6, list = FALSE) 
train08 <- train[idx.train, ] 
test02 <-  train[-idx.train, ]

if(!require("randomForest")) install.packages("randomForest"); library("randomForest")
rf <- randomForest(return_customer~., data = train08, method = "rf", ntree = 500, mtry = 6, importance = TRUE)
rf.caret <- train(return_customer~., data = train08, method = 'rf', trControl = trainControl(method = 'none'), importance = T, ntree = 500, tuneGrid = expand.grid(mtry = 6))
importance(rf, scale = FALSE)

varImpPlot(rf, scale = TRUE)
varImp(rf.caret)

varImp(rf.caret, scale = FALSE)
rowMeans(importance(rf.caret$finalModel))

###########################################
#### 4. Wrapper ####
###########################################

if(!require("caret")) install.packages("caret"); library("caret") 
if(!require("pROC")) install.packages("pROC"); library("pROC") 
# Start by calculating variable importance values
rf <- randomForest(return_customer~., data = train08,  
                   method = "rf", ntree = 500, mtry = 6, importance = TRUE)
# Sort the combined rank values in decreasing order
vrank <- importance(rf)[,"MeanDecreaseAccuracy"]
vrank.sorted <- sort(vrank, decreasing = TRUE)
#a vector to store the accuracy statistics
n <- length(vrank.sorted)
perf.trace = vector(length = n)
## Helper function to assess a candidate model
CalcCandAUC <- function(data, newdata) {
  # Build model 
  #A small RF is used
  rf <- randomForest(return_customer~., data = data, ntree = 150)
  # Assess model
  yhat <- predict(rf, newdata = newdata, type = "prob")
  # Recall that predict gives class probability estimates for 
  # both classes by default.
  # Thus, we restrict attention to the second column in yhat
  yhat <- yhat[, 2]
  # Calculate AUC based on the internal validation set
  auc <- pROC::auc(response = newdata$return_customer, predictor = yhat)
  return(auc)
}

# Create a validation set for the selection process
idx.val <- createDataPartition(y = train08$return_customer, p = 0.3, list = FALSE) # Draw a random, stratified sample including p percent of the data
selection.val <-  train08[idx.val, ] # test set
selection.train <- train08[-idx.val, ] # training set
# Stage-wise model building
for (i in 1:n) {
  # Create temporary data.frame including only the response variable, 
  # return_customer, and the first i variables sorted by importance
  df.tmp <- selection.train[,c("return_customer", names(vrank.sorted[1:i]))]
  # Estimate performance of a classification model that uses only the 
  # best i variables
  perf.trace[i] <- CalcCandAUC(df.tmp, selection.val)
  vars <- colnames(df.tmp)
}
# Simple plot of the performance development
plot(perf.trace, type = "l", xlab = "Iteration")
# Find the best performance
idx.opt <- which.max(perf.trace) 
# Variables tested in the idx'th iteration
var.selected <- names(vrank.sorted[1:idx.opt])
var.selected



###########################################
## Partial Dependence plots##
###########################################

partialPlot(rf, pred.data = train, x.var = "remitted_items")
# Marginal effect of the "first" class. let's check confusion matrix on rf to see which one is first class for customer_return
rf

## in this case 0 is the first class or in other words "non-returning" customer
partialPlot(rf, pred.data = train, which.class = 'non.returning.customer', x.var = "used_items")
partialPlot(rf, pred.data = train, x.var = "total_item_count")
### Create partial dependence plots for the most important variables
important.variables_part1 <- c('goods_value','coupon','actual_delivery_duration', 'time_interval_for_first_order')  
# Create a new plot device with a 4x4 structure
par(mfrow = c(2, 2), mar = c(4,3,0,0))
# Set up empty result list
rf.partialPlots <- list()
# For each of the variables, plot the partial dependence plot and also save the resulting object for further use
for (var in important.variables_part1) {
  message("Now calculating for variable ", var)
  # We use do.call() to pass the arguments to function partialPlot(). This is a recommended way to construct
  # function calls based on variables
  rf.partialPlots[[var]] <- do.call(partialPlot, list(rf, pred.data = train08, x.var = var, which.class = 'non.returning.customer', plot = TRUE, main=paste("Partial Dependence on", var)))
}
important.variables_part2 <- c('cost_shipping', 'allbook_count', 'model', 'newsletter')
# Create a new plot device with a 4x4 structure
par(mfrow = c(2, 2), mar = c(4,3,0,0))
# Set up empty result list
rf.partialPlots1 <- list()
for (var in important.variables_part2) {
  message("Now calculating for variable ", var)
  # We use do.call() to pass the arguments to function partialPlot(). This is a recommended way to construct
  # function calls based on variables
  rf.partialPlots1[[var]] <- do.call(partialPlot, list(rf, pred.data = train08, x.var = var, which.class = 'non.returning.customer', plot = TRUE, main=paste("Partial Dependence on", var)))
}
not.important.vars <- c('giftwrapping', 'hardware_count', 'title')
# Create a new plot device with a 4x4 structure
par(mfrow = c(2, 2), mar = c(4,3,0,0))
# Set up empty result list
rf.partialPlots2 <- list()
for (var in not.important.vars) {
  message("Now calculating for variable ", var)
  # We use do.call() to pass the arguments to function partialPlot(). This is a recommended way to construct
  # function calls based on variables
  rf.partialPlots2[[var]] <- do.call(partialPlot, list(rf, pred.data = train08, x.var = var, which.class = 'non.returning.customer', plot = TRUE, main=paste("Partial Dependence on not important vars", var)))
}

# Plot the partial dependence plots for the random forest model
## The standard dependence plots shows log probabilities on the y axis 
# We transform these (using the e function) to regular probabilities for better interpretation
for(var in names(rf.partialPlots1)){
  plot(rf.partialPlots1[[var]]$x, exp(rf.partialPlots1[[var]]$y), type = "l", xlab = var, ylab = 'Pred. prob. non return customer' )
}
# We transform these (using the e function) to regular probabilities for better interpretation
for(var in names(rf.partialPlots)){
  plot(rf.partialPlots[[var]]$x, exp(rf.partialPlots[[var]]$y), type = "l", xlab = var, ylab = 'Pred. prob. non return customer' )
}





###########################################
##XGBOOST VARIABLE IMPORTANCE##
###########################################

if(!require("xgboost")) install.packages("xgboost"); library("xgboost")
xgb <- train(return_customer~., data = train, method = 'xgbTree', trControl = trainControl(method = 'none'), importance = TRUE, tuneGrid = expand.grid(nrounds = 20, max_depth = 2, eta = 0.05, gamma = 0, colsample_bytree = 0.8, min_child_weight = 1, subsample = 0.8))
xgb.importance(model = xgb$finalModel, feature_names = xgb$finalModel$xNames)
varImp(xgb, scale = FALSE)
plot(varImp(rf.caret))
plot(varImp(xgb))
if(!require("pdp")) install.packages("pdp"); library("pdp") # load the package
# Set up empty result list
xgb.partialPlots <- list()
# For each of the variables, calculate the partial dependence object for further use
for (var in important.variables_part1) {
  message("Now calculating for variable ", var)
  # We use do.call() to pass the arguments to function partial(). This is a recommended way to construct
  # function calls based on values saved in variables
  xgb.partialPlots[[var]] <- do.call(partial, list(xgb, pred.var = var, which.class = 2, type = "classification", 
                                                   plot = FALSE, main=paste("PDP for xgb on", var)))
}

xgb.partialPlots2 <- list()
for (var in important.variables_part2) {
  message("Now calculating for variable ", var)
  # We use do.call() to pass the arguments to function partial(). This is a recommended way to construct
  # function calls based on values saved in variables
  xgb.partialPlots2[[var]] <- do.call(partial, list(xgb, pred.var = var, which.class = 2, type = "classification", 
                                                   plot = FALSE, main=paste("PDP for xgb on", var)))
}

xgb.partialPlots3 <- list()
for (var in var.selected) {
  message("Now calculating for variable ", var)
  # We use do.call() to pass the arguments to function partial(). This is a recommended way to construct
  # function calls based on values saved in variables
  xgb.partialPlots3[[var]] <- do.call(partial, list(xgb, pred.var = var, which.class = 2, type = "classification", 
                                                    plot = FALSE, main=paste("PDP for xgb on", var)))
}

##Plot the partial dependence plots for both models side-by-side to compare them
par(mfrow=c(4, 2))
for(var in names(xgb.partialPlots)){
  plot(x = xgb.partialPlots[[var]][,1], y = exp(xgb.partialPlots[[var]][,2]), type = "l", xlab = paste(var, " - xgboost"), ylab = 'Pred. prob. returning customer', ylim = c(0.4, 1) )
  plot(rf.partialPlots[[var]]$x, exp(rf.partialPlots[[var]]$y), type = "l", xlab = paste(var, " - randomForest"), ylab = 'Pred. prob. returning customer', ylim = c(0.4, 1) )
}
###########################################
##XGBOOST VARIABLE IMPORTANCE V2        ##
###########################################
#### Load all the libraries
install.packages("Matrix")
train.xgb <- train08
if (!require('vcd')) install.packages('vcd')
install.packages("data.table")
library(data.table)
library(vcd)
###One-hot encoding. Next step, we will transform the categorical data to dummy variables.
#The purpose is to transform each value of each categorical feature in a binary feature {0, 1}.
sparse_matric <- Matrix::sparse.model.matrix(return_customer~.-1, data = train.xgb)
head(sparse_matric)
output_vector = train.xgb[,"return_customer"] == "returning.customer"
library(xgboost)
### Build the Model
bst <- xgboost(data = sparse_matric, label = output_vector, max.depth = 4, eta = 1, nthread = 2, nround = 2000,objective = "binary:logistic")
##Build the feature importance data.table
importance <- xgb.importance(feature_names = sparse_matric@Dimnames[[2]], model = bst)
head(importance)
#Improvement in the interpretability of feature importance data.table
importanceRaw <- xgb.importance(feature_names = sparse_matric@Dimnames[[2]], model = bst, data = sparse_matric, label = output_vector)
# Cleaning for better display
importanceClean <- importanceRaw[,`:=`(Cover=NULL, Frequency=NULL)]
head(importanceClean)
#Plotting the feature importance
xgb.plot.importance(importance_matrix = importanceRaw)
model <- xgb.dump(bst, with.stats = T)
model[1:10]
names <- dimnames(sparse_matric)[[2]]
importance_matrix <- xgb.importance(names, model = bst)
xgb.plot.importance(importance_matrix[1:40,])
# checking if the results make sens with Chi2 test
test <- chisq.test(train.xgb$return_customer, output_vector)
print(test)
###########################################
###Result
###########################################

# we will delete giftwrapping, hardware_count, title
train$giftwrapping <- NULL
test$giftwrapping <- NULL
train$title <- NULL
test$title <- NULL
train$hardware_count <- NULL
test$hardware_count <- NULL
train$ID <- as.numeric(train$ID)
test$ID <- as.numeric(test$ID)

train$return_customer <- factor(train$return_customer, labels = c("non.returning.customer", "returning.customer"))
levels(train$return_customer)
make.names(levels(train$return_customer))

#### spliting train data 60% and 40%
idx.train <- createDataPartition(y = train$return_customer, p = 0.6, list = FALSE) 
train08 <- train[idx.train, ] 
test02 <-  train[-idx.train, ]
