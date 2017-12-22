##################################################################################################################################
############### 1. Logistic Regression and Decision Tree #########################################################################
##################################################################################################################################
# Define a function that computes the brier score when given a binary vector of outcomes and a vector of predicted probabilities
BrierScore <- function(y, yhat){
  sum((y - yhat)^2) / length(y) 
}
# Data Shaffling 
if(!require("rpart")) install.packages("caret"); library("rpart")
train.rnd.lr <- train08[sample(nrow(train08)),]
# Create k folds of approximately equal size
k.lr <- 5
folds.lr <- cut(1:nrow(train.rnd.lr), breaks = k.lr, labels = FALSE)
# The variable folds is a vector of integer values from 1 to k
folds.lr 
# We can use this vector in a logical indexing expression to query the
# training and validation data in every cross-validation iteration. 
# Cross-validation loop
# Vector to store results (i.e., performance estimates per CV iteration)
results.lr <- data.frame(lr = numeric(length = k.lr), dt = numeric(length = k.lr))
# recall that we need a cut-off to calculate crisp classifications
for (i in 1:k.lr) {
  # Split data into training and validation
  idx.val.lr <- which(folds.lr == i, arr.ind = TRUE)
  cv.train.lr <- train.rnd.lr[-idx.val.lr,]
  cv.val.lr <- train.rnd.lr[idx.val.lr,]
  # Build and evaluate models using these partitions
  lr <- glm(return_customer~., data = cv.train.lr, family = binomial(link = "logit"))
  dt.prunedMore <- rpart(return_customer~ ., data = cv.train.lr, cp = 0.015) # create decision tree classifier
  yhat.lr <- predict(lr, newdata = cv.val.lr, type = "response")
  yhat.dt <- predict(dt.prunedMore, newdata = cv.val.lr, type = "prob")[,2]
  # We use our above function to calculate the classification error
  results.lr[i, "lr"] <- BrierScore(as.numeric(cv.val.lr$return_customer)-1, yhat.lr)
  results.lr[i, "dt"] <- BrierScore(as.numeric(cv.val.lr$return_customer)-1, yhat.dt)
}
results.lr
summary(lr)
coef(summary(lr))

# Average performance ####
cv.perf <- apply(results.lr, 2, mean)
cv.perf.sd <- apply(results.lr, 2, sd)
# Now plot the results
txt <- paste("Classification brier score across", as.character(k.lr), "folds", sep=" ")
boxplot(results.lr,  ylab="Brier Score", 
        main = txt)
# we now we predict on test data and estimate the expected error
pred.lr <- predict(lr, newdata = test, type = "response")
pred.lr
err.pred.lr <- BrierScore(as.numeric(test02$return_customer)-1,pred.lr)
err.pred.lr
pred.dt.prunedMore <- predict(dt.prunedMore, newdata = test, type = "prob")[,2]
err.pred.dt.prunedMore <- BrierScore(as.numeric(test02$return_customer)-1,pred.dt.prunedMore)
err.pred.dt.prunedMore

TopDecileLift(pred.lr, as.numeric(test02$return_customer)-1)
##################################################################################################################################
############### 2. Naive Bayes ###################################################################################################
##################################################################################################################################
## Load packages
install.packages('e1071')
library(e1071)

# Run Naive Bayes
nb <- naiveBayes(return_customer~., data = train08) 
summary(nb)
yhat.nb <- predict(nb, newdata = test02, type = "raw")
head(yhat.nb)
yhat.nb.result <- yhat.nb[, 1] 
head(yhat.nb.result,100)
## Menn square error with Brier Score
brier.nb <- BrierScore(as.numeric(test02$return_customer)-1, yhat.nb)
## predict on test set
pred.nb <- predict(nb, newdata = test, type = "raw")
print(pred.nb)
TopDecileLift(yhat.nb, as.numeric(test02$return_customer)-1)
##################################################################################################################################
############### 3. Neural Networks ###############################################################################################
##################################################################################################################################
## 1. Neural Networks ####
if(!require("nnet")) install.packages("nnet"); library("nnet") # Try to load rpart, if it doesn't exist, then install and load it
if(!require("pROC")) install.packages("pROC"); library("pROC") # Try to load rpart, if it doesn't exist, then install and load it
if(!require("caret")) install.packages("caret"); library("caret")
if(!require("doParallel")) install.packages("doParallel"); library("doParallel") # load the package
if(!require("microbenchmark")) install.packages("microbenchmark"); library("microbenchmark")
# Splitting the data into a test and a training set 
idx.train <- createDataPartition(y = train$return_customer, p = 0.8, list = FALSE) # Draw a random, stratified sample including p percent of the data
train08 <- train[idx.train, ] # training set
test02 <-  train[-idx.train, ] # test set (drop all observations with train indeces)

# The number of folds and the specific folds for cv
k <- 10
head(train[sample(nrow(train08)),])
train.rnd.nn <- train[sample(nrow(train08)),]
folds <- cut(1:nrow(train.rnd.nn), breaks = k, labels = FALSE)

### Specify the setup:
# The number of nodes to try for the model
nnet.sizes <- seq(from = 3, to = 15, by = 3)
# Initialize the data frame that collects the results
results <- as.data.frame(matrix(NA, ncol = length(nnet.sizes), nrow = k))

# Setup up parallel backend
cl <- makeCluster( max(1,detectCores()-1))
registerDoParallel(cl)
results.par.nn <- foreach(n = 1:length(nnet.sizes), .combine = cbind, .packages = c("caret", "nnet", "pROC")) %:%
  # This is the cross-validation loop from before
  foreach(i = 1:k, .combine = c, .packages = c("caret","nnet", "pROC")) %dopar%{
    # Split data into training and validation
    idx.val.nn <- which(folds == i, arr.ind = TRUE)
    cv.train.nn <- train.rnd.nn[-idx.val.nn,]
    cv.val.nn <- train.rnd.nn[idx.val.nn,]
    # Train the neural network model with a number of nodes n
    neuralnet <- nnet(return_customer~., data = cv.train.nn, trace = FALSE, maxit = 100, MaxNWts= 8000, size = nnet.sizes[n])
    # Build and evaluate models using these partitions
    yhat.nn <- predict(neuralnet, newdata = cv.val.nn, type = "raw")
    # We use our above function to calculate the classification error
    auc(cv.val.nn$return_customer, as.vector(yhat.nn))
  }
stopCluster(cl)
registerDoSEQ()
# Average AUC over k folds for each tuning combination
results.nn.fold <- apply(results.par.nn , MARGIN = 1, mean) # Could also use rowMeans
names(results.nn.fold) <- paste(nnet.sizes)
##
print(results.par.nn$neuralnet)
plot(results.par.nn$neuralnet)
summary(results.par.nn)
if(!require("devtools")) install.packages("devtools"); library("devtools")
source_url("https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r")

if(!require("reshape")) install.packages("reshape"); library("reshape")
plot.nnet(results.par.nn$neuralnet)
## with Caret
model.control<- trainControl(
  method = "cv", # 'cv' for cross validation
  number = 10, # number of folds in cross validation
  repeats = 5, # number for repeated cross validation
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  allowParallel = TRUE, # Enable parallelization if available
  returnData = FALSE # The training data will not be included in the ouput training object
)

# Define a search grid of values to test
nn.parms <- expand.grid(decay = c(0, 10^seq(-4, 0, 1)), size = seq(6,16,2))
#Train neural network nn with 10-fold cv
nn <- caret::train(return_customer~., data = train08,  
                   method = "nnet", maxit = 100, trace = FALSE, MaxNWts= 8000, # options for nnet function
                   tuneGrid = nn.parms, # parameters to be tested
                   metric = "ROC", trControl = model.control)
# Analyze the cross-validation results
print(nn)
plot(nn)
summary(nn)
if(!require("devtools")) install.packages("devtools"); library("devtools")
source_url("https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r")

if(!require("reshape")) install.packages("reshape"); library("reshape")
plot.nnet(nn)
yhat.nn   <- predict(nn, newdata = test02, type = "prob")[,2]
# Obtain ROC curve and calculate AUC 
nn.roc <-roc(test02$return_customer, yhat.nn)
auc(nn.roc)
auc.caret.nn <- auc(nn.roc)
plot.roc(nn.roc)
# We can do the same but create a nice plot with the the Hmeasure package
library(hmeasure)
h <- HMeasure(true.class = as.numeric(test02$return_customer)-1, scores = yhat.nn)
# Note that AUC is the same; as it should be
h$metrics["AUC"]
plotROC(h, which=1)

## compare both of them
### 3. Comparing tuned RF and RF with caret ####

idx.besttuned.nn <- which.max(results.nn.fold)
nn.tuned <- nnet(return_customer~., data = train08, trace = FALSE, maxit = 100, MaxNWts= 1000, size = nnet.sizes[idx.besttuned.nn])
yhat.nn.tuned <- predict(nn.tuned, newdata = test02, type = "prob")[,2]
auc.tuned.nn <- auc(test02$return_customer, as.vector(yhat.nn.tuned))

message("The best tuning parameters are:\n", 
        "For caret:\n",
        "nnetsizes: ", nn$bestTune, " with ", nn$finalModel, "\n",
        "For neural tuning: \n",
        "netsizes: ", nnet.sizes[idx.besttuned.nn])

# Comparing AUC values of random forest tuned only over mtry with caret and tuned over both mtry and ntree
if (auc.caret.nn < auc.tuned.nn){
  sprintf("The tuned neural network  predicts more accurately.")
} else {
  if (auc.caret > auc.tuned) {sprintf("The neural network prediction with caret is more accurate.")}
  else {sprintf("Both model estimation methods achieve the same level of accuracy.")}    
}

##################################################################################################################################
############### 3. Random Forest #################################################################################################
##################################################################################################################################
## 1. Random Forest Model with Caret ####
if(!require("caret")) install.packages("caret"); library("caret") 
if(!require("randomForest")) install.packages("randomForest")

k <- 10
set.seed(123)
model.control <- trainControl(method = "cv", # 'cv' for cross validation
     number = k, # number of folds in cross validation
     classProbs = TRUE, # Return class probabilities
     summaryFunction = twoClassSummary, # twoClassSummary returns AUC
     allowParallel = TRUE # Enable parallelization if available
  )
randomforest.parms <- expand.grid(mtry = 1:10)
randomforest.caret <- train(return_customer~., data = train08, method = "rf", ntree = 500, tuneGrid = randomforest.parms, metric = "ROC", trControl = model.control)
randomforest.caret$results

plot(randomforest.caret)
# Predict the outcomes of the test set with the predict function, 
 # i.e. the probability of someone being a non-returning customer
yhat.randomforest.caret   <- predict(randomforest.caret, newdata = test02, type = "prob")[,2]
#the AUC is computed in order to evaluate our model performance. 
auc.caret <- auc(test02$return_customer, yhat.randomforest.caret) 
## Brier Score
y <- as.numeric(train08$return_customer) - 1 
brier.randomforest.caret <- sum((y - yhat.randomforest.caret)^2) / length(y)

## 2. Random Forest Model without Caret ####
if(!require("doParallel")) install.packages("doParallel"); library("doParallel")
if(!require("parallel")) install.packages("parallel"); library("parallel")
if(!require("foreach")) install.packages("foreach"); library("foreach")
train.random <- sample(nrow(train08))
folds <- cut(1:nrow(train08), breaks = k, labels = FALSE)
### Create a list of vectors that contain the indices of the observation of each training and test fold
folds.trainRowIndices <- lapply(1:k, function(x) train.random[which(folds != x)])
folds.validationRowIndices <- lapply(folds.trainRowIndices, function(x) setdiff(1:nrow(train08), x))
# Define a grid of candidate paramters, one combination per line  
randomf.parGrid <- expand.grid(mtry = 1:10, ntree = seq(400, 1000, 200))
#Set up Parallel backend
cl <- makeCluster( max(1,detectCores()-1))
registerDoParallel(cl)
  #Parallelization with foreach
results <- foreach(i = 1:nrow(randomf.parGrid), .combine = rbind, .packages = c("caret", "randomForest", "pROC")) %:%
    foreach(j = 1:k, .combine = c, .packages = c("caret","randomForest", "pROC")) %dopar%{
      # Splitting data into train and validation
      cv.train <- train08[folds.trainRowIndices[[j]],]
      cv.validation <- train08[folds.validationRowIndices[[j]],]
      # Random forest model with the i-th parameter 
      randomf <- randomForest(return_customer~., data = cv.train, mtry = randomf.parGrid$mtry[i], ntree = randomf.parGrid$ntree[i])
      # Predict the probabilities for the CV validation data
      yhat.randomf <- predict(randomf, newdata = cv.validation, type = "prob")[,2]
      # Calculate and return the AUC value for the current validation fold
      auc <- auc(cv.validation$return_customer, yhat)
      return(auc)
    }
  stopCluster(cl)
  registerDoSEQ()
  
# Average AUC over k folds for each mtry-ntree combination
results.foldAverage <- apply(results , MARGIN = 1, mean) # Could also use rowMeans
names(results.foldAverage) <- paste(randomf.parGrid[,1], randomf.parGrid[,2], sep = "-")

### 3. Comparing tuned RF and RF with caret ####

idx.besttuned <- which.max(results.foldAverage)
randomf.tuned <- randomForest(return_customer~., data = train, mtry = randomf.parGrid$mtry[idx.besttuned], 
                         ntree = randomf.parGrid$ntree[idx.besttuned])
yhat.randomf.tuned <- predict(randomf.tuned, newdata = test, type = "prob")[,2]
auc.tuned <- auc(test$return_customer, as.vector(yhat.randomf.tuned))

message("The best tuning parameters are:\n", 
        "For caret:\n",
        "mtry: ", rf.caret$bestTune, " with ntree: ", rf.caret$finalModel$ntree, "\n",
        "For ntree tuning: \n",
        "mtry: ", randomf.parGrid[idx.besttuned,1], "\nntree: ", randomf.parGrid[idx.besttuned,2])

# Comparing AUC values of random forest tuned only over mtry with caret and tuned over both mtry and ntree
if (auc.caret < auc.tuned){
  sprintf("The tuned random forest predicts more accurately.")
} else {
  if (auc.caret > auc.tuned) {sprintf("The random forest prediction with caret is more accurate.")}
  else {sprintf("Both model estimation methods achieve the same level of accuracy.")}    
}

## Plotting a 3D object to visualize performance of different tune mtry and ntree combinations
if(!require("lattice")) install.packages("lattice"); library("lattice") # load the package
wireframe(results.foldAverage~ mtry * ntree, data = cbind(randomf.parGrid, results.foldAverage), drape = TRUE, pretty = TRUE, screen = list(z= -45, x = -45))

##################################################################################################################################
############### 3. GBM #################################################################################################
##################################################################################################################################

if(!require("party"))install.packages("party")
if(!require("tree")) install.packages("tree")
if(!require("verification")) install.packages("verification")
if(!require("dplyr")) install.packages("dplyr")
if(!require("AUC")) install.packages("AUC")
if(!require("gbm")) install.packages("gbm")
if(!require("ROCR")) install.packages("ROCR")
if(!require("randomForest")) install.packages("randomForest")
if(!require("foreign")) install.packages("foreign")
if(!require("doParallel")) install.packages("doParallel")
                           
library("foreign")
library("randomForest")
library("tree")
library("AUC")
library("party")
library("gbm")
library("dplyr")
library("caret")
library("verification")
library("ROCR")
library("doParallel")
                           
#####
trees <- 1000
                           
return_num_train <- as.numeric(train08$return_customer) - 1
                           
fit = gbm.fit(x = train08[,which(names(train08) != "return_customer")], 
              y = return_num_train, 
              n.trees = trees, verbose=FALSE, shrinkage=0.005, 
              interaction.depth=20, n.minobsinnode=5, distribution="bernoulli")   
                           
prediction = predict(fit, newdata = test02, n.trees = trees ,type="response")
                           
return_num  = as.numeric(test02$return_customer) - 1
auc        = roc.area(return_num, prediction)$A
pred       = prediction(prediction,test02$return_customer )
perf       = performance(pred, measure="lift", x.measure="rpp")
lift       = cbind(perf@x.values[[1]], perf@y.values[[1]])
lift_score = lift[which.min(abs(lift[,1] - 0.1)) ,2]

result.perf.gbm = data.frame(AUC = auc, Lift = lift_score)
print(result.perf.gbm)
                           
final.pred = predict(fit, newdata = test, n.trees = trees ,type="response")
                           
FinalPredictions.gbm <- data.frame(Customer_ID = testdata$ID, EstimatedreturnProbability = final.pred)
                           
head(FinalPredictions.gbm)

## Make the prediction for test02

final.pred.test02 = predict(fit, newdata = test02, n.trees = trees ,type="response")
FinalPredictions.gbm_test02_ver3 <- data.frame(return_customer = test02$return_customer, EstimatedreturnProbability = final.pred.test02)

write.csv(FinalPredictions.gbm_test02_ver3,file='/Users/Barischnikow/Desktop/chloe/FinalPredictions.gbm_test02_ver3.csv')

##################################################################################################################################
############### 3. Extreme Gradient Boosting #####################################################################################
##################################################################################################################################

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
  number = 10, # number of folds in cross validation
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  allowParallel = TRUE, # Enable parallelization if available
  returnData = FALSE 
)

xgb.parms <- expand.grid(nrounds = 1000, 
                         max_depth = 14, 
                         eta = 0.001, 
                         gamma = 5,
                         colsample_bytree = 0.8,
                         min_child_weight = 1,
                         subsample = 0.8)
# Train model
xgb <- train(return_customer~., data = train08,  
             method = "xgbTree",
             tuneGrid = xgb.parms, 
             metric = "ROC", trControl = model.control)

# Make prediction on test set
xgb.pred <- predict(xgb, newdata = test02, type = "prob")[,2]
# Estimate performance on unseen data based on test set
auc_roc(test02$return_customer, xgb.pred)

if(!require("ROCR")) install.packages("ROCR"); library("ROCR") # load the package
pred <- prediction(xgb.pred, test02$return_customer)
perf.xgb <- performance(pred,"tpr","fpr")
plot(perf.xgb, main="ROC curve", colorize=T)

# And then a lift chart
perf.xgb.lift <- performance(pred,"lift","rpp")
perf.xgb.lift
plot(perf, main="XGB lift curve", colorize=T)

yhat.xgb <- predict(xgb, newdata = test, type = "prob")[,2]
print(yhat.xgb)

FinalPredictions7_final <- data.frame(Customer_ID = testdata$ID, 
                                EstimatedreturnProbability = yhat.xgb)

## Make the prediction for test02

yhat.xgb1000.test02 <- predict(xgb, newdata = test02, type = "prob")[,2]
print(yhat.xgb1000.test02)

# calculating AUC
xgb1000.ROC <- roc(predictor=yhat.xgb2000.test02,
                   response=test02$return_customer,
                   levels=rev(levels(test02$return_customer)))
xgb1000.ROC$auc

head(FinalPredictions7_final)
