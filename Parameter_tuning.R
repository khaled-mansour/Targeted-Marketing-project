##################################################################################################################################
############### 3. Extreme Gradient Boosting Params Tuning #######################################################################
##################################################################################################################################
if (!require('vcd')) install.packages('vcd')
install.packages("Matrix")
library(Matrix)
install.packages("data.table")
library(data.table)
library(vcd)
##### XGB TUNING #####
idx.train <- createDataPartition(y = train$return_customer, p = 0.6, list = FALSE) 
train06 <- train[idx.train, ] # training set
test04 <-  train[-idx.train, ] # testing set
#load libraries
library(data.table)
if(!require("mlr")) install.packages("mlr"); library("mlr") # load the package
#convert data frame to data table
train.tune <- setDT(train06)
test.tune <- setDT(test04)
#using one hot encoding
labels.tune <- train.tune$return_customer
ts_label.tune <- test.tune$return_customer
new_tr <- model.matrix(~.+0,data = train.tune[,-c("return_customer"),with=F])
new_ts <- model.matrix(~.+0,data = test.tune[,-c("return_customer"),with=F])
#convert factor to numeric
labels.tune <- as.numeric(labels.tune)-1
ts_label.tune <- as.numeric(ts_label.tune)-1
#preparing matrix
dtrain <- xgb.DMatrix(data = new_tr,label = labels.tune)
dtest <- xgb.DMatrix(data = new_ts,label=ts_label.tune)
#we'll first build our model using default parameters
#default parameters
params.tune <- list(
  booster = "gbtree",
  objective = "binary:logistic",
  eta=0.001,
  gamma=5,
  max_depth=15,
  min_child_weight=1,
  subsample=0.7,
  colsample_bytree=0.7
)
#Using the inbuilt xgb.cv function, let's calculate the best nround for this model
xgbcv.tune <- xgb.cv(params = params.tune
                ,data = dtrain
                ,nrounds = 4000
                ,nfold = 10
                ,showsd = T
                ,stratified = T
                ,print.every.n = 500
                ,early.stop.round = 4000
                ,maximize = F
)
min(xgbcv.tune$test.error.mean)
#first default - model training
xgb.tune1 <- xgb.train(
  params = params.tune
  ,data = dtrain
  ,nrounds = 4000
  ,watchlist = list(val=dtest,train=dtrain)
  ,print.every.n = 100
  ,early.stop.round = 4000
  ,maximize = F
  ,eval_metric = "error"
)

#model prediction
xgbpred <- predict(xgb.tune1,dtest)
xgbpred <- ifelse(xgbpred > 0.5,1,0)
#confusion matrix
library(caret)
confusionMatrix(xgbpred, ts_label.tune)
#create tasks
traintask <- makeClassifTask(data = train08,target = "return_customer")
testtask <- makeClassifTask(data = test02,target = "return_customer")
#do one hot encoding
traintask <- createDummyFeatures(obj = traintask,target = "return_customer")
testtask <- createDummyFeatures(obj = testtask,target = "return_customer")
#create learner
lrn.tune <- makeLearner("classif.xgboost",predict.type = "response")
lrn.tune$par.vals <- list(
  objective="binary:logistic",
  eval_metric="error",
  nrounds=4000L,
  eta=0.01
)

#set parameter space
params <- makeParamSet(
  makeDiscreteParam("booster",values = c("gbtree","gblinear")),
  makeIntegerParam("max_depth",lower = 6L,upper = 15L),
  makeNumericParam("min_child_weight",lower = 1L,upper = 10L),
  makeNumericParam("subsample",lower = 0.5,upper = 1),
  makeNumericParam("colsample_bytree",lower = 0.5,upper = 1)
)

#set resampling strategy
rdesc <- makeResampleDesc("CV",stratify = T,iters=10L)

#search strategy
ctrl <- makeTuneControlRandom(maxit = 100L)
#set parallel backend
if(!require("parallel")) install.packages("parallel"); library("parallel")
if(!require("parallelMap")) install.packages("parallelMap"); library("parallelMap")

parallelStartSocket(cpus = detectCores())

#parameter tuning
mytune <- tuneParams(learner = lrn.tune
                     ,task = traintask
                     ,resampling = rdesc
                     ,measures = acc
                     ,par.set = params
                     ,control = ctrl
                     ,show.info = T)

mytune$y
mytune$x
