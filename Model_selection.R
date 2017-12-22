##################################################################################################################################
############### Model Selection and Evaluation ###################################################################################
##################################################################################################################################

##Model library building ##
## set up GBM tuneGrid
gbmGrid <-  expand.grid(interaction.depth = 20, 
                        n.trees = 1000, 
                        shrinkage = 0.005,
                        n.minobsinnode = 5)
if(!require("caretEnsemble")) install.packages("caretEnsemble"); library("caretEnsemble")
ctrl  <- trainControl(method = "cv", number = 10, classProbs = TRUE,  savePredictions = "final", summaryFunction = twoClassSummary)
modelList <- list(caretModelSpec(method = "nnet", trace = "FALSE", tuneLength = 5, metric = "ROC"),
                  caretModelSpec(method = "rf", tuneGrid = randomforest.parms, metric = "ROC"),
                  caretModelSpec(method = "xgbTree", tuneGrid = xgb.parms, metric = "ROC"),
                  caretModelSpec(method = "gbm",verbose=FALSE, tuneGrid = gbmGrid, metric = "ROC")) 
models <- caretList(return_customer~., data = train08, trControl = ctrl, tuneList = modelList, continue_on_fail = FALSE)
#Stacking ###
ens.stack <- caretStack(models, method='glm')
ens.stack$ens_model$finalModel
ens.stack.pred <- predict(ens.stack, newdata = test02, type = "prob")
ens.predictions <- data.frame(STACKING = ens.stack.pred)
r.stack <- roc(test02$return_customer, ens.stack.pred)
plot(r.stack)
##Ensemble selection ###
ens.hc  <- caretEnsemble(models) 
ens.predictions$HILLCLIMBING <- predict(ens.hc, newdata = test02)
### Step 1: Initialize the ensemble with the best single model
ens.hc <- list()
# Collect the predictions of each of the base models on the train data
models.pred <- sapply(models, function(x) predict(x, newdata = train08, type = "prob")[,2])
# Calculate the AUC of each of the base models
models.auc <- apply(models.pred, 2, function(x) auc(response = train08$return_customer, predictor = x))
# Initialize the current AUC value to keep track of improvements
best_current_auc <- max(models.auc)
# Add the best performing base model to the ensemble list
message(paste("Start the ensemble with model", models[[which.max(models.auc)]]$method))
ens.hc[[1]] <- models[[which.max(models.auc)]]
# Step 2: Extend the ensemble by appending additional models until the AUC improvement
# becomes smaller than a threshold, here we choose 0.001
i = 1 # set first iteration
max.iter = 10 # maximum number of iterations
# We use a while loop that runs until the maximum number of iterations is reached or
# the AUC improvement becomes small and we break out of the loop before it ends using 
# the command break
while(i <= max.iter){
  # Collect the predictions for all models in the ensemble. This is not very efficient, but
  # clearer to see what is happening.
  hc.pred <- sapply(ens.hc, function(x) predict(x, newdata = train08, type = "prob")[,2])
  # Average the predictions of the ensemble AND each of the base models as possible extensions
  combinedModels.pred <- apply(models.pred, 2, function(x)rowMeans(cbind(hc.pred, x)))
  # Calculate the AUC value for the extended ensemble
  combinedModels.auc <- apply(combinedModels.pred, 2, function(x) auc(response = train08$return_customer, predictor = x))
  # Calculate the AUC improvement of the best extension compared to the last iteration
  auc_improvement <- max(combinedModels.auc) - best_current_auc 
  
  # Check if the improvement is still significantly large
  # If yes: extend the ensemble by the best new combination
  # If no: stop the loop
  if(auc_improvement < 0.001){
    message(paste("The final ensemble model is saved in ens.hc with an AUC of", max(combinedModels.auc),"\nIts structure is:", paste(sapply(ens.hc, "[[", "method"), collapse = " ") ))
    break # Stop the loop
  }else{
    message(paste("Added another", models[[which.max(combinedModels.auc)]]$method ,"model to the ensemble for an AUC increase of", auc_improvement))
    # Add the base model of the best extension to the list which makes up our ensemble
    ens.hc[[length(ens.hc) + 1]] <- models[[which.max(combinedModels.auc)]]
    # Update the best AUC value achieved so far to the AUC value of the current ensemble
    best_current_auc <- max(combinedModels.auc)
  }
}

ens.predictions$HILLCLIMBING <- rowMeans(sapply(ens.hc, function(x) predict(x, newdata = test02, type = "prob")[,2]))
### Comparing the models
# Note that we can also compute the performance of, e.g., the best random forest model as follows
ens.predictions$xgb <- predict(models$xgb, newdata = test02, type="prob")[,2]
ens.predictions$rf <- predict(models$rf, newdata = test02, type="prob")[,2]

# Plot the performance of the three models in ROC space
if(!require("hmeasure")) install.packages("hmeasure"); library("hmeasure") # load the package
# Function HMeasure reassigns the target levels to 0 and 1 alphabetically, so we do that by hand
# with ifelse()
h <- HMeasure(ifelse(test02$return_customer == "non.returning.customer", 1, 0), ens.predictions)
plotROC(h, which = 1)

#### Best performing model in terms of AUC is Extreme Gradient Boosting (XGB) which will be selected as our predictive model for this study case
