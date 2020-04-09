## SOlo Queue Modelling

library(tidyverse)
library(caret)
library(leaps)

### Solo Best Subset
models <- regsubsets(winPlacePerc~., data = solo.train, nvmax = 16)

res.sum <- summary(models)
data.frame(
  Adj.R2 = which.max(res.sum$adjr2),
  CP = which.min(res.sum$cp),
  BIC = which.min(res.sum$bic)
)
### cross validation BEST SUBSET
get_model_formula <- function(id, object, outcome){
  # get models data
  models <- summary(object)$which[id,-1]
  # Get outcome variable
  form <- as.formula(object$call[[2]])
  outcome <- all.vars(form)[1]
  # Get model predictors
  predictors <- names(which(models == TRUE))
  predictors <- paste(predictors, collapse = "+")
  # Build model formula
  as.formula(paste0(outcome, "~", predictors))
}

get_cv_error <- function(model.formula, data){
  set.seed(1)
  train.control <- trainControl(method = "cv", number = 10)
  cv <- train(model.formula, data = solo.train, method = "lm",
              trControl = train.control)
  cv$results$RMSE
}

model.ids <- 1:10
cv.errors <-  map(model.ids, get_model_formula, models, "winPlacePerc") %>%
  map(get_cv_error, data = solo.train) %>%
  unlist()
cv.errors

which.min(cv.errors)
###
library(randomForest)
newrf <- randomForest(winPlacePerc~.,data=solo.train,ntree=50,mtry=4,importance=TRUE,na.action=na.roughfix) 
new_pred <- predict(newrf,newdata=solo.test) 
mean((solo.test[, 16] - new_pred)^2) 

newrf <- randomForest(winPlacePerc~.,data=solo.train,ntree=50,mtry=8,importance=TRUE,na.action=na.roughfix) 
new_pred <- predict(newrf,newdata=solo.test) 
mean((solo.test[, 16] - new_pred)^2) 

newrf <- randomForest(winPlacePerc~.,data=solo.train,ntree=50,mtry=12,importance=TRUE,na.action=na.roughfix) 
new_pred <- predict(newrf,newdata=solo.test) 
mean((solo.test[, 16] - new_pred)^2)

newrf <- randomForest(winPlacePerc~.,data=solo.train,ntree=50,mtry=8,importance=TRUE,na.action=na.roughfix) 
new_pred <- predict(newrf,newdata=solo.test) 
mean((solo.test[, 16] - new_pred)^2)


imp<-importance(newrf)
solvarIMP<-varImpPlot(newrf)

library(xgboost)
library(tictoc)
library(doParallel)
library(tidyverse)
library(dplyr)
registerDoParallel(cores=4)
winPlacePercColumn <- which(names(solo.train) == 'winPlacePerc')
tic()
xgb <- xgboost(data = data.matrix(solo.train[-winPlacePercColumn]), 
               label = solo.train$winPlacePerc,
               booster = 'gbtree',
               eta = 0.1, # step size of each boosting step
               nround = 1000,
               eval_metric = 'rmse',
               objective = 'reg:linear',
               max_depth = 12,
               subsample = 0.5,
               colsample_bytree = 0.5)
toc()
xgb.pred<-predict(xgb,data.matrix(solo.test[,-16]))
mean((solo.test[,16] - xgb.pred)^2)

xgb.importance(colnames(solo.train), model = xgb)

importance_matrix <- xgb.importance(model = xgb)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix,cex.names=2.0)

library(compareDF)
comp<-compare_df(importance_matrix,importance_matrix_d, c("Feature"))
comp


