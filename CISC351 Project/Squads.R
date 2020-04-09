library(tidyverse)
library(caret)
library(leaps)

### Duo Best Subset
modelss <- regsubsets(winPlacePerc~., data = squad.train, nvmax = 20)

res.sums <- summary(modelss)
data.frame(
  Adj.R2 = which.max(res.sumd$adjr2),
  CP = which.min(res.sumd$cp),
  BIC = which.min(res.sumd$bic)
)

###
library(randomForest)
newrfs <- randomForest(winPlacePerc~.,data=squad.train,ntree=100,mtry=4,importance=TRUE,na.action=na.roughfix) 
new_preds <- predict(newrfs,newdata=squad.test) 
mean((squad.test[, 20] - new_preds)^2) 

newrfs <- randomForest(winPlacePerc~.,data=squad.train,ntree=100,mtry=8,importance=TRUE,na.action=na.roughfix) 
new_preds <- predict(newrfs,newdata=squad.test) 
mean((duo.test[, 20] - new_predd)^2) 

newrfs <- randomForest(winPlacePerc~.,data=squad.train,ntree=100,mtry=12,importance=TRUE,na.action=na.roughfix) 
new_preds <- predict(newrfs,newdata=squad.test) 
mean((squad.test[, 20] - new_preds)^2)

newrfd <- randomForest(winPlacePerc~.,data=squad.train,ntree=100,mtry=16,importance=TRUE,na.action=na.roughfix) 
new_preds <- predict(newrfs,newdata=squad.test) 
mean((squad.test[, 20] - new_preds)^2)


importance(newrfs)
duovarIMP<-varImpPlot(newrfs)

library(xgboost)
library(tictoc)
library(doParallel)
library(tidyverse)
library(dplyr)
registerDoParallel(cores=4)
winPlacePercColumn <- which(names(squad.train) == 'winPlacePerc')
tic()
xgb_s <- xgboost(data = data.matrix(squad.train[-winPlacePercColumn]), 
                 label = squad.train$winPlacePerc,
                 booster = 'gbtree',
                 eta = 0.1, # step size of each boosting step
                 nround = 1000,
                 eval_metric = 'rmse',
                 objective = 'reg:linear',
                 max_depth = 12,
                 subsample = 0.5,
                 colsample_bytree = 0.5)
toc()

xgb.preds<-predict(xgb_s,data.matrix(squad.test[,-20]))
mean((squad.test[,20] - xgb.preds)^2)

importance_matrix_s <- xgb.importance(model = xgb_s)
print(importance_matrix_s)
xgb.plot.importance(importance_matrix = importance_matrix_s,)