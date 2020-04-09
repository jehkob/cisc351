library(tidyverse)
library(caret)
library(leaps)

### Duo Best Subset
modelsd <- regsubsets(winPlacePerc~., data = duo.train, nvmax = 20)

res.sumd <- summary(modelsd)
data.frame(
  Adj.R2 = which.max(res.sumd$adjr2),
  CP = which.min(res.sumd$cp),
  BIC = which.min(res.sumd$bic)
)

###
library(randomForest)
newrfd <- randomForest(winPlacePerc~.,data=duo.train,ntree=100,mtry=4,importance=TRUE,na.action=na.roughfix) 
new_predd <- predict(newrfd,newdata=duo.test) 
mean((duo.test[, 20] - new_predd)^2) 

newrfd <- randomForest(winPlacePerc~.,data=duo.train,ntree=100,mtry=8,importance=TRUE,na.action=na.roughfix) 
new_predd <- predict(newrfd,newdata=duo.test) 
mean((duo.test[, 20] - new_predd)^2) 

newrfd <- randomForest(winPlacePerc~.,data=duo.train,ntree=100,mtry=12,importance=TRUE,na.action=na.roughfix) 
new_predd <- predict(newrfd,newdata=duo.test) 
mean((duo.test[, 20] - new_predd)^2)

newrfd <- randomForest(winPlacePerc~.,data=duo.train,ntree=100,mtry=16,importance=TRUE,na.action=na.roughfix) 
new_predd <- predict(newrfd,newdata=duo.test) 
mean((duo.test[, 20] - new_predd)^2)


importance(newrfd)
duovarIMP<-varImpPlot(newrfd)

library(xgboost)
library(tictoc)
library(doParallel)
library(tidyverse)
library(dplyr)
registerDoParallel(cores=4)
winPlacePercColumn <- which(names(duo.train) == 'winPlacePerc')
tic()
xgb_d <- xgboost(data = data.matrix(duo.train[-winPlacePercColumn]), 
               label = duo.train$winPlacePerc,
               booster = 'gbtree',
               eta = 0.1, # step size of each boosting step
               nround = 1000,
               eval_metric = 'rmse',
               objective = 'reg:linear',
               max_depth = 12,
               subsample = 0.5,
               colsample_bytree = 0.5)
toc()

xgb.predd<-predict(xgb_d,data.matrix(duo.test[,-20]))
mean((duo.test[,20] - xgb.predd)^2)

importance_matrix_d <- xgb.importance(model = xgb_d)
print(importance_matrix_d)
xgb.plot.importance(importance_matrix = importance_matrix_d)
