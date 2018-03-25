#part1
cars<-read.csv('car_data.csv')
set.seed(71923)
train_insts<-sample(nrow(cars),0.7*nrow(cars))
cars_train<-cars[train_insts,]
cars_test<-cars[-train_insts,]
#part2
attach(cars_train)
boxplot(VehOdo~IsBadBuy,ylab='VehOdo',xlab='IsBadBuy')
boxplot(VehicleAge~IsBadBuy,ylab='VehicleAge',xlab='IsBadBuy')
make_table<-table(IsBadBuy,Make)
make_table
make_table/rowSums(make_table)
#part3---linear regression model
car_lin<-lm(IsBadBuy~Auction+Size+Color+Make+MMRAcquisitionAuctionAveragePrice+MMRAcquisitionRetailAveragePrice+VehicleAge+VehOdo+WheelType)
summary(car_lin)
lin_preds_train <- predict(car_lin,newdata=cars_train)
lin_preds_test <- predict(car_lin,newdata=cars_test)
measure_perf <- function(predicted,actual){
  AE = mean(predicted-actual)
  RMSE = sqrt(mean((predicted-actual)^2))
  return(c(AE,RMSE))
}
measure_perf(lin_preds_train, cars_train$IsBadBuy)
measure_perf(lin_preds_test, cars_test$IsBadBuy)
confusion_matrix <- function(predicted, actual, cutoff){
  
  classifications <- ifelse(predicted>cutoff,1,0)
  confusion_matrix <- table(actual,classifications)
}
model_performance <- function(confusion_matrix){
  
  TP <- confusion_matrix[2,2]
  TN <- confusion_matrix[1,1]
  FP <- confusion_matrix[1,2]
  FN <- confusion_matrix[2,1]
  
  acc <- (TP+TN)/(TP+TN+FP+FN) #accuracy
  tpr <- TP/(TP+FN) #true positive rate/sensitivity
  tnr <- TN/(TN+FP) #true negative rate/specificity
  
  return(c(acc, tpr, tnr))
}
lin_matrix <- confusion_matrix(lin_preds_test, cars_test$IsBadBuy,.5)
lin_matrix
lin_metrics <- model_performance(lin_matrix)
lin_metrics
#part4---logistic regression model
car_log<-glm(IsBadBuy~Auction+Size+Color+Make+MMRAcquisitionAuctionAveragePrice+MMRAcquisitionRetailAveragePrice+VehicleAge+VehOdo+WheelType,data=cars_train,family='binomial')
summary(car_log)
log_preds_train <- predict(car_log,newdata=cars_train,type='response')
log_preds_test <- predict(car_log,newdata=cars_test,type='response')
measure_perf(log_preds_train, cars_train$IsBadBuy)
measure_perf(log_preds_test, cars_test$IsBadBuy)
log_matrix <- confusion_matrix(log_preds_test, cars_test$IsBadBuy,.5)
log_matrix
log_metrics <- model_performance(log_matrix)
log_metrics
pred_car <- data.frame(Auction='MANHEIM', VehicleAge=1, Make='NISSAN', Color='RED',WheelType='NULL', VehOdo=10,000, Size='COMPACT', MMRAcquisitionAuctionAveragePrice=8000, MMRAcquisitionRetailAveragePrice=10000)
p<- predict(car_log,newdata = pred_car, type='response')

