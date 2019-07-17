library(caTools)
library(ggplot2)
library(dplyr)
library(e1071)
library(plyr)
library(mice)
#Loan Prediction Problem

sum(is.na(train2))
sum(is.na(test2))
#missing values imputation
train2_imp <- mice(train2,2,method='cart')
train2 <- complete(train2_imp,2)
test2_imp <- mice(test2,m = 2,method = 'cart')
test2 <- complete(test2_imp,2)
#checking missing data again, after imputation
sapply(train2, function(x) sum(is.na(x)))
sapply(test2, function(x) sum(is.na(x)))

train <- train2
test <- test2

test$Loan_Status <- NA
combi <- rbind(train,test)
head(combi)

combi$FamilyInc <- combi$ApplicantIncome + combi$CoapplicantIncome
combi$LoanAmt_Family <- combi$LoanAmount / combi$FamilyInc
combi$LoanAmt_AppInc <- combi$LoanAmount / combi$ApplicantIncome
combi$Dependents <- revalue(combi$Dependents,c("3+"="3"))
nrow(combi)
nrow(train)
nrow(test)

train <- combi[1:614,]
test <- combi[615:nrow(combi),]

library(dplyr)
library(rpart)
library(rpart.plot)

train <- train[,-1]
set.seed(123)
sample <- sample.split(train,SplitRatio = 0.75)
train_sample <- subset(train,sample == TRUE)
test_sample <- subset(train,sample == FALSE)

#model building
#decison trees
model1 <- rpart(Loan_Status ~.,data = train_sample,method = 'class')
rpart.plot(model1,extra =0.1)
plot(model1)
text(model1, cex = 0.5)

predict <- predict(model1,newdata = test_sample,type = 'class')

#svm
model2 <- svm(Loan_Status ~.,data = train_sample,method = 'C-classification',kernel = 'linear')
predict2 <- predict(model2,newdata = test_sample,type = 'class')

#logistic regression
model3 <- glm(Loan_Status ~.,data = train_sample,family = binomial)
predict3 <- predict(model3,newdata = test_sample,type = 'response')
library(randomForest)
#random forest
model4 <- randomForest(Loan_Status ~.,data = train_sample,importance = TRUE)
predict4 <- predict(model4,newdat  = test_sample,type = 'response')
#confusion matrix
cm = table(test_sample$Loan_Status,predict4)
accuracy_Test <- sum(diag(cm)) / sum(cm)
accuracy_Test
#confusion matrix for logistic regression
cm_lm = table(test_sample$Loan_Status,predict3 >0.5)
accuracy_Test <- sum(diag(cm)) / sum(cm)
#test data
test <- test[,-13]
is.na(test)<-sapply(test, is.infinite)
test[is.na(test)]<-0
predict_submission <- predict(model3,newdata = test,type = 'class') #svm
test$Loan_Status <- predict_submission
predict_submission <- predict(model4,newdata = test,type = 'response') #logistic regression
##test$Loan_Status <- ifelse(predict_submission > 0.5,'Y','N')***
test$Loan_Status <- predict_submission
test_lm_submission <- test %>%
  select(c(Loan_ID,Loan_Status))
write.csv(test_lm_submission,file = "test_lm_submission.csv")
