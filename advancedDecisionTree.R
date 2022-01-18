rm(list=ls())

#install.packages("ISLR")
library(ISLR)
data(OJ)
set.seed(1234)

#install.packages("caTools")
library(caTools)
split = sample.split(OJ$Purchase, SplitRatio = 0.7)
train = OJ[split,]
test = OJ[-split,]
summary(train)
nrow(train)

library(rpart); library(rpart.plot)
tree = rpart(Purchase~PriceCH+PriceMM+DiscCH+DiscMM+SpecialCH+
               SpecialMM+LoyalCH+PriceDiff+PctDiscMM+PctDiscCH,
             data = train, method = 'class')

pred_test = predict(tree, type='prob', newdata = test)[,2]

library(ROCR)
ROCRPred = prediction(predictions = pred_test, labels = test$Purchase)
auc = as.numeric(performance(prediction.obj = ROCRPred,measure = 'auc')@y.values); auc


library(caret)
trControl = trainControl(method='cv',number = 10)
tuneGrid = expand.grid(.cp = seq(from = 0.001,to = 0.1,by = 0.001))
set.seed(100)
cvModel = train(Purchase~PriceCH+PriceMM+DiscCH+DiscMM+SpecialCH+
                  SpecialMM+LoyalCH+PriceDiff+PctDiscMM+PctDiscCH,
                data=train,
                method="rpart",
                trControl = trControl,
                tuneGrid = tuneGrid)

cvModel$results
library(ggplot2)
ggplot(data=cvModel$results, aes(x=cp, y=Accuracy))+
  geom_line(size=0.5,alpha=0.2)+
  geom_point(color='brown')+
  theme_bw()+
  ggtitle(label=paste('Lowest RMSE is at a cp of ',cvModel$bestTune$cp))


tree2 = rpart(Purchase~PriceCH+PriceMM+DiscCH+DiscMM+SpecialCH+SpecialMM+LoyalCH+
                PriceDiff+PctDiscMM+PctDiscCH,data = train, method = 'class', 
              control = rpart.control(cp = 0.005))
rpart.plot(tree2)
pred2_test = predict(tree2, type='prob', newdata = test)[,2]
ROCRPred2 = prediction(predictions = pred2_test, labels = test$Purchase)
auc2 = as.numeric(performance(prediction.obj = ROCRPred,measure = 'auc')@y.values); auc2


#install.packages("randomForest")
library(randomForest)
set.seed(617)
bag = randomForest(Purchase~.,data=train,mtry = ncol(train)-1,ntree=1000)
pred = predict(bag, type='prob')[,2]
pred_class = factor(ifelse(pred>0.5,'high','low'),
                    levels = c('low','high'))

data.frame(Predicted_Prob = pred,
           Predicted_Class = pred_class,
           Purchase = train$Purchase)[1:5,]
ct = table(train$Purchase,pred_class); ct
accuracy = sum(ct[1,1],ct[2,2])/nrow(train); accuracy
specificity = ct[1,1]/sum(ct[1,1],ct[1,2]); specificity
sensitivity = ct[2,2]/sum(ct[2,1],ct[2,2]); sensitivity
#library(caret)
#confusionMatrix(data = pred_class,
 #                reference = train$Purchase,
  #               positive = 'high')
pred_test = predict(bag, type='prob', newdata = test)[,2]
pred_test_class = factor(ifelse(pred_test>0.5,'high','low'),
                         levels = c('low','high'))
ct = table(test$Purchase,pred_test_class)
accuracy_test = sum(ct[1,1],ct[2,2])/nrow(test); accuracy_test
library(ROCR)
ROCRPred = prediction(predictions = pred_test, labels = test$Purchase)
auc = as.numeric(performance(prediction.obj = ROCRPred,measure = 'auc')@y.values); auc
round(auc,2)


library(randomForest)
set.seed(617)
forest = randomForest(Purchase~PriceCH+PriceMM+DiscCH+DiscMM+SpecialCH+
                        SpecialMM+LoyalCH+PriceDiff+PctDiscMM+PctDiscCH,
                      data=train,ntree = 1000)
pred3 = predict(forest, type='prob')[,2]
pred3_class = factor(ifelse(pred3>0.5,'high','low'),
                     levels = c('low','high'))

pred3_test = predict(forest, type='prob', newdata = test)[,2]
pred3_test_class = factor(ifelse(pred3_test>0.5,'high','low'),
                          levels = c('low','high'))
ct3 = table(test$Purchase,pred3_test_class)

library(ROCR)
ROCRPred3 = prediction(predictions = pred3_test, labels = test$Purchase)
auc3 = as.numeric(performance(prediction.obj = ROCRPred3,measure = 'auc')@y.values); auc3
round(auc3,2)


train$Purchase2 = as.numeric(train$Purchase)-1

test$Purchase2 = as.numeric(test$Purchase)-1

#install.packages("gbm")
library(gbm)
set.seed(617)
boost = gbm(Purchase2~PriceCH+PriceMM+DiscCH+DiscMM+SpecialCH+
              SpecialMM+LoyalCH+PriceDiff+PctDiscMM+PctDiscCH,
            data=train,
            distribution="bernoulli",
            n.trees = 1000,
            interaction.depth = 1,
            shrinkage = 0.04)

pred = predict(boost,n.trees = 100, type = "response")
pred_class = factor(ifelse(pred>0.5,'high','low'),
                    levels = c('low','high'))
data.frame(Predicted_Prob = pred,
           Predicted_Class = pred_class,
           Purchase2 = train$Purchase2)[1:5,]
ct = table(train$Purchase2,pred_class); ct
accuracy = sum(ct[1,1],ct[2,2])/nrow(train); accuracy
specificity = ct[1,1]/sum(ct[1,1],ct[1,2]); specificity
sensitivity = ct[2,2]/sum(ct[2,1],ct[2,2]); sensitivity

pred7_test = predict(boost,newdata=test,n.trees = 100, type = "response")
pred_test_class = factor(ifelse(pred_test>0.5,'high','low'),
                         levels = c('low','high'))
ct = table(test$Purchase2,pred_test_class)
accuracy_test = sum(ct[1,1],ct[2,2])/nrow(test); accuracy_test
library(ROCR)
ROCRPred = prediction(predictions = pred7_test, labels = test$Purchase)
auc7 = as.numeric(performance(prediction.obj = ROCRPred,measure = 'auc')@y.values); auc7


round(auc7,2)

