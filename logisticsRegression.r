setwd('C:/Users/Downloads')
eBay = read.csv("eBayAssignment.csv", stringsAsFactors=T)
nrow(eBay)

#Which of the following iPads does this dataset contain?
unique(eBay$productline)

#What is the uniqueID of the iPad with the highest startprice?
max(eBay$startprice)
eBay[eBay$startprice == max(eBay$startprice), "UniqueID"]

#80-20,sample.split(). How many rows are in the train sample?
library(caTools)
set.seed(196)
split = sample.split(Y = eBay$sold, SplitRatio = 0.8)
table(split)
train = eBay[split,]
test = eBay[!split,]
nrow(train)

#What is the median startprice of iPads that sold? 
train %>%
  filter(sold == 1) %>%
  summarize(median(startprice)) 
#or
median(train$startprice[train$sold == 1]) #sold
median(train$startprice[train$sold == 0]) #unsold

#Predict variables that influence sold or not: Since the variable to be predicted only takes on two values, we will use a logistic regression model. Use the 'glm' function to build a model.
model1 = glm(sold ~ biddable+startprice+condition+cellular+ carrier+
               color+storage+productline+noDescription+charCountDescription+
               upperCaseDescription+startprice_99end, data = train, 
             family = "binomial")
summary(model1)$aic 
#or
model1$aic
#AIC: Measure of relative quality of model. Lower is better. Can only be used to compare two models. Not meaningful in an absolute sense.

#influencing prefictors
summary(model1)

#Simpler models are generally preferred to more complex models because they are less likely to overfit the data. 
#So, let us drop out non-signficant variables from model1 above but keep variables that previous research or experience indicates should have an effect. 
model2 = glm(sold ~ biddable + startprice+ condition+ storage+ productline+ upperCaseDescription+startprice_99end, data = train, family = "binomial")

model2$aic


#You will note that the data contains a number of factor variables. In order to model factor variables, they have to be dummy coded. 
#Fortunately, glm and lm functions automatically dummy code factor variables and then run the dummy variables in the model. 
#The first level is usually selected to be the baseline or reference variable to which each of the other levels is compared.

#After controlling for the effects of all other variables in the model, what sells better iPad3 or iPad 1?
summary(model2)
levels(train$productline)
#Since the coefficient of ipad3 is positive, it sells better than the base,ipad1


#If startprice goes up by $1, what will be the % reduction in the chance of selling an iPad. 
#To interpret coefficients in logistic regression, you have to exponentiate the coefficient. E.g., exp(coefficient)
coef(model2)
100*(exp(summary(model2)$coef[2])-1)#-80%
model2_o = 100* (exp(summary(model2)$coef[2])); model2_o 
model2_1 = 100* (exp(summary(model2)$coef[2])+1); model2_1
(model2_o - model2_1)/model2_1 #-0.837. 
#If the price goes up by $1, it will be a reduction in the chance of selling an ipad for less than 1%


#Based on model2 (and controlling for the effects of all other variables), how much more (or less) likely is an iPad Air 1/2 to sell compared to an iPad 1?
summary(model2)$coef[12]
exp(summary(model2)$coef[12]) #6.6. times more likely to sell
100*(exp(summary(model2)$coef[12])-1) #560% more likely to sell

#Use model2 to generate predictions for 'sold' in the test set. What is the probability of sale for an iPad with UniqueID 10940?
pred2_t = predict(model2, newdata = test, type = 'response')
pred2_t[test$UniqueID == "10940"]

#What is the accuracy of model2 on the test set? Use a threshold of 0.5. 
ct = table(sold = test$sold,
           predictions = as.integer(pred2_t>0.5)); ct
accuracy = sum(ct[1,1],ct[2,2])/nrow(test); accuracy

specificity = ct[1,1]/sum(ct[1,1],ct[1,2]); specificity
#specificity = TN(predict and sold 0) / (TN + FP (predict 1, sold 0)). you accurately know when it is false
sensitivity = ct[2,2]/sum(ct[2,1],ct[2,2]); sensitivity
#sensitivity = TP(predict and sold 0) / (TP + FN (predict 0, sold 1)). you accurately know when it is correct

#model2 perform better than the baseline.
accuracy_0.05 = sum(as.integer(pred2_t>0.05)==test$sold)/nrow(test);accuracy_0.05


#In order to evaluate a model, one has to have a point of reference or a baseline. 
#A suitable baseline for the accuracy of the test sample is using the majority class in the train sample. 
#In other words, what is the accuracy in the test sample, if we assumed the majority class of the train sample for all observations?
prop.table(table(train$sold))
#or
max(sum(train$sold==0), sum(train$sold==1))/nrow(train)

#The accuracy measure depends on the cut-off value (or threshold) used. Hence a more popular measure is area under the curve (or AUC). AUC is computed by finding the area under the curve of a plot of Sensitivity vs. 1-Specificity. AUC is a model performance measure that is independent of any particular threshold. 
#What is the AUC for model2 on the test sample?
library(ROCR)
ROCRpred = prediction(pred2_t,test$sold)
ROCRperf = performance(ROCRpred,"tpr","fpr")
plot(ROCRperf)
as.numeric(performance(ROCRpred,"auc")@y.values) # auc measure

