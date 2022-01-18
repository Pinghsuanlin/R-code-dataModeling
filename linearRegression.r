set.seed(1031)
library(caret)
split = createDataPartition(y = houses$price, p = 0.7, list = F, groups = 100) 
train = houses[split,]
test = houses[-split,]

#What's the average price in the train and test sample?
mean(train$price)
mean(test$price)

#What is the living area (sqft_living) for the house with the most bedrooms?
library(dplyr); library(tidyr)
train %>%
  select(id,price:sqft_lot,age)%>%
  pivot_longer(price:age,names_to='numericVariable',values_to='value')%>%
  ggplot(aes(x='',y=value))+
  geom_boxplot(outlier.color = 'red')+
  facet_wrap(~numericVariable,scales="free_y")

max(train$bedrooms)
train[train$bedrooms == 33, "sqft_living"]

#What's correlation between x y
cor(train$sqft_living, train$price) 
#OR
x1 <- train$sqft_living
y1 <- train$price
plot(x1, y1)
cor(x1, y1)

#Construct a simple regression to predict house price from area (sqft_living). How well the model is predicting price; what is the p-value for the F-statistic?
model1=lm(price~sqft_living, data = train)
anova(model1)

#What's the r-squared for model1? 
summary(model1)
#R-squared interpret how well the regression model fits the observed data. Generally, a higher r-squared indicates a better fit for the model.

#what's the RMSE?
pred = predict(model1)
rmse1 = sqrt(mean((pred-train$price)^2)); rmse1
#RMSE can be interpreted as the standard deviation of the unexplained variance, and has the useful property of being in the same units as the response variable. Lower values of RMSE indicate better fit.
#OR
library(Metrics)
rmse(pred, train$price)

#Since model1 is built on sample data, it is important to see if the coefficient estimates will be non-zero in the population.
coef(model1)
paste('price','=',round(coef(model1)[1],5),'+',round(coef(model1)[2],5),'sqft_living')

#Based on model1, on average, what would a 1400 square foot house cost?
model1_p1 = model1$coef[1]+ model1$coef[2]*1400; model1_p1
#or
predict(model1,newdata=data.frame(sqft_living=1400))

#if a homeowner were to put in a 200 square foot addition on the house, how much would the price be expected to go up by?
model1_p2 = model1$coef[1]+ model1$coef[2]*1600
model1_p2 - model1_p1

#Run a multiple regression model with the following predictors: bedrooms, bathrooms, sqft_living, sqft_lot, floors, waterfront, view, condition, grade, age Call this model4. 
#What is the R2 for model2?
model2 = lm(price~bedrooms + bathrooms + sqft_living + sqft_lot + floors + waterfront + view + condition + grade + age, data = train)
summary(model4)
pred2 = predict(model2)
rmse2 = sqrt(mean((pred2 - train$price)^2)) ; rmse2
#if a person decides to add another bathroom, the increase in expected price would be coefficient estimate

#Which of the predictors in model2 exerts the strongest influence on price?
#install.packages("lm.beta")
library(lm.beta)
lm.beta(model2) #shows standardized coefficients. 
#The one with largest number has the strongest influence on price

#What is the R2 for the test sample?
#R^2 = 1 - SSE/SST
pred2_t = predict(model2, newdata=test)
sse2_t = sum((pred2_t - test$price)^2)
sst2_t = sum((mean(train$price)-test$price)^2)
model2_t = 1 - sse2_t/sst2_t; model2_t

#What's the rmse for the model2 on the test sample?
rmse2_t = sqrt(mean((pred2_t - test$price)^2)); rmse2_t
```

