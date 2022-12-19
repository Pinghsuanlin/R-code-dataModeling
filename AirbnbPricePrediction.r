library(dplyr)
library(tidyr)
#install.packages('skimr')
library(skimr)
library(ggplot2)
#install.packages('xgboost')
library(xgboost)
library(Metrics)
library(corrplot)
library(ggcorrplot)
library(readr)
library(forcats)
#install.packages('ISLR')
library(ISLR)
library(caTools)
library(randomForest)


setwd("D:/Fall2021/5200")
analysisData = read.csv('analysisData.csv')
scoringData = read.csv('scoringData.csv')

scoringData$type = 'test'
analysisData$type = 'train'
head(scoringData)

skim(analysisData)
skim(scoringData) #91 columns, no price column

ggplot(data=analysisData, aes(x=price)) + 
  geom_histogram(binwidth = 20) #shows right-tailed 

#Check correlations between all numeric variables to price by applying a function over
numeric_var <- which(sapply(analysisData, is.numeric))

#Add numeric_var to the training data set
analysisData_num <- analysisData[, numeric_var]
analysisData_cor <- cor(analysisData_num, use="pairwise.complete.obs"); analysisData_cor

#selecting only those variables with the correlation more than 0.1
cor_sorted <- as.matrix(sort(analysisData_cor[,'price'], decreasing = TRUE));cor_sorted

#From here, we see that weekly_price, accommodates, cleaning_fee, bedrooms, beds are highly correlated with price
cor_high <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.1)))
corMatrix = as.data.frame(cor(analysisData_num[,cor_high]))
corMatrix$var1 = rownames(corMatrix)

corMatrix %>%
  gather(key=var2,value=r,1:13)%>%
  arrange(var1,desc(var2))%>%
  ggplot(aes(x=var1,y=reorder(var2, order(var2,decreasing=F)),fill=r))+
  geom_tile()+
  geom_text(aes(label=round(r,2)),size=3)+
  scale_fill_gradientn(colours = c('#d7191c','#fdae61','#ffffbf','#a6d96a','#1a9641'))+
  theme(axis.text.x=element_text(angle=75,hjust = 1))+xlab('')+ylab('')
                              

ggplot(data=analysisData, aes(x=factor(accommodates), y=price))+ geom_boxplot() + labs(x='Total accommodation in room')

                              
neighbourhood_room <- analysisData %>%
  group_by(neighbourhood_group_cleansed) %>%
  count(room_type)

ggplot(neighbourhood_room, aes(x = neighbourhood_group_cleansed, y = n, fill = room_type)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_classic() +
  labs(title = "Number of properties by neighbourhood", x = "Neighbourhood", y = "Number of Properties") +
  theme(plot.title = element_text(face = "bold"))
                              
#Replace all non integers characters with blank 
combinedData_cleaned$zipcode <- gsub("[^\\d]", "", combinedData_cleaned$zipcode, perl=TRUE)
#Exact characters from 1st to 5th position
combinedData_cleaned$zipcode <- substr(combinedData_cleaned$zipcode, 1, 5)
#For zipcode having less than 5 digits, we replace them with NA
combinedData_cleaned$zipcode[nchar(combinedData_cleaned$zipcode)<5] <- NA
#Convert zipcode to factor so that the model can read it
combinedData_cleaned$zipcode <- as.factor(combinedData_cleaned$zipcode)

#Take the top 40 zipcode for frequency distribution and call all the rest zipcodes as "Others"
combinedData_cleaned$new_zip_code <- fct_lump_n(combinedData_cleaned$zipcode, 40)
#40 works the best after several trials
#Clean categorical variables that won't come into use 
drop <- c('name',
           'summary',
           'space',
           'description',
           'neighborhood_overview',
           'notes',
           'transit',
           'access',
           'interaction',
           'house_rules', 
           'host_name',
           'host_location',
           'host_neighbourhood',
           'host_about', 
           'host_verifications', 
           'host_since'
)

combinedData_cleaned <- combinedData_cleaned[, !(names(combinedData_cleaned) %in% drop)]

#Convert all character (text) variables to factor so that the model can read it
categorical_col = colnames(combinedData_cleaned[, sapply(combinedData_cleaned, class) == 'character'])


#Create a function to convert categorical variables to factor

factor_trans <- function(main_df, factor_cat_var)
{
  #Mention the list of variables
  main_list = factor_cat_var
  for (i in 1:length(main_list))
  {
    new_var = main_list[i]
    main_df[,new_var] <- as.factor(main_df[,new_var])
    
  }
  
  return (main_df)
  
}
#Utilize the function
combinedData_trans = factor_trans(combinedData_cleaned,categorical_col)

#Check if the conversion to categorical is successfully conducted
head(combinedData_cleaned$amenities)

#Split entire data to train and test
train_df = combinedData_trans[(combinedData_trans$type == 'train'),]
test_df = combinedData_trans[(combinedData_trans$type == 'test'),]


#Defining the list of variables which will be used for model building
list = c('accommodates', 'bedrooms', 'beds', 'guests_included', 'cleaning_fee',
'bathrooms', 'security_deposit', 'extra_people', 'review_scores_location', 
'review_scores_cleanliness','minimum_nights','maximum_nights','minimum_minimum_nights',
'maximum_minimum_nights','minimum_maximum_nights', 'maximum_maximum_nights',
'minimum_nights_avg_ntm','maximum_nights_avg_ntm','number_of_reviews','number_of_reviews_ltm',
'reviews_per_month','host_listings_count','calculated_host_listings_count_shared_rooms', 
'calculated_host_listings_count_private_rooms', 'calculated_host_listings_count',
'days_as_host', 'days_sincefirst', 'days_sincelast', 'host_response_rate', 
'host_acceptance_rate', 'new_zip_code', 'room_type')

other = c('availability_30','availability_60', 'availability_90')
#Creating all combinations of variables
n <-length(other)
set.seed(200)
IV1<-unlist(lapply(1:n,function(x)combn(1:n,x,simplify=F)),recursive=F)
list <-  paste(list, collapse = "+")
m1<-lapply(IV1,function(x)
  paste(list,paste(other[x],collapse="+"), collapse="+", sep = "+"))
#Select a combination of other at maximum 15 variables
n_c = length(other)
min = 1
max = 15
temp = 0
for(i in c(min:max)){
  temp = sum(temp, choose(n_c,i))
}
print(temp)


n2 = temp
m1 <- m1[1:n2]
#Create function for xgboost model selection

xgb_rmse <- function(train, test, test_price, IV_list){
  
  #Splitting all variables
  Ivar_list = IV_list[[1]]
  IV = unlist(strsplit(Ivar_list, "\\+"))
  
  train = train[, c(IV,"price")]
  test = test[, c(IV)]
  
  train_x = data.matrix(train[, names(train) != "price"])
  train_y = train[,'price']
  
  test_x = data.matrix(test)
  test_y = test_price
  
  xgb_train = xgb.DMatrix(data = train_x, label = train_y)
  xgb_test = xgb.DMatrix(data = test_x, label = test_y)
  
  
  params <- list(booster = "gbtree", objective = "reg:squarederror", eta=0.1, 
                 gamma=0, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1)
  xgbcv <- xgb.cv( params = params, data = xgb_train, nrounds = 200, nfold = 5, 
                   showsd = T, stratified = T, print_every_n = 10, early_stopping_rounds = 20, maximize = F)
  xgbc <- xgb.train (params = params, data = xgb_train, nrounds = xgbcv$best_iteration, 
                     watchlist = list(train=xgb_train), print_every_n = 10, early_stopping_rounds = 10, maximize = F , eval_metric = "rmse")
  pred_y = predict(xgbc, xgb_test)
  
  rmse_xgb = rmse(test_price, pred_y)
  
  return (rmse_xgb)
}
#Run the models on all combinations in m1 and save into dataframe

xgb_validation = data.frame()

starting_point = 1
DV = 'price'

for (a in starting_point:length(m1))
{
  #Create equation for the model
  model <- reformulate(as.character(m1[a]), DV)
  var_used <- paste0(m1[a], collapse = "+")
  
  #Split data in 80-20 ratio
  set.seed(196)
  split = sample.split(train_df$price,SplitRatio = 0.8)
  train = train_df[split,]
  test = train_df[-split,]
  test_price = test$price
  test = test[, names(test) != "price"]
  
  #Define the list of variables which will be used for model building
  IV_list = m1[a]
  
  #calculating RMSE with this set of variables
  rmse_1 = xgb_rmse(train, test, test_price, IV_list)  
  
  #Appending Modelling results in a single dataframe
  validation_result<- data.frame(rmse_1)
  validation_result$model_num = a
  validation_result$model_eqn = m1[a]
  
  xgb_validation = rbind(xgb_validation,validation_result)
  print(paste("Model Number",a," out of", length(m1), "model. RMSE = ", rmse_1))
  
}


n2 = temp
m1 <- m1[1:n2]
#Create function for xgboost model selection

xgb_rmse <- function(train, test, test_price, IV_list){
  
  #Splitting all variables
  Ivar_list = IV_list[[1]]
  IV = unlist(strsplit(Ivar_list, "\\+"))
  
  train = train[, c(IV,"price")]
  test = test[, c(IV)]
  
  train_x = data.matrix(train[, names(train) != "price"])
  train_y = train[,'price']
  
  test_x = data.matrix(test)
  test_y = test_price
  
  xgb_train = xgb.DMatrix(data = train_x, label = train_y)
  xgb_test = xgb.DMatrix(data = test_x, label = test_y)
  
  
  params <- list(booster = "gbtree", objective = "reg:squarederror", eta=0.1, 
                 gamma=0, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1)
  xgbcv <- xgb.cv( params = params, data = xgb_train, nrounds = 200, nfold = 5, 
                   showsd = T, stratified = T, print_every_n = 10, early_stopping_rounds = 20, maximize = F)
  xgbc <- xgb.train (params = params, data = xgb_train, nrounds = xgbcv$best_iteration, 
                     watchlist = list(train=xgb_train), print_every_n = 10, early_stopping_rounds = 10, maximize = F , eval_metric = "rmse")
  pred_y = predict(xgbc, xgb_test)
  
  rmse_xgb = rmse(test_price, pred_y)
  
  return (rmse_xgb)
}
#Run the models on all combinations in m1 and save into dataframe

xgb_validation = data.frame()

starting_point = 1
DV = 'price'

for (a in starting_point:length(m1))
{
  #Create equation for the model
  model <- reformulate(as.character(m1[a]), DV)
  var_used <- paste0(m1[a], collapse = "+")
  
  #Split data in 80-20 ratio
  set.seed(196)
  split = sample.split(train_df$price,SplitRatio = 0.8)
  train = train_df[split,]
  test = train_df[-split,]
  test_price = test$price
  test = test[, names(test) != "price"]
  
  #Define the list of variables which will be used for model building
  IV_list = m1[a]
  
  #calculating RMSE with this set of variables
  rmse_1 = xgb_rmse(train, test, test_price, IV_list)  
  
  #Appending Modelling results in a single dataframe
  validation_result<- data.frame(rmse_1)
  validation_result$model_num = a
  validation_result$model_eqn = m1[a]
  
  xgb_validation = rbind(xgb_validation,validation_result)
  print(paste("Model Number",a," out of", length(m1), "model. RMSE = ", rmse_1))
  
}

xgbc <- xgb.train (params = params, data = xgb_train, nrounds = xgbcv$best_iteration, watchlist = list(train=xgb_train), print_every_n = 10, early_stopping_rounds = 10, maximize = F , eval_metric = "rmse")


pred_y = predict(xgbc, xgb_test)
#Generating the test file with the predictions
submissionFile = data.frame(id = scoringData$id, price = pred_y)
write.csv(submissionFile, paste0(format(Sys.time(), "%d_%b_%H_%M"),"iter_",bm_iterNum,"_", note,"_", ".csv"), row.names = F)                              
