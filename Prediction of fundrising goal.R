
#remove the previous data in desktop
rm(list=ls())

# setting the working directory
getwd()
setwd("C:/Users/DELL/Desktop")
data_org<-read.csv("ML.csv",encoding="UTF-8")


#----------------------------------------------------#
                           #remove the ID
#-----------------------------------------------------------------------#
library(dplyr)
#No use of ID in machine learning
data<-select(data_org,-ID) 

#always view the structure before pre processing
str(data)

#-------------------------------------------------------------------------#
                           #remove the slogan
#-------------------------------------------------------------------------#
data<-select(data,-brandSlogan)

#-----------------------------------------------------------------#
                      # Data Prep processing
#-----------------------------------------------------------------#
#understang the dataset
#View(data)
head(data)

# number of rows and columns
dim(data)


#exploring the type and numeric of the dataset
summary(data)

#finding the number of missing values
 library("dplyr")
sum(!complete.cases(data))
sum(is.na(data))
sum(is.null(data))

library("mice")

# plotting the missing values
md.pattern(data)
library("VIM")
aggr(data,numbers=TRUE,prop=FALSE)
matrixplot(data)

#correlation between the variables
#install.packages("corrgram")
library("corrgram")
missdata<-data
corrgram(data,cex.labels = 1)
#corrplot(data)

hist(data$coinNum)
hist(data$rating)
hist(data$distributedPercentage)




summary(data)
#------------------------------------------------------------------------#
                      #Preprocessing on platform
#------------------------------------------------------------------------#
table(data$platform)

#remove all the unwanted space and leave one one space between word
library("tidyverse")
data$platform<- str_squish(data$platform)

# viewing the changes in the platform
table(data$platform)

#converting all the uppercase to lower case except the first letter
library("stringr")
data$platform<-str_to_title(data$platform)

# viewing the changes in the platform
table(data$platform)


#converting the similar platforms where there is spelling mistakes into one
data<-mutate(data,platform=recode(.x=platform,"X11"="X11 Blockchain"))
data<-mutate(data,platform=recode(.x=platform,"Stellar Protocol"="Stellar"))
data<-mutate(data,platform=recode(.x=platform,"Eth"="Ethereum"))
data<-mutate(data,platform=recode(.x=platform,"Ethererum"="Ethereum"))
data<-mutate(data,platform=recode(.x=platform,"Etherum"="Ethereum"))
data<-mutate(data,platform=recode(.x=platform,"Btc"="Bitcoin"))




# viewing the changes in the platform
table(data$platform)

#make a new column with length to remove the null values in platform
data<-mutate(data,platform_length=nchar(data$platform))
data<-filter(data,!platform_length==0)
data<-select(data,-platform_length)

#converting the platform that occurs only once into groups since it cannot 
#trained if it occurs in the testing data
frequency <- table(data[["platform"]])
lowValue <- names(frequency[frequency <= 5])
data[["platform"]][data[["platform"]] %in% lowValue] <- "Others"

str(data)
table(data$platform)

#------------------------------------------------------------------------#
                        #Preprocessing on country
#------------------------------------------------------------------------#

table(data$countryRegion)
#converting all the uppercase to lower case except the first letter
data$countryRegion<-str_to_title(data$countryRegion)

#converting the similar platforms where there is spelling mistakes into one
data<-mutate(data,countryRegion=recode(.x=countryRegion,"Curaçao"="Curacao"))
data<-mutate(data,countryRegion=recode(.x=countryRegion,"México"="Mexico"))

# viewing the changes in the Country Region
table(data$countryRegion)

# function to create a new column with all continents 
library(countrycode)
data$continent <- countrycode(sourcevar = data[, "countryRegion"],origin = "country.name",destination = "continent")


table(data$continent)
table(is.na(data$continent))

data$continent<-data$continent%>%replace_na("Unknown") 

table(data$continent)
#remove the NA values in the dataset
#data<-filter(data,!(is.na(data$continent)))

#------------------------------------------------------------------------#
                   #Preprocessing on Start and end date
#------------------------------------------------------------------------#

#convert the start and end date into data format from character

data$startDate<-as.Date(data$startDate,format="%d/%m/%Y")
data$endDate<-as.Date(data$endDate,format="%d/%m/%Y")

class(data$startDate)

str(data)

#finding the difference in time as duration
library(dplyr)
data<-mutate(data,duration=endDate-startDate)
str(data)

#change the type of duration to numeric
data$duration<-as.numeric(data$duration, units="days")

#remove those less than 0 as no event would have ended before it started
data<-filter(data,duration>=0)
#data$duration<-abs(data$duration)

 #-----------------------------------------------------------------------#
                                     #Missing values
#-----------------------------------------------------------------------#

boxplot(data$priceUSD)
hist(data$priceUSD)
boxplot(data$teamSize)

#imputing median for teamsize
data$teamSize[is.na(data$teamSize)]<- median(data$teamSize, na.rm=TRUE)

#imputing median for priceUSD
data$priceUSD[is.na(data$priceUSD)]<-median(data$priceUSD,na.rm=TRUE)

#imputed<- mice( subset(data, select = c('priceUSD','teamSize')), m = 10, maxit = 5)
#final_imputed<-complete(imputed)

#data$teamSize<-final_imputed$teamSize
#data$priceUSD<-final_imputed$priceUSD


#aggr(data,numbers=TRUE,prop=FALSE)
#---------------------------------------------------------------------#
                          #Feature Extraction
#---------------------------------------------------------------------#

# we need to do feature extraction so we already grouped the date hence we removed
#the unnecessary data
data<-select(data,-countryRegion)
data<-select(data,-startDate)
data<-select(data,-endDate)

str(data)


boxplot(data$priceUSD)

#---------------------------------------------------------------------#
                            #Outliers
#---------------------------------------------------------------------#

#iqr<-IQR(data$priceUSD)
#upper<-quantile(data$priceUSD,0.75)+iqr*1.5
#lower<-quantile(data$priceUSD,0.25)-iqr*1.5
#library(dplyr)
#data<-filter(data,priceUSD >= lower & priceUSD <= upper)
#dim(data)
#str(data)

 
mean_d<-mean(data$priceUSD)
sd_d<-sd(data$priceUSD)
lower_bound<-mean_d -(4.753*sd_d)
upper_bound<-mean_d + (4.753*sd_d)

data_check<-subset(data,priceUSD>=lower_bound & priceUSD<=upper_bound)

boxplot(data_check$priceUSD)
View(data_check)
boxplot(data_check$distributedPercentage)
data_check<-filter(data_check,distributedPercentage<100)
table(data_check$distributedPercentage<100)
boxplot(data_check$distributedPercentage)
boxplot(data_check$duration)

data_check<-filter(data_check,duration<1000)
View(data_check)
boxplot(data_check$duration)
summary(data_org)

str(data_check)
#----------------------------------------------------------------#
                           #decision tree
#----------------------------------------------------------------#
DT_data<- data_check

#converting the label into factors 
DT_data$success<-factor(DT_data$success,level=c("N","Y"),labels=c("No","Yes"))
DT_data$hasVideo<-factor(DT_data$hasVideo,level=c(1,0),labels=c("Yes","No"))
DT_data$hasGithub<-factor(DT_data$hasGithub,level=c(1,0),labels=c("Yes","No"))
DT_data$hasReddit<-factor(DT_data$hasReddit,level=c(1,0),labels=c("Yes","No"))
DT_data$minInvestment<-factor(DT_data$minInvestment,level=c(1,0),labels=c("Yes","No"))

str(DT_data)


#---------------------------------------------------------------------#
                    #separating data for training and testing
#---------------------------------------------------------------------#
#setting the 90 percent for training and using floor for rounding off
sample_size<-floor(0.90*nrow(DT_data))
?floor
set.seed(11)
train_index<-sample(nrow(DT_data),sample_size)
DT_data_train<-DT_data[train_index,]
DT_data_test<-DT_data[-train_index,]


#library(e1071)
#sms_classifier <- naiveBayes(ANN_data_train, ANN_data_train$success)

## Step 4: Evaluating model performance ----
#sms_test_pred <- predict(sms_classifier, select(ANN_data_test,-success))
#confusionMatrix(sms_test_pred,ANN_data_test$success,positive = "Yes")
#sensitivity(sms_test_pred,ANN_data_test$success,positive = "Yes")

#library for desicion tree
library(C50)
library(tidyverse)
library(rpart.plot)
library(rpart)
#creating the model
DT_model <- C5.0(select(DT_data_train, -success), DT_data_train$success)
DT_model
summary(DT_model)
plot(DT_model)


#predicting the model
DT_prediction <- predict(DT_model,DT_data_test)
#predicting the model in numeric(probability)
DT_prediction_prob<-predict(DT_model,DT_data_test,type="prob")
options(scipen = 100) 
head(DT_prediction_prob)
#creating a table with these prob for further check
DT_results <- data.frame(actual_type = DT_data_test$success,predict_type = DT_prediction, prob_Yes = round(DT_prediction_prob[ , 2], 4),prob_No = round(DT_prediction_prob[ , 1], 4))
head(DT_results)

#running the ROC Curve
library(ROCR)
library(pROC)


DT_pred_object <- prediction(DT_results$prob_Yes,DT_results$actual_type)
DT_ROC <- performance(DT_pred_object, measure = "tpr", x.measure = "fpr")

plot(DT_ROC, main = "ROC curve", col = "green", lwd = 2)
abline(a = 0, b = 1, lwd = 2, lty = 2)

#Create the AUC object
DT_auc_object<- performance(DT_pred_object,measure="auc")
DT_AUC<-DT_auc_object@y.values[[1]]
DT_AUC

#accuracy testing of model 
library(caret)
confusionMatrix(DT_results$predict_type, DT_results$actual_type, positive = "Yes")
sensitivity(DT_results$predict_type, DT_results$actual_type, positive = "Yes")
specificity(DT_results$predict_type, DT_results$actual_type, negative = "No")
posPredValue(DT_results$predict_type, DT_results$actual_type, positive = "Yes") 

#----------------------------------------#
#cross validation for Decision tree 
#----------------------------------------#

library(pROC)
library(C50)
library(tidyverse)
K1 = 10
set.seed(11)
# create the fold with actual class vector, k is the number of folds
folds <- createFolds(DT_data$success, k = K1)
# check the folds
str(folds)

# the list used to store result for each fold
DT_accuracy_list<-as.numeric()
DT_sensitivity_list<-as.numeric() 
DT_specificity_list<-as.numeric()
DT_precision_list<-as.numeric()

# loop, from fold 1 to fold K:
for(i in 1:K1){
  
  # for each fold, we conduct training and testing
  
  # use fold i for testing
  DT_fold_test <- DT_data[folds[[i]],]
  # use the remaining 9 folds for training
  DT_fold_train <- DT_data[-folds[[i]],] 
  
  # train the model
  DT_fold_model <- C5.0(select(DT_fold_train, -success), DT_fold_train$success) 
  
  # get predicted class labels for testing data
  DT_fold_prediction <- predict(DT_fold_model, DT_fold_test, type='class')
  DT_fold_confusion <- confusionMatrix(DT_fold_prediction, DT_fold_test$success, positive = "Yes")
  
  # use str(fold_cm_DT) to see how the fold_cm_DT is organised and then you can find way to access the variables inside it
  DT_fold_accuracy <- DT_fold_confusion$overall['Accuracy']
  DT_fold_sensitivity <- DT_fold_confusion$byClass['Sensitivity'] 
  DT_fold_specificity <- DT_fold_confusion$byClass['Specificity']
  DT_fold_precision <- DT_fold_confusion$byClass['Precision'] 
  
  # append the result to the accuracy list for each fold
  DT_accuracy_list<- append(DT_accuracy_list,DT_fold_accuracy)
  DT_sensitivity_list <-append(DT_sensitivity_list,DT_fold_sensitivity)
  DT_specificity_list<-append(DT_specificity_list,DT_fold_specificity)
  DT_precision_list<- append(DT_precision_list,DT_fold_precision)
}

DT_accuracy_list

# compute the average accuracy over 10 folds
DT_accuracy_avg<- mean(DT_accuracy_list)
DT_sensitivity_avg<-mean(DT_sensitivity_list)
DT_specificity_avg<-mean(DT_specificity_list)
DT_precision_avg<-mean(DT_precision_list)


#--------------------------------------------------------------------------------#
                                #adaptive boost model
#--------------------------------------------------------------------------------#
#creating the model
ADT_model <- C5.0(select(DT_data_train, -success), DT_data_train$success,trials = 10)
ADT_model
summary(ADT_model)

#predicting the model
ADT_prediction <- predict(ADT_model,DT_data_test)
#predicting the model in numeric(probability)
ADT_prediction_prob<-predict(ADT_model,DT_data_test,type="prob")
options(scipen = 100) 
head(ADT_prediction_prob)
#creating a table with these prob for further check
ADT_results <- data.frame(actual_type = DT_data_test$success,predict_type = ADT_prediction, 
                          prob_Yes = round(ADT_prediction_prob[ , 2], 4),prob_No = round(ADT_prediction_prob[ , 1], 4))
head(ADT_results)

#running the ROC Curve
library(ROCR)

ADT_pred_object <- prediction(ADT_results$prob_Yes,ADT_results$actual_type)
ADT_ROC <- performance(ADT_pred_object, measure = "tpr", x.measure = "fpr")
plot(ADT_ROC, main = "ROC curve DT", col = "red", lwd = 2,add=TRUE)
abline(a = 0, b = 1, lwd = 2, lty = 2)

#Create the AUC object
ADT_auc_object<- performance(ADT_pred_object,measure="auc")
ADT_AUC<-ADT_auc_object@y.values[[1]]
ADT_AUC

#accuracy testing of model 
library(caret)
confusionMatrix(ADT_results$predict_type, ADT_results$actual_type, positive = "Yes")
sensitivity(ADT_results$predict_type, ADT_results$actual_type, positive = "Yes")
specificity(ADT_results$predict_type, ADT_results$actual_type, negative = "No")
posPredValue(ADT_results$predict_type, ADT_results$actual_type, positive = "Yes") 

#----------------------------------------#
#cross validation for  Adaptive Decision tree 
#----------------------------------------#

library(pROC)
library(C50)
library(tidyverse)
K1 = 10
set.seed(11)
# create the fold with actual class vector, k is the number of folds
folds <- createFolds(DT_data$success, k = K1)
# check the folds
str(folds)

# the list used to store result for each fold
ADT_accuracy_list<-as.numeric()
ADT_sensitivity_list<-as.numeric() 
ADT_specificity_list<-as.numeric()
ADT_precision_list<-as.numeric()

# loop, from fold 1 to fold K:
for(i in 1:K1){
  
  # for each fold, we conduct training and testing
  
  # use fold i for testing
  ADT_fold_test <- DT_data[folds[[i]],]
  # use the remaining 9 folds for training
  ADT_fold_train <- DT_data[-folds[[i]],] 
  
  # train the model
  ADT_fold_model <- C5.0(select(ADT_fold_train, -success), ADT_fold_train$success,trials=5) 
  
  # get predicted class labels for testing data
  ADT_fold_prediction <- predict(ADT_fold_model, ADT_fold_test, type='class')
  ADT_fold_confusion <- confusionMatrix(ADT_fold_prediction, ADT_fold_test$success, positive = "Yes")
  
  # use str(fold_cm_DT) to see how the fold_cm_DT is organised and then you can find way to access the variables inside it
  ADT_fold_accuracy <- ADT_fold_confusion$overall['Accuracy']
  ADT_fold_sensitivity <- ADT_fold_confusion$byClass['Sensitivity'] 
  ADT_fold_specificity <- ADT_fold_confusion$byClass['Specificity']
  ADT_fold_precision <- ADT_fold_confusion$byClass['Precision'] 
  
  # append the result to the accuracy list for each fold
  ADT_accuracy_list<- append(ADT_accuracy_list,ADT_fold_accuracy)
  ADT_sensitivity_list <-append(ADT_sensitivity_list,ADT_fold_sensitivity)
  ADT_specificity_list<-append(ADT_specificity_list,ADT_fold_specificity)
  ADT_precision_list<- append(ADT_precision_list,ADT_fold_precision)
}

ADT_accuracy_list

# compute the average accuracy over 10 folds
ADT_accuracy_avg<- mean(ADT_accuracy_list)
ADT_sensitivity_avg<-mean(ADT_sensitivity_list)
ADT_specificity_avg<-mean(ADT_specificity_list)
ADT_precision_avg<-mean(ADT_precision_list)


#-------------------------------------------------------------------------------#
                                         #SVM
#-------------------------------------------------------------------------------#

#for creating dummy coding for the countryRegion and Platforms

#run this function for dummy coding
#install.packages("fastDummies")
library('fastDummies')
str(data)

#for creating dummy variable  
SVM_data <- dummy_cols(data_check, select_columns = c('platform','continent'),remove_selected_columns = TRUE)
str(SVM_data)




#converting the labels into factor
SVM_data$success<-factor(SVM_data$success,labels=c("No","Yes"))

# determining the same sample size as previous
sample_size<-floor(0.90*nrow(SVM_data))

set.seed(11)
train_index<-sample(nrow(SVM_data),sample_size)
SVM_data_train<-SVM_data[train_index,]
SVM_data_test<-SVM_data[-train_index,]

#prediction for SVM
library(kernlab)
SVM_classifier <- ksvm(success~ ., data = SVM_data_train,kernel = "polydot",C=1)
SVM_prediction <- predict(SVM_classifier, select(SVM_data_test,-success))
head(SVM_prediction)
?ksvm
#predicting the model in numeric(probability)
SVM_classifier_prob <- ksvm( success~ ., data = SVM_data_train,kernel = "polydot",C=1,prob.model=TRUE)
SVM_prediction_prob<-predict(SVM_classifier_prob,select(SVM_data_test,-success),type="probabilities")
options(scipen = 100)
head(SVM_prediction_prob)
#creating a table with these prob for further check
SVM_results <- data.frame(actual_type = SVM_data_test$success,predict_type = SVM_prediction, prob_Yes = round(SVM_prediction_prob[ , 2], 4),prob_No = round(SVM_prediction_prob[ , 1], 4))
head(SVM_results)


#ROC curve for SVM
SVM_pred_object <- prediction(SVM_results$prob_Yes,SVM_results$actual_type)
SVM_ROC <- performance(SVM_pred_object, measure = "tpr", x.measure = "fpr")
plot(SVM_ROC, main = "ROC curve SVM", col = "yellow", lwd = 2,add=TRUE)
abline(a = 0, b = 1, lwd = 2, lty = 2)

SVM_auc_object<- performance(SVM_pred_object,measure="auc")
SVM_AUC<-SVM_auc_object@y.values[[1]]
SVM_AUC


#checking performance 
library(caret)
confusionMatrix(SVM_results$predict_type, SVM_results$actual_type, positive = "Yes")
sensitivity(SVM_results$predict_type, SVM_results$actual_type, positive = "Yes")
specificity(SVM_results$predict_type, SVM_results$actual_type, negative = "No")
posPredValue(SVM_results$predict_type, SVM_results$actual_type, positive = "Yes") 

#--------------------------------------------------------------------#
                                   #k folding svm
#--------------------------------------------------------------------#
K2 = 10
set.seed(11)
# first parameter is the actual class vector, k is the number of folds
folds <- createFolds(SVM_data$success, k = K2)
# check the folds( 10 subsets of the IDs for data samples)
str(folds)

# the list used to store result for each fold, so we can compute the average performance later
SVM_accuracy_list<-as.numeric()
SVM_sensitivity_list<-as.numeric() 
SVM_specificity_list<-as.numeric()
SVM_precision_list<-as.numeric()


# loop, from fold 1 to fold K:
for(i in 1:K2){
  
  # for each fold, we conduct training and testing
  
  # use fold i for testing
  SVM_fold_test <- SVM_data[folds[[i]],]
  # use the remaining 9 folds for training
  SVM_fold_train <- SVM_data[-folds[[i]],] 
  
  # train the model
  SVM_fold_model <- ksvm( success~ ., data = SVM_fold_train,kernel = "polydot",C=1)
  
  # get predicted class labels for testing data
  SVM_fold_prediction <- predict(SVM_fold_model, select(SVM_fold_test,-success))
  SVM_fold_confusion <- confusionMatrix(SVM_fold_prediction,SVM_fold_test$success, positive = "Yes")
  
  # use str(fold_cm_DT) to see how the fold_cm_DT is organised and then you can find way to access the variables inside it
  SVM_fold_accuracy <- SVM_fold_confusion$overall['Accuracy']
  SVM_fold_sensitivity <- SVM_fold_confusion$byClass['Sensitivity'] 
  SVM_fold_specificity <- SVM_fold_confusion$byClass['Specificity']
  SVM_fold_precision <- SVM_fold_confusion$byClass['Precision'] 
  
  # append the result to the accuracy list for each fold
  SVM_accuracy_list<- append(SVM_accuracy_list,SVM_fold_accuracy)
  SVM_sensitivity_list <-append(SVM_sensitivity_list,SVM_fold_sensitivity)
  SVM_specificity_list<-append(SVM_specificity_list,SVM_fold_specificity)
  SVM_precision_list<- append(SVM_precision_list,SVM_fold_precision)
}



# compute the average accuracy over 10 folds
SVM_accuracy_avg<- mean(SVM_accuracy_list)
SVM_sensitivity_avg<-mean(SVM_sensitivity_list)
SVM_specificity_avg<-mean(SVM_specificity_list)
SVM_precision_avg<-mean(SVM_precision_list)
  
#---------------------------------------------------------------------#
      #VSVM
#---------------------------------------------------------------------#
library(kernlab)
VSVM_classifier <- ksvm(success~ ., data = SVM_data_train,kernel = "vanilladot",C=1)
VSVM_prediction <- predict(SVM_classifier, select(SVM_data_test,-success))
head(VSVM_prediction)
?ksvm
#predicting the model in numeric(probability)
VSVM_classifier_prob <- ksvm( success~ ., data = SVM_data_train,kernel = "vanilladot",C=1,prob.model=TRUE)
VSVM_prediction_prob<-predict(VSVM_classifier_prob,select(SVM_data_test,-success),type="probabilities")
options(scipen = 100)
head(VSVM_prediction_prob)
#creating a table with these prob for further check
VSVM_results <- data.frame(actual_type = SVM_data_test$success,predict_type = VSVM_prediction, prob_Yes = round(VSVM_prediction_prob[ , 2], 4),prob_No = round(VSVM_prediction_prob[ , 1], 4))
head(VSVM_results)


#ROC curve for SVM
VSVM_pred_object <- prediction(VSVM_results$prob_Yes,VSVM_results$actual_type)
VSVM_ROC <- performance(VSVM_pred_object, measure = "tpr", x.measure = "fpr")
plot(VSVM_ROC, main = "ROC curve SVM", col = "brown", lwd = 2,add=TRUE)
abline(a = 0, b = 1, lwd = 2, lty = 2)

VSVM_auc_object<- performance(VSVM_pred_object,measure="auc")
VSVM_AUC<-VSVM_auc_object@y.values[[1]]
VSVM_AUC


#checking performance 
library(caret)
confusionMatrix(VSVM_results$predict_type, VSVM_results$actual_type, positive = "Yes")
sensitivity(VSVM_results$predict_type, VSVM_results$actual_type, positive = "Yes")
specificity(VSVM_results$predict_type, VSVM_results$actual_type, negative = "No")
posPredValue(VSVM_results$predict_type, VSVM_results$actual_type, positive = "Yes") 

#--------------------------------------------------------------------#
#k folding svm
#--------------------------------------------------------------------#
K2 = 10
set.seed(11)
# first parameter is the actual class vector, k is the number of folds
folds <- createFolds(SVM_data$success, k = K2)
# check the folds( 10 subsets of the IDs for data samples)
str(folds)

# the list used to store result for each fold, so we can compute the average performance later
VSVM_accuracy_list<-as.numeric()
VSVM_sensitivity_list<-as.numeric() 
VSVM_specificity_list<-as.numeric()
VSVM_precision_list<-as.numeric()


# loop, from fold 1 to fold K:
for(i in 1:K2){
  
  # for each fold, we conduct training and testing
  
  # use fold i for testing
  VSVM_fold_test <- SVM_data[folds[[i]],]
  # use the remaining 9 folds for training
  VSVM_fold_train <- SVM_data[-folds[[i]],] 
  
  # train the model
  VSVM_fold_model <- ksvm( success~ ., data = SVM_fold_train,kernel = "vanilladot",C=1)
  
  # get predicted class labels for testing data
  VSVM_fold_prediction <- predict(VSVM_fold_model, select(VSVM_fold_test,-success))
  VSVM_fold_confusion <- confusionMatrix(VSVM_fold_prediction,VSVM_fold_test$success, positive = "Yes")
  
  # use str(fold_cm_DT) to see how the fold_cm_DT is organised and then you can find way to access the variables inside it
  VSVM_fold_accuracy <- VSVM_fold_confusion$overall['Accuracy']
  VSVM_fold_sensitivity <- VSVM_fold_confusion$byClass['Sensitivity'] 
  VSVM_fold_specificity <- VSVM_fold_confusion$byClass['Specificity']
  VSVM_fold_precision <- VSVM_fold_confusion$byClass['Precision'] 
  
  # append the result to the accuracy list for each fold
  VSVM_accuracy_list<- append(VSVM_accuracy_list,VSVM_fold_accuracy)
  VSVM_sensitivity_list <-append(VSVM_sensitivity_list,VSVM_fold_sensitivity)
  VSVM_specificity_list<-append(VSVM_specificity_list,VSVM_fold_specificity)
  VSVM_precision_list<- append(VSVM_precision_list,VSVM_fold_precision)
}



# compute the average accuracy over 10 folds
VSVM_accuracy_avg<- mean(VSVM_accuracy_list)
VSVM_sensitivity_avg<-mean(VSVM_sensitivity_list)
VSVM_specificity_avg<-mean(VSVM_specificity_list)
VSVM_precision_avg<-mean(VSVM_precision_list)



#------------------------------------------------------------------------#
                                   #KNN
#------------------------------------------------------------------------#

#duplicating the data
KNN_data<-SVM_data

#for knn it should in small ranges like 0-1 hence we should normalise
str(KNN_data)
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#applying the normalize for all columns and converting it into data frame
KNN_data_norm <- as.data.frame(lapply(KNN_data[2:28], normalize))

str(KNN_data_norm)

#determing the sample size
sample_size<-floor(0.90*nrow(KNN_data))

set.seed(11)
train_index<-sample(nrow(KNN_data),sample_size)
KNN_data_train<-KNN_data_norm[train_index,]
KNN_data_test<-KNN_data_norm[-train_index,]
KNN_data_train_labels<-KNN_data[train_index,1]
KNN_data_test_labels<-KNN_data[-train_index,1]
library(class)
library(class)
??knn3
#prediction for knn
K=49
KNN_prediction <- knn(train = KNN_data_train, test = KNN_data_test, 
                     cl =KNN_data_train_labels,k=K)

#KNN_prediction_prob <- knn(train = KNN_data_train, test = KNN_data_test, 
                         # cl =KNN_data_train_labels, k=K, prob=TRUE)

KNN_prediction_prob<-predict(caret::knn3(KNN_data_train, KNN_data_train_labels, k = K), KNN_data_test)
head(KNN_prediction_prob)


#converting the results in a data frame
#KNN_prob<-attributes(KNN_prediction_prob)$prob

KNN_results<- data.frame(actual_type = KNN_data_test_labels,
                         predict_type = KNN_prediction, 
                         prob_Yes = round(KNN_prediction_prob[,2],4),prob_No= round(KNN_prediction_prob[,1], 4))

head(KNN_results)



#ROC curve for knn
KNN_pred_object <- prediction(KNN_results$prob_Yes,KNN_results$actual_type)
KNN_ROC <- performance(KNN_pred_object, measure = "tpr", x.measure = "fpr")
plot(KNN_ROC, main = "ROC curve Knn", col = "black", lwd = 2,add=TRUE)
abline(a = 0, b = 1, lwd = 2, lty = 2)

KNN_auc_object<- performance(KNN_pred_object,measure="auc")
KNN_AUC<-KNN_auc_object@y.values[[1]]
KNN_AUC

library(caret)
#normal accuracy for knn
confusionMatrix(KNN_results$predict_type, KNN_results$actual_type, positive = "Yes")
#-----------------------------------------------#
                     #k folding
#-------------------------------------------------#
K3 = 10
set.seed(11)
# first parameter is the actual class vector, k is the number of folds
folds <- createFolds(KNN_data$success, k = K3)
# check the folds( 10 subsets of the IDs for data samples)
str(folds)

# the list used to store result for each fold, so we can compute the average performance later
KNN_accuracy_list<-as.numeric()
KNN_sensitivity_list<-as.numeric() 
KNN_specificity_list<-as.numeric()
KNN_precision_list<-as.numeric()

# loop, from fold 1 to fold K:
for(i in 1:K3){
  
  # for each fold, we conduct training and testing
  
  # use fold i for testing
  KNN_fold_test <- KNN_data_norm[folds[[i]],]
  # use the remaining 9 folds for training
  KNN_fold_train <- KNN_data_norm[-folds[[i]],]
  
  KNN_fold_test_labels<-KNN_data[folds[[i]],1]
  KNN_fold_train_labels<-KNN_data[-folds[[i]],1]
  
  # train the model
  
  KNN_fold_model <- knn(train = KNN_fold_train, test = KNN_fold_test, 
                       cl =KNN_fold_train_labels,k=K)
  
 
  
  KNN_fold_confusion<-confusionMatrix(KNN_fold_model, KNN_fold_test_labels, positive = "Yes")
  
  # use str(fold_cm_DT) to see how the fold_cm_DT is organised and then you can find way to access the variables inside it
  KNN_fold_accuracy <- KNN_fold_confusion$overall['Accuracy']
  KNN_fold_sensitivity <- KNN_fold_confusion$byClass['Sensitivity'] # is also called recall
  KNN_fold_specificity <- KNN_fold_confusion$byClass['Specificity']
  KNN_fold_precision <- KNN_fold_confusion$byClass['Precision'] # Precision is also called Positive Predictive Value (PPV)
  
 
  
  # append the result to the accuracy list for each fold
  KNN_accuracy_list<- append(KNN_accuracy_list,KNN_fold_accuracy)
  KNN_sensitivity_list <-append(KNN_sensitivity_list,KNN_fold_sensitivity)
  KNN_specificity_list<-append(KNN_specificity_list,KNN_fold_specificity)
  KNN_precision_list<- append(KNN_precision_list,KNN_fold_precision)
}
# compute the average accuracy over 10 folds
KNN_accuracy_avg<- mean(KNN_accuracy_list)
KNN_sensitivity_avg<-mean(KNN_sensitivity_list)
KNN_specificity_avg<-mean(KNN_specificity_list)
KNN_precision_avg<-mean(KNN_precision_list)


#---------------------------------------------------------------------------------------------------#
                                       #random forest tree
#-------------------------------------------------------------------------------------------#

library(randomForest)
RF_model <- randomForest(success ~ ., data=DT_data_train, ntree=500,importance=TRUE)
RF_prediction <- predict(RF_model, DT_data_test)
RF_prediction_prob <- predict(RF_model, DT_data_test,type="prob")
options(scipen=100)
head(RF_prediction_prob)
RF_results <- data.frame(actual_type=DT_data_test$success,predicted_type=RF_prediction, prob_Yes = round(RF_prediction_prob[,2],4),prob_No=round(RF_prediction_prob[,1],4))
RF_model

library(ROCR)
?randomForest
RF_pred_object<- prediction(RF_results$prob_Yes,RF_results$actual_type)
RF_ROC <- performance(RF_pred_object, measure = "tpr", x.measure = "fpr")
plot(RF_ROC, main = "ROC curve RF", col = "blue", lwd = 2,add=TRUE)
abline(a = 0, b = 1, lwd = 2, lty = 2)

RF_auc_object<- performance(RF_pred_object,measure="auc")
RF_AUC<-RF_auc_object@y.values[[1]]
RF_AUC
library(caret)
confusionMatrix(RF_prediction,DT_data_test$success,positive = "Yes")
sensitivity(RF_prediction,DT_data_test$success , positive = "Yes")
specificity(RF_prediction, DT_data_test$success, negative = "No")
posPredValue(RF_prediction, DT_data_test$success, positive = "Yes")

#--------------------------------------------------------------------#
                       #k folding random forest
#--------------------------------------------------------------------#
K4 = 10
set.seed(11)
# first parameter is the actual class vector, k is the number of folds
folds <- createFolds(DT_data$success, k = K4)
# check the folds(
str(folds)

# the list used to store result for each fold
RF_accuracy_list<-as.numeric()
RF_sensitivity_list<-as.numeric() 
RF_specificity_list<-as.numeric()
RF_precision_list<-as.numeric()


# loop, from fold 1 to fold K:
for(i in 1:K4){
  
  # for each fold, we conduct training and testing
  
  # use fold i for testing
  RF_fold_test <- DT_data[folds[[i]],]
  # use the remaining 9 folds for training
  RF_fold_train <- DT_data[-folds[[i]],] 
  
  # train the model
  RF_fold_model <- randomForest(success ~ ., data=RF_fold_train, ntree=500,importance=TRUE)
  
  # get predicted class labels for testing data
  RF_fold_prediction <- predict(RF_fold_model, RF_fold_test)
  RF_fold_confusion <- confusionMatrix(RF_fold_prediction,RF_fold_test$success, positive = "Yes")
  
  # use str(fold_cm_DT) to see how the fold_cm_DT is organised and then you can find way to access the variables inside it
  RF_fold_accuracy <- RF_fold_confusion$overall['Accuracy']
  RF_fold_sensitivity <-RF_fold_confusion$byClass['Sensitivity'] 
  RF_fold_specificity <-RF_fold_confusion$byClass['Specificity']
  RF_fold_precision <- RF_fold_confusion$byClass['Precision'] 
  
 
  # append the result to the accuracy list for each fold
  RF_accuracy_list<- append(RF_accuracy_list,RF_fold_accuracy)
  RF_sensitivity_list <-append(RF_sensitivity_list,RF_fold_sensitivity)
  RF_specificity_list<-append(RF_specificity_list,RF_fold_specificity)
  RF_precision_list<- append(RF_precision_list,RF_fold_precision)
}



# compute the average accuracy over 10 folds

RF_accuracy_avg<- mean(RF_accuracy_list)
RF_sensitivity_avg<-mean(RF_sensitivity_list)
RF_specificity_avg<-mean(RF_specificity_list)
RF_precision_avg<-mean(RF_precision_list)



legend("bottomright",c("DT","Ada Boost DT","SVM","SVM VanillaDot","KNN","RF","ANN"), lwd=3, col = c("green", "red","yellow","brown","black","blue","orange"))

#-------------------------------------------------------------------------------#
                                  #Neural Networks
#-------------------------------------------------------------------------------#
ANN_data<- SVM_data
str(ANN_data)
#convert the labels into dummy code
ANN_data$success <- ifelse(ANN_data$success == 'Yes', 1, 0)
str(ANN_data)
View(data_ann)

#function for normalize 
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#applying the normalize for all columns and converting it into data frame
ANN_data_norm <- as.data.frame(lapply(ANN_data, normalize))



#ANN_data_norm$success<-factor(ANN_data_norm$success,level=c(0,1),labels=c("No","Yes"))

#determing the sample size
sample_size<-floor(0.90*nrow(ANN_data_norm))

set.seed(11)
train_index<-sample(nrow(ANN_data_norm),sample_size)
ANN_data_train<-ANN_data_norm[train_index,]
ANN_data_test<-ANN_data_norm[-train_index,]

library(neuralnet)

?neuralnet
# by default, hidden =1, contains one hidden layer with one node
ANN_model <- neuralnet(formula = success ~ .,
                       data = ANN_data_train, hidden=3)
# single layer network
ANN_model_linear <- neuralnet(formula = success ~ .,
                              data = ANN_data_train,hidden=3)


# visualize the network topology
#plot(ANN_model)
#plot(ANN_model_linear)


library(tidyverse)
library(Metrics)
#obtain predicted strength values

ANN_prediction<- predict(ANN_model_linear, select(ANN_data_test, -success),type ="raw")
head(ANN_prediction)

ANN_predicted_results <- data.frame(ifelse(ANN_prediction > 0.5, "1", "0"))

names(ANN_predicted_results)[1] <- "Yes"




library(caret)
confusionMatrix(as.factor(ANN_predicted_results$Yes),as.factor(ANN_data_test$success),positive="1")


ANN_results <- data.frame(actual_type = as.factor(ANN_data_test$success),
                          predict_type = as.factor(ifelse(ANN_prediction > 0.5, "1", "0")),
                          prob_Yes = abs(round(ANN_prediction,5)))
head(ANN_results)
# 
# for (i in 1:nrow(ann_results)) {
#   ann_results$prob_success[i] <- (1-ann_results$prob_fail[i])
# }


pred_object_rocr_ann <- prediction(ANN_results$prob_Yes, ANN_results$actual_type)
ANN_roc <- performance(pred_object_rocr_ann, measure = "tpr", x.measure = "fpr")

ANN_auc_object <- performance(pred_object_rocr_ann, measure = "auc")


ANN_auc <- ANN_auc_object@y.values[[1]]
ANN_auc
# plot the ROC curve 
plot(ANN_roc, col = "orange", lwd =2,add=TRUE)
abline(a = 0, b = 1, lwd = 2, lty = 2)



legend("bottomright",c("DT","Ada Boost DT","SVM","SVM VanillaDot","KNN","RF","ANN"), lwd=3, col = c("green", "red","yellow","brown","black","blue","orange"))
K5 = 10
set.seed(11)
# first parameter is the actual class vector, k is the number of folds
folds <- createFolds(ANN_data$success, k = K5)
# check the folds(
str(folds)

# the list used to store result for each fold
ANN_accuracy_list<-as.numeric()
ANN_sensitivity_list<-as.numeric() 
ANN_specificity_list<-as.numeric()
ANN_precision_list<-as.numeric()


# loop, from fold 1 to fold K:
for(i in 1:K4){
  
  # for each fold, we conduct training and testing
  
  # use fold i for testing
  ANN_fold_test <- ANN_data_norm[folds[[i]],]
  # use the remaining 9 folds for training
  ANN_fold_train <- ANN_data_norm[-folds[[i]],] 
  
  
  ANN_Fold_model <- neuralnet(formula = success ~ .,
                         data = ANN_fold_train,hidden = 3)
  # train the model
  ANN_fold_prediction<- predict(ANN_Fold_model, select(ANN_fold_test, -success),type ="raw")
  ANN_fold_results <- data.frame(ifelse(ANN_fold_prediction > 0.5, "1", "0"))
  names(ANN_fold_results)[1] <- "Yes"
 
  library(caret)
  ANN_fold_confusion<-confusionMatrix(as.factor(ANN_fold_results$Yes),as.factor(ANN_fold_test$success),positive="1")
  
  # use str(fold_cm_DT) to see how the fold_cm_DT is organised and then you can find way to access the variables inside it
  ANN_fold_accuracy <- ANN_fold_confusion$overall['Accuracy']
  ANN_fold_sensitivity <-ANN_fold_confusion$byClass['Sensitivity'] 
  ANN_fold_specificity <-ANN_fold_confusion$byClass['Specificity']
  ANN_fold_precision <- ANN_fold_confusion$byClass['Precision'] 
  
  
  # append the result to the accuracy list for each fold
  ANN_accuracy_list<- append(ANN_accuracy_list,ANN_fold_accuracy)
  ANN_sensitivity_list <-append(ANN_sensitivity_list,ANN_fold_sensitivity)
  ANN_specificity_list<-append(ANN_specificity_list,ANN_fold_specificity)
  ANN_precision_list<- append(ANN_precision_list,ANN_fold_precision)
}



# compute the average accuracy over 10 folds

ANN_accuracy_avg<- mean(ANN_accuracy_list)
ANN_sensitivity_avg<-mean(ANN_sensitivity_list)
ANN_specificity_avg<-mean(ANN_specificity_list)
ANN_precision_avg<-mean(ANN_precision_list)





#install.packages("tm")
#install.packages("tokenizers")
#install.packages("SnowballC")
#install.packages("textstem")
#install.packages('gutenbergr')

# Add packages to Library
library(tm)
library(tokenizers)
library(textstem)
library(gutenbergr)

text<-data_org$brandSlogan
platocorpus <- SimpleCorpus(VectorSource(text))

dtm <- DocumentTermMatrix(platocorpus, control = list(
  removePunctuation = TRUE, removeNumbers = TRUE, 
  stopwords =  TRUE, tolower = TRUE, 
  wordLengths=c(1,Inf)))
findFreqTerms(dtm,50)
dtms <- removeSparseTerms(dtm, 0.99)
library("wordcloud")
freq = data.frame(sort(colSums(as.matrix(dtms)), decreasing=TRUE))
wordcloud(rownames(freq),freq[,1],min.words = 50)

