#### 0. INCLUDES ####
#Load Libraries: p_load can install, load,  and update packages
if(require("pacman")=="FALSE"){
  install.packages("pacman")
}

pacman::p_load(rstudioapi, dplyr,magrittr, tidyr, reshape2, readxl, stringi,
               ggplot2,caret,corrplot,rpart,e1071)

# Setwd (1º current wd where is the script, then we move back to the
# general folder)
current_path = getActiveDocumentContext()$path
setwd(dirname(current_path))
setwd("..")
rm(current_path)

#Data sets
existing<-read.csv("../multiple_regression/blackwells_multiple_regression/datasets/existingproductattributes2017.csv")
new<-read.csv("../multiple_regression/blackwells_multiple_regression/datasets/newproductattributes2017.csv")
str(existing)


#### 1. PRE-PROCESSING ####

#deleting attributes with NA values
summary(existing) # $BestSellersRank NA's:15
#removing attribute (column)
existing$BestSellersRank<-NULL

#removing outliers regarding volume (productNum 198 & 150)
existing_out<-existing[-c(50,73),]


#dummify the data
exis_dummy <- dummyVars(" ~ .", data = existing_out)

readyData<-data.frame(predict(exis_dummy,newdata = existing_out))


#### 1.1 Correlation matrices ####

#all variables
corr_all<-cor(readyData)
corr_all
corrplot(corr_all)
?corrplot

#removing lower and higher correlate attributes from data set
#creating a new data frame to remove irrelevant attributes
readyData_rel<-readyData

#removing irrelevant attributes
readyData_rel$Price<-NULL
readyData_rel$x5StarReviews<-NULL
readyData_rel$x2StarReviews<-NULL
readyData_rel$x1StarReviews<-NULL
readyData_rel$NegativeServiceReview<-NULL
readyData_rel$Recommendproduct<-NULL
readyData_rel$ShippingWeight<-NULL
readyData_rel$ProductDepth<-NULL
readyData_rel$ProductWidth<-NULL
readyData_rel$ProductHeight<-NULL
readyData_rel$ProfitMargin<-NULL


#correlation matrix with only relevant attributes
corr_rel<-cor(readyData_rel)
corr_rel
corrplot(corr_rel)

#### 1.2 decision tree ####


#decision tree with only relevant attributes
fit<-rpart(Volume~.,method = "class",data = readyData_rel)

fit
plot(fit)

#### 2. PLOTTING ####
#plotting bars to see the distribution of categorical variables
ggplot(existing,
       aes(x=ProductType))+
  geom_bar(color="blue", fill="yellow")

#plotting histograms to see the distribution of numerical or integer variables
ggplot(existing,
       aes(x=existing$Price))+
         geom_histogram(color="blue", fill="cyan", bins = 10) ## outliers here? (product 102)

ggplot(existing,
       aes(x=existing$x5StarReviews))+
  geom_histogram(color="blue", fill="cyan", bins = 10) ## outliers here? (products 198, 150)

ggplot(existing,
       aes(x=existing$x4StarReviews))+
  geom_histogram(color="blue", fill="cyan", bins = 10) ## outliers here? (product 150)

ggplot(existing,
       aes(x=existing$x3StarReviews))+
  geom_histogram(color="blue", fill="cyan", bins = 10)

ggplot(existing,
       aes(x=existing$x2StarReviews))+
  geom_histogram(color="blue", fill="cyan", bins = 10) ## outliers here? (product 123)
         

ggplot(existing,
       aes(x=existing$x1StarReviews))+
  geom_histogram(color="blue", fill="cyan", bins = 10) ## outliers here? (product 123)

ggplot(existing,
       aes(x=existing$PositiveServiceReview))+
  geom_histogram(color="blue", fill="cyan", bins = 10) ## outliers here? (product 150)

ggplot(existing,
       aes(x=existing$NegativeServiceReview))+
  geom_histogram(color="blue", fill="cyan", bins = 10) ## outliers here? (product 123)

ggplot(existing,
       aes(x=existing$Recommendproduct))+
  geom_histogram(color="blue", fill="cyan", bins = 10)

ggplot(existing,
       aes(x=existing$BestSellersRank))+
  geom_histogram(color="blue", fill="cyan", bins = 10)

ggplot(existing,
       aes(x=existing$ShippingWeight))+
  geom_histogram(color="blue", fill="cyan", bins = 10)

ggplot(existing,
       aes(x=existing$ProductDepth))+
  geom_histogram(color="blue", fill="cyan", bins = 10) ## outliers here? (product 153)

ggplot(existing,
       aes(x=existing$ProductWidth))+
  geom_histogram(color="blue", fill="cyan", bins = 10)

ggplot(existing,
       aes(x=existing$ProductHeight))+
  geom_histogram(color="blue", fill="cyan", bins = 10)

ggplot(existing,
       aes(x=existing$Volume))+
  geom_histogram(color="blue", fill="cyan", bins = 10) ## outliers here? (products 198, 150)

#outliers for product types based on volume
ggplot(existing,
       aes(x=existing$ProductType,y=existing$Volume))+
  geom_boxplot(outlier.size = 2, outlier.colour = "red")




#create a data set cointaining only PC (subset of ProductType)
only_pc <- existing[which(existing$ProductType == "PC"),names(existing)]

ggplot(only_pc,
       aes(x=ProductType,y=Volume))+
  geom_boxplot(outlier.size = 2, outlier.colour = "red")

  

#### 3. MODELING ####
#creating training and testing sets
set.seed(123)

inTraining<-createDataPartition(readyData_rel$Volume, p=.75, list = FALSE)
training<-readyData_rel[inTraining,]
testing<-readyData_rel[-inTraining,]

#4 fold (number=4) cross validation (repeatedcv)
cvtraincontrol <- trainControl(method = "repeatedcv", number = 4, repeats = 1)

#### 3.1 Random Forest - all variables ####
#creating the train Random Forest Regression model with .= all the variables of readyData_rel
rf_all <- train(Volume~., data = training, method = "rf", trControl=cvtraincontrol, tuneLength=5)

#training results from a Random Forest model with all attributes:
#mtry 16, rmse 130.5218, Rsquared 0.9462132,MAE 68.88377
rf_all

#creating the prediction of the model applied to the testing size
Pred_rf_all<-predict(rf_all,testing)

#prediction results applied to testing size from a Random Forest model with all variables
Pred_rf_all
summary(Pred_rf_all)

#comparing prediction with real values: RMSE 246.7253742, Rsquared 0.8687247, MAE 140.6123111
postResample(Pred_rf_all,testing$Volume)
summary(postResample(Pred_rf_all,testing$Volume))

#### 3.1.1 Random forest - with 3 relevant attributes ####
#data.frame for manual tuning of the grid --> number of grid = number of variables
rfgrid<-expand.grid(mtry=c(1,2,3))

#creating the train Random Forest Regression model with 3 and 4 stars and positive service
#system time wrapper. system.time()is used to measure process execution time
system.time(rf_rel <- train(Volume~x4StarReviews+x3StarReviews+PositiveServiceReview, data = training, method = "rf", trControl=cvtraincontrol, tuneGrid=rfgrid))

#training results from a Random Forest model with 3 attributes:
#mtry 2, rmse 190.5956, Rsquared 0.8800054,MAE 91.62429
rf_rel

#creating the prediction of the model applied to the testing size
Pred_rf_rel<-predict(rf_rel,testing)

#prediction results applied to testing size from a Random Forest model with all variables
Pred_rf_rel
summary(Pred_rf_rel)

#comparing prediction with real values: RMSE 252.254184, Rsquared 0.871624, MAE 150.044185
postResample(Pred_rf_rel,testing$Volume)
summary(postResample(Pred_rf_all,testing$Volume))

#### 3.2 knn - all variables ####
knnFit_all <- train(Volume ~ ., data = training, method = "knn", trControl = cvtraincontrol, tuneLength=20)

#training results from a knn model with all attributes:
#k=5, rmse 268.6125, Rsquared 0.8096786,MAE 132.5638
knnFit_all

#creating the prediction of the model applied to the testing size
Pred_knn_all<-predict(knnFit_all,testing)

#prediction results applied to testing size from a knn model with all variables
Pred_knn_all
summary(Pred_knn_all)

#comparing prediction with real values: RMSE 352.112103, Rsquared 0.777198, MAE 205.688889
postResample(Pred_knn_all,testing$Volume)
summary(postResample(Pred_knn_all,testing$Volume))

#### 3.3 knn - 3 attributes ####

#creating the train knn model with 3 and 4 stars and positive service
#system time wrapper. system.time()is used to measure process execution time
system.time(knn_rel <- train(Volume~x4StarReviews+x3StarReviews+PositiveServiceReview, data = training, method = "knn", trControl=cvtraincontrol))

#training results from a knn model with 3 attributes:
#k=5, rmse 262.8981, Rsquared 0.8650510,132.9468
knn_rel

#creating the prediction of the model applied to the testing size
Pred_knn_rel<-predict(knn_rel,testing)

#prediction results applied to testing size from a Random Forest model with all variables
Pred_knn_rel
summary(Pred_knn_rel)

#comparing prediction with real values: RMSE 315.1408050, Rsquared 0.8164688, MAE 181.1566138
postResample(Pred_knn_rel,testing$Volume)
summary(postResample(Pred_knn_all,testing$Volume))


