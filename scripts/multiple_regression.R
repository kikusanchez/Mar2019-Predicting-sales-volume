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
existing<-read.csv("../blackwells_multiple_regression/datasets/existingproductattributes2017.csv")
new<-read.csv("../blackwells_multiple_regression/datasets/newproductattributes2017.csv")
str(existing)


#### 0.1 Functions & loops ####

#function to test different models with different attributes
#SELECT MODELS
Models <- c("lm","rf", "knn", "svmLinear")

#SELECT FEATURES
Features <- c("Volume ~ .",
              "Volume ~ x4StarReviews + PositiveServiceReview + x3StarReviews",
              "Volume ~ x4StarReviews + PositiveServiceReview")

#RUN THE MODELS
compare.model <- c()

for(i in Models) {
  for (y in Features) {
    print(i)
    model <- train(formula(y), data = training, method = i)
    print(model$results)
    pred <- predict(model, newdata = testing)
    pred.metric <- postResample(testing$Volume, pred)
    compare.model <- cbind(compare.model , pred.metric)
  }
}
compare.model


#ggplots scatterplots for all numeric independent variables
a<-c("Price","x5StarReviews","x4StarReviews","x3StarReviews","x2StarReviews",
     "x1StarReviews","PositiveServiceReview","NegativeServiceReview",
     "Recommendproduct")


for (i in a) {
  print(i)
  
  p<-ggplot(existing_out,
            aes_string(x=i,y="Volume"))+
    geom_point(color="blue")+
    geom_smooth()
  
  print(p)
}



#ggplots scatterplots for all numeric independent variables (version 2)
a<-c("Price","x5StarReviews","x4StarReviews","x3StarReviews","x2StarReviews",
     "x1StarReviews","PositiveServiceReview","NegativeServiceReview",
     "Recommendproduct")

for (i in 1:length(a)) {
  
  print(i)
  
  p<-ggplot(existing_out,
            aes_string(x=a[i],y="Volume"))+
    geom_point(color="blue")+
    geom_smooth()
  
  print(p)
}



print(a[1])



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

#correlation matrix all variables
corr_all<-cor(readyData)
corr_all
corrplot(corr_all)
corrplot(corr_all,type="upper",tl.pos="td",method="circle",tl.cex = 0.5,tl.col='black',diag=FALSE)
?corrplot

#removing lower and higher correlate attributes from data set
#creating a new data frame to remove irrelevant attributes
  #readyData_rel<-readyData

  #removing irrelevant attributes
  #readyData_rel$Price<-NULL
  #readyData_rel$x5StarReviews<-NULL
  #readyData_rel$x2StarReviews<-NULL
  #readyData_rel$x1StarReviews<-NULL
  #readyData_rel$NegativeServiceReview<-NULL
  #readyData_rel$Recommendproduct<-NULL
  #readyData_rel$ShippingWeight<-NULL
  #readyData_rel$ProductDepth<-NULL
  #readyData_rel$ProductWidth<-NULL
  #readyData_rel$ProductHeight<-NULL
  #readyData_rel$ProfitMargin<-NULL


#correlation matrix with only relevant attributes
  #corr_rel<-cor(readyData_rel)
  #corr_rel
  #corrplot(corr_rel,method = "number")

#### 1.2 subsetting by similar product types ####

#creating subsets by similar product types
subset_pc<-subset(existing,ProductType=="PC")
subset_laptop<-subset(existing,ProductType=="Laptop")
subset_netbook<-subset(existing,ProductType=="Netbook")

subset_pc_laptop_netbook<-rbind(subset_pc,subset_laptop,subset_netbook)


#### 2. PLOTTING ####
#### 2.1 all variables ####
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
  geom_histogram(color="blue", fill="cyan", bins = 80) ## outliers here? (products 198, 150)


#outliers for product types based on volume
ggplot(existing,
       aes(x=ProductType,y=Volume))+
  geom_boxplot(outlier.size = 2, outlier.colour = "red")


#scatter Volume vs 4stars
ggplot(existing_out,
       aes(x=x4StarReviews,y=Volume))+
  geom_point(color="blue")+
  geom_smooth()

#scatter Volume vs positiveService
ggplot(existing_out,
       aes(x=PositiveServiceReview,y=Volume))+
  geom_point(color="blue")+
  geom_smooth()

#### 2.1 subsets ####

#create a data set cointaining only PC (subset of ProductType)
only_pc <- existing[which(existing$ProductType == "PC"),names(existing)]

ggplot(only_pc,
       aes(x=ProductType,y=Volume))+
  geom_boxplot(outlier.size = 2, outlier.colour = "red")

#by similar products
ggplot(subset_pc_laptop_netbook,
       aes(x=PositiveServiceReview,y=Volume))+
  geom_point()+
  geom_smooth()
  

#### 3. MODELING ####
#creating training and testing sets
set.seed(123)

inTraining<-createDataPartition(readyData$Volume, p=.75, list = FALSE)
training<-readyData[inTraining,]
testing<-readyData[-inTraining,]

#4 fold (number=4) cross validation (repeatedcv)
cvtraincontrol <- trainControl(method = "repeatedcv", number = 4, repeats = 2)

#### 3.1 Random Forest - all variables ####
#creating the train Random Forest Regression model with .= all the variables of readyData_rel
rf_all <- train(Volume~., data = training, method = "rf", trControl=cvtraincontrol, tuneLength=5)

#training results from a Random Forest model with all attributes
rf_all

#creating the prediction of the model applied to the testing size
Pred_rf_all<-predict(rf_all,testing)

#prediction results applied to testing size from a Random Forest model with all variables
Pred_rf_all
summary(Pred_rf_all)

#comparing prediction with real values
postResample(Pred_rf_all,testing$Volume)
summary(postResample(Pred_rf_all,testing$Volume))

# save the model to disk
saveRDS(Pred_rf_all, "../blackwells_multiple_regression/scripts/pred_rf_all.rds")

#### 3.1.1 Random forest - 3 attributes ####
#data.frame for manual tuning of the grid --> number of grid = number of variables
rfgrid<-expand.grid(mtry=c(1,2,3))

#creating the train Random Forest Regression model with 3 and 4 stars and positive service
#system time wrapper. system.time()is used to measure process execution time
rf_3att <- train(Volume~x4StarReviews+x3StarReviews+PositiveServiceReview, data = training, method = "rf", trControl=cvtraincontrol, tuneGrid=rfgrid)

#training results from a Random Forest model with 3 attributes:
rf_3att

#creating the prediction of the model applied to the testing size
Pred_rf_3att<-predict(rf_3att,testing)

#prediction results applied to testing size from a Random Forest model with all variables
Pred_rf_3att
summary(Pred_rf_3att)

#comparing prediction with real values: 
postResample(Pred_rf_3att,testing$Volume)
summary(postResample(Pred_rf_3att,testing$Volume))

# save the model to disk
saveRDS(Pred_rf_3att, "../blackwells_multiple_regression/scripts/pred_rf_3att.rds")

#### 3.1.2 Random forest - 2 attributes ####
#data.frame for manual tuning of the grid --> number of grid = number of variables
rfgrid2<-expand.grid(mtry=c(1,2))

#creating the train Random Forest Regression model with 3 and 4 stars and positive service
#system time wrapper. system.time()is used to measure process execution time
rf_2att <- train(Volume~x4StarReviews+PositiveServiceReview, data = training, method = "rf", trControl=cvtraincontrol, tuneGrid=rfgrid2)

#training results from a Random Forest model with 3 attributes:
rf_2att

#creating the prediction of the model applied to the testing size
Pred_rf_2att<-predict(rf_2att,testing)

#prediction results applied to testing size from a Random Forest model with all variables
Pred_rf_2att
summary(Pred_rf_2att)

#comparing prediction with real values: 
postResample(Pred_rf_2att,testing$Volume)
summary(postResample(Pred_rf_2att,testing$Volume))

# save the model to disk
saveRDS(Pred_rf_2att, "../blackwells_multiple_regression/scripts/pred_rf_2att.rds")

#### 3.2 knn - all variables ####
knnFit_all <- train(Volume ~ ., data = training, method = "knn", trControl = cvtraincontrol, tuneLength=20)

#training results from a knn model with all attributes:
knnFit_all

#creating the prediction of the model applied to the testing size
Pred_knn_all<-predict(knnFit_all,testing)

#prediction results applied to testing size from a knn model with all variables
Pred_knn_all
summary(Pred_knn_all)

#comparing prediction with real values: 
postResample(Pred_knn_all,testing$Volume)
summary(postResample(Pred_knn_all,testing$Volume))

# save the model to disk
saveRDS(Pred_knn_all, "../blackwells_multiple_regression/scripts/pred_knn_all.rds")

#### 3.2.1 knn - 3 attributes ####

#creating the train knn model with 3 and 4 stars and positive service
#system time wrapper. system.time()is used to measure process execution time
system.time(knn_3att <- train(Volume~x4StarReviews+x3StarReviews+PositiveServiceReview, data = training, method = "knn", trControl=cvtraincontrol))

#training results from a knn model with 3 attributes:
knn_3att

#creating the prediction of the model applied to the testing size
Pred_knn_3att<-predict(knn_3att,testing)

#prediction results applied to testing size from a Random Forest model with all variables
Pred_knn_3att
summary(Pred_knn_3att)

#comparing prediction with real values
postResample(Pred_knn_3att,testing$Volume)
summary(postResample(Pred_knn_3att,testing$Volume))

# save the model to disk
saveRDS(Pred_knn_3att, "../blackwells_multiple_regression/scripts/pred_knn_3att.rds")

#### 3.2.2 knn - 2 attributes ####

#creating the train knn model with 3 and 4 stars and positive service
#system time wrapper. system.time()is used to measure process execution time
system.time(knn_2att <- train(Volume~x4StarReviews+PositiveServiceReview, data = training, method = "knn", trControl=cvtraincontrol))

#training results from a knn model with 3 attributes:
knn_2att

#creating the prediction of the model applied to the testing size
Pred_knn_2att<-predict(knn_2att,testing)

#prediction results applied to testing size from a Random Forest model with all variables
Pred_knn_2att
summary(Pred_knn_2att)

#comparing prediction with real values
postResample(Pred_knn_2att,testing$Volume)
summary(postResample(Pred_knn_2att,testing$Volume))

# save the model to disk
saveRDS(Pred_knn_2att, "../blackwells_multiple_regression/scripts/pred_knn_2att.rds")

#### 3.3 SVM - all variables ####
svm_all <- train(Volume~., data = training, method = "svmLinear")

#training results from a svm model with all attributes:
svm_all

#creating the prediction of the model applied to the testing size
Pred_svm_all<-predict(svm_all,testing)

#prediction results applied to testing size from a SVM with all variables
Pred_svm_all
summary(Pred_svm_all)

#comparing prediction with real values
postResample(Pred_svm_all,testing$Volume)
summary(postResample(Pred_svm_all,testing$Volume))


#### 3.3.1 SVM - 3 attributes ####
svm_3att <- train(Volume~x4StarReviews+x3StarReviews+PositiveServiceReview, data = training, method = "svmLinear")

#training results from a svm model with 2 attributes:
svm_3att

#creating the prediction of the model applied to the testing size
Pred_svm_3att<-predict(svm_3att,testing)

#prediction results applied to testing size from a SVM with 2 variables
Pred_svm_3att
summary(Pred_svm_3att)

#comparing prediction with real values
postResample(Pred_svm_3att,testing$Volume)
summary(postResample(Pred_svm_3att,testing$Volume))

#### 3.3.2 SVM - 2 attributes ####
svm_2att <- train(Volume~x4StarReviews+PositiveServiceReview, data = training, method = "svmLinear")

#training results from a svm model with 2 attributes:
svm_2att

#creating the prediction of the model applied to the testing size
Pred_svm_2att<-predict(svm_2att,testing)

#prediction results applied to testing size from a SVM with 2 variables
Pred_svm_2att
summary(Pred_svm_2att)

#comparing prediction with real values
postResample(Pred_svm_2att,testing$Volume)
summary(postResample(Pred_svm_2att,testing$Volume))




#### 4. RESULTS ####

#duplicating the testing size to include the errors in columns
testing_errors<-testing


#create columns with the best prediction
#results column
testing_errors$pred_rf_all<-Pred_rf_all

#absolute error column
testing_errors$main_error<- testing_errors$Volume - testing_errors$pred_rf_all

#relative error
testing_errors$rel_error<- abs((testing_errors$pred_rf_all - testing_errors$Volume) / testing_errors$Volume)*100


#Exporting to csv the table with all the errors
write.csv(testing_errors,"testing_errors.csv")



# plotting the results
#absolute error
ggplot(testing_errors,
       aes(Volume,main_error))+
  geom_point(color="red")+
  geom_smooth()


#relative error
ggplot(testing_errors,
       aes(Volume,rel_error))+
  geom_point(color="red")+
  geom_smooth()

?geom_point
