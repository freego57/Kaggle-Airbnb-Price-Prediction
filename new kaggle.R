library(tidyverse)
library(stringr)
library(dplyr)
setwd('/Users/liyidan/desktop/5200/kaggle all in R')
data=read.csv('analysisData.csv',stringsAsFactors=F)
scoring=read.csv('scoringData.csv',stringsAsFactors=F)
score=read.csv('scoringData.csv',stringsAsFactors=F)
summary(data)
str(data)

data<-data[,-(1:12)]
scoring<-scoring[,-(1:12)]
str(data)
data$host_since=NULL
scoring$host_since=NULL

#delete some columns
data$host_about=NULL
data$amenities=NULL
data$host_response_time=NULL
data$host_location=NULL
scoring$host_about=NULL
scoring$amenities=NULL
scoring$host_response_time=NULL
scoring$host_location=NULL
#change 'N/A' to NA
data[data=="N/A"]<-NA
data[data=='']<-NA
scoring[scoring=='N/A']<-NA
scoring[scoring=='']<-NA
str(scoring)


sum(is.na(data$host_response_rate))
sum(is.na(data$host_acceptance_rate)) 
data$host_response_rate=NULL
data$host_acceptance_rate=NULL
scoring$host_response_rate=NULL
scoring$host_acceptance_rate=NULL
sum(is.na(data$host_is_superhost)) 
sum(is.na(data$host_neighbourhood)) 
data$host_neighbourhood=NULL
data$host_verifications=NULL
scoring$host_neighbourhood=NULL
scoring$host_verifications=NULL
sum(is.na(data$host_response_rate)) 
data$host_response_rate=NULL
scoring$host_response_rate=NULL
str(data)
sum(is.na(data$host_is_superhost))
sum(is.na(data$host_listings_count))
sum(is.na(data$host_total_listings_count))
which(is.na(data$host_is_superhost), arr.ind=TRUE)
which(is.na(data$host_listings_count), arr.ind=TRUE)
which(is.na(data$host_total_listings_count),arr.ind=TRUE)
data=data[-which(is.na(data$host_is_superhost), arr.ind=TRUE),]
sum(is.na(data$host_has_profile_pic))
sum(is.na(data$identity_verified))
data$street=NULL
scoring$street=NULL
factor(data$neighbourhood)
data$neighbourhood=NULL
scoring$neighbourhood=NULL
sum(is.na(data$neighbourhood_cleansed))
sum(is.na(data$neighbourhood_group_cleansed))
data$state=NULL
data$country_code=NULL
scoring$state=NULL
scoring$country_code=NULL
sum(is.na(data$market))
data$market=NULL
scoring$market=NULL
data$country=NULL
scoring$country=NULL
sum(is.na(data$is_location_exact));sum(is.na(data$property_type));sum(is.na(data$property_type));sum(is.na(data$room_type))
sum(is.na(data$accomodates));sum(is.na(data$bathrooms));sum(is.na(data$bedrooms));sum(is.na(data$beds))
data=data[-which(is.na(data$beds),arr.ind=TRUE),]
sum(is.na(data$bed_type))
sum(is.na(data$square_feet));data$square_feet=NULL;scoring$square_feet=NULL
str(data)
sum(is.na(data$weekly_price));sum(is.na(scoring$weekly_price));data$weekly_price=NULL;scoring$weekly_price=NULL
sum(is.na(data$monthly_price));sum(is.na(scoring$monthly_price));data$monthly_price=NULL;scoring$monthly_price=NULL
sum(is.na(data$security_deposit));sum(is.na(scoring$security_deposit));data$security_deposit=NULL;scoring$security_deposit=NULL
sum(is.na(data$cleaning_fee));sum(is.na(scoring$cleaning_fee));data$cleaning_fee=NULL;scoring$cleaning_fee=NULL
sum(is.na(data$guests_included))
sum(is.na(data$extra_people))

sum(is.na(data$minimum_nights));sum(is.na(data$maximum_nights));sum(is.na(data$minimum_maximum_nights));sum(is.na(data$minimum_minimum_nights))
sum(is.na(data$maximum_minimum_nights));sum(is.na(data$maximum_maximum_nights))
sum(is.na(data$minimum_nights_avg_ntm));sum(is.na(data$maximum_nights_avg_ntm))
data$calendar_updated=NULL
data$has_availability=NULL   
scoring$calendar_updated=NULL
scoring$has_availability=NULL   
sum(is.na(data$availability_30));sum(is.na(data$availability_60));sum(is.na(data$availability_90));sum(is.na(data$availability_365))
str(data)
sum(is.na(data$number_of_reviews));sum(is.na(data$number_of_reviews_ltm))
data$first_review=NULL;data$last_review=NULL
scoring$first_review=NULL;scoring$last_review=NULL
sum(is.na(data$review_scores_rating));sum(is.na(data$review_scores_accuracy));sum(is.na(data$review_scores_cleanliness));sum(is.na(data$review_scores_checkin))
sum(is.na(data$review_scores_communication));sum(is.na(data$review_scores_location));sum(is.na(data$review_scores_value))
data$requires_license=NULL;scoring$requires_license=NULL
sum(is.na(data$license))
data$license=NULL;scoring$license=NULL
sum(is.na(data$jurisdiction_names))
data$jurisdiction_names=NULL;scoring$jurisdiction_names=NULL
sum(is.na(data$instant_bookable))
sum(is.na(data$is_business_travel_ready));data$is_business_travel_ready=NULL;scoring$is_business_travel_ready=NULL
sum(is.na(data$cancellation_policy))
sum(is.na(data$calculated_host_listings_count));sum(is.na(data$calculated_host_listings_count_entire_homes))
sum(is.na(data$calculated_host_listings_count_private_rooms));sum(is.na(data$calculated_host_listings_count_shared_rooms))
sum(is.na(data$calculated_reviews_per_month))
sum(is.na(data))
x=which(is.na(data),arr.ind=TRUE)
data=data[-x,]
data$neighbourhood_cleansed=NULL
data$property_type=NULL
data$smart_location=NULL
scoring$neighbourhood_cleansed=NULL
scoring$property_type=NULL
scoring$smart_location=NULL
data$city=NULL
scoring$city=NULL
str(data)

#more clean of the scoring data
# NAs in scoring
sapply(scoring,function(x)
  sum(is.na(x)))

table(scoring$host_is_superhost)
scoring$host_is_superhost[which(is.na(scoring$host_is_superhost))]='f'
str(scoring)
# host_listings_count NA in scoring
scoring$host_listings_count[which(is.na(scoring$host_listings_count))]=mean(scoring$host_listings_count[-which(is.na(scoring$host_listings_count))])
# host_total_listings_count NA in scoring
scoring$host_total_listings_count[which(is.na(scoring$host_total_listings_count))]=mean(scoring$host_total_listings_count[-which(is.na(scoring$host_total_listings_count))])
# host_has_profile_pic NA in scoring
table(scoring$host_has_profile_pic)
scoring$host_has_profile_pic[which(is.na(scoring$host_has_profile_pic))]='t'
#host_identity_verified
table(scoring$host_identity_verified)
scoring$host_identity_verified[which(is.na(scoring$host_identity_verified))]='t'
#zipcode
scoring$zipcode[which(is.na(scoring$zipcode))]=mode(scoring$zipcode)
#beds
scoring$beds[which(is.na(scoring$beds))]=mode(scoring$beds[which(is.na(scoring$beds))])

sum(is.na(scoring))

#change all chr to factor
data1 <- as.data.frame(unclass(data))
scoring1<-as.data.frame(unclass(scoring))
str(data1)
str(scoring1)

#change all factor to numeric
data2<-data1
scoring2<-scoring1
unname(which(sapply(data2, is.character)))
unname(which(sapply(scoring2, is.character)))

data2[,1]=as.numeric(data2[,1])
data2[,4]=as.numeric(data2[,4])
data2[,5]=as.numeric(data2[,5]);data2[,6]=as.numeric(data2[,6]);data2[,7]=as.numeric(data2[,7]);data2[,8]=as.numeric(data2[,8]);data2[,9]=as.numeric(data2[,9])
data2[,14]=as.numeric(data2[,14]);data2[,39]=as.numeric(data2[,39]);data2[,40]=as.numeric(data2[,40]);data2[,41]=as.numeric(data2[,41]);data2[,42]=as.numeric(data2[,42])

scoring2[,1]=as.numeric(scoring2[,1]);scoring2[,4]=as.numeric(scoring2[,4]);scoring2[,5]=as.numeric(scoring2[,5])
scoring2[,6]=as.numeric(scoring2[,6]);scoring2[,7]=as.numeric(scoring2[,7]);scoring2[,8]=as.numeric(scoring2[,8]);scoring2[,9]=as.numeric(scoring2[,9])
scoring2[,14]=as.numeric(scoring2[,14]);scoring2[,38]=as.numeric(scoring2[,38]);scoring2[,39]=as.numeric(scoring2[,39])
scoring2[,40]=as.numeric(scoring2[,40]);scoring2[,41]=as.numeric(scoring2[,41])
data2$beds=as.numeric(data2$beds)
scoring2$beds=as.numeric(scoring2$beds)
str(data2)
str(scoring2)
write.csv(data1, 'cleaned data1.csv',row.names = F)
write.csv(data2, 'cleaned data2.csv',row.names = F)
write.csv(scoring1, 'cleaned scoring1.csv',row.names = F)
write.csv(scoring2, 'cleaned scoring2.csv',row.names = F)

#end of cleaning

#data1 ->factors
#data2->numeric


#forward with all variables

library(caret)
set.seed(1111)
split=createDataPartition(y=data2$price,p=0.7,list=F,groups=100)
train=data2[split,]
test=data2[-split,]
start_mod=lm(price~1,data=train)
empty_mod=lm(price~1,data=train)
full_mod=lm(price~.,data=train)
forwardStepwise=step(start_mod,scop=list(upper=full_mod,lower=empty_mod),direction='forward')
summary(forwardStepwise)
pred=predict(forwardStepwise)
rmse=sqrt(mean((pred-train$price)^2))
rmse #72
pred1=predict(forwardStepwise,newdata=test)
rmse1=sqrt(mean((pred1-test$price)^2))
rmse1  #74
pred = predict(forwardStepwise,newdata=scoring2)
submissionFile = data.frame(id = score$id, price = pred) 
write.csv(submissionFile, 'liyidan_submission1.csv',row.names = F)

#backward
library(caret)
set.seed(111)
split=createDataPartition(y=data2$price,p=0.7,list=F,groups=100)
train=data2[split,]
test=data2[-split,]
start_mod=lm(price~.,data=train)
empty_mod=lm(price~1,data=train)
full_mod=lm(price~.,data=train)
backwardStepwise=step(start_mod,scop=list(upper=full_mod,lower=empty_mod),direction='backward')
summary(backwardStepwise)
pred=predict(backwardStepwise)
rmse=sqrt(mean((pred-train$price)^2))
rmse  #73
pred1=predict(backwardStepwise,newdata=test)
rmse1=sqrt(mean((pred1-test$price)^2))
rmse1  #72
pred = predict(forwardStepwise,newdata=scoring2)
submissionFile = data.frame(id = score$id, price = pred) 
write.csv(submissionFile, 'liyidan_submission2.csv',row.names = F)

#stepwise
library(caret)
set.seed(111)
split=createDataPartition(y=data1$price,p=0.7,list=F,groups=100)
train=data2[split,]
test=data2[-split,]
start_mod=lm(price~1,data=train)
empty_mod=lm(price~1,data=train)
full_mod=lm(price~.,data=train)
hybridStepwise=step(start_mod,scope=list(upper=full_mod,lower=empty_mod),direction='both')
summary(hybridStepwise)
pred=predict(hybridStepwise)
rmse=sqrt(mean((pred-train$price)^2))
rmse  #73
pred1=predict(hybridStepwise,newdata=test)
rmse1=sqrt(mean((pred1-test$price)^2))
rmse1  #72
pred = predict(forwardStepwise,newdata=scoring2)
submissionFile = data.frame(id = score$id, price = pred) 
write.csv(submissionFile, 'liyidan_submission3.csv',row.names = F)

#lasso
library(glmnet)
library(caret)
set.seed(114)
split=createDataPartition(y=data1$price,p=0.7,list=F,groups=100)
train=data1[split,]
test=data1[-split,]
x=model.matrix(price~.-1,data=train)
y=train$price
lassoModel=glmnet(x,y,alpha=1)
plot(lassoModel,xvar='lambda',label=T)
plot(lassoModel,xvar='dev',label=T)
cv.lasso=cv.glmnet(x,y,alpha=1)
plot(cv.lasso)
coef(cv.lasso)
#can not use zip code
lassomodel=lm(price~host_is_superhost+neighbourhood_group_cleansed+room_type+accommodates+bathrooms+bedrooms+guests_included+extra_people+
                +minimum_nights+availability_30+availability_90+availability_365+number_of_reviews_ltm+number_of_reviews+
                +review_scores_cleanliness+review_scores_location+review_scores_value+review_scores_rating+
                reviews_per_month+cancellation_policy+
                +calculated_host_listings_count+calculated_host_listings_count_private_rooms +
                calculated_host_listings_count_shared_rooms ,data=train)
summary(lassomodel)
pred=predict(lassomodel)
rmse=sqrt(mean((pred-train$price)^2))
rmse  #73
pred1=predict(lassomodel,newdata=test)
rmse1=sqrt(mean((pred1-test$price)^2))
rmse1  #71.9



#dimension reduction
set.seed(156)
split=createDataPartition(y=data2$price,p=0.7,list=F,groups=100)
train=data2[split,]
test=data2[-split,]
trainPredictors=train[,-which(colnames(data2)=="price" )]
testPredictors=test[,-which(colnames(data2)=="price" )]
x = preProcess(x = trainPredictors,method = 'pca',thresh = 0.9)
pca = prcomp(trainPredictors,scale. = T)
head(pca)
trainComponents= predict(x,newdata=trainPredictors)
trainComponents$price = train$price
head(trainComponents)
train_model = lm(price~.,trainComponents)
summary(train_model)
pred=predict(train_model)

rmse=sqrt(mean((pred-train$price)^2))
rmse  #75
testComponents = predict(x,newdata=testPredictors)
testComponents$price = test$price   
head(testComponents)
pred=predict(train_model,newdata=testComponents)
rmse1=sqrt(mean((pred-test$price)^2))
rmse1   #75.6

#default tree  
library('rpart')
library('rpart.plot')
split=createDataPartition(y=data2$price,p=0.7,list=F,groups=100)
train=data2[split,]
test=data2[-split,]
set.seed(324)
tree=rpart(price~.,data=train)
rpart.plot(tree)
summary(tree)
pred=predict(tree)
rmse=sqrt(mean((pred-train$price)^2))
rmse  #74.3
predTree=predict(tree,newdata=test)
rmseTree=sqrt(mean((predTree-test$price)^2))
rmseTree  #75.4
pred = predict(tree,newdata=scoring2)
submissionFile = data.frame(id = score$id, price = pred) 
write.csv(submissionFile, 'liyidan_submission4.csv',row.names = F)

#maximal tree
library(rpart)
split=createDataPartition(y=data2$price,p=0.7,list=F,groups=100)
train=data2[split,]
test=data2[-split,]
set.seed(324)
maximalTree=rpart(price~.,data=train,control=rpart.control(minbucket=1))
predMaximalTree=predict(maximalTree,newdata=test)
rmseMaximalTree=sqrt(mean((predMaximalTree-test$price)^2))
rmseMaximalTree  #75.4
pred = predict(maximalTree,newdata=scoring2)
submissionFile = data.frame(id = score$id, price = pred) 
write.csv(submissionFile, 'liyidan_submission5.csv',row.names = F)

#tree with 10-fold cross-validation
library(caret)
split=createDataPartition(y=data2$price,p=0.7,list=F,groups=100)
train=data2[split,]
test=data2[-split,]
set.seed(324)
trControl=trainControl(method='cv',number=10)  
tuneGrid=expand.grid(.cp=seq(0.001,0.1,0.001))
cvModel=train(price~.,data=train,method='rpart',trControl=trControl,tuneGrid=tuneGrid)
cvModel$bestTune
treeCV=rpart(price~.,data=train,control=rpart.control(cp=cvModel$bestTune))
predTreeCV=predict(treeCV,newdata=test)
rmseCV=sqrt(mean((predTreeCV-test$price)^2))
rmseCV  #70.3
pred = predict(treeCV,newdata=scoring3)
submissionFile = data.frame(id = score$id, price = pred) 
write.csv(submissionFile, 'liyidan_submission6.csv',row.names = F)


#Bag Model  (slow, so only 10 trees)   new levels from zipcode
library(randomForest)
library(caret)
split=createDataPartition(y=data2$price,p=0.7,list=F,groups=100)
train=data2[split,]
test=data2[-split,]
set.seed(324)
bag=randomForest(price~.,data=train,mtry=ncol(train)-1,ntree=10)
predBag=predict(bag,newdata=test)
rmseBag=sqrt(mean((predBag-test$price)^2))
rmseBag  #65.7
plot(bag)
varImpPlot(bag)
importance(bag)
getTree(bag,k=10)
hist(treesize(bag))
pred = predict(bag,newdata=scoring2)
submissionFile = data.frame(id = score$id, price = pred) 
write.csv(submissionFile, 'liyidan_submission7.csv',row.names = F)

#randomForest
library(randomForest)
set.seed(617)
split=createDataPartition(y=data2$price,p=0.7,list=F,groups=100)
train=data2[split,]
test=data2[-split,]
forest=randomForest(price~.,data=train,ntree=30)
summary(forest)
plot(forest)
predForest=predict(forest,newdata=test)
rmseForest=sqrt(mean((predForest-test$price)^2))
rmseForest  #65
pred = predict(forest,newdata=scoring2)
submissionFile = data.frame(id =score$id, price = pred) 
write.csv(submissionFile, 'liyidan_submission8.csv',row.names = F)
sum(is.na(submissionFile))


#random forest with cross-validation (slow)
library(randomForest)
library(rpart)
library(caret)
set.seed(324)
split=createDataPartition(y=data2$price,p=0.7,list=F,groups=100)
train=data2[split,]
test=data2[-split,]
trControl=trainControl(method='cv',number=10)
tuneGrid=expand.grid(mtry=1:5)
cvForest=train(price~.,data=train,method='rf',ntree=20,trControl=trControl,tuneGrid=tuneGrid)
cvForest
#according to the result of cvForest, mtry=5 is the best
forest=randomForest(price~.,data=train,ntree=20,mtry=5)
predForest=predict(forest,newdata=test)
rmseForest=sqrt(mean((predForest-test$price)^2))
rmseForest  #67
pred = predict(forest,newdata=scoring2)
submissionFile = data.frame(id = score$id, price = pred) 
write.csv(submissionFile, 'liyidan_submission9.csv',row.names = F)
sum(is.na(submissionFile))

#boosting
library(gbm)
library(caret)
set.seed(324)
split=createDataPartition(y=data2$price,p=0.7,list=F,groups=100)
train=data2[split,]
test=data2[-split,]
boost=gbm(price~.-zipcode,data=train,distribution='gaussian',n.trees=1000,interaction.depth=40,shrinkage=0.01)
predBoostTrain=predict(boost,n.trees=10)
rmseBoostTrain=sqrt(mean((predBoostTrain-train$price)^2))
rmseBoostTrain
summary(boost) #42.8
predBoost=predict(boost,newdata=test,n.trees=10)
rmseBoost=sqrt(mean((predBoost-test$price)^2))
rmseBoost  #60.8
pred = predict(boost,newdata=scoring2,n.trees=10)
submissionFile = data.frame(id = score$id, price = pred) 
write.csv(submissionFile, 'liyidan_submission10.csv',row.names = F)

#boosting with cross-validation  (slow)
library(caret)
library(gbm)
set.seed(617)
split=createDataPartition(y=data2$price,p=0.5,list=F,groups=100)
train=data2[split,]
test=data2[-split,]
trControl=trainControl(method='cv',number=10)
tuneGrid=expand.grid(n.trees=70,interaction.depth=c(1,2),shrinkage=(1:70)*0.001,n.minobsinnode=5)
garbage=capture.output(cvBoost<-train(price~.,data=train,method='gbm',trControl=trControl,tuneGrid=tuneGrid))
boostCV=gbm(price~.,data=train,distribution='gaussian',n.trees=cvBoost$bestTune$n.trees,interaction.depth = cvBoost$bestTune$interaction.depth,shrinkage=cvBoost$bestTune$shrinkage,n.minobsinnode = cvBoost$bestTune$n.minobsinnode)
predBoostCV = predict(boostCV,test,n.trees=10)
rmseBoostCV = sqrt(mean((predBoostCV-test$price)^2)); rmseBoostCV 
pred = predict(boostCV,newdata=scoring2)
submissionFile = data.frame(id = score$id, price = pred) 
write.csv(submissionFile, 'liyidan_submission11.csv',row.names = F)



#do the above models with top 20 variables from forward (wrong)
library('rpart')
library('rpart.plot')
split=createDataPartition(y=data2$price,p=0.7,list=F,groups=100)
train=data2[split,]
test=data2[-split,]
set.seed(324)
tree=rpart(price~accommodates + room_type + bathrooms + review_scores_location + 
             bedrooms + review_scores_value + review_scores_rating + number_of_reviews_ltm + 
             neighbourhood_group_cleansed + extra_people + availability_365 + 
             review_scores_checkin + calculated_host_listings_count_private_rooms + 
             cancellation_policy + minimum_nights + review_scores_cleanliness + 
             host_is_superhost + reviews_per_month + availability_30 + 
             guests_included,data=train)
rpart.plot(tree)
summary(tree)
pred=predict(tree)
rmse=sqrt(mean((pred-train$price)^2))
rmse  #79.5
predTree=predict(tree,newdata=test)
rmseTree=sqrt(mean((predTree-test$price)^2))
rmseTree  #77
pred = predict(tree,newdata=scoring2)
submissionFile = data.frame(id = score$id, price = pred) 
write.csv(submissionFile, 'liyidan_submission12.csv',row.names = F)


#maximal tree
library(rpart)
maximalTree=rpart(price~ accommodates + room_type + bathrooms + review_scores_location + 
                    bedrooms + review_scores_value + review_scores_rating + number_of_reviews_ltm + 
                    neighbourhood_group_cleansed + extra_people + availability_365 + 
                    review_scores_checkin + calculated_host_listings_count_private_rooms + 
                    cancellation_policy + minimum_nights + review_scores_cleanliness + 
                    host_is_superhost + reviews_per_month + availability_30 + 
                    guests_included,data=train,control=rpart.control(minbucket=1))
predMaximalTree=predict(maximalTree,newdata=test)
rmseMaximalTree=sqrt(mean((predMaximalTree-test$price)^2))
rmseMaximalTree  #78
pred = predict(maximalTree,newdata=scoring2)
submissionFile = data.frame(id = score$id, price = pred) 
write.csv(submissionFile, 'liyidan_submission13.csv',row.names = F)

#randomForest
library(randomForest)
set.seed(617)
split=createDataPartition(y=data2$price,p=0.7,list=F,groups=100)
train=data2[split,]
test=data2[-split,]
forest=randomForest(price~accommodates + room_type + bathrooms + review_scores_location + 
                      bedrooms + review_scores_value + review_scores_rating + number_of_reviews_ltm + 
                      neighbourhood_group_cleansed + extra_people + availability_365 + 
                      review_scores_checkin + calculated_host_listings_count_private_rooms + 
                      cancellation_policy + minimum_nights + review_scores_cleanliness + 
                      host_is_superhost + reviews_per_month + availability_30 + 
                      guests_included,data=train,ntree=200)
summary(forest)
plot(forest)
predForest=predict(forest,newdata=test)
rmseForest=sqrt(mean((predForest-test$price)^2))
rmseForest  #66
pred = predict(forest,newdata=scoring2)
submissionFile = data.frame(id =score$id, price = pred) 
write.csv(submissionFile, 'liyidan_submission14.csv',row.names = F)
sum(is.na(submissionFile))

#how about combining pca with random forest? (wrong)
set.seed(324)
library(randomForest)
library(caret)
split=createDataPartition(y=data2$price,p=0.7,list=F,groups=100)
train=data2[split,]
test=data2[-split,]
trainPredictors=train[,-which(colnames(train)=="price" )]
testPredictors=test[,-which(colnames(test)=="price" )]
x = preProcess(x = trainPredictors,method = 'pca',thresh = 0.9)
pca = prcomp(trainPredictors,scale. = T)
head(pca)
trainComponents= predict(x,newdata=trainPredictors)
trainComponents$price = train$price
testComponents=predict(x,newdata=testPredictors)
head(trainComponents)
forest_pca=randomForest(price~.,trainComponents,ntree=50)
summary(forest_pca)
pred=predict(forest_pca)
which(is.na(pred))
sum(is.na(pred))
rmse=sqrt(mean((pred-train$price)^2))
rmse #69
pred=predict(forest_pca,newdata=testComponents)
rmse=sqrt(mean((pred-test$price)^2))
rmse  #68.9

#pca+random forest with 15 princial components
forest_pca_small=randomForest(price~PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+PC11+PC12+PC13+PC14+
                                PC15+PC16+PC17+PC18+PC19+PC20,trainComponents,ntree=50)
pred=predict(forest_pca_small)
rmse=sqrt(mean((pred-train$price)^2))
rmse  #70.5


#combine pca with decision tree
library(caret)
library(rpart)
split=createDataPartition(y=data$price,p=0.7,list=F,groups=100)
train=data[split,]
test=data[-split,]
trainPredictors=train[,-which(colnames(train)=="price" )]
testPredictors=test[,-which(colnames(test)=="price" )]
x = preProcess(x = trainPredictors,method = 'pca',thresh = 0.9)
pca = prcomp(trainPredictors,scale. = T)
head(pca)
trainComponents= predict(x,newdata=trainPredictors)
trainComponents$price = train$price
testComponents=predict(x,newdata=testPredictors)
head(trainComponents)
tree_pca=rpart(price~.,trainComponents,ntree=100)
summary(forest_pca)
pred=predict(forest_pca)
which(is.na(pred))
sum(is.na(pred))
rmse=sqrt(mean((pred-train$price)^2))
rmse
pred=predict(tree_pca,newdata=testComponents)
rmse=sqrt(mean((pred-test$price)^2))
rmse  #132....

#combine pca with boosting
set.seed(324)
split=createDataPartition(y=data2$price,p=0.7,list=F,groups=100)
train=data2[split,]
test=data2[-split,]
trainPredictors=train[,-which(colnames(train)=="price" )]
testPredictors=test[,-which(colnames(test)=="price" )]
x = preProcess(x = trainPredictors,method = 'pca',thresh = 0.9)
pca = prcomp(trainPredictors,scale. = T)
head(pca)
trainComponents= predict(x,newdata=trainPredictors)
trainComponents$price = train$price
testComponents=predict(x,newdata=testPredictors)
head(trainComponents)
library(gbm)
pca_boost=gbm(price~.,data=trainComponents,distribution='gaussian',n.trees=1000,interaction.depth=40,shrinkage=0.01)
predBoostTrain=predict(pca_boost,n.trees=1000)
rmseBoostTrain=sqrt(mean((predBoostTrain-train$price)^2))
rmseBoostTrain
summary(pca_boost) 
predBoost=predict(pca_boost,newdata=testComponents,n.trees=1000)
rmseBoost=sqrt(mean((predBoost-test$price)^2))
rmseBoost    #66


library(randomForest)
set.seed(617)
library(caret)
split=createDataPartition(y=data2$price,p=0.7,list=F,groups=100)
train=data2[split,]
test=data2[-split,]
forest=randomForest(price~.,data=train,ntree=200)
summary(forest)
plot(forest)
predForest=predict(forest,newdata=test)
rmseForest=sqrt(mean((predForest-test$price)^2))
rmseForest   #63.8
pred = predict(forest,newdata=scoring2)
submissionFile = data.frame(id =score$id, price = pred) 
write.csv(submissionFile, 'liyidan_submission15.csv',row.names = F) 
sum(is.na(submissionFile))



split=createDataPartition(y=data2$price,p=0.7,list=F,groups=100)
train=data2[split,]
test=data2[-split,]
forest=randomForest(price~.,data=train,ntree=200)
summary(forest)
plot(forest)
predForest=predict(forest,newdata=test)
rmseForest=sqrt(mean((predForest-test$price)^2))
rmseForest   #65.31  useless
pred = predict(forest,newdata=scoring2)
submissionFile = data.frame(id =score$id, price = pred) 
write.csv(submissionFile, 'liyidan_submission16.csv',row.names = F)  
sum(is.na(submissionFile))


#ranger
install.packages('ranger')
library(ranger)
library(caret)
split=createDataPartition(y=data2$price,p=0.7,list=F,groups=100)
train=data2[split,]
test=data2[-split,]
ranger=ranger(price~.,data=train,num.trees=1000)
predForest=predict(ranger,data=test)
sum(is.na(predForest))
head(predForest)
summary(predForest)
rmseForest=sqrt(mean((predForest-test$price)^2))
str(test)
rmseForest   #59.8
pred = predict(forest,newdata=scoring2)
submissionFile = data.frame(id =score$id, price = pred) 
write.csv(submissionFile, 'liyidan_submission17.csv',row.names = F)  
sum(is.na(submissionFile))



#boost with less variables
library(caret)
split=createDataPartition(y=data2$price,p=0.7,list=F,groups=100)
train=data2[split,]
test=data2[-split,]
boost=gbm(price~zipcode+room_type+bedrooms+accommodates+neighbourhood_group_cleansed,data=train,distribution='gaussian',n.trees=1000,interaction.depth=40,shrinkage=0.01)
predBoostTrain=predict(boost,n.trees=1000)
rmseBoostTrain=sqrt(mean((predBoostTrain-train$price)^2))
rmseBoostTrain
summary(boost) 
predBoost=predict(boost,newdata=test,n.trees=1000)
rmseBoost=sqrt(mean((predBoost-test$price)^2))
rmseBoost   #65

#use the variables in forward to do boosting

library(caret)
set.seed(1111)
split=createDataPartition(y=data$price,p=0.7,list=F,groups=100)
train=data[split,]
test=data[-split,]
start_mod=lm(price~1,data=train)
empty_mod=lm(price~1,data=train)
full_mod=lm(price~.,data=train)
forwardStepwise=step(start_mod,scop=list(upper=full_mod,lower=empty_mod),direction='forward')
summary(forwardStepwise)

start_mod=lm(price~1,data=train)
empty_mod=lm(price~1,data=train)
full_mod=lm(price~.,data=train)
hybridStepwise=step(start_mod,scope=list(upper=full_mod,lower=empty_mod),direction='both')
summary(hybridStepwise)


#use the variables from the stepwise model (not useful)
library(caret)
set.seed(1111)
split=createDataPartition(y=data2$price,p=0.7,list=F,groups=100)
train=data2[split,]
test=data2[-split,]
library(gbm)
boost=gbm(
  price ~ accommodates + zipcode + room_type + bathrooms + bedrooms + 
    availability_90 + review_scores_cleanliness + number_of_reviews_ltm + 
    guests_included + minimum_nights + calculated_host_listings_count + 
   availability_365 + review_scores_rating + 
    review_scores_value + minimum_minimum_nights + availability_30 + 
    cancellation_policy + reviews_per_month + extra_people ,data=train,distribution='gaussian',n.trees=1000,interaction.depth=40,shrinkage=0.01)
predBoostTrain=predict(boost,n.trees=1000)
rmseBoostTrain=sqrt(mean((predBoostTrain-train$price)^2))
rmseBoostTrain  #46.85
summary(boost) 
predBoost=predict(boost,newdata=test,n.trees=1000)
rmseBoost=sqrt(mean((predBoost-test$price)^2))
rmseBoost  #61.37



#use high correlation variables (not useful)
cor(data2,data2$price)
boost=gbm(
  price ~ zipcode+accommodates + room_type + bathrooms +
    bedrooms + review_scores_location + 
    review_scores_cleanliness +  extra_people+
     cancellation_policy +  guests_included +  beds +review_scores_location
    ,data=train,distribution='gaussian',n.trees=1000,interaction.depth=40,shrinkage=0.01)
predBoostTrain=predict(boost,n.trees=500)
rmseBoostTrain=sqrt(mean((predBoostTrain-train$price)^2))
rmseBoostTrain 
summary(boost) 
predBoost=predict(boost,newdata=test,n.trees=500)
rmseBoost=sqrt(mean((predBoost-test$price)^2))  #65
rmseBoost 



#deal with outliers
#how to solve the overfitting of boosting
plot(data$host_listings_count,data$price)
#remove outlier in maximum_nights
data=data[-which(data$maximum_nights==max(data$maximum_nights)),]
cor(data$host_listings_count,data$host_total_listings_count)
plot(data$extra_people,data$price)
plot(data$minimum_nights,data$price)
plot(data$maximum_nights,data$price)
plot(data$minimum_maximum_nights,data$price)
plot(data$maximum_nights_avg_ntm,data$price)

cor(data$number_of_reviews_ltm,data$number_of_reviews)



#remove some variables with high correlations
cor(data2$availability_30,data2$availability_60)
cor(data2$availability_90,data2$availability_365)
cor(data2$availability_60,data2$availability_90)
cor(data2$minimum_nights,data2$minimum_minimum_nights)
cor(data2$maximum_minimum_nights,data2$minimum_nights_avg_ntm)
cor(data2$calculated_host_listings_count_entire_homes,data2$calculated_host_listings_count)
cor(data2$host_listings_count,data2$host_total_listings_count)
data2$minimum_minimum_nights=NULL
data2$maximum_minimum_nights=NULL

data2$availability_60=NULL
data2$calculated_host_listings_count=NULL
data2$host_listings_count=NULL
scoring2$minimum_minimum_nights=NULL
scoring2$maximum_minimum_nights=NULL
scoring2$availability_60=NULL
scoring2$calculated_host_listings_count=NULL
scoring2$host_listings_count=NULL

str(data2)
str(scoring2)
install.packages('xgboost')
library(xgboost)
library(caret)
set.seed(1111)
split=createDataPartition(y=data2$price,p=0.7,list=F,groups=100)
train=data2[split,]
test=data2[-split,]
#xgboost
model=xgboost(as.matrix(train[,c(1:13,15:42)]),label=train$price,max_depth=3,eta=1,nthread=2,nrounds=30,objective='reg:squarederror')
pred_test=predict(model,newdata=as.matrix(test[,c(1:13,15:42)]))
rmseBoost=sqrt(mean((pred_test-test$price)^2))
rmseBoost   #66.48
pred = predict(model,newdata=as.matrix(scoring2[1:41]))
submissionFile = data.frame(id = score$id, price = pred) 
write.csv(submissionFile, 'liyidan_submission18.csv',row.names = F)
sum(is.na(submissionFile))

#boosting again
library(gbm)
library(caret)
set.seed(324)
split=createDataPartition(y=data2$price,p=0.7,list=F,groups=100)
train=data2[split,]
test=data2[-split,]
boost=gbm(price~.-zipcode,data=train,distribution='gaussian',n.trees=1000,interaction.depth=40,shrinkage=0.01)
predBoostTrain=predict(boost,n.trees=10)
rmseBoostTrain=sqrt(mean((predBoostTrain-train$price)^2))
rmseBoostTrain
summary(boost) 
predBoost=predict(boost,newdata=test,n.trees=10)
rmseBoost=sqrt(mean((predBoost-test$price)^2))
rmseBoost 
pred = predict(boost,newdata=scoring2,n.trees=10)
submissionFile = data.frame(id = score$id, price = pred) 
write.csv(submissionFile, 'liyidan_submission19.csv',row.names = F)


#less variables  (from stepwise boosting summary)
library(caret)
split=createDataPartition(y=data2$price,p=0.7,list=F,groups=100)
train=data2[split,]
test=data2[-split,]
library(gbm)
boost=gbm(
  price ~ accommodates +zipcode+ room_type + bathrooms  + 
    bedrooms +extra_people+reviews_per_month+availability_90+guests_included+review_scores_rating+beds+
    number_of_reviews+maximum_nights+review_scores_cleanliness,data=train,distribution='gaussian',n.trees=1000,interaction.depth=40,shrinkage=0.01)
predBoostTrain=predict(boost,n.trees=1000)
rmseBoostTrain=sqrt(mean((predBoostTrain-train$price)^2))
rmseBoostTrain  
summary(boost) 
predBoost=predict(boost,newdata=test,n.trees=1000)
rmseBoost=sqrt(mean((predBoost-test$price)^2))
rmseBoost



