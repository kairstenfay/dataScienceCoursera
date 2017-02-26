## Data from http://groupware.les.inf.puc-rio.br/har
setwd("~/Documents/dataScience/Coursera/Data_Science_Johns_Hopkins/Course 8 - Machine Learning")
url1 <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
url2 <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
if (!file.exists("training.csv")) {
  download.file(url1, "training.csv")
}
if (!file.exists("testing.csv")) {
  download.file(url2, "testing.csv")
}
WLE <- read.csv("training.csv", na.strings=c("", "NA"))
testing <- read.csv("testing.csv", na.strings=c("", "NA"))

## Preprocess data
str(WLE)
## columns 1 thru 7 are associated with identifiers and will not be useful in prediction
WLE[,8:159] <- lapply(WLE[,8:159], as.numeric)
small_WLE <- WLE[,-c(1:7)]
totalNAs <- is.na(apply(small_WLE[,-153], 2, sum))
build <- small_WLE[,!totalNAs]
rm(small_WLE, totalNAs)
library(caret)
library(rpart)
inBuild <- createDataPartition(build$classe, p=0.7, list=FALSE)
validation <- build[-inBuild,]; training <- build[inBuild,]

## Explore
plot(training[,c(1:4)])

## class variable
##    A: correct curl
##    B: elbows to front
##    C: lifting halfway
##    D: lowering halfway
##    E: throwing hips forward 

## help from https://rstudio-pubs-static.s3.amazonaws.com/64455_df98186f15a64e0ba37177de8b4191fa.html

## Regression tree
set.seed(1001)
fit0 <- train(classe~., data=training, method="rpart")
pred0 <- predict(fit0, validation)
confusionMatrix(pred0, validation$classe)[2:3]
## poor model w/ 49% accuracy. Cannot predict "D".

## Random forest
set.seed(1001)
fit1 <- train(classe~., data=training, method="rf")
pred1 <- predict(fit1, validation)
confusionMatrix(pred1, validation$classe)[2:3]
## 99.5% accuracy. 
varImp(fit1)
## most important variables: roll_belt, yaw_belt, magnet_dumbbell_z, pitch_forearm, pitch_belt
plot(fit1) ## cross-validation graph. Accuracy drops off sharply after about 31 predictors
plot(fit1$finalModel) ## overall error converges at around 100 trees. can speed up algorithm by tuning number of trees
plot(varImp(fit1), top=10) ## top 10 important variables in the model

## Boosting - 3 fold cross validation
set.seed(1001)
fit2 <- train(classe~., data=training, method="gbm", verbose=FALSE, 
              trControl=trainControl(method="cv", number=3))
pred2 <- predict(fit2, validation)
confusionMatrix(pred2, validation$classe)[2:3]
## 95.9% accuracy
plot(pred2)
plot(pred2, plotType="level") ## can also fine tune the model using the grid argument (read more on this later)

## Bagging
set.seed(1001)
fit3 <- train(classe~., data=training, method="treebag")
pred3 <- predict(fit3, validation)
confusionMatrix(pred3, validation$classe)[2:3]
## 97.5% accuracy
plot(varImp(fit3), top=10)

save(fit0, file="fit0.RData")
save(fit1, file="fit1.RData")
save(fit1.1, file="fit1.1.RData")
save(fit2, file="fit2.RData")
save(fit3, file="fit3.RData")

save(s0, file="s0.RData")
save(s1, file="s1.RData")
save(s1.1, file="s1.1.RData")
save(s2, file="s2.RData")
save(s3, file="s3.RData")

fit0 <- load(file="fit0.RData")
fit1 <- load(file="fit1.RData")
fit1.1 <- load(file="fit1.1.RData")
fit2 <- load(file="fit2.RData")
fit3 <- load(file="fit3.RData")

s0 <- load(file="s0.RData")
s1 <- load(file="s1.RData")
s1.1 <- load(file="s1.1.RData")
s2 <- load(file="s2.RData")
s3 <- load(file="s3.RData")



## Own
set.seed(1001)
cv_ctrl <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions=TRUE) ## k-fold cv

fit4 <- train(classe~., data=training, preProc="pca", method="gbm", verbose=FALSE, 
             trControl=cv_ctrl)
pred4 <- predict(fit4, validation)
confusionMatrix(pred4, validation$classe)[2:3]
## 82.1% accuracy

fit4$finalModel
##A gradient boosted model with multinomial loss function.
##150 iterations were performed.
##There were 38 predictors of which 32 had non-zero influence.
fit4$results
fit4$bestTune
fit4$pred
table(fit4$pred$Resample)
confusionMatrix(fit4$pred[,1], fit4$pred[,2])
## Accuracy 0.8643
print(fit4)
## Final values used for model were n.trees=150, interaction.depth=3, shrinkage=0.1, n.minobsinnode=10

set.seed(1001)
loocv_ctrl <- trainControl(method="LOOCV", savePredictions=TRUE)
fit5 <- train(classe~., data=training, preProc="pca", method="gbm", verbose=FALSE,
              trControl=loocv_ctrl)
pred5 <- predict(fit5, validation$classe)
confusionMatrix(pred5)


## Model building
library(caret)
mod1 <- train(classe~., method="gbm", data=train_mod) ## boosting w trees
gbmPred <- predict(mod1, newdata=test_mod)
confusionMatrix(gbmPred, test_mod$classe)

## site http://stackoverflow.com/questions/33470373/applying-k-fold-cross-validation-model-using-caret-package ## 
train_control <- trainControl(method="cv", number=10, savePredictions=TRUE)
model <- train(classe~., data=train_mod, trControl=train_control, method="rpart")
model$pred
confusionMatrix(model$pred[,1], model$pred[,2]) ## Accuracy 55%


## Boosting 

## Cross validation
##Used for:
##  1. picking variables to include in a model
##  2. picking the type of prediction function to use
##  3. picking the parameters in the prediction function
##  4. comparing different predictors

## if cross-validate to pick predictors estimate, you must estimate errors on
## independent data, otherwise you don't get a true out of sample error rate
## trainControl resampling
  ## cv = cross validation (k-fold)
  ## repeatedcv = repeated cross validation
  ## LOOCV = leave one out cross validation
  ## number = number of subsamples to take
  ## repeats = number of times to repeat subsampling
  ## set the seed!


## site github for following command ## 
train(classe~., method="glm", preProc="pca", trControl=trainControl(preProcOptions=list(thresh=0.8)))


## Expected out of sample error
##    from the confusionMatrix

## Why made choices

