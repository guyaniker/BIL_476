library(readr)
bankfull <- read_delim("C:/bankfull.csv", 
                       ";", escape_double = FALSE, trim_ws = TRUE)
output <- bankfull

str(output)
summary(output)
bankfull
names(output)[17]<- "hedef"
hist(bankfull$age, breaks = bankfull$y)
library(ggplot2)
#hedef
dev.off()
ggplot(data= output)+geom_bar(mapping = aes(x=hedef))
library(scales)
ggplot(data= output, aes(x=hedef, fill = hedef))+
  geom_bar(position = "stack")

#age
ggplot(output,aes(x =age )) + 
  geom_bar(position = "stack", color ="blue"
           , fill=rgb(0.2,0.7,0.1,0.4))

ggplot(output,aes(x =age ,fill = hedef)) + 
  geom_bar(position = "fill")

#job
ggplot(data= output, aes(x=job,fill = hedef))+
  geom_bar(position = "fill")
ggplot(data= bankfull, aes(x=job,fill = hedef))+
  geom_bar(position = "stack")

#marital
ggplot(output, aes(x = marital, fill = hedef)) +
  geom_bar( position="fill")

ggplot(output, aes(x = marital, fill = hedef)) +
  geom_bar( position="stack")

#education
ggplot(output, aes(x = education, fill = hedef)) +
  geom_bar( position="fill")

ggplot(output, aes(x = education, fill = hedef)) +
  geom_bar( position="stack")

#default
ggplot(output, aes(x = default, fill = hedef)) +
  geom_bar

#housing and loan 
ggplot(output, aes(x = housing, fill = hedef)) +
  geom_bar( position="fill")

ggplot(output, aes(x = loan, fill = hedef)) +
  geom_bar( position="fill")

#month 
ggplot(output, aes(x = month, fill = hedef)) +
  geom_bar( position="fill")
ggplot(output, aes(x = month, fill = hedef)) +
  geom_bar( position="stack")

#day 
ggplot(output, aes(x = day, fill = hedef)) +
  geom_bar( position="fill")


ggplot(output, aes(x = day, fill = hedef)) +
  geom_bar( position="fill")+
  facet_wrap(~month)
##subat ayi için günler
ggplot(subset(output, month %in% "feb")) +
  geom_bar( aes(day, fill=hedef), position = "fill")

ggplot(subset(output, month %in% "oct")) +
  geom_bar( aes(day, fill=hedef), position = "fill")

ggplot(subset(output, month %in% "sep")) +
  geom_bar( aes(day, fill=hedef), position = "fill")

ggplot(subset(output, month %in% "nov")) +
  geom_bar( aes(day, fill=hedef), position = "fill")

####veri manipulasyonu 

output$previous_C <- ifelse(output$previous>0, "yes","no")
output$campain_c <- ifelse(output$campaign == 1, "one",
                    ifelse(output$campaign==2,"two",
                           ifelse(output$campaign==3, "three", "more")))

output$age_c <- cut(output$age , breaks = c(0,30,59,100), labels = c("genc","orta","yasli"))

output <- output[, -c(1,9,13,14,15)]

library(caret)
names(output)[12]<- "hedef"
str(output)
output$balance <- scale(output$balance)
output$duration <- scale(output$duration)
output$balance <- as.numeric(output$balance)
output$duration <- as.numeric(output$duration)
sapply(output,class)
output[,-c(5,10)] <- as.data.frame(lapply(output[,-c(5,10)], factor))

#training and testing 
set.seed(100)
trainRowNumbers <- createDataPartition(output$hedef, p=0.8, list=FALSE)
trainData <- output[trainRowNumbers,]
testData <- output[-trainRowNumbers,]
prop.table(table(trainData$hedef))
prop.table(table(testData$hedef))

#cross-validation and sampling islemi 
control <- trainControl(method="repeatedcv", number = 10, repeats = 10,
                        returnResamp = "all",
                        classProbs = TRUE, savePredictions = TRUE,
                        summaryFunction = twoClassSummary, sampling = "down")

library(doParallel)
cl <- makeCluster(3)
doParallel::registerDoParallel(cl)
stopCluster(cl)



hedeff<- trainData[,12]
treebag<- caret::train( x = trainData[,-12], y= trainData$hedef , method="treebag",
                        trControl =control)
fitted <- predict(treebag,testData[,-12])
caret::confusionMatrix(fitted, testData$hedef,positive = levels(as.factor(testData$hedef))[2] )

c4.5<- caret::train( x = trainData[,-12], y= trainData$hedef , method="J48",
                     trControl =control)
fitted <- predict(c4.5,testData[,-12])
caret::confusionMatrix(fitted, testData$hedef,positive = levels(as.factor(testData$hedef))[2] )



rpart <- caret::train( x = trainData[,-12], y= trainData$hedef , method="rpart",
                       parms = list(split = "gain"),
                       trControl =control,tuneLength =7)
fitted <- predict(rpart,testData[,-12])
caret::confusionMatrix(fitted, testData$hedef,positive = levels(as.factor(testData$hedef))[2] 
                       
   

mlp <- caret::train( hedef~., data = trainData, method="mlp",
                     trControl =control,tuneLength =7)

fitted <- predict(mlp,testData[,-12])
caret::confusionMatrix(fitted, testData$hedef,positive = levels(as.factor(testData$hedef))[2] )

nnet <- caret::train( hedef~., data = trainData, method="nnet",
                      trControl =control)
fitted <- predict(nnet,testData[,-12])
caret::confusionMatrix(fitted, testData$hedef,positive = levels(as.factor(testData$hedef))[2] )

svm_linear <- caret::train( hedef~., data = trainData , method="svmLinear",
                            trControl =control)

fitted <- predict(svm_linear,testData[,-12])
caret::confusionMatrix(fitted, testData$hedef,positive = levels(as.factor(testData$hedef))[2] )


svm_radial <- caret::train( hedef~., data = trainData , method="svmRadial",
                            trControl =control)


fitted <- predict(svm_radial,testData[,-12])
caret::confusionMatrix(fitted, testData$hedef,positive = levels(as.factor(testData$hedef))[2] )

stopCluster(cl)

models_compare <- resamples(list(C4.5=c4.5, RPART=rpart, 
                                 MLP=mlp, NNET=nnet
                                 ,SVM_linear =svm_linear,
                                 SVM_RADIAl = svm_radial,TREEBAG = treebag))
summary(models_compare)
scales <- list(x=list(relation="free"), y=list(relation="free"))
bwplot(models_compare, scales=scales)
dotPlot(models_compare,scales=scales)
imp <- varImp(rpart)
plot(imp)
rocs <- models_compare[[2]]
