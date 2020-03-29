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
