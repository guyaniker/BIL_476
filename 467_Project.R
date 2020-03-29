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
