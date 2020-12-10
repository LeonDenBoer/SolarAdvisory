library(readr)
library(dplyr)
library(tidyverse)
library(tidytext)
library(lubridate)
library(caret)
library(ggplot2)



total_joule <- data.frame(Year=integer(),
                    joule=integer()) 


for (i in 0:19) {
  year = i + 2000
  row <- cbind(year, sum(bilt_joule[which(year(bilt_joule[,2])==year),3]))
  total_joule <- rbind(total_joule, row)
}

dependent_var <- total_joule$V2
cor.test(total_joule$year, total_joule$V2)

set.seed(32343)

intrain<-createDataPartition(y=dependent_var, p=0.75, list=FALSE)
training<- total_joule[intrain,]
testing <- total_joule[-intrain,]


modelFit <- train(V2~., data= training, method="lm")
modelFit


testing$predictions<-predict(modelFit, newdata = testing)



ggplot()+
  geom_point(aes(x=training$year, y=training$V2), color= "red")+
  geom_line(aes(x=training$year, y=predict(modelFit, newdata = training)), color= "blue")