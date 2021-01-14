library(readr)
library(dplyr)
library(tidyverse)
library(tidytext)
library(lubridate)
library(caret)
library(ggplot2)

utrecht$ori.ntatie[utrecht$ori.ntatie == "Zuid"] <- 0
utrecht$ori.ntatie[utrecht$ori.ntatie == "Oost"] <- -90
utrecht$ori.ntatie[utrecht$ori.ntatie == "West"] <- 90
utrecht$ori.ntatie[utrecht$ori.ntatie == "Zuid-oost"] <- -45
utrecht$ori.ntatie[utrecht$ori.ntatie == "Zuid-west"] <- 45
utrecht$ori.ntatie <- as.integer(utrecht$ori.ntatie)

total_joule <- data.frame(Year=integer(),
                    joule=integer()) 



bilt_joule$secondes <- as.numeric((as.POSIXct(bilt_joule$YYYYMMDD, origin = "1970-01-01")))

dependent_var <- bilt_joule$Q
cor.test(bilt_joule$secondes, bilt_joule$Q)

set.seed(32343)

intrain<-createDataPartition(y=dependent_var, p=0.75, list=FALSE)
training<- bilt_joule[intrain, c("Q", "secondes")]
testing <- bilt_joule[-intrain, c("Q", "secondes")]


modelFit <- train(Q~., data= training, method="lm")
modelFit
summary(modelFit)

testing$predictions<-predict(modelFit, newdata = testing)



ggplot()+
  geom_point(aes(x=training$secondes, y=training$Q), color= "red")+
  geom_line(aes(x=training$secondes, y=predict(modelFit, newdata = training)), color= "blue")






