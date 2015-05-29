#load libraries
library(dplyr)

#load data
train <- read.csv("F:/Programowanie/Kaggle_competitions/Titanic-Machine-Learning-from-Disaster_cmpt_0515_tit/downloaded/train.csv")
test <- read.csv("F:/Programowanie/Kaggle_competitions/Titanic-Machine-Learning-from-Disaster_cmpt_0515_tit/downloaded/test.csv")


# some exploratory analysis
table(train$Sex)
prop.table(table(train$Sex))

table(train$Survived)
prop.table(table(train$Survived))


# v1.0 they all perished
test$Survived <- rep(0,nrow(test))
submit<-select(test, PassengerId, Survived)



# save the submit file
write.csv(submit,file="output/submit_10.csv",row.names=FALSE)
