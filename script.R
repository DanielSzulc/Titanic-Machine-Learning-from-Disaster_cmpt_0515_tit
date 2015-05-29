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


# v2.0 based on sex
prop.table(table(train$Sex, train$Survived))
prop.table(table(train$Sex, train$Survived),1)

test$Survived[test$Sex=="female"]<-1

# v3.0 age

summary(train$Age)

        # new variable child
train$Child<-0
train$Child[train$Age<18]<-1
prop.table(table(train$Child,train$Survived),1)

        #AGE AND SEX table
aggregate(Survived~Child+Sex, data=train, FUN = sum)
group_by(train, Child,Sex) %>% summarize(Survived=sum(Survived))
group_by(train, Child,Sex) %>% summarize(Survived=sum(Survived),total=n(), prop=Survived/total)

        # class and fare
train$Fare_F <-cut(train$Fare,breaks = c(-1,10,20,30,max(train$Fare)))
summary(train$Fare)
summary(train$Fare_F)

group_by(train,Sex,Pclass,Fare_F) %>% summarise(Survived=sum(Survived),
                                                total=n(),p=Survived/total)
test$Survived<-0
test$Survived[test$Sex=="female"]<-1
test$Survived[test$Sex=="female" & test$Fare>20 & test$Pclass==3]<-0
# save the submit file
submit<-select(test, PassengerId, Survived)
write.csv(submit,file="output/submit_30.csv",row.names=FALSE)

