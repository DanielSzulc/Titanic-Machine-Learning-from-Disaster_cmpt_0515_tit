#load libraries
library(dplyr)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
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


# first real machine learning algorithm
# v4.0 decision tree
fit <-rpart(Survived ~ Pclass+Sex+Age+SibSp+Parch+Fare+Embarked, data=train,
            method="class")
rpart.plot(fit, extra = 4, type=2)
Prediction<-predict(fit43,test,type="class")
test$Survived<-Prediction

#control complexity of the tree
fit43 <-rpart(Survived ~ Pclass+Sex+Age+SibSp+Parch+Fare+Embarked, data=train,
            method="class",control=rpart.control(minsplit=10))

new.fit <- prp(fit42,snip=TRUE)$obj
fancyRpartPlot(fit43)

## feauture engineering
train$Name[1]
        # combine two sets (train and test) to make the same operations on feautre 
        # at the same time
test$Survived<-NA
combi<-rbind(train,test)

        # get rid off the factor (as deafult when read in data)
combi$Name<-as.character(combi$Name)
combi$Title<-sapply(combi$Name, function(x) {strsplit(x,split="[.,]")[[1]][2]})
combi$Title<-sub(" ","",combi$Title)
        # merge rare titles
combi$Title[combi$Title %in% c("Mlle","Mme")]<-"Mlle"
combi$Title[combi$Title %in% c("Capt","Col","Major","Sir","Jonkheer","Don")]<-"Sir"
combi$Title[combi$Title %in% c("Dona","Lady","the Countess")]<-"Lady"
table(combi$Title)
combi$Title <-factor(combi$Title)
combi$FamilySize <- combi$SibSp + combi$Parch + 1

#extract surname
combi$Surname <- sapply(combi$Name, function(x){strsplit(x,split="[,.]")[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize),combi$Surname,sep="")
combi$FamilyID[combi$FamilySize<=2] <-"Small"
small_family <- data.frame(table(combi$FamilyID))
small_family<-small_family[small_family$Freq<=2,]
combi$FamilyID[combi$FamilyID %in% small_family$Var1]<-"Small"
combi$FamilyID<-factor(combi$FamilyID)

train <-combi[1:891,]
test <-combi[892:1309,]

fit <-rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + 
                    Title + FamilySize + FamilyID,
            data = train,
            method="class")
fancyRpartPlot(fit)
Predict <-predict(fit,test,type="class")
test$Survived<-Predict

fit52 <-rpart(Survived ~  Fare + Embarked + Title + FamilySize,
            data = train,
            method="class")
fancyRpartPlot(fit52)
Predict <-predict(fit51,test,type="class")
test$Survived<-Predict

# save the submit file
submit<-select(test, PassengerId, Survived)
write.csv(submit,file="output/submit_51.csv",row.names=FALSE)

