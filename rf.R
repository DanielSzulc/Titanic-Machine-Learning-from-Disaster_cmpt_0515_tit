# random forest

# libraries
library(randomForest)
# load data
train <- read.csv("F:/Programowanie/Kaggle_competitions/Titanic-Machine-Learning-from-Disaster_cmpt_0515_tit/downloaded/train.csv")
test <- read.csv("F:/Programowanie/Kaggle_competitions/Titanic-Machine-Learning-from-Disaster_cmpt_0515_tit/downloaded/test.csv")
# feat engin

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


# NA elimination
summary(combi$Age)
AgeFit <- rpart(Age ~ Pclass + Sex + Fare + FamilySize + Embarked + Parch, 
                data = combi[!is.na(combi$Age),], method="anova")
combi$Age[is.na(combi$Age)]<-predict(AgeFit,combi[is.na(combi$Age),])

which(combi$Embarked=="")
combi$Embarked[which(combi$Embarked=="")]<-"S"
combi$Embarked<-factor(combi$Embarked)
which(is.na(combi$Fare))
combi$Fare[which(is.na(combi$Fare))]<-median(combi$Fare,na.rm=TRUE)
combi$FamilyID2<-combi$FamilyID
combi$FamilyID2<-as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize<=3] <-"Small"
combi$FamilyID2<-factor(combi$FamilyID2)

#split the data back
train <-combi[1:891,]
test<-combi[892:1309,]


# model
set.seed(415)
ft_RF <- randomForest(as.factor(Survived)~ Pclass + Sex + Age + SibSp + Parch +
                              Fare + Embarked + Title + FamilySize +
                              FamilyID2, data= train,
                      importance = TRUE,
                      ntree = 2000)
varImpPlot(ft_RF)
#predict
Predicted <- predict(ft_RF, test)

#output
test$Survived<-Predicted
submit<-select(test, PassengerId, Survived)
write.csv(submit,file="output/submit_60.csv",row.names=FALSE,quote=FALSE)

## v 7.0 conditional inference trees

#model
library(party)
#predict
set.seed(415)
fit_CIT <- cforest(as.factor(Survived)~Pclass + Sex + Age + SibSp + Parch+ Fare+ 
                           Embarked + Title + FamilySize + FamilyID,
                   data=train,
                   controls=cforest_unbiased(ntree=2000,mtry=3))
#outcome
Predicted_CIT <- predict(fit_CIT,test, OOB = TRUE, type="response")

test$Survived<-Predicted_CIT
submit<-select(test, PassengerId, Survived)
write.csv(submit,file="output/submit_70.csv",row.names=FALSE, quote=FALSE)
