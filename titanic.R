#Titanic, Zachary 
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(party)

#import the training data
train <- read.csv("~/R/Kaggle-Titanic/data/train.csv")
#and the test data
test <- read.csv("~/R/Kaggle-Titanic/data/test.csv")


#Combine them to do some feature engineering
test$Survived <- NA #test doesn't have the survived column, so it needs to be created
combi <- rbind(train, test)

#pull out the titles from the names
combi$Name <- as.character(combi$Name) #chars instead of factors
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)#get rid of the odd space
table(combi$Title)

#Group some of the titles together
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
combi$Title <- factor(combi$Title)

#Looking at and engineering family attributes
combi$FamilySize <- combi$SibSp + combi$Parch + 1
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'

#Catgorize more small families
famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)


#need to clean up the data
#guess the ages 
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data=combi[!is.na(combi$Age),], method="anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])

combi$Embarked[c(62,830)] = "S"
combi$Embarked <- factor(combi$Embarked)
combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)

#Now break the sets apart again
train <- combi[1:891,]
test <- combi[892:1309,]

#Using party to make a forest

set.seed(69)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
               data = train, controls=cforest_unbiased(ntree=2000, mtry=3))

#create the file
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "mysubmission.csv", row.names = FALSE)

