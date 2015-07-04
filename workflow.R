#Titanic, Zachary Nafziger

#import the training data
train <- read.csv("~/R/Kaggle-Titanic/data/train.csv")
#and the test data
test <- read.csv("~/R/Kaggle-Titanic/data/test.csv")

#look at it a bit
str(train)
table(train$Survived)
prop.table(table(train$Survived))#proportions

#dividing survival by gender
prop.table(table(train$Sex, train$Survived))

#finding proportions within gender division
prop.table(table(train$Sex, train$Survived),1)

#adding children column
train$Child <- 0
train$Child[train$Age < 18] <- 1

#looking at proportions with age and gender
aggregate(Survived ~ Child + Sex, data=train, FUN=sum)#count
aggregate(Survived ~ Child + Sex, data=train, FUN=function(x) {sum(x)/length(x)})#percent

#categorizing fare
#train$Fare2 <- '30+'
#train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
#train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
#train$Fare2[train$Fare < 10] <- '<10'
#aggregate(Survived ~ Fare2 + Pclass + Sex, data=train, FUN=function(x) {sum(x)/length(x)})


#now, taking things to the next level with decision trees
library(rpart)

#build a decision tree based off all of the non-unique columns
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, method="class")

#view it
plot(fit)
text(fit)

#make it pretty
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(fit)

#creating an rplot prediction
Prediction <- predict(fit, test, type = "class")

#combine them to do some feature engineering
test$Survived <- NA
combi <- rbind(train, test)

#pull out the titles from the names
combi$Name <- as.character(combi$Name) #chars instead of factors
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)#get rid of the odd space
table(combi$Title)

#group some of the titles together
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
combi$Title <- factor(combi$Title)

#looking at family attributes
combi$FamilySize <- combi$SibSp + combi$Parch + 1
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'

#get more small families
famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)

#split them apart again
train <- combi[1:891,]
test <- combi[892:1309,]

#predict from that
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
             data=train, method="class")
Prediction <- predict(fit, test, type = "class")

#now... randomforest
#need to clean up the data
#guess the ages 
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data=combi[!is.na(combi$Age),], method="anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])

combi$Embarked[c(62,830)] = "S"
combi$Embarked <- factor(combi$Embarked)
combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)

combi$FamilyID2 <- combi$FamilyID
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
combi$FamilyID2 <- factor(combi$FamilyID2)

#split them apart again
train <- combi[1:891,]
test <- combi[892:1309,]

library(randomForest)

set.seed(415)

fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize +
                      FamilyID2, data=train, importance=TRUE, ntree=2000)

Prediction <- predict(fit, test)

#now using party to make forest

set.seed(415)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
               data = train, controls=cforest_unbiased(ntree=2000, mtry=3))

#create the file
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "mysubmission.csv", row.names = FALSE)