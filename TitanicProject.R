library(rpart)
library(rpart.plot)
library(caret)
library(ggplot2)

str(train)
View(train)
summary(train)
# We can analyze the variable of the training dataset to get a better idea of how to construct our model.
# After a quick analysis we can see that name, ticket number, and cabin are categorical and will not be a effictive feature
# in our predictive model.
# We discover a 38% survival rate based on training data set which could potentially assume that ~38% will survive the test data set.
# At this point the model is very simple and just simple probability but still gives us a better idea about predicting survival.
# With this survival rate it is safe to assume that a majority of passengers will not survive.
# Now we will prep the data for a decision tree model. We will fit the table with the necessary information.
# Emphasis on the variables selected as they have more quanitfiable information for a prediction model.

fit <- rpart(train$Survived ~ train$Pclass + train$Sex + train$Age + train$SibSp + train$Parch + train$Fare + train$Embarked, data=train, method="class")
rpart.plot(fit)


# With this visualization we can get a better image on hot the decision tree algorithm explores the data. A simple glance
# you can notice that a female, with a ticket class less than 2.5 has the most likely chance of surviving. Also,
# we can see that male children under 6 had a very likely chance to survive.
# We can use this as our base prediction model and now run it on the test data to check it's accuracy.

# With this visualization we can get a better image on hot the decision tree algorithm explores the data. A simple glance
# you can notice that a female, with a ticket class less than 2.5 has the most likely chance of surviving. Also,
# we can see that male children under 6 had a very likely chance to survive.
# We can use this as our base prediction model and now run it on the test data to check it's accuracy.


# Now explore the relationships as indicated from the tree.

# Lets explore gender relationship
summary(train$Sex)
prop.table(table(train$Sex, train$Survived),1)
barplot(table(train$Sex, train$Survived),1,ylab = "Passenger Count",xlab = "Binary survival" )

# Lets explore age
summary(train$Age)

# Change feature of children to be binary
train$Child <- 0
train$Child[train$Age < 18] <- 1


aggregate(train$Survived ~ train$Child, data=train, FUN=function(x){sum(x)/length(x)})
aggregate(train$Survived ~ train$Child + train$Sex, data=train, FUN=function(x){sum(x)/length(x)})

# Lets explore PClass

summary(train$Pclass)
aggregate(train$Survived ~ train$Pclass, data=train, FUN = sum)
aggregate(train$Survived ~ train$Pclass, data=train, FUN = function(x){sum(x)/length(x)})
aggregate(train$Survived ~  train$Child + train$Sex + train$Pclass, data=train, FUN=function(x){sum(x)/length(x)})

# Build prediction model
finalFit <- rpart(train$Survived ~ train$Pclass + train$Sex + train$Age, data=train, method="class")
rpart.plot(finalFit)

# Lets run a confusion matrix to test model
prediction<-predict(finalFit,newdata=train)

# Create test prediction variable that shows our model result
train$Pred <- 0
train$Pred <- ifelse(prediction[,2]>.5,1,0)
predictionAnswer<- factor(train$Pred)
survivedAnswer <- factor(train$Survived)
confusionMatrix(predictionAnswer,survivedAnswer)
cm1 <- (table(Actual = survivedAnswer, Prediction = predictionAnswer))
sum(diag(cm1))
addmargins(cm1)



