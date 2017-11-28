# Load packages



library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
library('randomForest') # classification algorithm

install.packages('e1071')

library(foreign)
require(lmtest)
require(sandwich)
require(e1071)

setwd("E:/Kaggle/Titanic")
train <- read.csv("train.csv")
test <- read.csv("test.csv")
str(train)


ggplot(data = train , mapping = aes(x = Pclass , fill = factor(Survived))) + geom_bar(position='dodge')


#Linear Model
fm <- lm(Survived ~ Pclass + Sex + Age + factor(SibSp) + factor(Parch), data = train )
summary(fm)

#GLM

fm1 <- glm(Survived ~ Pclass + Sex + Age + factor(SibSp) + factor(Parch), data = train, family = "binomial")
summary(fm1)
plot(fm1)
par(mfrow=c(2,2))

train <- train %>% select(Survived ,Pclass,Sex,Age)
train <- na.omit(train)
test <- test %>% select(Pclass,Sex,Age)
test <- na.omit(test)
train$Pclass = factor(train$Pclass)
test$Pclass = factor(test$Pclass)
train$Survived=factor(train$Survived)
fm2 <- glm(Survived ~ Pclass + Sex + Age , data = train, family = binomial(link = "logit"))
summary(fm2)


coeftest(fm2,vcovHC(fm2, type="HC1")) 
pyhat <- predict(fm2, train,type="response")
plot(pyhat)
summary(pyhat)
yhat <- ifelse(pyhat > 0.5 , 1,0)
library(caret)

confusionMatrix(as.factor(yhat) , train$Survived)

test$Survived = as.factor(yhat)
