rm(list=ls())

df<- read.csv(".\\ds\\adult.data", sep=",")
summary(df)

names(df)[names(df)=="X..50K"] <- 'mt50k'
names(df)
library(randomForest)
randomForest(mt50k ~ .,data=df,ntree=100, importance=TRUE)


install.packages('gbm')
library(gbm)
df$isMT50k <- ifelse(df$mt50k == '<=50K',1,0)
head(subsetdf)
df$mt50k <- NULL
gbm <- gbm(isMT50k ~ .,data=df, distribution = "bernoulli",
          n.trees = 100, interaction.depth = 10, shrinkage = 0.01)
install.packages('e1071')
library(e1071)
md <- svm(isMT50k ~ ., data = df,
          kernel = "radial", gamma = 'default', cost = 1)
?svm