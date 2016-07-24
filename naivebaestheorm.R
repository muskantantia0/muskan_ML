## Install the following packages
install.packages("mlbench")
install.packages("e1071")

## call them
library(mlbench)
library(e1071)

## Calling the following dataset
data(HouseVotes84, package = "mlbench")
HouseVotes84
str(HouseVotes84)
model <- naiveBayes(Class ~., data = HouseVotes84)

## Predict
predict(model,HouseVotes84[1:10,])
predict(model,HouseVotes84[1:10,],type = "raw")

pred <- predict(model,HouseVotes84)
table(pred,HouseVotes84$Class)


#predicting 
pred


## This model cannot be learned as it is congress data. Here we cannot increase tupels but we can add features.


## Computer Purchase Data
library(psych)
file1 <- read.clipboard.tab(header = TRUE)

model1 <- naiveBayes(Computer.bought ~., data = file1)

## Predict
predict(model1,file1[1:10,])
predict(model1,file1[1:10,],type = "raw")

pred <- predict(model1,file1)
table(pred,file1$Computer.bought)

