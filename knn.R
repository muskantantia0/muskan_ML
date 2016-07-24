## KNN ##

library(MASS)
data <- iris
summary(data)

# Using the describe function
describe(data)

#Exploring the data

# Univariate 
ggplot(data,aes(Sepal.Length))+
  geom_density()

ggplot(data,aes(Sepal.Width))+
  geom_density()

ggplot(data,aes(Petal.Length))+
  geom_density()

ggplot(data,aes(Petal.Width))+
  geom_density()

# Mulivariate
ggplot(data,aes(x=Species,y=Sepal.Length))+
  geom_boxplot()

ggplot(data,aes(x=Species,y=Sepal.Width))+
  geom_boxplot()

ggplot(data,aes(x=Species,y=Petal.Width))+
  geom_boxplot()

ggplot(data,aes(x=Species,y=Petal.Length))+
  geom_boxplot()

# Considering any of the Petal.Width or Petal.Length We can easily classify classify the data.
install.packages("ggvis")
library(ggvis)

iris %>% ggvis(~Sepal.Length,~Sepal.Width,fill = ~Species) %>% layer_points()
iris %>% ggvis(~Petal.Length,~Petal.Width,fill = ~Species) %>% layer_points()


summary(iris)

## Building the model
library(class)

normalize <- function(x){
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}

iris_norm <- as.data.frame(lapply(iris[1:4],normalize))
summary(iris_norm)


## Dividing the data into training and testing dataset
set.seed(1234)
ind <- sample(2,nrow(iris),replace=TRUE,prob = c(0.67,0.33))

iris.training <- iris[ind==1,1:4]
iris.test <- iris[ind==2,1:4]

iris.trainLabels <- iris[ind==1,5]
iris.testLabels <- iris[ind==2,5]

iris_pred <- knn(train = iris.training, test = iris.test, cl=iris.trainLabels,k = 3)

iris_pred
iris.testLabels

table(iris_pred,iris.testLabels)


# Checking for k=5
iris_pred <- knn(train = iris.training, test = iris.test, cl=iris.trainLabels,k = 5)

iris_pred
iris.testLabels

table(iris_pred,iris.testLabels)

#Checking for K =7
iris_pred <- knn(train = iris.training, test = iris.test, cl=iris.trainLabels,k = 7)

iris_pred
iris.testLabels

table(iris_pred,iris.testLabels)

#Checking for K = 11
iris_pred <- knn(train = iris.training, test = iris.test, cl=iris.trainLabels,k = 11)

iris_pred
iris.testLabels

table(iris_pred,iris.testLabels)

#Checking for K = 2
iris_pred <- knn(train = iris.training, test = iris.test, cl=iris.trainLabels,k = 2)

iris_pred
iris.testLabels

table(iris_pred,iris.testLabels)


## fgl data from MASS Library
library(MASS)
data<- fgl

# Considering any of the Petal.Width or Petal.Length We can easily classify classify the data.
install.packages("ggvis")
library(ggvis)

data %>% ggvis(~RI,~Na,fill = ~type) %>% layer_points()
data %>% ggvis(~Mg,~Al,fill = ~type) %>% layer_points()

ggplot(data,aes(x=type,y=RI))+
  geom_boxplot()

ggplot(data,aes(x=type,y=Na))+
  geom_boxplot()

ggplot(data,aes(x=type,y=Mg))+
  geom_boxplot()


summary(data)

## Building the model
library(class)

normalize <- function(x){
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}

data_norm <- as.data.frame(lapply(data[1:9],normalize))
summary(data_norm)


## Dividing the data into training and testing dataset
set.seed(1234)
ind <- sample(2,nrow(data_norm),replace=TRUE,prob = c(0.67,0.33))

data.training <- data_norm[ind==1,1:9]
data.test <- data_norm[ind==2,1:9]

data.trainLabels <- data[ind==1,10]
data.testLabels <- data[ind==2,10]

data_pred <- knn(train = data.training, test = data.test, cl=data.trainLabels,k = 3)

data_pred
data.testLabels

table(data_pred,data.testLabels)
Accuracy = 46/61
Accuracy

# Using K = 5
data_pred <- knn(train = data.training, test = data.test, cl=data.trainLabels,k = 5)

data_pred
data.testLabels

table(data_pred,data.testLabels)
Accuracy = 41/(41+20)
Accuracy

# Using K = 1
data_pred <- knn(train = data.training, test = data.test, cl=data.trainLabels,k = 1)

data_pred
data.testLabels

table(data_pred,data.testLabels)
Accuracy = 45/(45+16)
Accuracy


# Using K = 2
data_pred <- knn(train = data.training, test = data.test, cl=data.trainLabels,k = 2)

data_pred
data.testLabels

table(data_pred,data.testLabels)
Accuracy = 46/(61)
Accuracy


# Using K = 4
data_pred <- knn(train = data.training, test = data.test, cl=data.trainLabels,k = 4)

data_pred
data.testLabels

table(data_pred,data.testLabels)
Accuracy = 43/(61)
Accuracy


# Using K = 10
data_pred <- knn(train = data.training, test = data.test, cl=data.trainLabels,k = 10)

data_pred
data.testLabels

table(data_pred,data.testLabels)
Accuracy = sum(diag(table(data_pred,data.testLabels)))/sum(table(data_pred,data.testLabels))
Accuracy


# Not using normalizing 
## Dividing the data into training and testing dataset
set.seed(1234)
ind <- sample(2,nrow(data),replace=TRUE,prob = c(0.67,0.33))

data.training <- data[ind==1,1:9]
data.test <- data[ind==2,1:9]

data.trainLabels <- data[ind==1,10]
data.testLabels <- data[ind==2,10]

data_pred <- knn(train = data.training, test = data.test, cl=data.trainLabels,k = 3)

data_pred
data.testLabels

table(data_pred,data.testLabels)
Accuracy = sum(diag(table(data_pred,data.testLabels)))/sum(table(data_pred,data.testLabels))
Accuracy
