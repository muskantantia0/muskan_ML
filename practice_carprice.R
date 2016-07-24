## Toyota Corolla

#Getting the data 
data <- read.csv(file.choose(),sep = ",",header = TRUE)

# Checking the dimensions of the data
dim(data)

# Check the names of the data
names(data)

#Check the str of the data
str(data)

#Check the summary of the data
summary(data)


# We have Seen that MetColor, Doors and Automatic need to be converted to factors.
data$MetColor <- as.factor(data$MetColor)
data$Automatic <- as.factor(data$Automatic)
data$Doors <- as.factor(data$Doors)

#Checking for Linearity
pairs(data)

## Loading the ggplot and making all the graphs to check the collinearity.
library(ggplot2)
ggplot(data,aes(y=Price, x = Age))+
  geom_point()
#Negitively correlated.

ggplot(data,aes(y=Price, x= KM))+
  geom_point()
#We see an Inverse Trend in the data

ggplot(data,aes(y=Price, x= FuelType))+
  geom_boxplot()
# We see that the median is equally same but some petrol cars very highly priced compared to others. A few Outliers in the Diesel

ggplot(data,aes(y=Price, x=MetColor))+
  geom_boxplot()
#MetColor Doesn't have high significance as the median is the same 

ggplot(data,aes(y=Price,x= Automatic))+
  geom_boxplot()
#A lot of cars are outside the range

ggplot(data,aes(y=Price, x= CC))+
  geom_point()
# A small change in the data. We will try to coherce the data to Factors and merge some factors
data$CC <- as.factor(data$CC)
levels(data$CC)
data$CC[data$CC == "1332"] <- "1300"
data$CC[data$CC == "1398"] <- "1400"
data$CC[data$CC == "1587"] <- "1600"
data$CC[data$CC == "1598"] <- "1600"
data$CC[data$CC == "1975"] <- "2000"
data$CC[data$CC == "1995"] <- "2000"
levels(data$CC)
data$CC <- factor(data$CC)
levels(data$CC)


ggplot(data,aes(x=Doors, y = Price))+
  geom_boxplot()
## Some of the 3,4,5 door cars very extensively priced as compared to the overall cars

ggplot(data,aes(x=Weight,y=Price))+
  geom_point()


## Filtering the data which is more than 1600 in weight 
which.max(data$Weight)
data <- data[-222,]
which.max(data$Weight)
which.max(quantile(data$Weight,probs  = seq(from=0.95, to = 1, by= 0.01)))
data<- data[-6,]

##There is a bad levarage point at data$Weight > 1400 and data$Price <10000
which.max(data$Weight > 1400 & data$Price < 10000)
data <- data[-960,]

##
ggplot(data,aes(x=Weight,y=Price))+
  geom_point()

## Dividing the data into train and test 
set.seed(100)
train = sample(1:1434,1000)
test= -train

training_data= data[train,]
testing_data= data[test,]


## Now since we are done with the cleaning of data. We will try to find the most significant model by using linear regression and
## keeping all the variables 
model1 <- lm(Price~., data = training_data)
summary(model1)

## We see that MetColor, Automatic and Doors are Unsignificant 
model2 <- lm(Price~. -Automatic-MetColor-Doors-HP,data = training_data)
summary(model2)


## Checking for Multi-collinearity 
library(car)
vif(model2)

## We can see that FuelType and CC are highly correlated. So removing the CC from the equation 
model3 <- lm(Price~ Age+KM+FuelType+Weight,data = training_data)
summary(model3)
names(model3)

# Plotting the residuals against the model shows us a trend in the residuals. So there might be some non-linearity in the data
plot(predict(model3),residuals(model3))

# Checking for non-linearity
ggplot(training_data,aes(x=Age,y=Price))+
  geom_point()
#Trend is linear

ggplot(training_data,aes(x=KM,y=Price))+
  geom_point()
#Trend is Curvilnear.


ggplot(training_data,aes(x=Weight,y=Price))+
  geom_point()
#Trend is Linear.


# Plotting the residual data
plot(predict(model3),residuals(model3))

### Assess the model using the testing data
testing_y = testing_data[,-1]
predicted_y = predict(model3,testing_y)

##compute the MSE 
error = testing_data$Price - (predicted_y)
error_squared = (error)^2
MSE = mean(error^2)
MSE

#The MSE is 1608326
# Multiple R2 ---> 86.81


## Inferences
#1. As Age and No of KM's travlled Increased, the Price of the Car decreased.
#2. Controlling Age,KM and Weight, Petrol Car return better value than Desiel Car
#3. Weight of the Car is Positively correlated with the Price. As Weight Increases the Price of the Car Increase
#4. CC,Automatic and Doors Don't contribute to the Price of the Car effectively.
