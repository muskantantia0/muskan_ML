## Gold Deposits 

data_gold <- read.csv(file.choose(),sep = ",",header = TRUE)

# See the header of the data
head(data_gold)

# header of the column
names(data_gold)

#str of the data
str(data_gold)

# Changing the factors to dummy variables
# absent = 1
# Present = 0 
# in both the 3rd and 4th column

# Changing the column names 
names(data_gold)[1:4]<- c("Aslevel","Sblevel","Lprox","Golddeposit")
names(data_gold)


#Dividing the dataset into training and testing data
set.seed(100)
train = sample(1:64,40)
test= -train

training_data= data_gold[train,]
testing_data= data_gold[test,]

# Plotting the data
ggplot(training_data,aes(Aslevel))+
  geom_density()

ggplot(training_data,aes(Sblevel))+
  geom_density()

cor(training_data$Aslevel,training_data$Sblevel)
#Aslevel and Sblevel are higly correlated

table(data_gold$Lprox,data_gold$Golddeposit)


## Building a logistic Regression on the Equation
model1 <- glm(Golddeposit~ Aslevel+Lprox,data = training_data,family = "binomial")
summary(model1)

# it shows that Lprox is not significant 
model1 <- glm(Golddeposit~ Aslevel,data = training_data,family = "binomial")
summary(model1)

### Assess Model1 
predicted_y = predict(model1, testing_data, type = "response")
head(predicted_y)

### create the categorical variable using the predicted probabilties 
predicted_y_cat = rep("absent",24)
predicted_y_cat[predicted_y >0.5] = "present"
head(predicted_y_cat,24)

testing_y = testing_data[,4]

## Create a confusion matrix
table(testing_y, predicted_y_cat)
mean(testing_y == predicted_y_cat)

## We got 100% Accuracy.

#Inferences 
#1. Low Value of AsLevel means GoldDeposit is absent and High Value of AsLevel means there is Golddeposit
#2. presence and absense of Lprox doesn't have anything to do with GoldDeposit.
