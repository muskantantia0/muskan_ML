#Rscript for Universal bank 
data <- UniversalBank1
str(data)

## Data cleaning 


## Checking for Multi-colleniarity
cor(data$PersonalLoan,data$Education)
cor(data$PersonalLoan,data$Income)
cor(data$PersonalLoan,data$SecuritiesAccount)
cor(data$PersonalLoan,data$Online)
cor(data$PersonalLoan,data$CreditCard)
cor(data$PersonalLoan,data$Family)

cor(data$Income,data$Family)
cor(data$Income,data$CCAvg)



## Ignoring the data ID and Zip.Code from the equation
model5 <- glm(PersonalLoan~. -ZIP.Code-ID,data = data, family = "binomial")
summary(model5)

model6 <- glm(PersonalLoan ~ Income+Family+CCAvg+Education+SecuritiesAccount+CDAccount+Online+CreditCard,data=data,family = "binomial")
summary(model6)

predicted_y = predict(model5, testing_data, type = "response")
head(predicted_y)

### create the categorical variable using the predicted probabilties 
predicted_y_cat = rep("Down",252)
predicted_y_cat[predicted_y >0.5] = "Up"
head(predicted_y_cat,200)

testing_y = testing_data$Direction 

## Create a confusion matrix
table(testing_y, predicted_y_cat)
mean(testing_y == predicted_y_cat)


##plotting the model 
library(ggplot2)
ggplot(data,aes(x=PersonalLoan))
