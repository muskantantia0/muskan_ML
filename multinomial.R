## Multinominal ##

install.packages("nnet")
library(nnet)

# Read the file from the excel file classroomexamples ---> Multinomial 
library(psych)
data <- read.clipboard.tab(header = TRUE)


#Regression Model
Model <-multinom(Loan.approval~.,data = data)
summary(Model)

#Testing 
#Salary = 400, Age = 23
Pw = exp(-2.968765+(-0.0006972633*400)+(0.06673459*23))*0.00011
Py = exp(1.248181+(0.0408701713*400)-(0.36954070*23))*0.00011
Pn = 1/(8926.801+0.1803+1)
Pw = 0.000019
Py = 0.98
Pn = 0.00011


# Regression Model only with salary
Model_Salary <- multinom(Loan.approval~ Salary,data = data)
summary(Model_Salary)

# testing 
#Salary = 400, Age = 23
exp(-0.9278587+(0.002520472*400))*Pn
exp(-1.3943540+(0.004230500*400))*Pn
Pn=1/(1.346954+1.083645+1)
Pn = 0.2914943
Pw = 0.3158
Py = 0.3926

# Regression Model only with Age
Model_Age <- multinom(Loan.approval~ Age,data = data)
summary(Model_Age)


# testing 
#Salary = 400, Age = 23
exp(-3.0377310+(0.06341143*23))*Pn
exp(0.7912124+(-0.01924094*23))*Pn
Pn=1/(1.417183+0.2061259+1)
Pn = 0.381198
Pw = 0.07857477
Py = 0.5402271

