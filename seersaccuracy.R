## What is the Probability of a person purchasing from the store in the next 12 months?

# The customers are noncontractual, The key challenge of noncontractual settings is how to differentiate 
#those customers who have ended their relationship with the firm (without informing the firm)
#from those who are simply in the midst of a long hiatus between transactions.

## Import the file 
data <- read.csv(file.choose(),sep = ",",header = TRUE)
str(data)
summary(data)


#Assumptions 
#1. All the clients are alive for the next one year.
#2. We are going to use the Parteto/NBD model to identify our customers which was developed by  Schmittlein et al.
#   (1987), called hereafter SMC. It assumes that customers buy at a steady rate (albeit in a stochastic manner) for a period of time, 
#   and then become inactive. More specifically, time to “dropout” is modelled using the Pareto (exponential-gamma mixture) 
#   timing model,and repeat-buying behavior while active is modelled using the NBD (Poisson-gamma mixture) counting
#   model. The Pareto/NBD is a powerful model for customer-base analysis, but its empirical application can be challenging, 
#   especially in terms of parameter estimation

# Pre-Processing the data. 
# Converting the data format to dmy in Transaction_date and DOB features and calculating the Age using the Mutate function 
# in dplyr
library(dplyr)
library(lubridate)

data$Transaction_Date <- dmy(data$Transaction_Date)
data$DOB<- dmy(data$DOB)

# Turning the Client_ID into factors.
data$Client_ID <- as.factor(data$Client_ID)
summary(data$Client_ID)
unique(data$Client_ID)
str(data)

#Removing the year from the Transaction date 
data_yearwise <- data %>% mutate(Year = year(data$Transaction_Date))
str(data_yearwise)
