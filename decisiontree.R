# Using rpart

## Install the package
install.packages("rpart")
library(rpart)
kyphosis

#Grow Tree
fit <- rpart(Kyphosis ~ Age + Number + Start, method = "class", data = kyphosis)

## this is a generic Algorithm. Means we can use any data instead of kyphosis

#Plot tree
plot(fit,uniform = TRUE, main = "Classification Tree for Kyphosis")
text(fit,use.n = TRUE,all = TRUE,cex=.8)


# Using Decision tree
## Install the Libraries given below incase if you don't have
library(MASS)
library(tree)
data("mcycle")
mcycle

plot(accel~times,data=mcycle)
mct <- tree(accel ~ times, data=mcycle)
plot(mct,col=8)
text(mct,cex=.75)
