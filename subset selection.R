

## Subset Selection Using Leaps ##
# Find the best model to predict house price #

rm(list=ls())


library(plotly)
library("RColorBrewer")
library(MASS)
library(ISLR)
library(glmnet)
library(dplyr)
library(plyr)    
library(ggplot2)
library(ggridges)
library(corrplot)
library(lubridate)
library(boot)
library(ggpubr)
library(tidyverse)
library(caret)
library(Rcpp)
library(class)


df <- read.csv(choose.files(), header=TRUE, na.strings = "-")
# df <- read_csv("kc_house_data.csv")
head(df)

df <- subset(df, select = -c(id, date))
head(df)

names(df)
sapply(df, class)

df$floors <- as.factor(df$floors)
df$grade <- as.factor(df$grade)
df$zipcode <- as.factor(df$zipcode)
# df$bathrooms <- as.integer(df$bathrooms)
df$bathrooms <- as.factor(df$bathrooms)

names(df)
head(df)
dim(df)
sum(is.na(df))

df <- na.omit(df)
dim(df)
sum(is.na(df))

# leaps library to perform subset selection.
library(leaps)

regfit.full <- regsubsets(price~.,data = df)


# Best subset selection
regfit.full <- regsubsets(price~.,data = df, nvmax = 15)

summary(regfit.full)

# observe the variables associated with the best-fitting model
reg.summary.full <- summary(regfit.full)
names(regfit.full)

#Examine the adjusted R^2 fit
reg.summary.full$rsq
reg.summary.full$adjr2

par(mfrow=c(1,1))
plot(reg.summary.full$rsq,xlab="Number of Regression", ylab="R-square", type = "l") 
plot(reg.summary.full$adjr2,xlab="Number of Regression", ylab="Adjusted R-square", type = "l")
a <- which.max(reg.summary.full$adjr2)
points(a,reg.summary.full$adjr2[a], col="red", cex=2, pch=20)

#The best subset selection, the fit increases at 3 predictors, then levels off.

reg.summary.full$adjr2[11]
#With all 11 predictors, adjusted R^2 = 0.8608


par(mfrow=c(1,1))

# CP fit
plot(reg.summary.full$cp,xlab="Number of Regression", ylab="Cp", type = "l")
a1 <- which.min(reg.summary.full$cp)
points(a1,reg.summary.full$cp[a1], col="red", cex=2, pch=20)

reg.summary.full$cp[11]
#With 11 predictors, PC = 6.500

# BIC fit
plot(reg.summary.full$bic,xlab="Number of Regression", ylab="BIC", type = "l")
a2 <- which.min(reg.summary.full$bic)
points(a2,reg.summary.full$bic[a2], col="red", cex=2, pch=20)
#The fit improves dramatically at 6 predictors, then levels off.

reg.summary.full$bic[11]
#With 11 predictors, bic = -338.36.


par(mfrow=c(1,1))

plot(regfit.full, scale="r2")
plot(regfit.full, scale="adjr2")
plot(regfit.full, scale="Cp")
plot(regfit.full, scale="bic")

coef(regfit.full, 4)


regfit.fwd <- regsubsets(price~.,data=df,nvmax=15,method="forward")
reg.summary.fwd <- summary(regfit.fwd)
reg.summary.fwd

regfit.bwb <- regsubsets(price~.,data=df,nvmax=15,method="backward")
reg.summary.bwb <- summary(regfit.bwb)
reg.summary.bwb

coef(regfit.full,7)
coef(regfit.fwd,7)
coef(regfit.bwb,7)

par(mfrow=c(1,1))

plot(reg.summary.full$bic,xlab="Number pf Regression", ylab="BIC",main="Best Subset Selection", type = 'l')
a2 <- which.min(reg.summary.full$bic)
points(a2,reg.summary.full$bic[a2], col="red", cex=2, pch=20)

plot(reg.summary.fwd$bic,xlab="Number pf Regression", ylab="BIC",main="Forward stepwise Selection", type = "l")
a2 <- which.min(reg.summary.fwd$bic)
points(a2,reg.summary.fwd$bic[a2], col="red", cex=2, pch=20)

plot(reg.summary.bwb$bic,xlab="Number of Regression", ylab="BIC",main="Backward Stepwise Selection", type = "l")
a2 <- which.min(reg.summary.bwb$bic)
points(a2,reg.summary.bwb$bic[a2], col="red", cex=2, pch=20)

par(mfrow=c(1,1))

plot(regfit.full, scale="bic", main="Best Subset Selection")
plot(regfit.fwd, scale="bic", main="Forward Stepwise Selection")
plot(regfit.bwb, scale="bic", main="Backward Stepwise Selection")





