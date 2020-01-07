# Title:   Coding Example - General and Generalized Linear Regression in R
# Author:  Peter Scarbrough
# Date:    6 Jan 2020
# Purpose: Demonstrate how to build and examine some basic LR and GRL
#          Models in R

###
### 1) Load required packages
###

library(tidyverse)  ## basic data manipulation and plotting package in R (e.g. dplyr, ggplot2)
library(caret)      ## cross-validation and modeling tools

###
### 2) Linear regression of infant mortality in `swiss` dataset
###    Just building models and showing syntax: No diagnostics here...
###

### 2i) Simple linear regression

m1 <- lm(Infant.Mortality ~ Fertility, data=swiss)
summary(m1)


### 2ii) Multiple linear regression (using all predictors)

m2 <- lm(Infant.Mortality ~ ., data=swiss)
summary(m2)

### 2iii) Same as above but add squared predictors
m3 <- lm(Infant.Mortality ~ Fertility + Agriculture + Examination + Education + Catholic +
         I(Fertility^2) + I(Agriculture^2) + I(Examination^2) + I(Education^2) + I(Catholic^2),
         data=swiss)
summary(m3)

###
### 3) Do simple variable selection in `mtcars` dataset; model `mpg`
###

### 3i) Note presense of high correlation -- signifies need for variable selection

mtcars %>% cor %>% round(2)

### 3ii) Further evidence of co-linearity problems in modeling
###        First, note many positive trends with `mpg`

plot(mtcars)

###        Now, note that no terms reach significance in full model -- co-linearity is possible problem

m4 <- lm(mpg ~ ., data=mtcars)
summary(m4)

### 3iii) Split into test and training data sets; 30/70 split

set.seed(123)
n         <- nrow(mtcars)
p         <- ncol(mtcars)
trainI    <- sample(1:n, size=0.7*n, replace=F)
trainData <- mtcars[trainI,]
testData  <- mtcars[-trainI,]

### 3iv) Set training control (10-fold cross-validation with 5 repeats), run var selection
###      Variable selection: stepwise (forward and backward) using AIC selection

myControl <- trainControl(method="repeatedcv", number=10, repeats=5)
lmFit     <- train(mpg ~ .,
                   data=trainData,
                   trControl=myControl,
                   method="lmStepAIC",
                   trace=F)
summary(lmFit)

### 3v) estimate R-squared with test dataset

predictMPG     <- predict(lmFit, newdata=select(testData, -mpg))
estCorrelation <- cor(predictMPG, testData$mpg)
estR2          <- estCorrelation^2
estR2

###
### 4) Logistic regression example, using esophogeal cancer data after reshaping
###

### 4i) Quick look at the data, then reshape data for logistic regression
###     Reshape to a tidy data set -- 1 row = 1 case or 1 control

### notice starting data structure
head(esoph)

### create empty data frame to hold reshaped result
reshapeEsoph <- esoph[0,] %>% 
  select(-ncases, -ncontrols) %>% 
  mutate(status = factor(character(), levels=c("Control", "Case")))

### loop through original data set and reshape to tidy data frame
for(i in 1:nrow(esoph)){
  agegp <- esoph[i, 1]
  alcgp <- esoph[i, 2]
  tobgp <- esoph[i, 3]
  ncas  <- esoph[i, 4]
  ncon  <- esoph[i, 5]
  if(ncas != 0){
    temp  <- data.frame(agegp=agegp, alcgp=alcgp, tobgp=tobgp, status="Case")
    for(j in 1:ncas){
      reshapeEsoph <- rbind(reshapeEsoph, temp)
    }
  }
  if(ncon != 0){
    temp  <- data.frame(agegp=agegp, alcgp=alcgp, tobgp=tobgp, status="Control")
    for(j in 1:ncon){
      reshapeEsoph <- rbind(reshapeEsoph, temp)
    }
  }
}

### 4ii) Perform logistic regression on reshaped data
###      Just calculating the full model -- no diagnostics or features selection

gm1 <- glm(status ~ ., data=reshapeEsoph, family="binomial")
summary(gm1)

