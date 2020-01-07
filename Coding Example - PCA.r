# Title:   Coding Example - Demonstration of Principal Component Analysis
# Author:  Peter Scarbrough
# Date:    7 Jan 2020
# Purpose: Demonstrate a data analysis example using principal 
#          component analysis (PCA)

###
### 1) Load required packages
###

library(tidyverse)  # for data manipulation (dplyr) tools and ggplot2
library(caret)

### 
### 2) Load data, do brief exploratory data anlaysis
###    Data are chemical features to try to explain critical temperature for superconductivity
###    Source: https://archive.ics.uci.edu/ml/datasets/Superconductivty+Data
###

data <- read_csv("train.csv")

### get dimensions of dataset
### note: many columsn suggest dimension reduction would be helpful 
### since all data are continuous, PCA is a good option

dim(data)

### get number of missiner per column

sapply(data, FUN = function(x) sum(is.na(x)))

### reorder columns to put dependent variable in front, everything else behind

data <- select(data, critical_temp, everything())

###
### 3) do pca of indepdendent variables, get plot of variance by PC number
###    get number of principal components to explain 95% of variance
###

depVar  <- data[,-1]
stdData <- sapply(depVar, function(x) scale(x, center=T, scale=T))    
pcaData <- prcomp(stdData)

### plot cumulative variance, get 95% variance explained PC #

cumVar <- cumsum(pcaData$sdev^2)/sum(pcaData$sdev^2)
critPC <- which(cumVar > 0.95) %>% min
critPC
plot(x = 1:length(cumVar), y = cumVar,
     main = "Cumulative Variance vs Principal Component",
     ylab = "Proportion of Cumulative Variance",
     xlab = "Principal Component",
     type = "b")
abline(v = critPC, col = "red")
text(x=critPC+20, y=0.5, paste0("Min PC # for 95% Cumulative Var: ", critPC), col="red")

### select critical PC data, add critical_temp data (response variable)

critData               <- pcaData$x[,1:critPC] %>%
  as.data.frame %>%
  mutate(critical_temp = data$critical_temp) %>%
  select(critical_temp, everything())

###
### 4) model with data post-dimensional reduction data, using random forest, summarize
###    cross validate with 10-fold cross-validation
###    note: just using default tuning grid for hyperparameter selection
###

### separate into training and test subsets
### note: since the data are so large, will not need to use cross-validation

set.seed(123)
n         <- nrow(critData)
trainI    <- sample(1:n, size=0.8*n, replace=F)
trainData <- critData[trainI,]
testData  <- critData[-trainI,]

### perform random forest modeling
### note: commenting out modeling step to save computation time
###       loading precalculated object instead

myControl <- trainControl(method="none")
# rfFit     <- train(critical_temp ~ .,
#                    data=trainData,
#                    trControl=myControl,
#                    method="rf")

### save `rfFit` so it doesn't have to be re-calculated

# save(rfFit, file="rfFit.robject")
load("rfFit.robject")

###
### 5) report model performance with root mean squared error (RMSE) and r-squared
###

rfPredict <- predict(rfFit, newdata=select(testData, -critical_temp))
rmse      <- mean((rfPredict - testData$critical_temp)^2)
rmse
rSquared  <- cor(rfPredict, testData$critical_temp)^2
rSquared
