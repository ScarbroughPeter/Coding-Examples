# Title:   Coding Example - Machine Learning Methods in R
# Author:  Peter Scarbrough
# Date:    6 Jan 2020
# Purpose: Demonstrate basic machine learning method execution in R
#          Will be predicting Species using built-in `iris` dataset

###
### 1) Load required packages
###
library(tidyverse)  # for data manipulation (dplyr tools) and ggplot2
library(caret)      # for cross-validation and machine learning methods
library(GGally)     # for fancier pairs plots plotting with ggplot2

###
### 2) Load data, split 80/20 into training/test sets
###
iris
n         <- nrow(iris)
p         <- ncol(iris)
trainI    <- sample(1:n, size=0.8*n) 
trainData <- iris[trainI,]
testData  <- iris[-trainI,]

###
### 3a) Exploratory data anaalysis
###
ggpairs(iris)                            # pairs plot
sapply(iris, function(x) sum(is.na(x)))  # check for missing data (there is none)
summary(iris)                            # basic numeric summaries

###
### 3b) Get summary measurements by Speciies
###

### 3b-i) Using tapply
data.frame(Mean    = sapply(select(iris,-Species), function(x){
                       tapply(x, iris$Species, mean) %>% round(2)
                     }),
           SD      = sapply(select(iris,-Species), function(x){
                       tapply(x, iris$Species, sd) %>% round(2)
                     })
           )

### 3b-ii) Using dplyr with custom function: `summarizeIris`
summarizeIris <- function(fun){
  # a) summarize by species given summary function
  temp <- iris %>% 
    group_by(Species) %>%
    summarize_all(fun)
  # b) get function name for column renaming of summary data table
  funName <- substitute(fun) %>% deparse
  # c) rename all columns but the Sepcies column by adding function name as suffix
  names(temp)[-1] <- paste0(names(temp)[-1], paste0("." , funName))
  return(temp)
}

# create summary tables of iris, merge and round numeric values
irisMeans <- summarizeIris(mean)
irisSDs   <- summarizeIris(sd)
inner_join(irisMeans, irisSDs) %>% 
  mutate_if(is.numeric, round, 2) %>%
  as.data.frame 

###
### 4) Plotting an example of how data are related to Species
###

### 4i) Plot of sepal features by species
ggplot(iris, aes(x=Sepal.Width, y=Sepal.Length)) + 
  geom_point(aes(color=Species), size=2, alpha=0.5) +
  labs(title = "Sepal Features by Species", 
       x     = "Sepal Width",
       y     = "Sepal Length")

### 4ii) Plot of petal features by species
ggplot(iris, aes(x=Petal.Width, y=Petal.Length)) + 
  geom_point(aes(color=Species), size=2, alpha=0.5) +
  labs(title = "Petal Features by Species", 
       x     = "Petal Width",
       y     = "Petal Length")

### Note: Clearly distinguishable relationships between sepal, petal features and species


###
### 5) Machine learning models, report misclassification rate (MCR)
###

### 5i) Species cross-validation method, 10-fold CV with 5 repeats (due to small data size)
myControl <- trainControl(method="repeatedcv", number=10, repeats=5)

### 5ii) K-neearest neighbors, report confusion matrix and MCR
knnFit     <- train(Species ~ .,
                    data=trainData,
                    method="knn",
                    trControl=myControl,
                    tuneLength=30)
knnPredict <- predict(knnFit, newdata=select(testData, -Species))
knnCM      <- table(knnPredict, testData$Species)
knnMCR     <- 1 - sum(diag(knnCM))/sum(knnCM)
knnCM
knnMCR

### 5iii) Random Forest, report confusion matrix and MCR
rfFit      <- train(Species ~ .,
                    data=trainData,
                    method="rf",
                    trControl=myControl)
rfPredict  <- predict(rfFit, newdata=select(testData, -Species))
rfCM       <- table(rfPredict, testData$Species)
rfMCR      <- 1 - sum(diag(rfCM))/sum(rfCM)
rfCM
rfMCR

### 5iv) Neural Net, report confusion matrix and MCR
nnFit      <- train(Species ~ .,
                    data=trainData,
                    method="nnet",
                    trControl=myControl,
                    trace=F)
nnPredict  <- predict(nnFit, newdata=select(testData, -Species))
nnCM       <- table(nnPredict, testData$Species)
nnMCR      <- 1 - sum(diag(nnCM))/sum(nnCM)
nnCM
nnMCR

### 5v) Summarize Model Performance
###     doing some data reshaping to make it easier to add models to summary data.frame
data.frame(KNN  = knnMCR,
           RF   = rfMCR,
           NNet = nnMCR) %>%
  t %>% as.data.frame %>% rownames_to_column(var="Model") %>% rename(MCR = V1) %>%
  arrange(MCR) %>% mutate_if(is.numeric, round, 3) 

###
### 6) Plot model performance
###

myIris <- mutate(iris, Predict=predict(nnFit, newdata=select(iris, -Species)))
ggplot(myIris, aes(x=Sepal.Width, y=Sepal.Length)) +
  geom_point(aes(color=(Species==Predict)), size=2, alpha=0.5) + 
  labs(title = "Performance of Neural Net Model",
       x     = "Sepal Width",
       y     = "Sepal Length") + 
  scale_color_discrete(name  = "Prediction",
                       label = c("Incorrect", "Correct"))

###
### 7) Comments:
### 

### Note that the models performed fairly well and were similar which is probably expected
### given that the data features appeared relatively distinguishable by Species to begin with