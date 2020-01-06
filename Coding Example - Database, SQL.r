# Title:   Coding Example - Databases and SQL Code in R
# Author:  Peter Scarbrough
# Date:    6 Jan 2020
# Purpose: Demostrate how to create databases and query with 
#          dplyr and SQL code

###
### 1) Load Required Packages
###

library(tidyverse)  # Standard data manipulation tools in R (e.g. dplyr)
library(RSQLite)    # Database backend tools

###
### 2) Create example database from iris data
###

### 2i)  Create copy of iris data with key column

n      <- nrow(iris)
myIris <- mutate(iris, key=1:n)

### 2ii) Use iris to create fake, randomized companion dataset to go in database
###      Contains manufactured info on color, owner, and mass
###        colors -- to be assigned based on species with some random noise
###        owners -- to be randomly assigned
###        mass   -- to be assigned through numeric variables with some random error

owners    <- c("Peter", "Paul", "Mary")
colors    <- c("Red", "Green", "Blue")
otherIris <- data.frame(key=1:n)
otherIris$owner <- sample(owners, size=n, replace=T)
otherIris$color <- sapply(iris$Species %>% as.character, function(x){
                     switch(x,
                            "setosa"     = colors[1],
                            "virginica"  = colors[2],
                            "versicolor" = colors[3])
                   })
otherIris$color <- ifelse(runif(n) > 0.25, otherIris$color, sample(colors, size=n, replace=T))
otherIris$mass  <- iris$Sepal.Length + iris$Sepal.Width + 
                   iris$Petal.Length + iris$Petal.Width + abs(rnorm(150, mean=0, sd=2))

### 2iii) create database: add the two iris tables

myDB <- src_sqlite("myDB.sqlite", create=T)
copy_to(myDB, myIris)
copy_to(myDB, otherIris)

###
### 3) Database query examples using dplyr and SQL code
###

### 3i) Get all sepal features from red flowers owned by Peter
###     Using dplyr Code
###     keep owner and color data to show query worked as intended

t1     <- tbl(myDB, "myIris")
t2     <- tbl(myDB, "otherIris")
res.3i <- select(t1, contains("Sepal"), "key") %>%
  inner_join(t2, by="key") %>%
  filter(color == "Red" & owner == "Peter") %>%
  select(contains("Sepal"), color, owner) %>%
  collect()
res.3i

### 3ii) Same as above
###      Using SQL code

res.3ii <- tbl(myDB, sql(
  "
    SELECT `Sepal.Length`, `Sepal.Width`, `color`, `owner` FROM `myIris`
    INNER JOIN `otherIris`
    USING (`key`)
    WHERE `color` = 'Red' AND `owner` = 'Peter'
    
  "
)) %>% collect()
res.3ii

### 3iii) Show that results are the same -- only matches exist
unique(res.3i == res.3ii)

###
### 4) Database summarizing example using dplyr and SQL code
###

### 4i) Get avg petal measurements and avg mass per owner for green flowers only
###     using dplyr code

t1     <- tbl(myDB, "myIris")
t2     <- tbl(myDB, "otherIris")
res.4i <- select(t1, contains("Petal"), "key") %>%
  inner_join(t2, by="key") %>%
  filter(color == "Green") %>%
  group_by(owner) %>%
  summarize(avg_petal_length = mean(Petal.Length),
            avg_petal_width  = mean(Petal.Width), 
            avg_mass         = mean(mass)) %>%
  collect() 
res.4i

### 4ii) Same as above but using SQL code

res.4ii <- tbl(myDB, sql(
  "
    SELECT `owner`, 
           AVG(`Petal.Length`) AS `avg_petal_length`, 
           AVG(`Petal.Width`)  AS `avg_petal_width`,
           AVG(`mass`)         AS `avg_mass`
    FROM `myIris`
    INNER JOIN `otherIris`
    USING (`key`)
    WHERE `color` = 'Green'
    GROUP BY `owner`
  "
)) %>% collect()
res.4ii