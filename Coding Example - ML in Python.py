# title:   Coding Example - Machine Learning Examples in Python
# author:  Peter Scarbrough
# date:    8 Jan 2020
# purpose: Showcase basic machine learning examples in Python

### 1. load required packages
import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
from sklearn import datasets
from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestClassifier

### 2. Load `iris` data and convert to data frame
iris = datasets.load_iris()

### 2a. Get object type, attributes, column names
print(type(iris))
print(dir(iris))
print(iris.feature_names)
print(iris.target_names)

### 2b. Convert `iris` to pandas data frame
iris_df = pd.DataFrame(data    = np.c_[iris["data"], iris["target"]],
                       columns  = ["sepal_length", "sepal_width", \
                                   "petal_length","petal_width", \
                                   "species"])

### 3. recode species as factor

### 3a. get factor name dictionary
sp_numb = iris_df["species"].unique()
sp_name = iris.target_names
sp_dict = dict(zip(sp_numb, sp_name))

### 3b. convert to character, then factor
iris_df["species"] = iris_df["species"].apply(lambda x: sp_dict[x])
iris_df["species"] = iris_df["species"].astype('category')

### 3c. confirm variable names and data type
print(iris_df.apply(lambda x: x.dtype))

### 3d. check data, print first 10 rows
iris_df.head(10)

### 4. simple exploratory data analysis of `iris` data

### custom print function
def printn(*args):
    for arg in args:
        print(arg)
    print()
    return(None)

### 4a. check for missing values
printn("Number of Missing values: ", 
       iris_df.isnull().sum())

### 4b. col means, col sds
col_means = iris_df.iloc[:,0:4].apply(lambda x: x.mean())
col_sds   = iris_df.iloc[:,0:4].apply(lambda x: x.std())
printn("Column Means:",
       col_means)
printn("Column Stds:",
       col_sds)

### 4c. get species counts
printn("Species Counts: ",
       iris_df["species"].value_counts())

### 4d. get means, stds by species
printn("Means by Species:",
       iris_df.groupby("species").mean())
printn("Standard deviation by Species:",
       iris_df.groupby("species").std())

### 4e. more summary measures and dataset information
printn(iris_df.describe())
print(iris_df.info())

### 5. plots of some plant features by species

g = sns.relplot(x="sepal_length",
                y="sepal_width",
                hue="species",
                data=iris_df);
g.set_axis_labels("Sepal Length", "Sepal Width")
g.fig.suptitle("Sepal Features by Species")
plt.show(g)

### pairs plot

sns.pairplot(iris_df, hue="species")

### 6. split data into test and training data (20/80) split

### define predictor (x) and response (y) subsets

x = iris_df.iloc[:,0:4]
y = iris_df["species"]

x_train, x_test, y_train, y_test = train_test_split(x, y, test_size=0.2)

### 7. do random forest model

### get gausian classifier
rfc = RandomForestClassifier(n_estimators=100)

### get random forest model with training data
rfc.fit(x_train, y_train)

### get random forest prediction
y_pred = rfc.predict(x_test)

### get misclassification rate
mcr = 1 - sum(y_pred == y_test)/len(y_pred)
printn("Misclassification Rate: ",
       mcr)

### 8. Plot random forest results

### add prediction outcome to data
pred_total = rfc.predict(x)
iris_plot = iris_df.copy()
iris_plot["predicted"] = (pred_total == iris_plot["species"])

### plot model results of correctly predicted species
g = sns.relplot(x="sepal_length",
                y="sepal_width",
                hue="predicted",
                data=iris_plot);
g.set_axis_labels("Sepal Length", "Sepal Width")
g.fig.suptitle("Sepal Features by Correctly Predicted Species")
plt.show(g)