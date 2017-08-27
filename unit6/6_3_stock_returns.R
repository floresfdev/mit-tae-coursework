# PREDICTING STOCK RETURNS WITH CLUSTER-THEN-PREDICT


# Problem 1.1 - Exploring the Dataset
# Load StocksCluster.csv into a data frame called "stocks". How many 
# observations are in the dataset?
stocks <- read.csv("./data_unit6_3/StocksCluster.csv")
str(stocks)
## 11580


# Problem 1.2 - Exploring the Dataset
# What proportion of the observations have positive returns in December?
table(stocks$PositiveDec)
6324 / (5256 + 6324)
## 0.546114


# Problem 1.3 - Exploring the Dataset
# What is the maximum correlation between any two return variables in the 
# dataset? You should look at the pairwise correlations between ReturnJan, 
# ReturnFeb, ReturnMar, ReturnApr, ReturnMay, ReturnJune, ReturnJuly, ReturnAug,
# ReturnSep, ReturnOct, and ReturnNov.
cor(stocks[, 1:11])
## ReturnOct vs ReturnNov = 0.19167279


# Problem 1.4 - Exploring the Dataset
# Which month (from January through November) has the largest mean return across
# all observations in the dataset?
str(stocks)
colMeans(stocks[, 1:11])
## ReturnApr = 0.026308147

# Which month (from January through November) has the smallest mean return 
# across all observations in the dataset?
## ReturnSep = -0.014720768


# Problem 2.1 - Initial Logistic Regression Model
# Run the following commands to split the data into a training set and testing 
# set, putting 70% of the data in the training set and 30% of the data in the 
# testing set:
library(caTools)
set.seed(144)
spl <- sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain <- subset(stocks, spl == TRUE)
stocksTest <- subset(stocks, spl == FALSE)
# Then, use the stocksTrain data frame to train a logistic regression model
# (name it StocksModel) to predict PositiveDec using all the other variables as
# independent variables. Don't forget to add the argument family=binomial to 
# your glm command.
StocksModel <- glm(PositiveDec ~ ., data = stocksTrain, family = binomial)
# What is the overall accuracy on the training set, using a threshold of 0.5?
StocksModelTrainPred <- predict(StocksModel, type = "response")
head(StocksModelTrainPred)
table(stocksTrain$PositiveDec, StocksModelTrainPred >= 0.5)
(990 + 3640) / nrow(stocksTrain)
## 0.5711818


# Problem 2.2 - Initial Logistic Regression Model
# Now obtain test set predictions from StocksModel. What is the overall accuracy
# of the model on the test, again using a threshold of 0.5?
StocksModelTestPred <- 
    predict(StocksModel, newdata = stocksTest, type = "response")
head(StocksModelTestPred)
table(stocksTest$PositiveDec, StocksModelTestPred >= 0.5)
(417 + 1553) / nrow(stocksTest)
## 0.5670697


# Problem 2.3 - Initial Logistic Regression Model
# What is the accuracy on the test set of a baseline model that always predicts 
# the most common outcome (PositiveDec = 1)?
table(stocksTest$PositiveDec)
1897 / (1577 + 1897)
## 0.5460564


# Problem 3.1 - Clustering Stocks
# Now, let's cluster the stocks. The first step in this process is to remove the
# dependent variable using the following commands:
limitedTrain <- stocksTrain
limitedTrain$PositiveDec <- NULL
limitedTest <- stocksTest
limitedTest$PositiveDec <- NULL
# Why do we need to remove the dependent variable in the clustering phase of the
# cluster-then-predict methodology?
## Needing to know the dependent variable value to assign an observation to a 
## cluster defeats the purpose of the methodology 


# Problem 3.2 - Clustering Stocks
# In the market segmentation assignment in this week's homework, you were 
# introduced to the preProcess command from the caret package, which normalizes
# variables by subtracting by the mean and dividing by the standard deviation.
# In cases where we have a training and testing set, we'll want to normalize by 
# the mean and standard deviation of the variables in the training set. We can 
# do this by passing just the training set to the preProcess function:
library(caret)
preproc <- preProcess(limitedTrain)
normTrain <- predict(preproc, limitedTrain)
normTest <- predict(preproc, limitedTest)
# What is the mean of the ReturnJan variable in normTrain?
summary(normTrain)
## 0

# What is the mean of the ReturnJan variable in normTest?
summary(normTest)
## -0.000419


# Problem 3.3 - Clustering Stocks
# Why is the mean ReturnJan variable much closer to 0 in normTrain than in 
# normTest?
## The distribution of the ReturnJan variable is different in the training and 
## testing set


# Problem 3.4 - Clustering Stocks
# Set the random seed to 144 (it is important to do this again, even though we 
# did it earlier). Run k-means clustering with 3 clusters on normTrain, storing
# the result in an object called km.
set.seed(144)
km <- kmeans(normTrain, centers = 3)
# Which cluster has the largest number of observations?
table(km$cluster)
## Cluster 2


# Problem 3.5 - Clustering Stocks
# Recall from the recitation that we can use the flexclust package to obtain 
# training set and testing set cluster assignments for our observations (note 
# that the call to as.kcca may take a while to complete):
library(flexclust)
km.kcca <- as.kcca(km, normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata=normTest)
# How many test-set observations were assigned to Cluster 2?
table(clusterTest)
## 2080


# Problem 4.1 - Cluster-Specific Predictions
# Using the subset function, build data frames stocksTrain1, stocksTrain2, and 
# stocksTrain3, containing the elements in the stocksTrain data frame assigned 
# to clusters 1, 2, and 3, respectively (be careful to take subsets of 
# stocksTrain, not of normTrain). Similarly build stocksTest1, stocksTest2, and 
# stocksTest3 from the stocksTest data frame.
stocksTrain1 <- subset(stocksTrain, clusterTrain == 1)
stocksTrain2 <- subset(stocksTrain, clusterTrain == 2)
stocksTrain3 <- subset(stocksTrain, clusterTrain == 3)
stocksTest1 <- subset(stocksTest, clusterTest == 1)
stocksTest2 <- subset(stocksTest, clusterTest == 2)
stocksTest3 <- subset(stocksTest, clusterTest == 3)
# Which training set data frame has the highest average value of the dependent 
# variable?
mean(stocksTrain1$PositiveDec)
mean(stocksTrain2$PositiveDec)
mean(stocksTrain3$PositiveDec)
## stocksTrain1


# Problem 4.2 - Cluster-Specific Predictions
# Build logistic regression models StocksModel1, StocksModel2, and StocksModel3,
# which predict PositiveDec using all the other variables as independent 
# variables. StocksModel1 should be trained on stocksTrain1, StocksModel2 should
# be trained on stocksTrain2, and StocksModel3 should be trained on 
# stocksTrain3.
StocksModel1 <- glm(PositiveDec ~ ., data = stocksTrain1, family = binomial)
StocksModel2 <- glm(PositiveDec ~ ., data = stocksTrain2, family = binomial)
StocksModel3 <- glm(PositiveDec ~ ., data = stocksTrain3, family = binomial)
# Which variables have a positive sign for the coefficient in at least one of 
# StocksModel1, StocksModel2, and StocksModel3 and a negative sign for the 
# coefficient in at least one of StocksModel1, StocksModel2, and StocksModel3? 
# Select all that apply.
StocksModel1$coefficients
StocksModel2$coefficients
StocksModel3$coefficients
rbind(StocksModel1$coefficients > 0,
      StocksModel2$coefficients > 0,
      StocksModel3$coefficients > 0)
## ReturnJan, ReturnFeb, ReturnMar, ReturnJune, ReturnAug, ReturnOct


# Problem 4.3 - Cluster-Specific Predictions
# Using StocksModel1, make test-set predictions called PredictTest1 on the data 
# frame stocksTest1. Using StocksModel2, make test-set predictions called 
# PredictTest2 on the data frame stocksTest2. Using StocksModel3, make test-set 
# predictions called PredictTest3 on the data frame stocksTest3.
PredictTest1 <- predict(StocksModel1, newdata = stocksTest1, type = "response")
PredictTest2 <- predict(StocksModel2, newdata = stocksTest2, type = "response")
PredictTest3 <- predict(StocksModel3, newdata = stocksTest3, type = "response")
# What is the overall accuracy of StocksModel1 on the test set stocksTest1, 
# using a threshold of 0.5?
table(stocksTest1$PositiveDec, PredictTest1 >= 0.5)
(30 + 774) / nrow(stocksTest1)
## 0.6194145

# What is the overall accuracy of StocksModel2 on the test set stocksTest2, 
# using a threshold of 0.5?
table(stocksTest2$PositiveDec, PredictTest2 >= 0.5)
(388 + 757) / nrow(stocksTest2)
## 0.5504808

# What is the overall accuracy of StocksModel3 on the test set stocksTest3, 
# using a threshold of 0.5?
table(stocksTest3$PositiveDec, PredictTest3 >= 0.5)
(49 + 13) / nrow(stocksTest3)
## 0.6458333


# Problem 4.4 - Cluster-Specific Predictions
# To compute the overall test-set accuracy of the cluster-then-predict approach,
# we can combine all the test-set predictions into a single vector and all the 
# true outcomes into a single vector:
AllPredictions <- c(PredictTest1, PredictTest2, PredictTest3)
AllOutcomes <- c(stocksTest1$PositiveDec, 
                 stocksTest2$PositiveDec, 
                 stocksTest3$PositiveDec)
# What is the overall test-set accuracy of the cluster-then-predict approach, 
# again using a threshold of 0.5?
table(AllOutcomes, AllPredictions >= 0.5)
length(AllOutcomes)
(467 + 1544) / length(AllOutcomes)
## 0.5788716

# We see a modest improvement over the original logistic regression model. Since
# predicting stock returns is a notoriously hard problem, this is a good 
# increase in accuracy. By investing in stocks for which we are more confident 
# that they will have positive returns (by selecting the ones with higher 
# predicted probabilities), this cluster-then-predict model can give us an edge
# over the original logistic regression model.