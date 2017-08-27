# FORECASTING INTEREST RATE HIKES BY THE U.S. FEDERAL RESERVE
# 
# The federal funds rate is the key interest rate that the U.S. Federal Reserve
# uses to influence economic growth. The Federal Open Market Committee meets
# regularly to decide whether to increase, decrease, or maintain the target
# interest rate. Their choice has important ramifications that cascade through
# the economy, so the announcement of the interest rates is eagerly awaited each
# month.
# 
# In this problem, we will use analytics to try to predict when the Federal 
# Reserve will raise interest rates. We will look at monthly economic and 
# political data dating back to the mid-1960's. In this analysis, the dependent
# variable will be the binary outcome variable RaisedFedFunds, which takes value
# 1 if the federal funds rate was increased that month and 0 if it was lowered
# or stayed the same. For each month, the file federalFundsRate.csv contains the
# following independent variables:
# 
# Date: The date the change was announced.
# Chairman: The name of the Federal Reserve Chairman at the time the change was
# announced.
# PreviousRate: The federal funds rate in the prior month.
# Streak: The current streak of raising or not raising the rate, e.g. +8
# indicates the rate has been increased 8 months in a row, whereas -3 indicates
# the rate has been lowered or stayed the same for 3 months in a row.
# GDP: The U.S. Gross Domestic Product, in Billions of Chained 2009 US Dollars.
# Unemployment: The unemployment rate in the U.S.
# CPI: The Consumer Price Index, an indicator of inflation, in the U.S.
# HomeownershipRate: The rate of homeownership in the U.S.
# DebtAsPctGDP: The U.S. national debt as a percentage of GDP
# DemocraticPres: Whether the sitting U.S. President is a Democrat 
# (DemocraticPres=1) or a Republican (DemocraticPres=0)
# MonthsUntilElection: The number of remaining months until the next U.S. 
# presidential election.


# Problem 1 - Loading the Data
# 
# (1 point possible)
# Use the read.csv function to load the contents of federalFundsRate.csv into a
# data frame called fedFunds, using stringsAsFactors=FALSE. What proportion of
# months did the Fed raise the interest rate?
fedFunds <- read.csv("./data_1/federalFundsRate.csv")
str(fedFunds)
summary(fedFunds)
table(fedFunds$RaisedFedFunds)
294 / (291 + 294)
## 0.5025641


# Problem 2 - The Longest-Serving Fed Chair
# 
# (1 point possible)
# Which Fed Reserve Chair has presided over the most interest rate decisions?
table(fedFunds$Chairman)
## Greenspan, Alan


# Problem 3 - Converting Variables to Factors
# 
# (1 point possible)
# Convert the following variables to factors using the as.factor function:
#     
# - Chairman
# - DemocraticPres
# - RaisedFedFunds
# 
# Which of the following methods requires the dependent variable be stored as a 
# factor variable when training a model for classification?
str(fedFunds)
fedFunds$Chairman <- as.factor(fedFunds$Chairman)
fedFunds$DemocraticPres <- as.factor(fedFunds$DemocraticPres)
fedFunds$RaisedFedFunds <- as.factor(fedFunds$RaisedFedFunds)
str(fedFunds)
## Random forest (randomForest) 


# Problem 4 - Splitting into a Training and Testing Set
# 
# (1 point possible)
# Obtain a random training/testing set split with:
set.seed(201)
library(caTools)
spl <- sample.split(fedFunds$RaisedFedFunds, 0.7)
# Split months into a training data frame called "training" using the 
# observations for which spl is TRUE and a testing data frame called "testing" 
# using the observations for which spl is FALSE.
training <- subset(fedFunds, spl == TRUE)
testing <- subset(fedFunds, spl == FALSE)
# Why do we use the sample.split() function to split into a training and testing
# set?
## It balances the dependent variable between the training and testing sets


# Problem 5 - Training a Logistic Regression Model
# 
# (1 point possible)
# Train a logistic regression model using independent variables "PreviousRate", 
# "Streak", "Unemployment", "HomeownershipRate", "DemocraticPres", and 
# "MonthsUntilElection", using the training set to obtain the model.
LogIntRate <- glm(RaisedFedFunds ~ PreviousRate + Streak + Unemployment +
                      HomeownershipRate + DemocraticPres + MonthsUntilElection, 
                  data = training, family = binomial)
# Which of the following characteristics is the most statistically significant 
# associated with an increased chance of the federal funds rate being raised?
summary(LogIntRate)
## A longer consecutive streak of months in which the federal funds rate was 
## raised


# Problem 6 - Predicting Using a Logistic Regression Model
# 
# (1 point possible)
# Imagine you are an analyst at a bank and your manager has asked you to predict
# whether the federal funds rate will be raised next month. You know that the 
# rate has been lowered for 3 straight months (Streak = -3) and that the 
# previous month's rate was 1.7%. The unemployment rate is 5.1% and the 
# homeownership rate is 65.3%. The current U.S. president is a Republican and 
# the next election will be held in 18 months. According to the logistic 
# regression model you built in Problem 5, what is the predicted probability 
# that the interest rate will be raised?
## 9.121012 + PreviousRate*(-0.003427) + Streak*0.157658 +
##   Unemployment*(-0.047449) + HomeownershipRate*(-0.136451) + 
##   DemocraticPres1*0.347829 + MonthsUntilElection*(-0.006931)
9.121012 + 1.7*(-0.003427) - 3*0.157658 + 
    5.1*(-0.047449) + 65.3*(-0.136451) + 
    0*0.347829 + 18*(-0.006931)
## -0.6347861 => Need to plug it into the logistic response function
problem6 <- training[1, ]
problem6$PreviousRate <- 1.7
problem6$Streak <- -3
problem6$Unemployment <- 5.1
problem6$HomeownershipRate <- 65.3
problem6$DemocraticPres <- as.factor(0)
problem6$MonthsUntilElection <- 18
problem6
str(problem6)
problem6PredProb <- predict(LogIntRate, newdata = problem6, type = "response")
problem6PredProb
## 0.3464297


# Problem 7 - Interpreting Model Coefficients
# 
# (1 point possible)
# What is the meaning of the coefficient labeled "DemocraticPres1" in the 
# logistic regression summary output?
summary(LogIntRate)
## When the president is Democratic, the odds of the federal funds rate 
## increasing are 41.6% higher than in an otherise identical month (i.e. 
## identical among the variables in the model). 
## EXPLANATION:
# The coefficients of the model are the log odds associated with that variable; 
# so we see that the odds of being sold are exp(0.347829)=1.41599 those of an 
# otherwise identical month. This means the month is predicted to have 41.6% 
# higher odds of being sold.


# Problem 8 - Obtaining Test Set Predictions
# 
# (2 points possible)
# Using your logistic regression model, obtain predictions on the test set. 
# Then, using a probability threshold of 0.5, create a confusion matrix for the
# test set. On how many test set observations does your logistic regression 
# model make a different prediction than the prediction the naive baseline model
# would make? (Remember that the naive baseline model we use in this class 
# always predicts the most frequent outcome in the training set for all 
# observations in the test set.)
str(testing)
PredProb <- predict(LogIntRate, newdata = testing, type = "response")
table(testing$RaisedFedFunds, PredProb >= 0.5)
table(training$RaisedFedFunds)
## 91 (60 + 31 were predicted less than 0.5)


# Problem 9 - Computing Test-Set AUC
# 
# (2 points possible)
# What is the test-set AUC of the logistic regression model?
library(ROCR)
PredTestLogROCR <- prediction(PredProb, testing$RaisedFedFunds)
performance(PredTestLogROCR, "auc")@y.values
## 0.704023


# Problem 10 - Interpreting AUC
# 
# (1 point possible)
# What is the meaning of the AUC?
## The proportion of the time the model can differentiate between a randomly 
## selected month during which the federal funds were raised and a randomly 
## selected month during which the federal funds were not raised.


# Problem 11 - ROC Curves
# 
# (1 point possible)
# Which logistic regression threshold is associated with the upper-right corner 
# of the ROC plot (true positive rate 1 and false positive rate 1)?
## 0
## EXPLANATION
# A model with threshold 0 predicts 1 for all observations, yielding a 100% true
# positive rate and a 100% false positive rate.


# Problem 12 - ROC Curves
# 
# (1 point possible)
# Plot the colorized ROC curve for the logistic regression model's performance 
# on the test set.
# 
# At roughly which logistic regression cutoff does the model achieve a true 
# positive rate of 85% and a false positive rate of 60%?
ROCRperf <- performance(PredTestLogROCR, "tpr", "fpr")
plot(ROCRperf, colorize = TRUE, 
     print.cutoffs.at = seq(0, 1, by = 0.1), 
     text.adj = c(-0.2, 1.7))
##  0.37


# Problem 13 - Cross-Validation to Select Parameters
# 
# (1 point possible)
# Which of the following best describes how 10-fold cross-validation works when
# selecting between 2 different parameter values?
## 20 models are trained on subsets of the training set and evaluated on a 
## portion of the training set


# Problem 14 - Cross-Validation for a CART Model
# 
# (1 point possible)
# Set the random seed to 201 (even though you have already done so earlier in 
# the problem). Then use the caret package and the train function to perform 
# 10-fold cross validation with the training data set to select the best cp 
# value for a CART model that predicts the dependent variable "RaisedFedFunds" 
# using the independent variables "PreviousRate," "Streak," "Unemployment," 
# "HomeownershipRate," "DemocraticPres," and "MonthsUntilElection." Select the 
# cp value from a grid consisting of the 50 values 0.001, 0.002, ..., 0.05.
library(caret)
library(e1071)
set.seed(201)
# Define cross-validation experiment
numFolds <- trainControl(method = "cv", number = 10)
cpGrid <- expand.grid(.cp = seq(0.001, 0.05, 0.001)) 
# Perform the cross validation
trainCV <- train(RaisedFedFunds ~ PreviousRate + Streak + Unemployment + 
                     HomeownershipRate + DemocraticPres + MonthsUntilElection, 
                 data = training, 
                 method = "rpart", 
                 trControl = numFolds, 
                 tuneGrid = cpGrid)
trainCV
# What cp value maximizes the cross-validation accuracy?
## 0.016


# Problem 15 - Train CART Model
# 
# (1 point possible)
# Build and plot the CART model trained with the parameter identified in Problem
# 14, again predicting the dependent variable using "PreviousRate", "Streak", 
# "Unemployment", "HomeownershipRate", "DemocraticPres", and 
# "MonthsUntilElection". What variable is used as the first (upper-most) split 
# in the tree?
library(rpart)
library(rpart.plot)
TreeIntRate <- trainCV$finalModel
prp(TreeIntRate)
TreeIntRate

TreeIntRate2 <- rpart(RaisedFedFunds ~ PreviousRate + Streak + Unemployment + 
                          HomeownershipRate + DemocraticPres + 
                          MonthsUntilElection, 
                      data = training, 
                      method = "class", 
                      cp = 0.016)
prp(TreeIntRate2)
## Streak


# Problem 16 - Predicting Using a CART Model
# 
# (1 point possible)
# If you were to use the CART model you created in Problem 15 to answer the 
# question asked of the analyst in Problem 6, what would you predict for next 
# month?
# 
# Remember: The rate has been lowered for 3 straight months (Streak = -3). The 
# previous month's rate was 1.7%. The unemployment rate is 5.1%. The 
# homeownership rate is 65.3%. The current U.S. president is a Republican and 
# the next election will be held in 18 months.
## The Fed will not raise the federal funds rate. The Fed will not raise the 
## federal funds rate.


# Problem 17 - Test-Set Accuracy for CART Model
# 
# (2 points possible)
# Using the CART model you created in Problem 15, obtain predictions on the test
# set (using the parameter type="class" with the predict function). Then, create
# a confusion matrix for the test set.
PredClassTree <- predict(TreeIntRate2, newdata = testing, type = "class")
# What is the accuracy of your CART model?
table(PredClassTree, testing$RaisedFedFunds)
(64 + 48) / nrow(testing)
## 0.64