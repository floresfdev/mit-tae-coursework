# LETTER RECOGNITION

letters <- read.csv("./data_unit4_2/letters_ABPR.csv")
summary(letters)
str(letters)

# Problem 1.1 - Predicting B or not B
# Let's warm up by attempting to predict just whether a letter is B or not. To 
# begin, load the file letters_ABPR.csv into R, and call it letters. Then, 
# create a new variable isB in the dataframe, which takes the value "TRUE" if 
# the observation corresponds to the letter B, and "FALSE" if it does not. You 
# can do this by typing the following command into your R console:
letters$isB <- as.factor(letters$letter == "B")
# Now split the data set into a training and testing set, putting 50% of the 
# data in the training set. Set the seed to 1000 before making the split. The 
# first argument to sample.split should be the dependent variable "letters$isB".
# Remember that TRUE values from sample.split should go in the training set.
library(caTools)
set.seed(100)
lettersSplit = sample.split(letters$isB, SplitRatio = 0.5)
lettersTrain = subset(letters, lettersSplit == TRUE)
lettersTest = subset(letters, lettersSplit == FALSE)
# Before building models, let's consider a baseline method that always predicts 
# the most frequent outcome, which is "not B". What is the accuracy of this 
# baseline method on the test set?
table(letters$isB)
table(lettersTest$isB)
1175 / (1175 + 383)
## 0.754172


# Problem 1.2 - Predicting B or not B
# Now build a classification tree to predict whether a letter is a B or not, 
# using the training set to build your model. Remember to remove the variable 
# "letter" out of the model, as this is related to what we are trying to 
# predict! To just remove one variable, you can either write out the other 
# variables, or remember what we did in the Billboards problem in Week 3, and 
# use the following notation:
CARTb <- rpart(isB ~ . - letter, data = lettersTrain, method="class")
# We are just using the default parameters in our CART model, so we don't need 
# to add the minbucket or cp arguments at all. We also added the argument 
# method="class" since this is a classification problem.
# What is the accuracy of the CART model on the test set? (Use type="class" when
# making predictions on the test set.)
bPredict <- predict(CARTb, newdata = lettersTest, type = "class")
table(lettersTest$isB, bPredict)
(1126 + 342) / nrow(lettersTest)


# Problem 1.3 - Predicting B or Not B
# Now, build a random forest model to predict whether the letter is a B or not 
# (the isB variable) using the training set. You should use all of the other 
# variables as independent variables, except letter (since it helped us define 
# what we are trying to predict!). Use the default settings for ntree and 
# nodesize (don't include these arguments at all). Right before building the 
# model, set the seed to 1000. (NOTE: You might get a slightly different answer 
# on this problem, even if you set the random seed. This has to do with your 
# operating system and the implementation of the random forest algorithm.)
library(randomForest)
set.seed(1000)
bForest <- randomForest(isB ~ . - letter, data = lettersTrain)
# What is the accuracy of the model on the test set?
bForestPredict <- predict(bForest, newdata = lettersTest, type = "class")
table(lettersTest$isB, bForestPredict)
(1164 + 372) / nrow(lettersTest)
## 0.9858793

# In lecture, we noted that random forests tends to improve on CART in terms of
# predictive accuracy. Sometimes, this improvement can be quite significant, as 
# it is here.


# Problem 2.1 - Predicting the letters A, B, P, R
# Let us now move on to the problem that we were originally interested in, which
# is to predict whether or not a letter is one of the four letters A, B, P or R.
# As we saw in the D2Hawkeye lecture, building a multiclass classification CART
# model in R is no harder than building the models for binary classification 
# problems. Fortunately, building a random forest model is just as easy.
# The variable in our data frame which we will be trying to predict is "letter".
# Start by converting letter in the original data set (letters) to a factor by 
# running the following command in R:
letters$letter <- as.factor(letters$letter)
# Now, generate new training and testing sets of the letters data frame using 
# letters$letter as the first input to the sample.split function. Before 
# splitting, set your seed to 2000. Again put 50% of the data in the training 
# set. (Why do we need to split the data again? Remember that sample.split 
# balances the outcome variable in the training and testing sets. With a new 
# outcome variable, we want to re-generate our split.)
set.seed(2000)
lettersAllSplit <- sample.split(letters$letter, SplitRatio = 0.5)
lettersAllTrain <- subset(letters, lettersAllSplit == TRUE)
lettersAllTest <- subset(letters, lettersAllSplit == FALSE)
# In a multiclass classification problem, a simple baseline model is to predict
# the most frequent class of all of the options.
# What is the baseline accuracy on the testing set?
table(letters$letter)
table(lettersAllTest$letter)
## P is the most frequent class in the test set
401 / nrow(lettersAllTest)
## 0.2573813


# Problem 2.2 - Predicting the letters A, B, P, R
# Now build a classification tree to predict "letter", using the training set 
# to build your model. You should use all of the other variables as independent 
# variables, except "isB", since it is related to what we are trying to predict!
# Just use the default parameters in your CART model. Add the argument 
# method="class" since this is a classification problem. Even though we have 
# multiple classes here, nothing changes in how we build the model from the 
# binary case.
CARTletters <- rpart(letter ~ . - isB, data = lettersAllTrain, method="class")
summary(CARTletters)
prp(CARTletters)
# What is the test set accuracy of your CART model? Use the argument 
# type="class" when making predictions.
# (HINT: When you are computing the test set accuracy using the confusion 
# matrix, you want to add everything on the main diagonal and divide by the 
# total number of observations in the test set, which can be computed with 
# nrow(test), where test is the name of your test set).
lettersPredict <- 
    as.vector(predict(CARTletters, newdata = lettersAllTest, type = "class"))
length(lettersPredict)
lettersPredict
nrow(lettersAllTest)
table(lettersAllTest$letter, lettersPredict)
(348 + 318 + 363 + 340) / nrow(lettersAllTest)
## 0.8786906


# Problem 2.3 - Predicting the letters A, B, P, R
# Now build a random forest model on the training data, using the same 
# independent variables as in the previous problem -- again, don't forget to 
# remove the isB variable. Just use the default parameter values for ntree and 
# nodesize (you don't need to include these arguments at all). Set the seed to 
# 1000 right before building your model. (Remember that you might get a slightly
# different result even if you set the random seed.)
set.seed(1000)
lettersForest <- randomForest(letter ~ . - isB, data = lettersAllTrain)
# What is the test set accuracy of your random forest model?
lettersForestPredict <- 
    as.vector(predict(lettersForest, newdata = lettersAllTest, type = "class"))
lettersForestPredict
table(lettersAllTest$letter, lettersForestPredict)
(390 + 380 + 393 + 364) / nrow(lettersAllTest)
## 0.9801027

# You should find this value rather striking, for several reasons. The first is
# that it is significantly higher than the value for CART, highlighting the gain
# in accuracy that is possible from using random forest models. The second is 
# that while the accuracy of CART decreased significantly as we transitioned 
# from the problem of predicting B/not B (a relatively simple problem) to the 
# problem of predicting the four letters (certainly a harder problem), the 
# accuracy of the random forest model decreased by a tiny amount.