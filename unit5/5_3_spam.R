# SEPARATING SPAM FROM HAM (PART 1)

# Problem 1.1 - Loading the Dataset
# Begin by loading the dataset emails.csv into a data frame called emails. 
# Remember to pass the stringsAsFactors=FALSE option when loading the data.
emails <- read.csv("./data_unit5_3/emails.csv", stringsAsFactors = FALSE)
# How many emails are in the dataset?
str(emails)
## 5728


# Problem 1.2 - Loading the Dataset
# How many of the emails are spam?
table(emails$spam)
## 1368


# Problem 1.3 - Loading the Dataset
# Which word appears at the beginning of every email in the dataset? Respond as
# a lower-case word with punctuation removed.
head(emails$text)
## subject


# Problem 1.4 - Loading the Dataset
# Could a spam classifier potentially benefit from including the frequency of 
# the word that appears in every email?
##  Yes -- the number of times the word appears might help us differentiate spam
## from ham. 


# Problem 1.5 - Loading the Dataset
# The nchar() function counts the number of characters in a piece of text. How
# many characters are in the longest email in the dataset (where longest is 
# measured in terms of the maximum number of characters)?
max(nchar(emails$text))
which.max(nchar(emails$text))
emails[2651, ]
nchar(emails[2651, ]$text)
## 43952


# Problem 1.6 - Loading the Dataset
# Which row contains the shortest email in the dataset? (Just like in the 
# previous problem, shortest is measured in terms of the fewest number of 
# characters.)
min(nchar(emails$text))
which.min(nchar(emails$text))
emails[1992, ]
## 1992


# Problem 2.1 - Preparing the Corpus
# Follow the standard steps to build and pre-process the corpus:

# 1) Build a new corpus variable called corpus.
library(tm)
library(SnowballC)
corpus <- Corpus(VectorSource(emails$text))
corpus[[1]]$content
# 2) Using tm_map, convert the text to lowercase.
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, PlainTextDocument)
corpus[[1]]$content
# 3) Using tm_map, remove all punctuation from the corpus.
corpus <- tm_map(corpus, removePunctuation)
corpus[[1]]$content
# 4) Using tm_map, remove all English stopwords from the corpus.
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus[[1]]$content
# 5) Using tm_map, stem the words in the corpus.
corpus <- tm_map(corpus, stemDocument)
corpus[[1]]$content
# 6) Build a document term matrix from the corpus, called dtm.
dtm <- DocumentTermMatrix(corpus)
# If the code length(stopwords("english")) does not return 174 for you, then 
# please run the line of code in this file, which will store the standard stop
# words in a variable called sw. When removing stop words, use 
# tm_map(corpus, removeWords, sw) instead of 
# tm_map(corpus, removeWords, stopwords("english")).

# How many terms are in dtm?
dtm
## 28687


# Problem 2.2 - Preparing the Corpus
# To obtain a more reasonable number of terms, limit dtm to contain terms 
# appearing in at least 5% of documents, and store this result as spdtm (don't 
# overwrite dtm, because we will use it in a later step of this homework). How
# many terms are in spdtm?
spdtm <- removeSparseTerms(dtm, 0.95)
dtm
spdtm
## 330


# Problem 2.3 - Preparing the Corpus
# Build a data frame called emailsSparse from spdtm, and use the make.names 
# function to make the variable names of emailsSparse valid.
emailsSparse <- as.data.frame(as.matrix(spdtm))
colnames(emailsSparse) <- make.names(colnames(emailsSparse))
# colSums() is an R function that returns the sum of values for each variable in
# our data frame. Our data frame contains the number of times each word stem 
# (columns) appeared in each email (rows). Therefore, colSums(emailsSparse) 
# returns the number of times a word stem appeared across all the emails in the 
# dataset. What is the word stem that shows up most frequently across all the 
# emails in the dataset? Hint: think about how you can use sort() or which.max()
# to pick out the maximum frequency.
sort(colSums(emailsSparse))
## enron


# Problem 2.4 - Preparing the Corpus
# Add a variable called "spam" to emailsSparse containing the email spam labels.
# You can do this by copying over the "spam" variable from the original data 
# frame (remember how we did this in the Twitter lecture).
emailsSparse$spam <- emails$spam
# How many word stems appear at least 5000 times in the ham emails in the 
# dataset? Hint: in this and the next question, remember not to count the 
# dependent variable we just added.
table(emailsSparse$spam)
emailsSparseHam <- subset(emailsSparse, spam == 0)
table(emailsSparseHam$spam)
sum(colSums(emailsSparseHam) >= 5000)
## 6


# Problem 2.5 - Preparing the Corpus
# How many word stems appear at least 1000 times in the spam emails in the 
# dataset?
emailsSparseSpam <- subset(emailsSparse, spam == 1)
table(emailsSparseSpam$spam)
sum(colSums(emailsSparseSpam) >= 1000) - 1 #because "spam" is counted
## 3


# Problem 2.6 - Preparing the Corpus
# The lists of most common words are significantly different between the spam 
# and ham emails. What does this likely imply?
## The frequencies of these most common words are likely to help differentiate 
## between spam and ham.


# Problem 2.7 - Preparing the Corpus
# Several of the most common word stems from the ham documents, such as "enron",
# "hou" (short for Houston), "vinc" (the word stem of "Vince") and "kaminski", 
# are likely specific to Vincent Kaminski's inbox. What does this mean about the
# applicability of the text analytics models we will train for the spam 
# filtering problem?
## The models we build are personalized, and would need to be further tested
## before being used as a spam filter for another person. The models we build 
## are personalized, and would need to be further tested before being used as a
## spam filter for another person.


# Problem 3.1 - Building machine learning models
# First, convert the dependent variable to a factor with 
# "emailsSparse$spam = as.factor(emailsSparse$spam)".
emailsSparse$spam <- as.factor(emailsSparse$spam)
# Next, set the random seed to 123 and use the sample.split function to split 
# emailsSparse 70/30 into a training set called "train" and a testing set called
# "test". Make sure to perform this step on emailsSparse instead of emails.
library(caTools)
set.seed(123)
spamSplit <- sample.split(emailsSparse$spam, 0.7)
train <- subset(emailsSparse, spamSplit == TRUE)
test <- subset(emailsSparse, spamSplit == FALSE)
# Using the training set, train the following three machine learning models. The
# models should predict the dependent variable "spam", using all other available
# variables as independent variables. Please be patient, as these models may 
# take a few minutes to train.
# 1) A logistic regression model called spamLog. You may see a warning message 
# here - we'll discuss this more later.
spamLog <- glm(spam ~ ., data = train, family = binomial)
# 2) A CART model called spamCART, using the default parameters to train the 
# model (don't worry about adding minbucket or cp). Remember to add the argument
# method="class" since this is a binary classification problem.
library(rpart)
library(rpart.plot)
spamCART <- rpart(spam ~ ., data = train, method = "class")
# 3) A random forest model called spamRF, using the default parameters to train
# the model (don't worry about specifying ntree or nodesize). Directly before 
# training the random forest model, set the random seed to 123 (even though 
# we've already done this earlier in the problem, it's important to set the seed
# right before training the model so we all obtain the same results. Keep in 
# mind though that on certain operating systems, your results might still be 
# slightly different).
library(randomForest)
set.seed(123)
spamRF <- randomForest(spam ~ ., data = train)
# For each model, obtain the predicted spam probabilities for the training set. 
# Be careful to obtain probabilities instead of predicted classes, because we 
# will be using these values to compute training set AUC values. Recall that you
# can obtain probabilities for CART models by not passing any type parameter to 
# the predict() function, and you can obtain probabilities from a random forest
# by adding the argument type="prob". For CART and random forest, you need to 
# select the second column of the output of the predict() function, 
# corresponding to the probability of a message being spam.
## Logistic 
predTrainLogProb <- predict(spamLog, type = "response")
summary(predTrainLogProb)
table(predTrainLogProb)
length(predTrainLogProb)

## CART
predTrainCART <- predict(spamCART)
predTrainCART[1:10,]
predTrainCARTProb <- predTrainCART[, 2]
summary(predTrainCARTProb)

## Random forest
predTrainRF <- predict(spamRF, type = "prob")
predTrainRF[1:10,]
predTrainRFProb <- predTrainRF[, 2]
summary(predTrainRFProb)

# You may have noticed that training the logistic regression model yielded the 
# messages "algorithm did not converge" and "fitted probabilities numerically 0 
# or 1 occurred". Both of these messages often indicate overfitting and the 
# first indicates particularly severe overfitting, often to the point that the 
# training set observations are fit perfectly by the model. Let's investigate 
# the predicted probabilities from the logistic regression model.

# How many of the training set predicted probabilities from spamLog are less 
# than 0.00001?
sum(predTrainLogProb < 0.00001)
## 3046

# How many of the training set predicted probabilities from spamLog are more 
# than 0.99999?
sum(predTrainLogProb > 0.99999)
## 954

# How many of the training set predicted probabilities from spamLog are between 
# 0.00001 and 0.99999?
sum(predTrainLogProb > 0.00001 & predTrainLogProb < 0.99999)
## 10


# Problem 3.2 - Building Machine Learning Models
# How many variables are labeled as significant (at the p=0.05 level) in the 
# logistic regression summary output?
summary(spamLog)
# 0


# Problem 3.3 - Building Machine Learning Models
# How many of the word stems "enron", "hou", "vinc", and "kaminski" appear in 
# the CART tree? Recall that we suspect these word stems are specific to Vincent
# Kaminski and might affect the generalizability of a spam filter built with his
# ham data.
prp(spamCART)
# 2 


# Problem 3.4 - Building Machine Learning Models
# What is the training set accuracy of spamLog, using a threshold of 0.5 for 
# predictions?
table(train$spam, predTrainLogProb >= 0.5)
(3052 + 954) / nrow(train)
## 0.9990025


# Problem 3.5 - Building Machine Learning Models
# What is the training set AUC of spamLog?
library(ROCR)
predTrainLogROCR <- prediction(predTrainLogProb, train$spam)
performance(predTrainLogROCR, "auc")@y.values
## 0.9999959


# Problem 3.6 - Building Machine Learning Models
# What is the training set accuracy of spamCART, using a threshold of 0.5 for 
# predictions? (Remember that if you used the type="class" argument when making 
# predictions, you automatically used a threshold of 0.5. If you did not add in 
# the type argument to the predict function, the probabilities are in the second
# column of the predict output.)
table(train$spam, predTrainCARTProb >= 0.5)
(2885 + 894) / nrow(train)
## 0.942394


# Problem 3.7 - Building Machine Learning Models
# What is the training set AUC of spamCART? (Remember that you have to pass the
# prediction function predicted probabilities, so don't include the type 
# argument when making predictions for your CART model.)
predTrainCARTROCR <- prediction(predTrainCARTProb, train$spam)
performance(predTrainCARTROCR, "auc")@y.values
## 0.9696044


# Problem 3.8 - Building Machine Learning Models
# What is the training set accuracy of spamRF, using a threshold of 0.5 for 
# predictions? (Remember that your answer might not match ours exactly, due to 
# random behavior in the random forest algorithm on different operating systems.)
table(train$spam, predTrainRFProb >= 0.5)
(3013 + 914) / nrow(train)
## 0.9793017


# Problem 3.9 - Building Machine Learning Models
# What is the training set AUC of spamRF? (Remember to pass the argument 
# type="prob" to the predict function to get predicted probabilities for a 
# random forest model. The probabilities will be the second column of the 
# output.)
predTrainRFROCR <- prediction(predTrainRFProb, train$spam)
performance(predTrainRFROCR, "auc")@y.values
## 0.9979116


# Problem 3.10 - Building Machine Learning Models
# Which model had the best training set performance, in terms of accuracy and 
# AUC?
## Logistic regression 


# Problem 4.1 - Evaluating on the Test Set
# Obtain predicted probabilities for the testing set for each of the models, 
# again ensuring that probabilities instead of classes are obtained.
## Logistic
predTestLogProb <- predict(spamLog, newdata = test, type = "response")
summary(predTestLogProb)
table(predTestLogProb)
length(predTestLogProb)

## CART
predTestCART <- predict(spamCART, newdata = test)
predTestCART[1:10,]
predTestCARTProb <- predTestCART[, 2]
summary(predTestCARTProb)

## Random forest
predTestRF <- predict(spamRF, newdata = test, type = "prob")
predTestRF[1:10,]
predTestRFProb <- predTestRF[, 2]
summary(predTestRFProb)

# What is the testing set accuracy of spamLog, using a threshold of 0.5 for 
# predictions?
table(test$spam, predTestLogProb >= 0.5)
(1257 + 376) / nrow(test)
## 0.9505239


# Problem 4.2 - Evaluating on the Test Set
# What is the testing set AUC of spamLog?
predTestLogROCR <- prediction(predTestLogProb, test$spam)
performance(predTestLogROCR, "auc")@y.values
## 0.9627517


# Problem 4.3 - Evaluating on the Test Set
# What is the testing set accuracy of spamCART, using a threshold of 0.5 for 
# predictions?
table(test$spam, predTestCARTProb >= 0.5)
(1228 + 386) / nrow(test)
## 0.9394645


# Problem 4.4 - Evaluating on the Test Set
# What is the testing set AUC of spamCART?
predTestCARTROCR <- prediction(predTestCARTProb, test$spam)
performance(predTestCARTROCR, "auc")@y.values
## 0.963176


# Problem 4.5 - Evaluating on the Test Set
# What is the testing set accuracy of spamRF, using a threshold of 0.5 for 
# predictions?
table(test$spam, predTestRFProb >= 0.5)
(1290 + 386) / nrow(test)
## 0.975553


# Problem 4.6 - Evaluating on the Test Set
# What is the testing set AUC of spamRF?
predTestRFROCR <- prediction(predTestRFProb, test$spam)
performance(predTestRFROCR, "auc")@y.values
## 0.9975656


# Problem 4.7 - Evaluating on the Test Set
# Which model had the best testing set performance, in terms of accuracy and 
# AUC?
## Random forest


# Problem 4.8 - Evaluating on the Test Set
# Which model demonstrated the greatest degree of overfitting?
## Logistic regression