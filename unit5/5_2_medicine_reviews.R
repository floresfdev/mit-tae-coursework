# AUTOMATING REVIEWS IN MEDICINE

# Problem 1.1 - Loading the Data
# Load clinical_trial.csv into a data frame called trials (remembering to add 
# the argument stringsAsFactors=FALSE), and investigate the data frame with 
# summary() and str().
trials <- read.csv("./data_unit5_2/clinical_trial.csv", stringsAsFactors = FALSE)
str(trials)
summary(trials)
# IMPORTANT NOTE: Some students have been getting errors like "invalid multibyte
# string" when performing certain parts of this homework question. If this is 
# happening to you, use the argument fileEncoding="latin1" when reading in the 
# file with read.csv. This should cause those errors to go away.
# We can use R's string functions to learn more about the titles and abstracts 
# of the located papers. The nchar() function counts the number of characters in
# a piece of text. Using the nchar() function on the variables in the data 
# frame, answer the following questions:
# How many characters are there in the longest abstract? (Longest here is 
# defined as the abstract with the largest number of characters.)
max(nchar(trials$abstract))
which.max(nchar(trials$abstract))
trials[664, ]
nchar(trials[664, ]$abstract)
## 3708


# Problem 1.2 - Loading the Data
# How many search results provided no abstract? (HINT: A search result provided
# no abstract if the number of characters in the abstract field is zero.)
sum(nchar(trials$abstract) == 0)
## 112


# Problem 1.3 - Loading the Data
# Find the observation with the minimum number of characters in the title (the 
# variable "title") out of all of the observations in this dataset. What is the 
# text of the title of this article? Include capitalization and punctuation in 
# your response, but don't include the quotes.
min(nchar(trials$title))
which.min(nchar(trials$title))
trials[1258,]
trials[1258,]$title


# Problem 2.1 - Preparing the Corpus
# Because we have both title and abstract information for trials, we need to 
# build two corpera instead of one. Name them corpusTitle and corpusAbstract.

# Following the commands from lecture, perform the following tasks (you might 
# need to load the "tm" package first if it isn't already loaded). Make sure to
# perform them in this order.

# 1) Convert the title variable to corpusTitle and the abstract variable to 
# corpusAbstract.
library(tm)
library(SnowballC)
corpusTitle <- Corpus(VectorSource(trials$title))
corpusTitle[[1]]$content
corpusAbstract <- Corpus(VectorSource(trials$abstract))
corpusAbstract[[1]]$content
# 2) Convert corpusTitle and corpusAbstract to lowercase. After performing this
# step, remember to run the lines:
corpusTitle = tm_map(corpusTitle, content_transformer(tolower))
corpusAbstract = tm_map(corpusAbstract, content_transformer(tolower))
corpusTitle = tm_map(corpusTitle, PlainTextDocument)
corpusAbstract = tm_map(corpusAbstract, PlainTextDocument)
# 3) Remove the punctuation in corpusTitle and corpusAbstract.
corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusTitle[[2]]$content
corpusAbstract = tm_map(corpusAbstract, removePunctuation)
corpusAbstract[[2]]$content
# 4) Remove the English language stop words from corpusTitle and corpusAbstract.
corpusTitle <- tm_map(corpusTitle, removeWords, stopwords("english"))
corpusTitle[[2]]$content
corpusAbstract <- tm_map(corpusAbstract, removeWords, stopwords("english"))
corpusAbstract[[2]]$content
# 5) Stem the words in corpusTitle and corpusAbstract (each stemming might take
# a few minutes).
corpusTitle = tm_map(corpusTitle, stemDocument)
corpusTitle[[2]]$content
corpusAbstract = tm_map(corpusAbstract, stemDocument)
corpusAbstract[[2]]$content
# 6) Build a document term matrix called dtmTitle from corpusTitle and 
# dtmAbstract from corpusAbstract.
dtmTitle = DocumentTermMatrix(corpusTitle)
dtmTitle
dtmAbstract = DocumentTermMatrix(corpusAbstract)
dtmAbstract
# 7) Limit dtmTitle and dtmAbstract to terms with sparseness of at most 95% (aka
# terms that appear in at least 5% of documents).
dtmTitle <- removeSparseTerms(dtmTitle, 0.95)
dtmTitle
dtmAbstract <- removeSparseTerms(dtmAbstract, 0.95)
dtmAbstract
# 8) Convert dtmTitle and dtmAbstract to data frames (keep the names dtmTitle 
# and dtmAbstract).
dtmTitle <- as.data.frame(as.matrix(dtmTitle))
dtmAbstract <- as.data.frame(as.matrix(dtmAbstract))
# If the code length(stopwords("english")) does not return 174 for you, then 
# please run the line of code in this file, which will store the standard stop 
# words in a variable called sw. When removing stop words, use 
# tm_map(corpusTitle, removeWords, sw) and 
# tm_map(corpusAbstract, removeWords, sw) instead of 
# tm_map(corpusTitle, removeWords, stopwords("english")) and 
# tm_map(corpusAbstract, removeWords, stopwords("english")).
length(stopwords("english"))
# How many terms remain in dtmTitle after removing sparse terms (aka how many 
# columns does it have)?
str(dtmTitle)
## 31

# How many terms remain in dtmAbstract?
str(dtmAbstract)
## 335


# Problem 2.2 - Preparing the Corpus
# What is the most likely reason why dtmAbstract has so many more terms than 
# dtmTitle?
## Abstracts tend to have many more words than titles 


# Problem 2.3 - Preparing the Corpus
# What is the most frequent word stem across all the abstracts? Hint: you can 
# use colSums() to compute the frequency of a word across all the abstracts.
?colSums
colSums(dtmAbstract)
max(colSums(dtmAbstract))
which.max(colSums(dtmAbstract))
dtmAbstract$patient


# Problem 3.1 - Building a model
# We want to combine dtmTitle and dtmAbstract into a single data frame to make
# predictions. However, some of the variables in these data frames have the 
# same names. To fix this issue, run the following commands:
colnames(dtmTitle) <- paste0("T", colnames(dtmTitle))
str(dtmTitle)
colnames(dtmAbstract) <- paste0("A", colnames(dtmAbstract))
str(dtmAbstract)
# What was the effect of these functions?
## Adding the letter T in front of all the title variable names and adding the 
## letter A in front of all the abstract variable names.


# Problem 3.2 - Building a Model
# Using cbind(), combine dtmTitle and dtmAbstract into a single data frame 
# called dtm:
dtm <- cbind(dtmTitle, dtmAbstract)
str(dtm)
# As we did in class, add the dependent variable "trial" to dtm, copying it from
# the original data frame called trials. How many columns are in this combined 
# data frame?
dtm$trial <- trials$trial
str(dtm)
## 367


# Problem 3.3 - Building a Model
# Now that we have prepared our data frame, it's time to split it into a 
# training and testing set and to build regression models. Set the random seed 
# to 144 and use the sample.split function from the caTools package to split 
# dtm into data frames named "train" and "test", putting 70% of the data in the 
# training set.
library(caTools)
set.seed(144)
trialSplit <- sample.split(dtm$trial, 0.7)
train <- subset(dtm, trialSplit == TRUE)
test <- subset(dtm, trialSplit == FALSE)
# What is the accuracy of the baseline model on the training set? (Remember that
# the baseline model predicts the most frequent outcome in the training set for 
# all observations.)
table(train$trial)
730 / (730 + 572)
## 0.5606759


# Problem 3.4 - Building a Model
# Build a CART model called trialCART, using all the independent variables in 
# the training set to train the model, and then plot the CART model. Just use 
# the default parameters to build the model (don't add a minbucket or cp value).
# Remember to add the method="class" argument, since this is a classification 
# problem.
library(rpart)
library(rpart.plot)
trialCART <- rpart(trial ~ ., data = train, method = "class")
# What is the name of the first variable the model split on?
prp(trialCART)
## Tphase


# Problem 3.5 - Building a Model
# Obtain the training set predictions for the model (do not yet predict on the 
# test set). Extract the predicted probability of a result being a trial (recall
# that this involves not setting a type argument, and keeping only the second 
# column of the predict output). What is the maximum predicted probability for 
# any result?
predTrain <- predict(trialCART)
predTrain[1:10,]
predTrainProb <- predTrain[, 2]
max(predTrainProb)
summary(predTrainProb)
## 0.8718861


# Problem 3.6 - Building a Model
# Without running the analysis, how do you expect the maximum predicted 
# probability to differ in the testing set?
## The maximum predicted probability will likely be exactly the same in the 
## testing set. 
## EXPLANATION: Because the CART tree assigns the same predicted probability to 
## each leaf node and there are a small number of leaf nodes compared to data 
## points, we expect exactly the same maximum predicted probability.



# Problem 3.7 - Building a Model
# For these questions, use a threshold probability of 0.5 to predict that an 
# observation is a clinical trial.
table(train$trial, predTrainProb >= 0.5)
# What is the training set accuracy of the CART model?
(631 + 441) / nrow(train)
## 0.8233487

# What is the training set sensitivity of the CART model?
441 / (441 + 131)
## 0.770979

# What is the training set specificity of the CART model?
631 / (631 + 99)
## 0.8643836


# Problem 4.1 - Evaluating the model on the testing set
# Evaluate the CART model on the testing set using the predict function and 
# creating a vector of predicted probabilities predTest.
pred <- predict(trialCART, newdata = test)
pred[1:10,]
predTest <- pred[, 2]
# What is the testing set accuracy, assuming a probability threshold of 0.5 for 
# predicting that a result is a clinical trial?
table(test$trial, predTest >= 0.5)
(261 + 162) / nrow(test)
## 0.7580645


# Problem 4.2 - Evaluating the Model on the Testing Set
# Using the ROCR package, what is the testing set AUC of the prediction model?
library(ROCR)
predROCR <- prediction(predTest, test$trial)
performance(predROCR, "auc")@y.values
## 0.8371063


# Problem 5.1 - Decision-Maker Tradeoffs
# What is the cost associated with the model in Step 1 making a false negative 
# prediction?
## A paper that should have been included in Set A will be missed, affecting the
## quality of the results of Step 3.


# Problem 5.2 - Decision-Maker Tradeoffs
# What is the cost associated with the model in Step 1 making a false positive 
# prediction?
## A paper will be mistakenly added to Set A, yielding additional work in Step 2
## of the process but not affecting the quality of the results of Step 3.


# Problem 5.3 - Decision-Maker Tradeoffs
# Given the costs associated with false positives and false negatives, which of
# the following is most accurate?
## A false negative is more costly than a false positive; the decision maker 
## should use a probability threshold less than 0.5 for the machine learning 
## model.