# DETECTING VANDALISM ON WIKIPEDIA

# Problem 1.1 - Bags of Words
# Load the data wiki.csv with the option stringsAsFactors=FALSE, calling the 
# data frame "wiki". Convert the "Vandal" column to a factor using the command 
# wiki$Vandal = as.factor(wiki$Vandal).
wiki <- read.csv("./data_unit5_1/wiki.csv", stringsAsFactors = FALSE)
wiki$Vandal <- as.factor(wiki$Vandal)
str(wiki)
# How many cases of vandalism were detected in the history of this page?
table(wiki$Vandal)
## 1815


# Problem 1.2 - Bags of Words
# We will now use the bag of words approach to build a model. We have two 
# columns of textual data, with different meanings. For example, adding rude 
# words has a different meaning to removing rude words. We'll start like we did
# in class by building a document term matrix from the Added column. The text 
# already is lowercase and stripped of punctuation. So to pre-process the data, 
# just complete the following four steps:
# 1) Create the corpus for the Added column, and call it "corpusAdded".
library(tm)
library(SnowballC)
corpusAdded <- Corpus(VectorSource(wiki$Added))
corpusAdded[[1]]$content
# 2) Remove the English-language stopwords.
corpusAdded <- tm_map(corpusAdded, removeWords, stopwords("english"))
corpusAdded[[1]]$content
# 3) Stem the words.
corpusAdded <- tm_map(corpusAdded, stemDocument)
corpusAdded[[1]]$content
# 4) Build the DocumentTermMatrix, and call it dtmAdded.
dtmAdded = DocumentTermMatrix(corpusAdded)
dtmAdded
# If the code length(stopwords("english")) does not return 174 for you, then 
# please run the line of code in this file, which will store the standard stop 
# words in a variable called sw. When removing stop words, use 
# tm_map(corpusAdded, removeWords, sw) instead of 
# tm_map(corpusAdded, removeWords, stopwords("english")).
length(stopwords("english"))
# How many terms appear in dtmAdded?
## 6675


# Problem 1.3 - Bags of Words
# Filter out sparse terms by keeping only terms that appear in 0.3% or more of 
# the revisions, and call the new matrix sparseAdded. How many terms appear in 
# sparseAdded?
sparseAdded <- removeSparseTerms(dtmAdded, 0.997)
sparseAdded
## 166


# Problem 1.4 - Bags of Words
# Convert sparseAdded to a data frame called wordsAdded, and then prepend all 
# the words with the letter A, by using the command:
#   colnames(wordsAdded) = paste("A", colnames(wordsAdded))
wordsAdded <- as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) <- paste("A", colnames(wordsAdded))
str(wordsAdded)
# Now repeat all of the steps we've done so far (create a corpus, remove stop 
# words, stem the document, create a sparse document term matrix, and convert it
# to a data frame) to create a Removed bag-of-words dataframe, called 
# wordsRemoved, except this time, prepend all of the words with the letter R:
#   colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))
# 1) Create the corpus for the Removed column, and call it "corpusRemoved".
corpusRemoved <- Corpus(VectorSource(wiki$Removed))
corpusRemoved[[3]]$content
# 2) Remove the English-language stopwords.
corpusRemoved <- tm_map(corpusRemoved, removeWords, stopwords("english"))
corpusRemoved[[3]]$content
# 3) Stem the words.
corpusRemoved <- tm_map(corpusRemoved, stemDocument)
corpusRemoved[[3]]$content
# 4) Build the DocumentTermMatrix, and call it dtmRemoved.
dtmRemoved <- DocumentTermMatrix(corpusRemoved)
dtmRemoved
# 5) Sparse document term matrix
sparseRemoved <- removeSparseTerms(dtmRemoved, 0.997)
sparseRemoved
# 6) Create dataframe and preppend the colnames with the letter "R"
wordsRemoved <- as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) <- paste("R", colnames(wordsRemoved))
str(wordsRemoved)
# How many words are in the wordsRemoved data frame?
## 162


# Problem 1.5 - Bags of Words
# Combine the two data frames into a data frame called wikiWords with the 
# following line of code:
wikiWords <- cbind(wordsAdded, wordsRemoved)
# The cbind function combines two sets of variables for the same observations 
# into one data frame. Then add the Vandal column (HINT: remember how we added 
# the dependent variable back into our data frame in the Twitter lecture). Set 
# the random seed to 123 and then split the data set using sample.split from the
# "caTools" package to put 70% in the training set.
wikiWords$Vandal <- wiki$Vandal
library(caTools)
set.seed(123)
wikiSplit <- sample.split(wikiWords$Vandal, 0.7)
wikiTrain <- subset(wikiWords, wikiSplit == TRUE)
wikiTest <- subset(wikiWords, wikiSplit == FALSE)
# What is the accuracy on the test set of a baseline method that always predicts
# "not vandalism" (the most frequent outcome)?
table(wikiTest$Vandal)
618 / (618 + 545)
## 0.5313844


# Problem 1.6 - Bags of Words
# Build a CART model to predict Vandal, using all of the other variables as 
# independent variables. Use the training set to build the model and the default
# parameters (don't set values for minbucket or cp).
library(rpart)
library(rpart.plot)
wikiTree <- rpart(Vandal ~ ., data = wikiTrain, method = "class")
# What is the accuracy of the model on the test set, using a threshold of 0.5? 
# (Remember that if you add the argument type="class" when making predictions, 
# the output of predict will automatically use a threshold of 0.5.)
wikiPred <- as.vector(predict(wikiTree, newdata = wikiTest, type="class"))
head(wikiPred)
table(wikiTest$Vandal, wikiPred)
(618 + 12) / nrow(wikiTest)
## 0.5417025


# Problem 1.7 - Bags of Words
# Plot the CART tree. How many word stems does the CART model use?
prp(wikiTree)
## 2


# Problem 1.8 - Bags of Words
# Given the performance of the CART model relative to the baseline, what is the
# best explanation of these results?
##  Although it beats the baseline, bag of words is not very predictive for this
## problem. 


# Problem 2.1 - Problem-specific Knowledge
# We weren't able to improve on the baseline using the raw textual information. 
# More specifically, the words themselves were not useful. There are other 
# options though, and in this section we will try two techniques - identifying a
# key class of words, and counting words.
# The key class of words we will use are website addresses. "Website addresses" 
# (also known as URLs - Uniform Resource Locators) are comprised of two main 
# parts. An example would be "http://www.google.com". The first part is the 
# protocol, which is usually "http" (HyperText Transfer Protocol). The second 
# part is the address of the site, e.g. "www.google.com". We have stripped all 
# punctuation so links to websites appear in the data as one word, e.g. 
# "httpwwwgooglecom". We hypothesize that given that a lot of vandalism seems to
# be adding links to promotional or irrelevant websites, the presence of a web
# address is a sign of vandalism.
# We can search for the presence of a web address in the words added by 
# searching for "http" in the Added column. The grepl function returns TRUE if 
# a string is found in another string, e.g.
#   grepl("cat","dogs and cats",fixed=TRUE) # TRUE
#   grepl("cat","dogs and rats",fixed=TRUE) # FALSE
# Create a copy of your dataframe from the previous question:
wikiWords2 <- wikiWords
# Make a new column in wikiWords2 that is 1 if "http" was in Added:
wikiWords2$HTTP <- ifelse(grepl("http", wiki$Added, fixed = TRUE), 1, 0)
# Based on this new column, how many revisions added a link?
table(wikiWords2$HTTP)
## 217


# Problem 2.2 - Problem-Specific Knowledge
# In problem 1.5, you computed a vector called "spl" that identified the 
# observations to put in the training and testing sets. Use that variable (do 
# not recompute it with sample.split) to make new training and testing sets:
wikiTrain2 <- subset(wikiWords2, wikiSplit == TRUE)
wikiTest2 <- subset(wikiWords2, wikiSplit == FALSE)
# Then create a new CART model using this new variable as one of the independent
# variables.
wikiTree2 <- rpart(Vandal ~ ., data = wikiTrain2, method = "class")
# What is the new accuracy of the CART model on the test set, using a threshold
# of 0.5?
wikiPred2 <- as.vector(predict(wikiTree2, newdata = wikiTest2, type="class"))
head(wikiPred2)
table(wikiTest2$Vandal, wikiPred2)
(609 + 57) / nrow(wikiTest2)
## 0.5726569


# Problem 2.3 - Problem-Specific Knowledge
# Another possibility is that the number of words added and removed is 
# predictive, perhaps more so than the actual words themselves. We already have
# a word count available in the form of the document-term matrices (DTMs).
# Sum the rows of dtmAdded and dtmRemoved and add them as new variables in your
# data frame wikiWords2 (called NumWordsAdded and NumWordsRemoved) by using the
# following commands:
wikiWords2$NumWordsAdded <- rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved <- rowSums(as.matrix(dtmRemoved))
# What is the average number of words added?
mean(wikiWords2$NumWordsAdded)
## 4.050052


# Problem 2.4 - Problem-Specific Knowledge
# In problem 1.5, you computed a vector called "spl" that identified the 
# observations to put in the training and testing sets. Use that variable (do 
# not recompute it with sample.split) to make new training and testing sets with
# wikiWords2. Create the CART model again (using the training set and the 
# default parameters).
wikiTrain2b <- subset(wikiWords2, wikiSplit == TRUE)
wikiTest2b <- subset(wikiWords2, wikiSplit == FALSE)
# Then create a new CART model using this new variable as one of the independent
# variables.
wikiTree2b <- rpart(Vandal ~ ., data = wikiTrain2b, method = "class")
# What is the new accuracy of the CART model on the test set?
wikiPred2b <- as.vector(predict(wikiTree2b, newdata = wikiTest2b, type="class"))
head(wikiPred2b)
table(wikiTest2b$Vandal, wikiPred2b)
(514 + 248) / nrow(wikiTest2b)
## 0.6552021


# Problem 3.1 - Using Non-Textual Data
# We have two pieces of "metadata" (data about data) that we haven't yet used. 
# Make a copy of wikiWords2, and call it wikiWords3:
wikiWords3 <- wikiWords2
# Then add the two original variables Minor and Loggedin to this new data frame:
wikiWords3$Minor <- wiki$Minor
wikiWords3$Loggedin <- wiki$Loggedin
# In problem 1.5, you computed a vector called "spl" that identified the 
# observations to put in the training and testing sets. Use that variable (do 
# not recompute it with sample.split) to make new training and testing sets with
# wikiWords3.
wikiTrain3 <- subset(wikiWords3, wikiSplit == TRUE)
wikiTest3 <- subset(wikiWords3, wikiSplit == FALSE)
# Build a CART model using all the training data. What is the accuracy of the 
# model on the test set?
# Then create a new CART model using this new variable as one of the independent
# variables.
wikiTree3 <- rpart(Vandal ~ ., data = wikiTrain3, method = "class")
wikiPred3 <- as.vector(predict(wikiTree3, newdata = wikiTest3, type="class"))
head(wikiPred3)
table(wikiTest3$Vandal, wikiPred3)
(595 + 241) / nrow(wikiTest3)
## 0.7188306


# Problem 3.2 - Using Non-Textual Data
# There is a substantial difference in the accuracy of the model using the meta
# data. Is this because we made a more complicated model?
# Plot the CART tree. How many splits are there in the tree?
prp(wikiTree3)
## 3