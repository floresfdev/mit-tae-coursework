# Load the datasets

## Load the datasets as-is
trainOriginal <- read.csv("./data/train2016.csv")
testOriginal <- read.csv("./data/test2016.csv")
## Load columns as NA if the field is blank or NA
train <- read.csv("./data/train2016.csv", na.strings = c("","NA"))
test <- read.csv("./data/test2016.csv", na.strings = c("","NA"))
?read.csv


# Summaries
str(train)
str(test)


# Investigate NAs
sum(is.na(trainOriginal)) 
## 333 NAs by default
sum(is.na(train)) 
## 220073 NAs with na.strings while loading
sum(is.na(testOriginal)) 
## 82 NAs by default
sum(is.na(test)) 
## 55846 NAs with na.strings while loading

View(trainOriginal)
View(train)

colnames(train)[colSums(is.na(train)) > 0]
## YOB, Gender, Income, HouseholdStatus, EducationLevel and all 101 questions
## (so in the training dataset, all columns have NAs, except USER_ID and Party)

colnames(test)[colSums(is.na(test)) > 0]
## YOB, Gender, Income, HouseholdStatus, EducationLevel and all 101 questions
## (so in the testing dataset, all columns have NAs, except USER_ID and Party)


# For imputation of missing values, see script kaggle_ImputingNAs.R


## Dataset 1 - Simple logistic regression
threshold <- 0.5

train.imputed.1 <- 
    read.csv("./data/train_imputed_1.csv", na.strings = c("","NA"))
str(train.imputed.1)
head(train.imputed.1[, -1], 1)
train.imputed.1 <- train.imputed.1[, -1]
sum(is.na(train.imputed.1)) 

SimpleMod1 <- glm(Party ~ . -USER_ID, data = train.imputed.1, family = binomial)
summary(SimpleMod1)

PredTest1 <- predict(SimpleMod1, newdata = test, type = "response")
head(PredTest1)

PredTestLabels1 <- 
    as.factor(ifelse(PredTest1 < threshold, "Democrat", "Republican"))
head(PredTestLabels1)
sum(is.na(PredTestLabels1)) ## 1224 NAs!!! In the simple log there was only 82

submission1 <- data.frame(USER_ID = test$USER_ID, Predictions = PredTestLabels1)
head(submission1)

write.csv(submission1, "./dataout/submission1.csv", 
          row.names = FALSE, quote = FALSE)

## -------------------

# Random forests
library(randomForest)

RF1 <- randomForest(Party ~ . -USER_ID, data = train.imputed.1, 
                    ntree = 200, nodesize = 500)
summary(RF1)
PredTestRF1 <- predict(RF1, newdata = test)
head(PredTestRF1)
sum(is.na(PredTestRF1))

## -------------------

# CART
library(rpart)
library(rpart.plot)

Tree1 <- rpart(Party ~ . -USER_ID, data = train.imputed.1, 
               method = "class", minbucket = 25)
prp(Tree1)
PredTestTree1 <- predict(Tree1, newdata = test, type = "class")
head(PredTestTree1)
sum(is.na(PredTestTree1))

### AUC for training data
library(ROCR)

PredTrainTree1.class <- predict(Tree1, type = "class")
sum(is.na(PredTrainTree1.class))

table(PredTrainTree1.class, train.imputed.1$Party)
(1945 + 1531) / nrow(train.imputed.1)
## 0.6242816 accuracy of training set (imputed #1)

PredTrainTree1.prob <- predict(Tree1)
head(PredTrainTree1.prob)
#### PredTrainTree1.prob has 2 columns: Democrat and Republican, both with probs
nrow(PredTrainTree1.prob)
nrow(train.imputed.1)

head(train.imputed.1$Party)
#### Party column has 2 Levels: 1=Democrat, 2=Republican
PredTrainTree1ROC <- prediction(PredTrainTree1.prob[, 1], 
                                train.imputed.1$Party == "Democrat")
PerfTrainTree1ROC <- performance(PredTrainTree1ROC, "tpr", "fpr")
plot(PerfTrainTree1ROC)
plot(PerfTrainTree1ROC, colorize=TRUE, 
     print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

#PredTrainTree1.class.from.probs <- PredTrainTree1[, 1] > 0.6
PredTrainTree1.threshold <- 0.4
PredTrainTree1.class.from.probs <- 
    as.factor(ifelse(PredTrainTree1.prob[, 1] > PredTrainTree1.threshold, 
                     "Democrat", "Republican"))
head(PredTrainTree1.class.from.probs)
table(PredTrainTree1.class.from.probs)
head(PredTrainTree1.class)
table(PredTrainTree1.class)

PredTestTree1.prob <- predict(Tree1, newdata = test)
head(PredTestTree1.prob)
sum(is.na(PredTestTree1))

PredTestTree1.threshold <- 0.4
PredTestTree1.class.from.probs <- 
    as.factor(ifelse(PredTestTree1.prob[, 1] > PredTestTree1.threshold, 
                     "Democrat", "Republican"))
head(PredTestTree1.class.from.probs)
table(PredTestTree1.class.from.probs)
head(PredTestTree1)
table(PredTestTree1)


submission.20160612.3 <- 
    data.frame(USER_ID = test$USER_ID, 
               Predictions = PredTestTree1.class.from.probs)
head(submission.20160612.3)
sum(is.na(submission.20160612.3))
str(submission.20160612.3)
summary(submission.20160612.3)

write.csv(submission.20160612.3, "./dataout/submission-20160612-3.csv", 
          row.names = FALSE, quote = FALSE)

#submission.20160607.2 <- 
#    data.frame(USER_ID = test$USER_ID, Predictions = PredTestTree1)
#head(submission.20160607.2)
#sum(is.na(submission.20160607.2))
#str(submission.20160607.2)

#write.csv(submission.20160607.2, "./dataout/submission-20160607-2.csv", 
#          row.names = FALSE, quote = FALSE)

# -----------------------

## Cross-validation and then CART
library(caret)
library(e1071)

### Define cross-validation experiment
numFolds <- trainControl(method = "cv", number = 10)
cpGrid <- expand.grid(.cp = seq(0.01,0.5,0.01))

### Perform the cross validation
train(Party ~ . -USER_ID, data = train.imputed.1, 
      method = "rpart", trControl = numFolds, tuneGrid = cpGrid )

### Create a new CART model
TreeCV1 <- rpart(Party ~ . -USER_ID, data = train.imputed.1, 
               method="class", cp = 0.02)
prp(TreeCV1)
PredTestTreeCV1 <- predict(TreeCV1, newdata = test, type = "class")
head(PredTestTreeCV1)
sum(is.na(PredTestTreeCV1))

### Make predictions
PredTestTreeCV1 <- predict(TreeCV1, newdata = test, type = "class")
head(PredTestTreeCV1)
sum(is.na(PredTestTreeCV1))

### Output
submission.20160607.3 <- 
    data.frame(USER_ID = test$USER_ID, Predictions = PredTestTreeCV1)
head(submission.20160607.3)
sum(is.na(submission.20160607.3))
str(submission.20160607.3)

write.csv(submission.20160607.3, "./dataout/submission-20160607-3.csv", 
          row.names = FALSE, quote = FALSE)

## -------------------

# CART with bagging of the 6 imputed datasets (minbucket = 25)

## CART with bagging 1
TreeBag1 <- rpart(Party ~ . -USER_ID, data = train.imputed.1, 
                  method = "class", minbucket = 25)
prp(TreeBag1)
PredTestTreeBag1 <- predict(TreeBag1, newdata = test, type = "class")
head(PredTestTreeBag1)
sum(is.na(PredTestTreeBag1))

## CART with bagging 2
train.imputed.2 <- read.csv("./data/train_imputed_2.csv")
train.imputed.2 <- train.imputed.2[, -1]

TreeBag2 <- rpart(Party ~ . -USER_ID, data = train.imputed.2, 
                  method = "class", minbucket = 25)
prp(TreeBag2)
PredTestTreeBag2 <- predict(TreeBag2, newdata = test, type = "class")
head(PredTestTreeBag2)
sum(is.na(PredTestTreeBag2))

## CART with bagging 3
train.imputed.3 <- read.csv("./data/train_imputed_3.csv")
train.imputed.3 <- train.imputed.3[, -1]

TreeBag3 <- rpart(Party ~ . -USER_ID, data = train.imputed.3, 
                  method = "class", minbucket = 25)
prp(TreeBag3)
PredTestTreeBag3 <- predict(TreeBag3, newdata = test, type = "class")
head(PredTestTreeBag3)
sum(is.na(PredTestTreeBag3))

## CART with bagging 4
train.imputed.4 <- read.csv("./data/train_imputed_4.csv")
train.imputed.4 <- train.imputed.4[, -1]

TreeBag4 <- rpart(Party ~ . -USER_ID, data = train.imputed.4, 
                  method = "class", minbucket = 25)
prp(TreeBag4)
PredTestTreeBag4 <- predict(TreeBag4, newdata = test, type = "class")
head(PredTestTreeBag4)
sum(is.na(PredTestTreeBag4))

### Interesting: TreeBag4 looks different than the others, 1 more decission node

## CART with bagging 5
train.imputed.5 <- read.csv("./data/train_imputed_5.csv")
train.imputed.5 <- train.imputed.5[, -1]

TreeBag5 <- rpart(Party ~ . -USER_ID, data = train.imputed.5, 
                  method = "class", minbucket = 25)
prp(TreeBag5)
PredTestTreeBag5 <- predict(TreeBag5, newdata = test, type = "class")
head(PredTestTreeBag5)
sum(is.na(PredTestTreeBag5))

## CART with bagging 6
train.imputed.6 <- read.csv("./data/train_imputed_6.csv")
train.imputed.6 <- train.imputed.6[, -1]

TreeBag6 <- rpart(Party ~ . -USER_ID, data = train.imputed.6, 
                  method = "class", minbucket = 25)
prp(TreeBag6)
PredTestTreeBag6 <- predict(TreeBag6, newdata = test, type = "class")
head(PredTestTreeBag6)
sum(is.na(PredTestTreeBag6))

## Generate dataset for averaging predictions
baggedPred <- data.frame(USER_ID = test$USER_ID, 
                         PredBag1 = PredTestTreeBag1,
                         PredBag2 = PredTestTreeBag2,
                         PredBag3 = PredTestTreeBag3,
                         PredBag4 = PredTestTreeBag4,
                         PredBag5 = PredTestTreeBag5,
                         PredBag6 = PredTestTreeBag6)
head(baggedPred)
str(baggedPred)

baggedPredEqual <- subset(baggedPred,
                         PredBag1 == PredBag2 & PredBag1 == PredBag3
                         & PredBag1 == PredBag4 & PredBag1 == PredBag5 
                         & PredBag1 == PredBag6 )
str(baggedPredEqual)

baggedPredDiff <- subset(baggedPred,
                          !(PredBag1 == PredBag2 & PredBag1 == PredBag3
                          & PredBag1 == PredBag4 & PredBag1 == PredBag5 
                          & PredBag1 == PredBag6))
str(baggedPredDiff)
head(baggedPredDiff)
View(baggedPredDiff)


## Submission
#submission.20160607.4 <- 
#    data.frame(USER_ID = test$USER_ID, Predictions = PredTestTree1)
#head(submission.20160607.4)
#sum(is.na(submission.20160607.4))
#str(submission.20160607.4)
#
#write.csv(submission.20160607.4, "./dataout/submission-20160607-4.csv", 
#          row.names = FALSE, quote = FALSE)

### ONLY USE TreeBag4 to analyze how one extra decission node makes a difference
submission.20160607.4 <- 
    data.frame(USER_ID = test$USER_ID, Predictions = PredTestTreeBag4)
head(submission.20160607.4)
sum(is.na(submission.20160607.4))
str(submission.20160607.4)

write.csv(submission.20160607.4, "./dataout/submission-20160607-4.csv", 
          row.names = FALSE, quote = FALSE)
### WORST SCORE!

## -------------------


## -------------------

# Simple logistic regression
SimpleMod <- glm(Party ~ . -USER_ID, data = train, family = binomial)

# Predictions
PredTest <- predict(SimpleMod, newdata = test, type = "response")
threshold <- 0.5
PredTestLabels <- 
    as.factor(ifelse(PredTest < threshold, "Democrat", "Republican"))
