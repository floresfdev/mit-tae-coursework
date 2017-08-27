# Load the datasets

## Load columns as NA if the field is blank or NA
train <- read.csv("./data/train2016.csv", na.strings = c("","NA"))
test <- read.csv("./data/test2016.csv", na.strings = c("","NA"))

## Imputed training datasets
train.imputed.b.1 <- 
    read.csv("./data/train_imputed_b_1.csv", na.strings = c("","NA"))

train.imputed.b.2 <- 
    read.csv("./data/train_imputed_b_2.csv", na.strings = c("","NA"))

train.imputed.b.3 <- 
    read.csv("./data/train_imputed_b_3.csv", na.strings = c("","NA"))

train.imputed.b.4 <- 
    read.csv("./data/train_imputed_b_4.csv", na.strings = c("","NA"))

train.imputed.b.5 <- 
    read.csv("./data/train_imputed_b_5.csv", na.strings = c("","NA"))

train.imputed.b.6 <- 
    read.csv("./data/train_imputed_b_6.csv", na.strings = c("","NA"))

train.imputed.b.7 <- 
    read.csv("./data/train_imputed_b_7.csv", na.strings = c("","NA"))

train.imputed.b.8 <- 
    read.csv("./data/train_imputed_b_8.csv", na.strings = c("","NA"))

train.imputed.b.9 <- 
    read.csv("./data/train_imputed_b_9.csv", na.strings = c("","NA"))

train.imputed.b.10 <- 
    read.csv("./data/train_imputed_b_10.csv", na.strings = c("","NA"))

train.imputed.b.11 <- 
    read.csv("./data/train_imputed_b_11.csv", na.strings = c("","NA"))

train.imputed.b.12 <- 
    read.csv("./data/train_imputed_b_12.csv", na.strings = c("","NA"))


## Imputed testing datasets
test.imputed.b.1 <- 
    read.csv("./data/test_imputed_b_1.csv", na.strings = c("","NA"))

test.imputed.b.2 <- 
    read.csv("./data/test_imputed_b_2.csv", na.strings = c("","NA"))

test.imputed.b.3 <- 
    read.csv("./data/test_imputed_b_3.csv", na.strings = c("","NA"))

test.imputed.b.4 <- 
    read.csv("./data/test_imputed_b_4.csv", na.strings = c("","NA"))

test.imputed.b.5 <- 
    read.csv("./data/test_imputed_b_5.csv", na.strings = c("","NA"))

test.imputed.b.6 <- 
    read.csv("./data/test_imputed_b_6.csv", na.strings = c("","NA"))

test.imputed.b.7 <- 
    read.csv("./data/test_imputed_b_7.csv", na.strings = c("","NA"))

test.imputed.b.8 <- 
    read.csv("./data/test_imputed_b_8.csv", na.strings = c("","NA"))

test.imputed.b.9 <- 
    read.csv("./data/test_imputed_b_9.csv", na.strings = c("","NA"))

test.imputed.b.10 <- 
    read.csv("./data/test_imputed_b_10.csv", na.strings = c("","NA"))

test.imputed.b.11 <- 
    read.csv("./data/test_imputed_b_11.csv", na.strings = c("","NA"))

test.imputed.b.12 <- 
    read.csv("./data/test_imputed_b_12.csv", na.strings = c("","NA"))


# Cross-validation and variable importance
require(caret)
require(mlbench)

## Prepare training scheme
control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

## Train the model for imputed training dataset 1
# model.cv.lvq.1 <- train(Party ~ . -USER_ID, data = train.imputed.b.1, 
#                         method = "lvq", preProcess = "scale", 
#                         trControl = control)
## NOT WORKING! R session terminates...

# # estimate variable importance
# importance <- varImp(model, scale=FALSE)
# # summarize importance
# print(importance)
# # plot importance
# plot(importance)


# Global threshold
LogThreshold <- 0.5


# Logistic regresion with feature selection

## Logistic regression for imputed training dataset 1 with selected features
LogFS1 <- glm(Party ~ Income + EducationLevel
              + Gender + HouseholdStatus + YOB + Q98197 + Q98869
              + Q99480 + Q101163 + Q104996 + Q105840 + Q106272 + Q106997
              + Q108855 + Q109244 + Q110740 + Q113181 + Q115611 + Q115899
              + Q116881 + Q116953 + Q119851 + Q120379 + Q120472, 
              data = train.imputed.b.1, family = binomial)
summary(LogFS1)

PredTestLogFS1 <- 
    predict(LogFS1, newdata = test.imputed.b.1, type = "response")
head(PredTestLogFS1)

PredTestLabelsLogFS1 <- 
    as.factor(ifelse(PredTestLogFS1 < LogThreshold, "Democrat", "Republican"))
head(PredTestLabelsLogFS1)
sum(is.na(PredTestLabelsLogFS1)) 


## Logistic regression for imputed training dataset 2 with selected features
LogFS2 <- glm(Party ~ Income + EducationLevel
              + Gender + HouseholdStatus + YOB + Q98197 + Q98869
              + Q99480 + Q101163 + Q104996 + Q105840 + Q106272 + Q106997
              + Q108855 + Q109244 + Q110740 + Q113181 + Q115611 + Q115899
              + Q116881 + Q116953 + Q119851 + Q120379 + Q120472, 
              data = train.imputed.b.2, family = binomial)
summary(LogFS2)

PredTestLogFS2 <- 
    predict(LogFS2, newdata = test.imputed.b.2, type = "response")
head(PredTestLogFS2)

PredTestLabelsLogFS2 <- 
    as.factor(ifelse(PredTestLogFS2 < LogThreshold, "Democrat", "Republican"))
head(PredTestLabelsLogFS2)
sum(is.na(PredTestLabelsLogFS2)) 


## Logistic regression for imputed training dataset 3 with selected features
LogFS3 <- glm(Party ~ Income + EducationLevel
              + Gender + HouseholdStatus + YOB + Q98197 + Q98869
              + Q99480 + Q101163 + Q104996 + Q105840 + Q106272 + Q106997
              + Q108855 + Q109244 + Q110740 + Q113181 + Q115611 + Q115899
              + Q116881 + Q116953 + Q119851 + Q120379 + Q120472, 
              data = train.imputed.b.3, family = binomial)
summary(LogFS3)

PredTestLogFS3 <- 
    predict(LogFS3, newdata = test.imputed.b.3, type = "response")
head(PredTestLogFS3)

PredTestLabelsLogFS3 <- 
    as.factor(ifelse(PredTestLogFS3 < LogThreshold, "Democrat", "Republican"))
head(PredTestLabelsLogFS3)
sum(is.na(PredTestLabelsLogFS3)) 


## Logistic regression for imputed training dataset 4 with selected features
LogFS4 <- glm(Party ~ Income + EducationLevel
              + Gender + HouseholdStatus + YOB + Q98197 + Q98869
              + Q99480 + Q101163 + Q104996 + Q105840 + Q106272 + Q106997
              + Q108855 + Q109244 + Q110740 + Q113181 + Q115611 + Q115899
              + Q116881 + Q116953 + Q119851 + Q120379 + Q120472, 
              data = train.imputed.b.4, family = binomial)
summary(LogFS4)

PredTestLogFS4 <- 
    predict(LogFS4, newdata = test.imputed.b.4, type = "response")
head(PredTestLogFS4)

PredTestLabelsLogFS4 <- 
    as.factor(ifelse(PredTestLogFS4 < LogThreshold, "Democrat", "Republican"))
head(PredTestLabelsLogFS4)
sum(is.na(PredTestLabelsLogFS4)) 


## Logistic regression for imputed training dataset 5 with selected features
LogFS5 <- glm(Party ~ Income + EducationLevel
              + Gender + HouseholdStatus + YOB + Q98197 + Q98869
              + Q99480 + Q101163 + Q104996 + Q105840 + Q106272 + Q106997
              + Q108855 + Q109244 + Q110740 + Q113181 + Q115611 + Q115899
              + Q116881 + Q116953 + Q119851 + Q120379 + Q120472, 
              data = train.imputed.b.5, family = binomial)
summary(LogFS5)

PredTestLogFS5 <- 
    predict(LogFS5, newdata = test.imputed.b.5, type = "response")
head(PredTestLogFS5)

PredTestLabelsLogFS5 <- 
    as.factor(ifelse(PredTestLogFS5 < LogThreshold, "Democrat", "Republican"))
head(PredTestLabelsLogFS5)
sum(is.na(PredTestLabelsLogFS5)) 


## Logistic regression for imputed training dataset 6 with selected features
LogFS6 <- glm(Party ~ Income + EducationLevel
              + Gender + HouseholdStatus + YOB + Q98197 + Q98869
              + Q99480 + Q101163 + Q104996 + Q105840 + Q106272 + Q106997
              + Q108855 + Q109244 + Q110740 + Q113181 + Q115611 + Q115899
              + Q116881 + Q116953 + Q119851 + Q120379 + Q120472, 
              data = train.imputed.b.6, family = binomial)
summary(LogFS6)

PredTestLogFS6 <- 
    predict(LogFS6, newdata = test.imputed.b.6, type = "response")
head(PredTestLogFS6)

PredTestLabelsLogFS6 <- 
    as.factor(ifelse(PredTestLogFS6 < LogThreshold, "Democrat", "Republican"))
head(PredTestLabelsLogFS6)
sum(is.na(PredTestLabelsLogFS6)) 


## Logistic regression for imputed training dataset 7 with selected features
LogFS7 <- glm(Party ~ Income + EducationLevel
              + Gender + HouseholdStatus + YOB + Q98197 + Q98869
              + Q99480 + Q101163 + Q104996 + Q105840 + Q106272 + Q106997
              + Q108855 + Q109244 + Q110740 + Q113181 + Q115611 + Q115899
              + Q116881 + Q116953 + Q119851 + Q120379 + Q120472, 
              data = train.imputed.b.7, family = binomial)
summary(LogFS7)

PredTestLogFS7 <- 
    predict(LogFS7, newdata = test.imputed.b.7, type = "response")
head(PredTestLogFS7)

PredTestLabelsLogFS7 <- 
    as.factor(ifelse(PredTestLogFS7 < LogThreshold, "Democrat", "Republican"))
head(PredTestLabelsLogFS7)
sum(is.na(PredTestLabelsLogFS7)) 


## Logistic regression for imputed training dataset 8 with selected features
LogFS8 <- glm(Party ~ Income + EducationLevel
              + Gender + HouseholdStatus + YOB + Q98197 + Q98869
              + Q99480 + Q101163 + Q104996 + Q105840 + Q106272 + Q106997
              + Q108855 + Q109244 + Q110740 + Q113181 + Q115611 + Q115899
              + Q116881 + Q116953 + Q119851 + Q120379 + Q120472, 
              data = train.imputed.b.8, family = binomial)
summary(LogFS8)

PredTestLogFS8 <- 
    predict(LogFS8, newdata = test.imputed.b.8, type = "response")
head(PredTestLogFS8)

PredTestLabelsLogFS8 <- 
    as.factor(ifelse(PredTestLogFS8 < LogThreshold, "Democrat", "Republican"))
head(PredTestLabelsLogFS8)
sum(is.na(PredTestLabelsLogFS8)) 


## Logistic regression for imputed training dataset 9 with selected features
LogFS9 <- glm(Party ~ Income + EducationLevel
              + Gender + HouseholdStatus + YOB + Q98197 + Q98869
              + Q99480 + Q101163 + Q104996 + Q105840 + Q106272 + Q106997
              + Q108855 + Q109244 + Q110740 + Q113181 + Q115611 + Q115899
              + Q116881 + Q116953 + Q119851 + Q120379 + Q120472, 
              data = train.imputed.b.9, family = binomial)
summary(LogFS9)

PredTestLogFS9 <- 
    predict(LogFS9, newdata = test.imputed.b.9, type = "response")
head(PredTestLogFS9)

PredTestLabelsLogFS9 <- 
    as.factor(ifelse(PredTestLogFS9 < LogThreshold, "Democrat", "Republican"))
head(PredTestLabelsLogFS9)
sum(is.na(PredTestLabelsLogFS9)) 


## Logistic regression for imputed training dataset 10 with selected features
LogFS10 <- glm(Party ~ Income + EducationLevel
              + Gender + HouseholdStatus + YOB + Q98197 + Q98869
              + Q99480 + Q101163 + Q104996 + Q105840 + Q106272 + Q106997
              + Q108855 + Q109244 + Q110740 + Q113181 + Q115611 + Q115899
              + Q116881 + Q116953 + Q119851 + Q120379 + Q120472, 
              data = train.imputed.b.10, family = binomial)
summary(LogFS10)

PredTestLogFS10 <- 
    predict(LogFS10, newdata = test.imputed.b.10, type = "response")
head(PredTestLogFS10)

PredTestLabelsLogFS10 <- 
    as.factor(ifelse(PredTestLogFS10 < LogThreshold, "Democrat", "Republican"))
head(PredTestLabelsLogFS10)
sum(is.na(PredTestLabelsLogFS10)) 


## Logistic regression for imputed training dataset 11 with selected features
LogFS11 <- glm(Party ~ Income + EducationLevel
              + Gender + HouseholdStatus + YOB + Q98197 + Q98869
              + Q99480 + Q101163 + Q104996 + Q105840 + Q106272 + Q106997
              + Q108855 + Q109244 + Q110740 + Q113181 + Q115611 + Q115899
              + Q116881 + Q116953 + Q119851 + Q120379 + Q120472, 
              data = train.imputed.b.11, family = binomial)
summary(LogFS11)

PredTestLogFS11 <- 
    predict(LogFS11, newdata = test.imputed.b.11, type = "response")
head(PredTestLogFS11)

PredTestLabelsLogFS11 <- 
    as.factor(ifelse(PredTestLogFS11 < LogThreshold, "Democrat", "Republican"))
head(PredTestLabelsLogFS11)
sum(is.na(PredTestLabelsLogFS11)) 


## Logistic regression for imputed training dataset 12 with selected features
LogFS12 <- glm(Party ~ Income + EducationLevel
              + Gender + HouseholdStatus + YOB + Q98197 + Q98869
              + Q99480 + Q101163 + Q104996 + Q105840 + Q106272 + Q106997
              + Q108855 + Q109244 + Q110740 + Q113181 + Q115611 + Q115899
              + Q116881 + Q116953 + Q119851 + Q120379 + Q120472, 
              data = train.imputed.b.12, family = binomial)
summary(LogFS12)

PredTestLogFS12 <- 
    predict(LogFS12, newdata = test.imputed.b.12, type = "response")
head(PredTestLogFS12)

PredTestLabelsLogFS12 <- 
    as.factor(ifelse(PredTestLogFS12 < LogThreshold, "Democrat", "Republican"))
head(PredTestLabelsLogFS12)
sum(is.na(PredTestLabelsLogFS12)) 


# Generate dataset for averaging predictions
avgPredLog <- data.frame(USER_ID = test$USER_ID, 
                      PredLogFS1 = PredTestLabelsLogFS1,
                      PredLogFS2 = PredTestLabelsLogFS1,
                      PredLogFS3 = PredTestLabelsLogFS1,
                      PredLogFS4 = PredTestLabelsLogFS4,
                      PredLogFS5 = PredTestLabelsLogFS5,
                      PredLogFS6 = PredTestLabelsLogFS6,
                      PredLogFS7 = PredTestLabelsLogFS7,
                      PredLogFS8 = PredTestLabelsLogFS8,
                      PredLogFS9 = PredTestLabelsLogFS9,
                      PredLogFS10 = PredTestLabelsLogFS10,
                      PredLogFS11 = PredTestLabelsLogFS11,
                      PredLogFS12 = PredTestLabelsLogFS12,
                      PredProbLogFS1 = PredTestLogFS1,
                      PredProbLogFS2 = PredTestLogFS2,
                      PredProbLogFS3 = PredTestLogFS3,
                      PredProbLogFS4 = PredTestLogFS4,
                      PredProbLogFS5 = PredTestLogFS5,
                      PredProbLogFS6 = PredTestLogFS6,
                      PredProbLogFS7 = PredTestLogFS7,
                      PredProbLogFS8 = PredTestLogFS8,
                      PredProbLogFS9 = PredTestLogFS9,
                      PredProbLogFS10 = PredTestLogFS10,
                      PredProbLogFS11 = PredTestLogFS11,
                      PredProbLogFS12 = PredTestLogFS12)
str(avgPredLog)
summary(avgPredLog)

avgPredLog$average <- ((as.numeric(avgPredLog$PredLogFS1) - 1) + 
                           (as.numeric(avgPredLog$PredLogFS2) - 1) + 
                           (as.numeric(avgPredLog$PredLogFS3) - 1) + 
                           (as.numeric(avgPredLog$PredLogFS4) - 1) + 
                           (as.numeric(avgPredLog$PredLogFS5) - 1) + 
                           (as.numeric(avgPredLog$PredLogFS6) - 1) +
                           (as.numeric(avgPredLog$PredLogFS7) - 1) + 
                           (as.numeric(avgPredLog$PredLogFS8) - 1) + 
                           (as.numeric(avgPredLog$PredLogFS9) - 1) + 
                           (as.numeric(avgPredLog$PredLogFS10) - 1) + 
                           (as.numeric(avgPredLog$PredLogFS11) - 1) + 
                           (as.numeric(avgPredLog$PredLogFS12) - 1)) / 12

avgPredLog$averageProb <- (avgPredLog$PredProbLogFS1 + 
                               avgPredLog$PredProbLogFS2 + 
                               avgPredLog$PredProbLogFS3 +
                               avgPredLog$PredProbLogFS4 + 
                               avgPredLog$PredProbLogFS5 + 
                               avgPredLog$PredProbLogFS6 +
                               avgPredLog$PredProbLogFS7 + 
                               avgPredLog$PredProbLogFS8 + 
                               avgPredLog$PredProbLogFS9 +    
                               avgPredLog$PredProbLogFS10 + 
                               avgPredLog$PredProbLogFS11 + 
                               avgPredLog$PredProbLogFS12) / 12

head(avgPredLog)
table(avgPredLog$average)
table(avgPredLog$averageProb)

## If average is between 0 and 0.5, the average prediction is "Democrat"
## If average is between 0.5 and 1, the average prediction is "Republican"

avgThreshold <- 0.5

avgPredLog$averageClassPrediction <- 
    as.factor(ifelse(avgPredLog$average <= avgThreshold, 
                     "Democrat", "Republican"))

avgPredLog$averageProbPrediction <- 
    as.factor(ifelse(avgPredLog$averageProb <= avgThreshold, 
                     "Democrat", "Republican"))

head(avgPredLog)

## Check how different are the averaged prediction with the imputed dataset #1 
nrow(subset(avgPredLog, PredTestLabelsLogFS1 != averageClassPrediction))
## => 106 rows are different

## Check how different are the averaged predictions from class and prob
nrow(subset(avgPredLog, averageClassPrediction != averageProbPrediction))
## => 109 rows are different


# Save Output dataset with averaged prediction from class
submission.20160614.1 <- 
    data.frame(USER_ID = avgPredLog$USER_ID, 
               Predictions = avgPredLog$averageClassPrediction)
head(submission.20160614.1)
sum(is.na(submission.20160614.1))
str(submission.20160614.1)
summary(submission.20160614.1)

write.csv(submission.20160614.1, "./dataout/submission-20160614-1-late.csv", 
          row.names = FALSE, quote = FALSE)


# Save Output dataset with averaged prediction from probs
submission.20160614.2 <- 
    data.frame(USER_ID = avgPredLog$USER_ID, 
               Predictions = avgPredLog$averageProbPrediction)
head(submission.20160614.2)
sum(is.na(submission.20160614.2))
str(submission.20160614.2)
summary(submission.20160614.2)

write.csv(submission.20160614.2, "./dataout/submission-20160614-2-late.csv", 
          row.names = FALSE, quote = FALSE)
