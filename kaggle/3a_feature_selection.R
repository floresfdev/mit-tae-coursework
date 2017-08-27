# Load the datasets

## Load columns as NA if the field is blank or NA
train <- read.csv("./data/train2016.csv", na.strings = c("","NA"))
test <- read.csv("./data/test2016.csv", na.strings = c("","NA"))

## Imputed datasets
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


# Cross-validation and variable importance
require(caret)
require(rpart)
#library(e1071)

## Define cross-validation experiment 
numFolds <- trainControl(method = "cv", number = 10)
cpGrid <- expand.grid(.cp = seq(0.01,0.5,0.01))


## Perform the cross validation with original training dataset
model.cv.original <- train(Party ~ . -USER_ID, data = train, 
                           method = "rpart", 
                           trControl = numFolds, tuneGrid = cpGrid )

## estimate variable importance
importance.original <- varImp(model.cv.original) #scale=FALSE
## summarize importance
print(importance.original)
## plot importance
plot(importance.original)


## Perform the cross validation with imputed training dataset #1
model.cv.1 <- train(Party ~ . -USER_ID, data = train.imputed.b.1, 
                          method = "rpart", 
                          trControl = numFolds, tuneGrid = cpGrid )
model.cv.1
cp.1 <- 0.02
## estimate variable importance
importance.1 <- varImp(model.cv.1) #scale=FALSE
## summarize importance
print(importance.1)
# Q109244Yes                          100.000
# Q113181Yes                           71.955
# Q115611Yes                           70.163
# Q98197Yes                            70.055
# GenderMale                           28.699
# Q101163Mom                           15.887
# HouseholdStatusMarried (w/kids)       8.239
# Q108855Yes!                           3.538
## plot importance
plot(importance.1)


## Perform the cross validation with imputed training dataset #2
model.cv.2 <- train(Party ~ . -USER_ID, data = train.imputed.b.2, 
                    method = "rpart", 
                    trControl = numFolds, tuneGrid = cpGrid )
model.cv.2
cp.2 <- 0.02

## estimate variable importance
importance.2 <- varImp(model.cv.2) #scale=FALSE
## summarize importance
print(importance.2)
# Q109244Yes                        100.000
# Q115611Yes                         81.963
# Q98197Yes                          65.378
# Q113181Yes                         61.988
# GenderMale                         24.543
# Q98869Yes                          15.591
# HouseholdStatusMarried (w/kids)    15.307
# Q106272Yes                          4.908
# YOB                                 4.045
## plot importance
plot(importance.2)


## Perform the cross validation with imputed training dataset #3
model.cv.3 <- train(Party ~ . -USER_ID, data = train.imputed.b.3, 
                    method = "rpart", 
                    trControl = numFolds, tuneGrid = cpGrid )
model.cv.3
cp.3 <- 0.03

## estimate variable importance
importance.3 <- varImp(model.cv.3) #scale=FALSE
## summarize importance
print(importance.3)
# Q109244Yes                      100.000
# Q98197Yes                        93.461
# Q115611Yes                       91.679
# Q113181Yes                       82.553
# GenderMale                       29.495
# HouseholdStatusMarried (w/kids)  16.100
# Q101163Mom                       15.388
# Q99480Yes                         6.131
## plot importance
plot(importance.3)


## Perform the cross validation with imputed training dataset #4
model.cv.4 <- train(Party ~ . -USER_ID, data = train.imputed.b.4, 
                    method = "rpart", 
                    trControl = numFolds, tuneGrid = cpGrid )
model.cv.4
cp.4 <- 0.03

## estimate variable importance
importance.4 <- varImp(model.cv.4) #scale=FALSE
## summarize importance
print(importance.4)
# Q109244Yes                            100.000
# Q115611Yes                             75.105
# Q98197Yes                              58.736
# Q113181Yes                             56.792
# GenderMale                             25.086
# Q116881Right                           12.832
# Q98869Yes                              10.179
# Q116953Yes                              5.365
# Q106272Yes                              4.974
# Q110740PC                               4.557
## plot importance
plot(importance.4)


## Perform the cross validation with imputed training dataset #5
model.cv.5 <- train(Party ~ . -USER_ID, data = train.imputed.b.5, 
                    method = "rpart", 
                    trControl = numFolds, tuneGrid = cpGrid )
model.cv.5
cp.5 <- 0.03

## estimate variable importance
importance.5 <- varImp(model.cv.5) #scale=FALSE
## summarize importance
print(importance.5)
# Q115611Yes                         100.000
# Q109244Yes                          96.215
# Q98197Yes                           76.331
# Q113181Yes                          61.919
# GenderMale                          32.544
# Q98869Yes                           12.720
# Q101163Mom                           9.765
# Q119851Yes                           4.772
# Q110740PC                            4.568
# Q106272Yes                           3.877
## plot importance
plot(importance.5)


## Perform the cross validation with imputed training dataset #6
model.cv.6 <- train(Party ~ . -USER_ID, data = train.imputed.b.6, 
                    method = "rpart", 
                    trControl = numFolds, tuneGrid = cpGrid )
model.cv.6
cp.6 <- 0.02

## estimate variable importance
importance.6 <- varImp(model.cv.6) #scale=FALSE
## summarize importance
print(importance.6)
# Q109244Yes                        100.000
# Q113181Yes                         83.415
# Q115611Yes                         79.537
# Q98197Yes                          62.586
# GenderMale                         24.150
# Q106272Yes                         17.279
# Q98869Yes                           8.833
# Q120472Science                      4.769
# Q99480Yes                           3.982
# Q110740PC                           3.773
## plot importance
plot(importance.6)


## Perform the cross validation with imputed training dataset #7
model.cv.7 <- train(Party ~ . -USER_ID, data = train.imputed.b.7, 
                    method = "rpart", 
                    trControl = numFolds, tuneGrid = cpGrid )
model.cv.7
cp.7 <- 0.02

## estimate variable importance
importance.7 <- varImp(model.cv.7)
## summarize importance
print(importance.7)
# Q109244Yes                         100.000
# Q115611Yes                          69.622
# Q113181Yes                          67.820
# Q98197Yes                           66.671
# GenderMale                          25.880
# HouseholdStatusMarried (w/kids)     14.103
# Q98869Yes                            9.964
# Q106272Yes                           6.644
# Q99480Yes                            4.116
# Q106997Yay people!                   3.560


## Perform the cross validation with imputed training dataset #8
model.cv.8 <- train(Party ~ . -USER_ID, data = train.imputed.b.8, 
                    method = "rpart", 
                    trControl = numFolds, tuneGrid = cpGrid )
model.cv.8
cp.8 <- 0.02

## estimate variable importance
importance.8 <- varImp(model.cv.8)
## summarize importance
print(importance.8)
# Q109244Yes                      100.000
# Q115611Yes                       60.715
# Q98197Yes                        56.233
# Q113181Yes                       52.488
# GenderMale                       21.240
# Q101163Mom                        9.071
# Q99480Yes                         7.283
# Q120379Yes                        3.505
# Q104996Yes                        2.973
# Q115899Me                         2.695
# HouseholdStatusMarried (w/kids)   2.375


## Perform the cross validation with imputed training dataset #9
model.cv.9 <- train(Party ~ . -USER_ID, data = train.imputed.b.9, 
                    method = "rpart", 
                    trControl = numFolds, tuneGrid = cpGrid )
model.cv.9
cp.9 <- 0.03

## estimate variable importance
importance.9 <- varImp(model.cv.9)
## summarize importance
print(importance.9)
# Q109244Yes            100.000
# Q115611Yes             91.688
# Q113181Yes             76.752
# Q98197Yes              64.614
# GenderMale             28.058
# Q106272Yes             22.467
# Q98869Yes              10.317
# Q99480Yes               4.703
# Q115899Me               3.791
# Q119851Yes              3.506


## Perform the cross validation with imputed training dataset #10
model.cv.10 <- train(Party ~ . -USER_ID, data = train.imputed.b.10, 
                    method = "rpart", 
                    trControl = numFolds, tuneGrid = cpGrid )
model.cv.10
cp.10 <- 0.02

## estimate variable importance
importance.10 <- varImp(model.cv.10)
## summarize importance
print(importance.10)
# Q109244Yes    100.000
# Q115611Yes     79.498
# Q113181Yes     72.525
# Q98197Yes      62.589
# GenderMale     27.314
# Q98869Yes      12.871
# Q106272Yes      8.164
# Q104996Yes      3.916
# Q105840Yes      3.062


## Perform the cross validation with imputed training dataset #11
model.cv.11 <- train(Party ~ . -USER_ID, data = train.imputed.b.11, 
                    method = "rpart", 
                    trControl = numFolds, tuneGrid = cpGrid )
model.cv.11
cp.11 <- 0.02

## estimate variable importance
importance.11 <- varImp(model.cv.11)
## summarize importance
print(importance.11)
# Q109244Yes            100.000
# Q115611Yes             87.671
# Q113181Yes             76.949
# Q98197Yes              76.818
# GenderMale             30.290
# Q98869Yes               8.767
# Q101163Mom              8.490
# Q99480Yes               4.206
# Q119851Yes              3.772
# Q106272Yes              3.734


## Perform the cross validation with imputed training dataset #12
model.cv.12 <- train(Party ~ . -USER_ID, data = train.imputed.b.12, 
                    method = "rpart", 
                    trControl = numFolds, tuneGrid = cpGrid )
model.cv.12
cp.12 <- 0.02

## estimate variable importance
importance.12 <- varImp(model.cv.12)
## summarize importance
print(importance.12)
# Q109244Yes                            100.000
# Q115611Yes                             73.789
# Q113181Yes                             53.779
# Q98197Yes                              45.995
# GenderMale                             25.362
# Q116881Right                           13.378
# HouseholdStatusMarried (w/kids)         6.373
# Q106272Yes                              4.426
# Q101163Mom                              3.181



# Identify the important variables for all imputed training datasets

# Income -> added because I think it's important
# EducationLevel -> added because I think it's important
# Gender
# HouseholdStatus
# YOB
# Q98197 - Do you pray or meditate on a regular basis? Yes,No
# Q98869 - Does life have a purpose? Yes,No
# Q99480 - Did your parents spank you as a form of discipline/punishment? Yes,No
# Q101163 - Which parent "wore the pants" in your household? Mom,Dad
# Q104996 - Do you brush your teeth two or more times every day? Yes,No
# Q105840 - Do you ever treat yourself to "retail therapy"? Yes,No
# Q106272 - Do you own any power tools? (power saws, drills, etc.) Yes,No
# Q106997 - Do you generally like people, or do most of them tend to get on your
#   nerves pretty easily? Yay people!,Grrr people
# Q108855 - Do you enjoy getting together with your extended family? Yes!,Umm...
# Q109244 - Are you a feminist? Yes,No
# Q110740 - Mac or PC? Mac,PC
# Q113181 - Do you meditate or pray on a regular basis? Yes,No
# Q115611 - Do you personally own a gun? Yes,No
# Q115899 - Would you say most of the hardship in your life has been the result
#   of circumstances beyond your own control, or has it been mostly the result
#   of your own decisions and actions? Circumstances,Me
# Q116881 - Would you rather be happy or right? Happy,Right
# Q116953 - Do you like rules? Yes,No
# Q119851 - Are you in the middle of reading a good book right now? Yes,No
# Q120379 - Do you have (or plan to pursue) a Masters or Doctoral degree? Yes,No
# Q120472 - Science or Art? Science,Art


# CART 
require(rpart)
require(rpart.plot)

## Tree for imputed training dataset 1 with selected features
str(train.imputed.b.1)

# List of dependant variables from the CV experiment with 12 imputed datasets
TreeFS1 <- rpart(Party ~ Income + EducationLevel
                 + Gender + HouseholdStatus + YOB + Q98197 + Q98869
                 + Q99480 + Q101163 + Q104996 + Q105840 + Q106272 + Q106997
                 + Q108855 + Q109244 + Q110740 + Q113181 + Q115611 + Q115899
                 + Q116881 + Q116953 + Q119851 + Q120379 + Q120472, 
                 data = train.imputed.b.1, 
                 method = "class", cp = cp.1)
prp(TreeFS1)
PredTestTreeFS1 <- predict(TreeFS1, newdata = test, type = "class")
head(PredTestTreeFS1)
sum(is.na(PredTestTreeFS1))


## Tree for imputed training dataset 2 with selected features
TreeFS2 <- rpart(Party ~ Income + EducationLevel
                 + Gender + HouseholdStatus + YOB + Q98197 + Q98869
                 + Q99480 + Q101163 + Q104996 + Q105840 + Q106272 + Q106997
                 + Q108855 + Q109244 + Q110740 + Q113181 + Q115611 + Q115899
                 + Q116881 + Q116953 + Q119851 + Q120379 + Q120472, 
                 data = train.imputed.b.2, 
                 method = "class", cp = cp.2)
prp(TreeFS2)
PredTestTreeFS2 <- predict(TreeFS2, newdata = test, type = "class")
head(PredTestTreeFS2)
sum(is.na(PredTestTreeFS2))


## Tree for imputed training dataset 3 with selected features
TreeFS3 <- rpart(Party ~ Income + EducationLevel
                 + Gender + HouseholdStatus + YOB + Q98197 + Q98869
                 + Q99480 + Q101163 + Q104996 + Q105840 + Q106272 + Q106997
                 + Q108855 + Q109244 + Q110740 + Q113181 + Q115611 + Q115899
                 + Q116881 + Q116953 + Q119851 + Q120379 + Q120472, 
                 data = train.imputed.b.3, 
                 method = "class", cp = cp.3)
prp(TreeFS3)
PredTestTreeFS3 <- predict(TreeFS3, newdata = test, type = "class")
head(PredTestTreeFS3)
sum(is.na(PredTestTreeFS3))


## Tree for imputed training dataset 4 with selected features
TreeFS4 <- rpart(Party ~ Income + EducationLevel
                 + Gender + HouseholdStatus + YOB + Q98197 + Q98869
                 + Q99480 + Q101163 + Q104996 + Q105840 + Q106272 + Q106997
                 + Q108855 + Q109244 + Q110740 + Q113181 + Q115611 + Q115899
                 + Q116881 + Q116953 + Q119851 + Q120379 + Q120472, 
                 data = train.imputed.b.4, 
                 method = "class", cp = cp.4)
prp(TreeFS4)
PredTestTreeFS4 <- predict(TreeFS4, newdata = test, type = "class")
head(PredTestTreeFS4)
sum(is.na(PredTestTreeFS4))


## Tree for imputed training dataset 5 with selected features
TreeFS5 <- rpart(Party ~ Income + EducationLevel
                 + Gender + HouseholdStatus + YOB + Q98197 + Q98869
                 + Q99480 + Q101163 + Q104996 + Q105840 + Q106272 + Q106997
                 + Q108855 + Q109244 + Q110740 + Q113181 + Q115611 + Q115899
                 + Q116881 + Q116953 + Q119851 + Q120379 + Q120472, 
                 data = train.imputed.b.5, 
                 method = "class", cp = cp.5)
prp(TreeFS5)
PredTestTreeFS5 <- predict(TreeFS5, newdata = test, type = "class")
head(PredTestTreeFS5)
sum(is.na(PredTestTreeFS5))


## Tree for imputed training dataset 6 with selected features
TreeFS6 <- rpart(Party ~ Income + EducationLevel
                 + Gender + HouseholdStatus + YOB + Q98197 + Q98869
                 + Q99480 + Q101163 + Q104996 + Q105840 + Q106272 + Q106997
                 + Q108855 + Q109244 + Q110740 + Q113181 + Q115611 + Q115899
                 + Q116881 + Q116953 + Q119851 + Q120379 + Q120472, 
                 data = train.imputed.b.6, 
                 method = "class", cp = cp.6)
prp(TreeFS6)
PredTestTreeFS6 <- predict(TreeFS6, newdata = test, type = "class")
head(PredTestTreeFS6)
sum(is.na(PredTestTreeFS6))


## Tree for imputed training dataset 7 with selected features
TreeFS7 <- rpart(Party ~ Income + EducationLevel
                 + Gender + HouseholdStatus + YOB + Q98197 + Q98869
                 + Q99480 + Q101163 + Q104996 + Q105840 + Q106272 + Q106997
                 + Q108855 + Q109244 + Q110740 + Q113181 + Q115611 + Q115899
                 + Q116881 + Q116953 + Q119851 + Q120379 + Q120472, 
                 data = train.imputed.b.7, 
                 method = "class", cp = cp.7)
prp(TreeFS7)
PredTestTreeFS7 <- predict(TreeFS7, newdata = test, type = "class")
head(PredTestTreeFS7)
sum(is.na(PredTestTreeFS7))


## Tree for imputed training dataset 8 with selected features
TreeFS8 <- rpart(Party ~ Income + EducationLevel
                 + Gender + HouseholdStatus + YOB + Q98197 + Q98869
                 + Q99480 + Q101163 + Q104996 + Q105840 + Q106272 + Q106997
                 + Q108855 + Q109244 + Q110740 + Q113181 + Q115611 + Q115899
                 + Q116881 + Q116953 + Q119851 + Q120379 + Q120472, 
                 data = train.imputed.b.8, 
                 method = "class", cp = cp.8)
prp(TreeFS8)
PredTestTreeFS8 <- predict(TreeFS8, newdata = test, type = "class")
head(PredTestTreeFS8)
sum(is.na(PredTestTreeFS8))


## Tree for imputed training dataset 9 with selected features
TreeFS9 <- rpart(Party ~ Income + EducationLevel
                 + Gender + HouseholdStatus + YOB + Q98197 + Q98869
                 + Q99480 + Q101163 + Q104996 + Q105840 + Q106272 + Q106997
                 + Q108855 + Q109244 + Q110740 + Q113181 + Q115611 + Q115899
                 + Q116881 + Q116953 + Q119851 + Q120379 + Q120472, 
                 data = train.imputed.b.9, 
                 method = "class", cp = cp.9)
prp(TreeFS9)
PredTestTreeFS9 <- predict(TreeFS9, newdata = test, type = "class")
head(PredTestTreeFS9)
sum(is.na(PredTestTreeFS9))


## Tree for imputed training dataset 10 with selected features
TreeFS10 <- rpart(Party ~ Income + EducationLevel
                  + Gender + HouseholdStatus + YOB + Q98197 + Q98869
                  + Q99480 + Q101163 + Q104996 + Q105840 + Q106272 + Q106997
                  + Q108855 + Q109244 + Q110740 + Q113181 + Q115611 + Q115899
                  + Q116881 + Q116953 + Q119851 + Q120379 + Q120472, 
                 data = train.imputed.b.10, 
                 method = "class", cp = cp.10)
prp(TreeFS10)
PredTestTreeFS10 <- predict(TreeFS10, newdata = test, type = "class")
head(PredTestTreeFS10)
sum(is.na(PredTestTreeFS10))


## Tree for imputed training dataset 11 with selected features
TreeFS11 <- rpart(Party ~ Income + EducationLevel
                  + Gender + HouseholdStatus + YOB + Q98197 + Q98869
                  + Q99480 + Q101163 + Q104996 + Q105840 + Q106272 + Q106997
                  + Q108855 + Q109244 + Q110740 + Q113181 + Q115611 + Q115899
                  + Q116881 + Q116953 + Q119851 + Q120379 + Q120472, 
                 data = train.imputed.b.11, 
                 method = "class", cp = cp.11)
prp(TreeFS11)
PredTestTreeFS11 <- predict(TreeFS11, newdata = test, type = "class")
head(PredTestTreeFS11)
sum(is.na(PredTestTreeFS11))


## Tree for imputed training dataset 12 with selected features
TreeFS12 <- rpart(Party ~ Income + EducationLevel
                  + Gender + HouseholdStatus + YOB + Q98197 + Q98869
                  + Q99480 + Q101163 + Q104996 + Q105840 + Q106272 + Q106997
                  + Q108855 + Q109244 + Q110740 + Q113181 + Q115611 + Q115899
                  + Q116881 + Q116953 + Q119851 + Q120379 + Q120472, 
                 data = train.imputed.b.12, 
                 method = "class", cp = cp.12)
prp(TreeFS12)
PredTestTreeFS12 <- predict(TreeFS12, newdata = test, type = "class")
head(PredTestTreeFS12)
sum(is.na(PredTestTreeFS12))



# Generate dataset for averaging predictions
avgPred <- data.frame(USER_ID = test$USER_ID, 
                      PredFS1 = PredTestTreeFS1,
                      PredFS2 = PredTestTreeFS2,
                      PredFS3 = PredTestTreeFS3,
                      PredFS4 = PredTestTreeFS4,
                      PredFS5 = PredTestTreeFS5,
                      PredFS6 = PredTestTreeFS6,
                      PredFS7 = PredTestTreeFS7,
                      PredFS8 = PredTestTreeFS8,
                      PredFS9 = PredTestTreeFS9,
                      PredFS10 = PredTestTreeFS10,
                      PredFS11 = PredTestTreeFS11,
                      PredFS12 = PredTestTreeFS12)
str(avgPred)
summary(avgPred)

avgPred$average <- ((as.numeric(avgPred$PredFS1) - 1) + 
                        (as.numeric(avgPred$PredFS2) - 1) + 
                        (as.numeric(avgPred$PredFS3) - 1) + 
                        (as.numeric(avgPred$PredFS4) - 1) + 
                        (as.numeric(avgPred$PredFS5) - 1) + 
                        (as.numeric(avgPred$PredFS6) - 1) +
                        (as.numeric(avgPred$PredFS7) - 1) + 
                        (as.numeric(avgPred$PredFS8) - 1) + 
                        (as.numeric(avgPred$PredFS9) - 1) + 
                        (as.numeric(avgPred$PredFS10) - 1) + 
                        (as.numeric(avgPred$PredFS11) - 1) + 
                        (as.numeric(avgPred$PredFS12) - 1)) / 12
head(avgPred)
table(avgPred$average)

## If average is between 0 and 0.5, the average prediction is "Democrat"
## If average is between 0.5 and 1, the average prediction is "Republican"

avgThreshold <- 0.5
avgPred$averagePrediction <- 
    as.factor(ifelse(avgPred$average <= avgThreshold, "Democrat", "Republican"))
head(avgPred)

## Check how different are the averaged prediction with the imputed dataset #1 
nrow(subset(avgPred, PredFS1 != averagePrediction))
## => 95 rows are different


# Save Output dataset
submission.20160613.6 <- 
    data.frame(USER_ID = avgPred$USER_ID, 
               Predictions = avgPred$averagePrediction)
head(submission.20160613.6)
sum(is.na(submission.20160613.6))
str(submission.20160613.6)
summary(submission.20160613.6)

write.csv(submission.20160613.6, "./dataout/submission-20160613-6.csv", 
          row.names = FALSE, quote = FALSE)
