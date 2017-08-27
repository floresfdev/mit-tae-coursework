# Load the datasets

## Load columns as NA if the field is blank or NA
train <- read.csv("./data/train2016.csv", na.strings = c("","NA"))
test <- read.csv("./data/test2016.csv", na.strings = c("","NA"))


# Imputation of NAs in the training dataset

## See Recitation of Unit 3 to use the mice package
require(mice)
subset(train, select = -c(USER_ID, Party))[1, ]
trainDependant <- subset(train, select = -c(USER_ID, Party))
set.seed(1357)
## CAREFUL! MICE takes a lot of time
#trainImputed <- complete(mice(trainDependant))
colnames(trainImputed)[colSums(is.na(trainImputed)) > 0]


## Trying with Amelia package
install.packages("Amelia")
require(Amelia)
## See http://fastml.com/impute-missing-values-with-amelia/
## See http://gking.harvard.edu/amelia/

?amelia
nominalCols <- c("Gender", "HouseholdStatus", "EducationLevel")
ordinalCols <- 
    c("YOB", "Income", "Q124742", "Q124122", "Q123464", "Q123621", 
      "Q122769", "Q122770", "Q122771", "Q122120", "Q121699", "Q121700", 
      "Q120978", "Q121011", "Q120379", "Q120650", "Q120472", "Q120194", 
      "Q120012", "Q120014", "Q119334", "Q119851", "Q119650", "Q118892", 
      "Q118117", "Q118232", "Q118233", "Q118237", "Q117186", "Q117193", 
      "Q116797", "Q116881", "Q116953", "Q116601", "Q116441", "Q116448", 
      "Q116197", "Q115602", "Q115777", "Q115610", "Q115611", "Q115899", 
      "Q115390", "Q114961", "Q114748", "Q115195", "Q114517", "Q114386", 
      "Q113992", "Q114152", "Q113583", "Q113584", "Q113181", "Q112478", 
      "Q112512", "Q112270", "Q111848", "Q111580", "Q111220", "Q110740", 
      "Q109367", "Q108950", "Q109244", "Q108855", "Q108617", "Q108856", 
      "Q108754", "Q108342", "Q108343", "Q107869", "Q107491", "Q106993", 
      "Q106997", "Q106272", "Q106388", "Q106389", "Q106042", "Q105840", 
      "Q105655", "Q104996", "Q103293", "Q102906", "Q102674", "Q102687", 
      "Q102289", "Q102089", "Q101162", "Q101163", "Q101596", "Q100689", 
      "Q100680", "Q100562", "Q99982", "Q100010", "Q99716", "Q99581", "Q99480", 
      "Q98869", "Q98578", "Q98059", "Q98078", "Q98197", "Q96024")
idCols <- c("USER_ID", "Party")
numCPUs <- 2
m <- numCPUs * 3
amelia.out <- amelia(train, 
                     noms = nominalCols, ords = ordinalCols, idvars = idCols,
                     m = m, parallel = "multicore", ncpus = numCPUs)

write.amelia(amelia.out, file.stem = "./data/train_imputed_")

## Missing map
missmap(amelia.out)



# Imputation with Amelia (train + test) for 12 output datasets

## Row bind of training and testing sets
colnames(train)
trainWithoutParty <- train
trainWithoutParty$Party <- NULL
str(trainWithoutParty)
str(test)
5568 + 1392 ## 6960 for the train+test dataset
trainAndTest <- rbind(trainWithoutParty, test)
str(trainAndTest)


require(Amelia)
## See http://fastml.com/impute-missing-values-with-amelia/
## See http://gking.harvard.edu/amelia/

nominalCols <- c("Gender", "HouseholdStatus", "EducationLevel")
ordinalCols <- 
    c("YOB", "Income", "Q124742", "Q124122", "Q123464", "Q123621", 
      "Q122769", "Q122770", "Q122771", "Q122120", "Q121699", "Q121700", 
      "Q120978", "Q121011", "Q120379", "Q120650", "Q120472", "Q120194", 
      "Q120012", "Q120014", "Q119334", "Q119851", "Q119650", "Q118892", 
      "Q118117", "Q118232", "Q118233", "Q118237", "Q117186", "Q117193", 
      "Q116797", "Q116881", "Q116953", "Q116601", "Q116441", "Q116448", 
      "Q116197", "Q115602", "Q115777", "Q115610", "Q115611", "Q115899", 
      "Q115390", "Q114961", "Q114748", "Q115195", "Q114517", "Q114386", 
      "Q113992", "Q114152", "Q113583", "Q113584", "Q113181", "Q112478", 
      "Q112512", "Q112270", "Q111848", "Q111580", "Q111220", "Q110740", 
      "Q109367", "Q108950", "Q109244", "Q108855", "Q108617", "Q108856", 
      "Q108754", "Q108342", "Q108343", "Q107869", "Q107491", "Q106993", 
      "Q106997", "Q106272", "Q106388", "Q106389", "Q106042", "Q105840", 
      "Q105655", "Q104996", "Q103293", "Q102906", "Q102674", "Q102687", 
      "Q102289", "Q102089", "Q101162", "Q101163", "Q101596", "Q100689", 
      "Q100680", "Q100562", "Q99982", "Q100010", "Q99716", "Q99581", "Q99480", 
      "Q98869", "Q98578", "Q98059", "Q98078", "Q98197", "Q96024")
idCols <- c("USER_ID")
numCPUs <- 2
m <- numCPUs * 6
amelia.out.2 <- amelia(trainAndTest, 
                       noms = nominalCols, ords = ordinalCols, idvars = idCols,
                       m = m, parallel = "multicore", ncpus = numCPUs)

write.amelia(amelia.out.2, file.stem = "./data/trainAndTest_imputed_")

## Missing map
missmap(amelia.out.2)


## Extract only training imputed datasets

### Dataset N (generic code)
datasetNumber <- 12
pathTrainAndTest <- 
    paste0("./data/trainAndTest_imputed_", datasetNumber, ".csv")
pathTrain <- 
    paste0("./data/train_imputed_b_", datasetNumber, ".csv")

trainAndTest.imputed <- 
    read.csv(pathTrainAndTest, na.strings = c("","NA"))
##str(trainAndTest.imputed)
sum(is.na(trainAndTest.imputed))
# Remove column "X"
trainAndTest.imputed <- trainAndTest.imputed[, -1]
# Filter only the first 5568 rows, corresponding to the training dataset
train.imputed.b <- trainAndTest.imputed[1:5568, ]
##str(train.imputed.b)
# Control the sum of IDs of the extracted and original train datasets
sum(train.imputed.b$USER_ID)
sum(train$USER_ID)
# Control the order of the IDs
##head(train.imputed.b$USER_ID)
##head(train$USER_ID)
##tail(train.imputed.b$USER_ID)
##tail(train$USER_ID)
# Add Party column again
train.imputed.b$Party <- train$Party
# Write dataset
write.csv(train.imputed.b, pathTrain, row.names = FALSE, quote = TRUE)



## Extract only testing imputed datasets

### Dataset N (generic code)
datasetNumber <- 12
pathTrainAndTest <- 
    paste0("./data/trainAndTest_imputed_", datasetNumber, ".csv")
pathTrain <- 
    paste0("./data/test_imputed_b_", datasetNumber, ".csv")

trainAndTest.imputed <- 
    read.csv(pathTrainAndTest, na.strings = c("","NA"))
##str(trainAndTest.imputed)
sum(is.na(trainAndTest.imputed))
# Remove column "X"
trainAndTest.imputed <- trainAndTest.imputed[, -1]
# Filter only the last 1392 rows, corresponding to the testing dataset
test.imputed.b <- trainAndTest.imputed[5569:6960, ]
##str(test.imputed.b)
# Control the sum of IDs of the extracted and original testing datasets
sum(test.imputed.b$USER_ID)
sum(test$USER_ID)
# Control the order of the IDs
##head(test.imputed.b$USER_ID)
##head(test$USER_ID)
##tail(test.imputed.b$USER_ID)
##tail(test$USER_ID)
# Write dataset
write.csv(test.imputed.b, pathTrain, row.names = FALSE, quote = TRUE)
