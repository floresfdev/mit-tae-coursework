# KAGGLE COMPETITION - GETTING STARTED

# This script file is intended to help you get started on the Kaggle platform, 
# and to show you how to make a submission to the competition.


# Let's start by reading the data into R
# Make sure you have downloaded these files from the Kaggle website, and have 
# navigated to the directory where you saved the files on your computer

train = read.csv("./data/train2016.csv")

test = read.csv("./data/test2016.csv")

# We will just create a simple logistic regression model, to predict Party using
# all other variables in the dataset, except for the user ID:

SimpleMod = glm(Party ~ . -USER_ID, data=train, family=binomial)

# And then make predictions on the test set:

PredTest = predict(SimpleMod, newdata=test, type="response")

threshold = 0.5

PredTestLabels = as.factor(ifelse(PredTest<threshold, "Democrat", "Republican"))

# However, you can submit the file on Kaggle to see how well the model performs.
# You can make up to 5 submissions per day, so don't hesitate to just upload a 
# solution to see how you did.

# Let's prepare a submission file for Kaggle (for more about this, see the 
# "Evaluation" page on the competition site):

## From Discussion forum: 
## https://courses.edx.org/courses/course-v1:MITx+15.071x_3+1T2016/ ...
## discussion/forum/9214edfd8fe8a5cd00c2f66fdc149e0723eec3bb/threads/ ... 
## 574470803ba2a9055900021f
##   Note: there is an error in the sample code file. Change PREDICTION to 
##   Predictions so it looks like this....

MySubmission = data.frame(USER_ID = test$USER_ID, Predictions = PredTestLabels)

write.csv(MySubmission, "./dataout/SubmissionSimpleLog.csv", 
          row.names = FALSE, quote = FALSE)

# You should upload the submission "SubmissionSimpleLog.csv" on the Kaggle 
# website to use this as a submission to the competition

# This model was just designed to help you get started - to do well in the 
# competition, you will need to build better models!