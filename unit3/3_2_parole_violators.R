# PREDICTING PAROLE VIOLATORS


# Problem 1.1 - Loading the Dataset
# Load the dataset parole.csv into a data frame called parole, and investigate 
# it using the str() and summary() functions.
parole <- read.csv("./data_unit3_2/parole.csv")
str(parole)
summary(parole)
# How many parolees are contained in the dataset?
## 675


# Problem 1.2 - Loading the Dataset
# How many of the parolees in the dataset violated the terms of their parole?
table(parole$violator)
## 78


# Problem 2.1 - Preparing the Dataset
# You should be familiar with unordered factors (if not, review the Week 2
# homework problem "Reading Test Scores"). Which variables in this dataset are
# unordered factors with at least three levels? Select all that apply.
## state, crime


# Problem 2.2 - Preparing the Dataset
# In the last subproblem, we identified variables that are unordered factors
# with at least 3 levels, so we need to convert them to factors for our
# prediction problem (we introduced this idea in the "Reading Test Scores"
# problem last week). Using the as.factor() function, convert these variables to
# factors. Keep in mind that we are not changing the values, just the way R
# understands them (the values are still numbers).
parole$state <- as.factor(parole$state)
parole$crime <- as.factor(parole$crime)
# How does the output of summary() change for a factor variable as compared to
# a numerical variable?
summary(parole)
## The output becomes similar to that of the table() function applied to that
## variable


# Problem 3.1 - Splitting into a Training and Testing Set
# To ensure consistent training/testing set splits, run the following 5 lines of
# code (do not include the line numbers at the beginning):
set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)
# Roughly what proportion of parolees have been allocated to the training and
# testing sets?
str(train)
473 / 675  ## 0.7007407
str(test)
202 / 675  ## 0.2992593
## 70% to the training set, 30% to the testing set


# Problem 3.2 - Splitting into a Training and Testing Set
# Now, suppose you re-ran lines [1]-[5] of Problem 3.1. What would you expect?
?sample.split
## The exact same training/testing set split as the first execution of [1]-[5]

# If you instead ONLY re-ran lines [3]-[5], what would you expect?
## A different training/testing set split from the first execution of [1]-[5]

# If you instead called set.seed() with a different number and then re-ran lines
# [3]-[5] of Problem 3.1, what would you expect?
## A different training/testing set split from the first execution of [1]-[5]


# Problem 4.1 - Building a Logistic Regression Model
# If you tested other training/testing set splits in the previous section, 
# please re-run the original 5 lines of code to obtain the original split.
# Using glm (and remembering the parameter family="binomial"), train a logistic
# regression model on the training set. Your dependent variable is "violator",
# and you should use all of the other variables as independent variables.
# What variables are significant in this model? Significant variables should 
# have a least one star, or should have a probability less than 0.05 (the column
# Pr(>|z|) in the summary output). Select all that apply.
ParoleViolatorLog <- glm(violator ~ ., data = train, family = binomial)
summary(ParoleViolatorLog)
## race, state4, multiple.offenses


# Problem 4.2 - Building a Logistic Regression Model
# What can we say based on the coefficient of the multiple.offenses variable?
# The following two properties might be useful to you when answering this
# question:
# 1) If we have a coefficient c for a variable, then that means the log odds (or
# Logit) are increased by c for a unit increase in the variable.
# 2) If we have a coefficient c for a variable, then that means the odds are
# multiplied by e^c for a unit increase in the variable.
exp(1.6119919) ## 5.012786
## Our model predicts that a parolee who committed multiple offenses has 5.01
## times higher odds of being a violator than a parolee who did not commit
## multiple offenses but is otherwise identical.


# Problem 4.3 - Building a Logistic Regression Model
# Consider a parolee who is male, of white race, aged 50 years at prison
# release, from the state of Maryland, served 3 months, had a maximum sentence
# of 12 months, did not commit multiple offenses, and committed a larceny.
# Answer the following questions based on the model's predictions for this
# individual. (HINT: You should use the coefficients of your model, the Logistic
# Response Function, and the Odds equation to solve this problem.)
# According to the model, what are the odds this individual is a violator?
exp(-4.2411574 + # intercept
        0.3869904 * 1 + # male
        0.8867192 * 1 + # white race
        -0.0001756 * 50 + # aged 50
        0.4433007*0 + 0.8349797*0 + -3.3967878*0 + # Maryland
        -0.1238867 * 3 + # served 3 months
        0.0802954 * 12 + # max sentence of 12 months
        1.6119919 * 0 + # did not commit multiple offenses
        0.6837143*1 + -0.2781054*0 + -0.0117627*0
)
## 0.1825687

# According to the model, what is the probability this individual is a violator?
1 / (1 + exp(-1 * (-4.2411574 + # intercept
                       0.3869904 * 1 + # male
                       0.8867192 * 1 + # white race
                       -0.0001756 * 50 + # aged 50
                       0.4433007*0 + 0.8349797*0 + -3.3967878*0 + # Maryland
                       -0.1238867 * 3 + # served 3 months
                       0.0802954 * 12 + # max sentence of 12 months
                       1.6119919 * 0 + # did not commit multiple offenses
                       0.6837143*1 + -0.2781054*0 + -0.0117627*0
                   )))
## Logistic Response Function -> P(y = 1) = 0.1543832


# Problem 5.1 - Evaluating the Model on the Testing Set
# Use the predict() function to obtain the model's predicted probabilities for
# parolees in the testing set, remembering to pass type="response".
ParolePredTest <- predict(ParoleViolatorLog, type = "response", newdata = test)
# What is the maximum predicted probability of a violation?
max(ParolePredTest)
## 0.9072791


# Problem 5.2 - Evaluating the Model on the Testing Set
# In the following questions, evaluate the model's predictions on the test set
# using a threshold of 0.5.
table(test$violator, ParolePredTest > 0.5)
# What is the model's sensitivity?
12 / (11 + 12) # TP / (TP + FN)
## 0.5217391

# What is the model's specificity?
167 / (167 + 12) # TN / (TN + FP)
## 0.9329609

# What is the model's accuracy?
(167 + 12) / nrow(test) # (TN + TP) / N
## 0.8861386


# Problem 5.3 - Evaluating the Model on the Testing Set
# What is the accuracy of a simple model that predicts that every parolee is a
# non-violator?
table(test$violator)
179 / (179 + 23)
## 0.8861386


# Problem 5.4 - Evaluating the Model on the Testing Set
# Consider a parole board using the model to predict whether parolees will be
# violators or not. The job of a parole board is to make sure that a prisoner is
# ready to be released into free society, and therefore parole boards tend to be
# particularily concerned about releasing prisoners who will violate their 
# parole. Which of the following most likely describes their preferences and
# best course of action?
## The board assigns more cost to a false negative than a false positive, and
## should therefore use a logistic regression cutoff less than 0.5.


# Problem 5.5 - Evaluating the Model on the Testing Set
# Which of the following is the most accurate assessment of the value of the
# logistic regression model with a cutoff 0.5 to a parole board, based on the
# model's accuracy as compared to the simple baseline model?
## The model is likely of value to the board, and using a different logistic
## regression cutoff is likely to improve the model's value. 


# Problem 5.6 - Evaluating the Model on the Testing Set
# Using the ROCR package, what is the AUC value for the model?
library(ROCR)
ROCRpred = prediction(ParolePredTest, test$violator)
as.numeric(performance(ROCRpred, "auc")@y.values)
## 0.8945834


# Problem 5.7 - Evaluating the Model on the Testing Set
# Describe the meaning of AUC in this context.
## The probability the model can correctly differentiate between a randomly 
## selected parole violator and a randomly selected parole non-violator.


# Problem 6.1 - Identifying Bias in Observational Data
# Our goal has been to predict the outcome of a parole decision, and we used a
# publicly available dataset of parole releases for predictions. In this final
# problem, we'll evaluate a potential source of bias associated with our
# analysis. It is always important to evaluate a dataset for possible sources of
# bias.
# The dataset contains all individuals released from parole in 2004, either due
# to completing their parole term or violating the terms of their parole. 
# However, it does not contain parolees who neither violated their parole nor
# completed their term in 2004, causing non-violators to be underrepresented. 
# This is called "selection bias" or "selecting on the dependent variable," 
# because only a subset of all relevant parolees were included in our analysis, 
# based on our dependent variable in this analysis (parole violation). How 
# could we improve our dataset to best address selection bias?
## We should use a dataset tracking a group of parolees from the start of their
## parole until either they violated parole or they completed their term.
