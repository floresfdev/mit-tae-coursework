# UNDERSTANDING WHY PEOPLE VOTE

# Problem 1.1 - Exploration and Logistic Regression
# We will first get familiar with the data. Load the CSV file gerber.csv into R.
# What proportion of people in this dataset voted in this election?
gerber <- read.csv("./data_unit4_1/gerber.csv")
str(gerber)
table(gerber$voting)
108696 / (235388 + 108696)
## 0.3158996


# Problem 1.2 - Exploration and Logistic Regression
# Which of the four "treatment groups" had the largest percentage of people who
# actually voted (voting = 1)?
table(gerber$voting, gerber$hawthorne)
12316 / (25888 + 12316)
## 0.3223746
table(gerber$voting, gerber$civicduty)
12021 / (26197 + 12021)
## 0.3145377
table(gerber$voting, gerber$neighbors)
14438 / (23763 + 14438)
## 0.3779482
table(gerber$voting, gerber$self)
13191 / (25027 + 13191)
## 0.3451515

## Neighbors Neighbors - correct


# Problem 1.3 - Exploration and Logistic Regression
# Build a logistic regression model for voting using the four treatment group 
# variables as the independent variables (civicduty, hawthorne, self, and 
# neighbors). Use all the data to build the model (DO NOT split the data into a
# training set and testing set). Which of the following coefficients are 
# significant in the logistic regression model? Select all that apply.
VotingLog <- glm(voting ~ civicduty + hawthorne + self + neighbors, 
                 data = gerber, family = binomial)
summary(VotingLog)
## all


# Problem 1.4 - Exploration and Logistic Regression
# Using a threshold of 0.3, what is the accuracy of the logistic regression 
# model? (When making predictions, you don't need to use the newdata argument 
# since we didn't split our data.)
predictVoting <- predict(VotingLog, type = "response")
table(gerber$voting, predictVoting > 0.3)
(134513 + 51966) / nrow(gerber)
## 0.5419578


# Problem 1.5 - Exploration and Logistic Regression
# Using a threshold of 0.5, what is the accuracy of the logistic regression 
# model?
table(gerber$voting, predictVoting > 0.5)
(235388) / nrow(gerber)
## 0.6841004


# Problem 1.6 - Exploration and Logistic Regression
# Compare your previous two answers to the percentage of people who did not vote
# (the baseline accuracy) and compute the AUC of the model. What is happening 
# here?
table(gerber$voting)
235388 / (235388 + 108696)
## 0.6841004 => equal to accuracy of threshold of 0.5
library(ROCR)
ROCRpred = prediction(predictVoting, gerber$voting)
as.numeric(performance(ROCRpred, "auc")@y.values)
## 0.5308461

## Even though all of the variables are significant, this is a weak predictive 
## model.


# Problem 2.1 - Trees
# We will now try out trees. Build a CART tree for voting using all data and the
# same four treatment variables we used before. Don't set the option 
# method="class" - we are actually going to create a regression tree here. We 
# are interested in building a tree to explore the fraction of people who vote, 
# or the probability of voting. We'd like CART to split our groups if they have
# different probabilities of voting. If we used method='class', CART would only
# split if one of the groups had a probability of voting above 50% and the other
# had a probability of voting less than 50% (since the predicted outcomes would
# be different). However, with regression trees, CART will split even if both
# groups have probability less than 50%.
# Leave all the parameters at their default values. You can use the following
# command in R to build the tree:
library(rpart)
library(rpart.plot)
CARTmodel <- 
    rpart(voting ~ civicduty + hawthorne + self + neighbors, data = gerber)
# Plot the tree. What happens, and if relevant, why?
prp(CARTmodel)
## No variables are used (the tree is only a root node) - none of the variables 
## make a big enough effect to be split on.


# Problem 2.2 - Trees
# Now build the tree using the command:
CARTmodel2 <- 
    rpart(voting ~ civicduty + hawthorne + self + neighbors, 
          data=gerber, cp=0.0)
# to force the complete tree to be built. Then plot the tree. What do you 
# observe about the order of the splits?
prp(CARTmodel2)
## Neighbor is the first split, civic duty is the last.


# Problem 2.3 - Trees
# Using only the CART tree plot, determine what fraction (a number between 0 
# and 1) of "Civic Duty" people voted:
## 0.31


# Problem 2.4 - Trees
# Make a new tree that includes the "sex" variable, again with cp = 0.0. Notice 
# that sex appears as a split that is of secondary importance to the treatment 
# group.
CARTmodel3 <- 
    rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, 
          data=gerber, cp=0.0)
prp(CARTmodel3)
# In the control group, which gender is more likely to vote?
## Men (0) 
# In the "Civic Duty" group, which gender is more likely to vote?
## Men (0)


# Problem 3.1 - Interaction Terms
# We know trees can handle "nonlinear" relationships, e.g. "in the 'Civic Duty' 
# group and female", but as we will see in the next few questions, it is 
# possible to do the same for logistic regression. First, let's explore what 
# trees can tell us some more.
# Let's just focus on the "Control" treatment group. Create a regression tree 
# using just the "control" variable, then create another tree with the "control"
# and "sex" variables, both with cp=0.0.
CARTcontrol <- rpart(voting ~ control, data = gerber, cp = 0.0)
CARTcontrolAndSex <- rpart(voting ~ control + sex, data = gerber, cp = 0.0)
# In the "control" only tree, what is the absolute value of the difference in 
# the predicted probability of voting between being in the control group versus
# being in a different group? You can use the absolute value function to get
# answer, i.e. abs(Control Prediction - Non-Control Prediction). Add the 
# argument "digits = 6" to the prp command to get a more accurate estimate.
prp(CARTcontrol, digits = 6)
abs(0.296638 - 0.34)
## 0.043362


# Problem 3.2 - Interaction Terms
# Now, using the second tree (with control and sex), determine who is affected
# more by NOT being in the control group (being in any of the four treatment 
# groups):
prp(CARTcontrolAndSex, digits = 6)
## They are affected about the same (change in probability within 0.001 of each
## other).


# Problem 3.3 - Interaction Terms
# Going back to logistic regression now, create a model using "sex" and
# "control". Interpret the coefficient for "sex":
VotingControlAndSexLog <- 
    glm(voting ~ control + sex, data = gerber, family = binomial)
summary(VotingControlAndSexLog)
## Coefficient is negative, reflecting that women are less likely to vote


# Problem 3.4 - Interaction Terms
# The regression tree calculated the percentage voting exactly for every one of
# the four possibilities (Man, Not Control), (Man, Control), (Woman, Not 
# Control), (Woman, Control). Logistic regression has attempted to do the same, 
# although it wasn't able to do as well because it can't consider exactly the 
# joint possibility of being a women and in the control group.
# We can quantify this precisely. Create the following dataframe (this contains
# all of the possible values of sex and control), and evaluate your logistic 
# regression using the predict function (where "LogModelSex" is the name of 
# your logistic regression model that uses both control and sex):
Possibilities <- data.frame(sex=c(0,0,1,1), control=c(0,1,0,1))
predict(VotingControlAndSexLog, newdata=Possibilities, type="response")
# The four values in the results correspond to the four possibilities in the 
# order they are stated above ( (Man, Not Control), (Man, Control), (Woman, Not
# Control), (Woman, Control) ). What is the absolute difference between the tree
# and the logistic regression for the (Woman, Control) case? Give an answer with
# five numbers after the decimal point.
abs(0.290456 - 0.2908065)
## 0.0003505 
round(0.0003505, digits = 5)
## 0.00035


# Problem 3.5 - Interaction Terms
# So the difference is not too big for this dataset, but it is there. We're 
# going to add a new term to our logistic regression now, that is the 
# combination of the "sex" and "control" variables - so if this new variable is 
# 1, that means the person is a woman AND in the control group. We can do that 
# with the following command:
LogModel2 <- glm(voting ~ sex + control + sex:control, 
                 data = gerber, family = "binomial")
# How do you interpret the coefficient for the new variable in isolation? That 
# is, how does it relate to the dependent variable?
summary(LogModel2)
## If a person is a woman and in the control group, the chance that she voted 
## goes down.


# Problem 3.6 - Interaction Terms
# Run the same code as before to calculate the average for each group:
predict(LogModel2, newdata=Possibilities, type="response")
# Now what is the difference between the logistic regression model and the CART
# model for the (Woman, Control) case? Again, give your answer with five numbers
# after the decimal point.
abs(0.290456 - 0.2904558)
## 2e-07
round(2e-07, digits = 5)


# Problem 3.7 - Interaction Terms
# This example has shown that trees can capture nonlinear relationships that 
# logistic regression can not, but that we can get around this sometimes by 
# using variables that are the combination of two variables. Should we always
# include all possible interaction terms of the independent variables when 
# building a logistic regression model?
## No (because of overfitting)