# CLIMATE CHANGE

climate <- read.csv("./data_unit2_1/climate_change.csv")
str(climate)
summary(climate)


# Problem 1.1 - Creating Our First Model
## We are interested in how changes in these variables affect future 
## temperatures, as well as how well these variables explain temperature changes 
## so far. To do this, first read the dataset climate_change.csv into R.

## Then, split the data into a training set, consisting of all the observations 
## up to and including 2006, and a testing set consisting of the remaining years
## (hint: use subset). A training set refers to the data that will be used to 
## build the model (this is the data we give to the lm() function), and a 
## testing set refers to the data we will use to test our predictive ability.
climate_train <- subset(climate, Year <= 2006)
climate_test <- subset(climate, Year > 2006)
str(climate_train)
summary(climate_train)
str(climate_test)
summary(climate_test)

## Next, build a linear regression model to predict the dependent variable Temp, 
## using MEI, CO2, CH4, N2O, CFC.11, CFC.12, TSI, and Aerosols as independent 
## variables (Year and Month should NOT be used in the model). Use the training 
## set to build the model.
fit.climate <- 
    lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, 
       data = climate_train)

## Enter the model R2 (the "Multiple R-squared" value):
summary(fit.climate)
## 0.7509


# Problem 1.2 - Creating Our First Model
## Which variables are significant in the model? We will consider a variable 
## signficant only if the p-value is below 0.05. (Select all that apply.) 
## MEI, CO2, CFC.11, CFC.12, TSI, Aerosols


# Problem 2.1 - Understanding the Model
## Current scientific opinion is that nitrous oxide and CFC-11 are greenhouse 
## gases: gases that are able to trap heat from the sun and contribute to the 
## heating of the Earth. However, the regression coefficients of both the N2O 
## and CFC-11 variables are negative, indicating that increasing atmospheric 
## concentrations of either of these two compounds is associated with lower 
## global temperatures.

## Which of the following is the simplest correct explanation for this 
## contradiction?
## All of the gas concentration variables reflect human development - N2O and 
## CFC.11 are correlated with other variables in the data set.


# Problem 2.2 - Understanding the Model
## Compute the correlations between all the variables in the training set. 
cor(climate_train)

## Which of the following independent variables is N2O highly correlated with 
## (absolute correlation greater than 0.7)? Select all that apply.
## CO2, CH4, CFC.12

## Which of the following independent variables is CFC.11 highly correlated 
## with? Select all that apply.
## CH4, CFC.12


# Problem 3 - Simplifying the Model
## Given that the correlations are so high, let us focus on the N2O variable and
## build a model with only MEI, TSI, Aerosols and N2O as independent variables. 
## Remember to use the training set to build the model.
fit.climate.2 <- 
    lm(Temp ~ MEI + N2O + TSI + Aerosols, 
       data = climate_train)

## Enter the coefficient of N2O in this reduced model:
summary(fit.climate.2)
## 2.532e-02
    
## (How does this compare to the coefficient in the previous model with all of the variables?)
## Enter the model R2:
## 0.7261


# Problem 4 - Automatically Building the Model
## We have many variables in this problem, and as we have seen above, dropping 
## some from the model does not decrease model quality. R provides a function, 
## step, that will automate the procedure of trying different combinations of 
## variables to find a good compromise of model simplicity and R2. This 
## trade-off is formalized by the Akaike information criterion (AIC) - it can be
## informally thought of as the quality of the model with a penalty for the 
## number of variables in the model.

## The step function has one argument - the name of the initial model. It 
## returns a simplified model. Use the step function in R to derive a new model,
## with the full model as the initial model (HINT: If your initial full model 
## was called "climateLM", you could create a new model with the step function 
## by typing step(climateLM). Be sure to save your new model to a variable name 
## so that you can look at the summary. For more information about the step 
## function, type ?step in your R console.)
fit.climate.step <- step(fit.climate)
summary(fit.climate.step)

## Enter the R2 value of the model produced by the step function:
## 0.7508
    
## Which of the following variable(s) were eliminated from the full model by the
## step function? Select all that apply.
## 

## It is interesting to note that the step function does not address the 
## collinearity of the variables, except that adding highly correlated variables
## will not improve the R2 significantly. The consequence of this is that the 
## step function will not necessarily produce a very interpretable model - just 
## a model that has balanced quality and simplicity for a particular weighting 
## of quality and simplicity (AIC).


# Problem 5 - Testing on Unseen Data
## We have developed an understanding of how well we can fit a linear regression
## to the training data, but does the model quality hold when applied to unseen 
## data?

## Using the model produced from the step function, calculate temperature 
## predictions for the testing data set, using the predict function.
TempPredictions <- predict(fit.climate.step, newdata = climate_test)

## Enter the testing set R2:
climate.SSE = sum((TempPredictions - climate_test$Temp)^2)
climate.SST = sum((climate_test$Temp - mean(climate_train$Temp))^2)
1 - climate.SSE/climate.SST
## 0.6286051