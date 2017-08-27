# AN ANALYTICAL DETECTIVE

# Problem 1 - Loading the data

mvt <- read.csv("./data_unit1_1/mvtWeek1.csv")
str(mvt)
summary(mvt)

## Q.1.1: How many rows of data (observations) are in this dataset?
## 191641

## Q.1.2: How many variables are in this dataset?
## 11

## Q.1.3: Using the "max" function, what is the maximum value of the 
## variable "ID"?
max(mvt$ID)
## 9181151

## Q.1.4: What is the minimum value of the variable "Beat"?
min(mvt$Beat)
## 111

## Q.1.5: How many observations have value TRUE in the Arrest variable 
## (this is the number of crimes for which an arrest was made)?
## 15536

## Q.1.6: How many observations have a LocationDescription value of ALLEY?
## 2308


# Problem 2.1 - Understanding Dates in R

## In many datasets, like this one, you have a date field. Unfortunately, 
## R does not automatically recognize entries that look like dates. We need 
## to use a function in R to extract the date and time. Take a look at the 
## first entry of Date (remember to use square brackets when looking at a 
## certain entry of a variable).
## Q 2.1: In what format are the entries in the variable Date?
mvt$Date[1]
## 12/31/12 23:15 => 


# Problem 2.2 - Understanding Dates in R

## Now, let's convert these characters into a Date object in R. In your R 
## console, type
DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
## This converts the variable "Date" into a Date object in R. Take a look at 
## the variable DateConvert using the summary function.
summary(DateConvert)
## Q.2.2: What is the month and year of the median date in our dataset? Enter 
## your answer as "Month Year", without the quotes. (Ex: if the answer was 
## 2008-03-28, you would give the answer "March 2008", without the quotes.)
## "2006-05-21" => May 2006


# Problem 2.3 - Understanding Dates in R

## Now, let's extract the month and the day of the week, and add these 
## variables to our data frame mvt. We can do this with two simple functions. 
## Type the following commands in R:
mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)
str(mvt)
## This creates two new variables in our data frame, Month and Weekday, and 
## sets them equal to the month and weekday values that we can extract from 
## the Date object. Lastly, replace the old Date variable with DateConvert by 
## typing:
mvt$Date = DateConvert
## Using the table command, answer the following questions.
## Q.2.3: In which month did the fewest motor vehicle thefts occur?
table(mvt$Month)
## February = 13511


# Problem 2.4 - Understanding Dates in R

## Q.2.4: On which weekday did the most motor vehicle thefts occur?
table(mvt$Weekday)
## Friday = 29284


# Problem 2.5 - Understanding Dates in R

## Each observation in the dataset represents a motor vehicle theft, and the 
## Arrest variable indicates whether an arrest was later made for this theft. 
## Q.2.5: Which month has the largest number of motor vehicle thefts for which 
## an arrest was made?
str(mvt)
mvtArrest <- subset(mvt, Arrest == TRUE)
table(mvtArrest$Month)
## January = 1435


# Problem 3.1 - Visualizing Crime Trends

## Now, let's make some plots to help us better understand how crime has 
## changed over time in Chicago. Throughout this problem, and in general, you 
## can save your plot to a file. For more information, this website very 
## clearly explains the process.
## First, let's make a histogram of the variable Date. We'll add an extra 
## argument, to specify the number of bars we want in our histogram. In your 
## R console, type
hist(mvt$Date, breaks=100)
## Looking at the histogram, answer the following questions.

## Q.3.1: 
## In general, does it look like crime increases or decreases from 2002 - 2012?
## Decreases

## In general, does it look like crime increases or decreases from 2005 - 2008?
## Decreases

## In general, does it look like crime increases or decreases from 2009 - 2011?
## Increases


# Problem 3.2 - Visualizing Crime Trends
## Now, let's see how arrests have changed over time. Create a boxplot of the 
## variable "Date", sorted by the variable "Arrest" (if you are not familiar 
## with boxplots and would like to learn more, check out this tutorial). In a 
## boxplot, the bold horizontal line is the median value of the data, the box 
## shows the range of values between the first quartile and third quartile, 
## and the whiskers (the dotted lines extending outside the box) show the 
## minimum and maximum values, excluding any outliers (which are plotted as 
## circles). Outliers are defined by first computing the difference between 
## the first and third quartile values, or the height of the box. This number 
## is called the Inter-Quartile Range (IQR). Any point that is greater than 
## the third quartile plus the IQR or less than the first quartile minus the 
## IQR is considered an outlier.
boxplot(mvt$Date ~ mvt$Arrest, 
        xlab = "Arrest",
        ylab = "Date", 
        main = "Boxplot of Date by arrests")
## Q.3.2: Does it look like there were more crimes for which arrests were 
## made in the first half of the time period or the second half of the time 
## period? (Note that the time period is from 2001 to 2012, so the middle of 
## the time period is the beginning of 2007.)
## First half


# Problem 3.3 - Visualizing Crime Trends
## Let's investigate this further. Use the table function for the next few 
## questions.
## Q.3.3: For what proportion of motor vehicle thefts in 2001 was an arrest 
## made? Note: in this question and many others in the course, we are asking 
## for an answer as a proportion. Therefore, your answer should take a value 
## between 0 and 1.
str(mvt)
mvt2001 <- subset(mvt, Year == 2001)
summary(mvt2001)
table(mvt2001$Arrest)
## FALSE = 18517
## TRUE = 2152
(18517 + 2152)
## Total = 20669
(2152 * 100) / 20669
## 10.41173 % => 0.1041173


# Problem 3.4 - Visualizing Crime Trends
## Q.3.4: For what proportion of motor vehicle thefts in 2007 was an arrest 
## made?
mvt2007 <- subset(mvt, Year == 2007)
summary(mvt2007)
table(mvt2007$Arrest)
## FALSE = 13068
## TRUE = 1212
1212 / (13068 + 1212)
## 0.08487395


# Problem 3.5 - Visualizing Crime Trends
## Q.3.5: For what proportion of motor vehicle thefts in 2012 was an arrest 
## made?
mvt2012 <- subset(mvt, Year == 2012)
summary(mvt2012)
table(mvt2012$Arrest)
## FALSE = 13542
## TRUE = 550
550 / (13542 + 550)
## 0.03902924

## Since there may still be open investigations for recent crimes, this could 
## explain the trend we are seeing in the data. There could also be other 
## factors at play, and this trend should be investigated further. However, 
## since we don't know when the arrests were actually made, our detective work 
## in this area has reached a dead end.


# Problem 4.1 - Popular Locations

## Analyzing this data could be useful to the Chicago Police Department when 
## deciding where to allocate resources. If they want to increase the number 
## of arrests that are made for motor vehicle thefts, where should they focus 
## their efforts?
## We want to find the top five locations where motor vehicle thefts occur. 
## If you create a table of the LocationDescription variable, it is 
## unfortunately very hard to read since there are 78 different locations in 
## the data set. By using the sort function, we can view this same table, but 
## sorted by the number of observations in each category. In your R console, 
## type:
sort(table(mvt$LocationDescription))
## Q.4.1: Which locations are the top five locations for motor vehicle thefts, 
## excluding the "Other" category? You should select 5 of the following options.
## - DRIVEWAY - RESIDENTIAL = 1675 
## - GAS STATION = 2111 
## - ALLEY = 2308 
## - OTHER = 4573 => TO EXCLUDE!
## - PARKING LOT/GARAGE(NON.RESID.) = 14852 
## - STREET == 156564 


# Problem 4.2 - Popular Locations
## Create a subset of your data, only taking observations for which the theft 
## happened in one of these five locations, and call this new data set "Top5".
## To do this, you can use the | symbol. In lecture, we used the & symbol to 
## use two criteria to make a subset of the data. To only take observations 
## that have a certain value in one variable or the other, the | character can 
## be used in place of the & symbol. This is also called a logical "or" 
## operation.
## Alternately, you could create five different subsets, and then merge them 
## together into one data frame using rbind.
Top5 <- subset(mvt, LocationDescription == "DRIVEWAY - RESIDENTIAL"
               | LocationDescription == "GAS STATION"
               | LocationDescription == "ALLEY"
               | LocationDescription == "PARKING LOT/GARAGE(NON.RESID.)"
               | LocationDescription == "STREET")
summary(Top5)
str(Top5)
## Q.4.2: How many observations are in Top5?
## 177510


# Problem 4.3 - Popular Locations
## R will remember the other categories of the LocationDescription variable 
## from the original dataset, so running table(Top5$LocationDescription) will 
## have a lot of unnecessary output. To make our tables a bit nicer to read, 
## we can refresh this factor variable. In your R console, type:
Top5$LocationDescription = factor(Top5$LocationDescription)
## If you run the str or table function on Top5 now, you should see that 
## LocationDescription now only has 5 values, as we expect.
str(Top5)
table(Top5$LocationDescription)
## Use the Top5 data frame to answer the remaining questions.
## Q.4.3: One of the locations has a much higher arrest rate than the other 
## locations. Which is it? Please enter the text in exactly the same way as how 
## it looks in the answer options for Problem 4.1.
table(Top5$LocationDescription, Top5$Arrest)
##                                 FALSE   TRUE   % of Arrest
## ALLEY                            2059    249   0.1078856
## DRIVEWAY - RESIDENTIAL           1543    132   0.07880597
## GAS STATION                      1672    439   0.2079583
## PARKING LOT/GARAGE(NON.RESID.)  13249   1603   0.1079316
## STREET                         144969  11595   0.07405917
249 / (2059 + 249)
132 / (1543 +132)
439 / (1672 + 439)
1603 / (13249 + 1603)
11595 / (144969 + 11595)
# Location with higher % of Arrest (rate) is "GAS STATION". Answer "Gas Station"


# Problem 4.4 - Popular Locations
## Q.4.4: On which day of the week do the most motor vehicle thefts at gas 
## stations happen?
Top5GasStations <- subset(Top5, LocationDescription == "GAS STATION")
summary(Top5GasStations)
table(Top5GasStations$Weekday)
## Saturday = 338


# Problem 4.5 - Popular Locations
## Q.4.5: On which day of the week do the fewest motor vehicle thefts in 
## residential driveways happen?
Top5ResDriveways <- 
    subset(Top5, LocationDescription == "DRIVEWAY - RESIDENTIAL")
summary(Top5ResDriveways)
table(Top5ResDriveways$Weekday)
## Saturday = 202


