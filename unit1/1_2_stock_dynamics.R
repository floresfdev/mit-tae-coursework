# STOCK DYNAMICS

# Load files
IBM <- read.csv("./data_unit1_2/IBMStock.csv")
GE <- read.csv("./data_unit1_2/GEStock.csv")
ProcterGamble <- read.csv("./data_unit1_2/ProcterGambleStock.csv")
CocaCola <- read.csv("./data_unit1_2/CocaColaStock.csv")
Boeing <- read.csv("./data_unit1_2/BoeingStock.csv")

summary(IBM)
summary(GE)
summary(ProcterGamble)
summary(CocaCola)
summary(Boeing)


# Problem 1.1 - Summary Statistics
## Before working with these data sets, we need to convert the dates into a 
## format that R can understand. Take a look at the structure of one of the 
## datasets using the str function. Right now, the date variable is stored as 
## a factor. We can convert this to a "Date" object in R by using the following 
## five commands (one for each data set):
IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
GE$Date = as.Date(GE$Date, "%m/%d/%y")
CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")
ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")
Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")
## The first argument to the as.Date function is the variable we want to 
## convert, and the second argument is the format of the Date variable. We can 
## just overwrite the original Date variable values with the output of this 
## function. Now, answer the following questions using the str and summary 
## functions.

## Q.1.1: Our five datasets all have the same number of observations. How many 
## observations are there in each data set?
str(IBM)
str(GE)
str(CocaCola)
str(ProcterGamble)
str(Boeing)
## 480 observations


# Problem 1.2 - Summary Statistics
## Q.1.2: What is the earliest year in our datasets?
summary(IBM)
summary(GE)
summary(CocaCola)
summary(ProcterGamble)
summary(Boeing)
## 1970


# Problem 1.3 - Summary Statistics
## Q.1.3: What is the latest year in our datasets?
## 2009


# Problem 1.4 - Summary Statistics
## Q.1.4: What is the mean stock price of IBM over this time period?
summary(IBM)
## 144.38


# Problem 1.5 - Summary Statistics
## Q.1.5: What is the minimum stock price of General Electric (GE) over this 
## time period?
summary(GE)
## 9.294


# Problem 1.6 - Summary Statistics
## Q.1.6: What is the maximum stock price of Coca-Cola over this time period?
summary(CocaCola)
## 146.58


# Problem 1.7 - Summary Statistics
## Q.1.7: What is the median stock price of Boeing over this time period?
summary(Boeing)
## 44.88


# Problem 1.8 - Summary Statistics
## Q.1.8: What is the standard deviation of the stock price of Procter & Gamble 
## over this time period?
sd(ProcterGamble$StockPrice)
## 18.19414


# Problem 2.1 - Visualizing Stock Dynamics
## Let's plot the stock prices to see if we can visualize trends in stock 
## prices during this time period. Using the plot function, plot the Date on 
## the x-axis and the StockPrice on the y-axis, for Coca-Cola.
plot(CocaCola$Date, CocaCola$StockPrice)
## This plots our observations as points, but we would really like to see a 
## line instead, since this is a continuous time period. To do this, add the 
## argument type="l" to your plot command, and re-generate the plot (the 
## character is quotes is the letter l, for line). You should now see a line 
## plot of the Coca-Cola stock price.
plot(CocaCola$Date, CocaCola$StockPrice, type = "l")

## Q.2.1: 
## Around what yr did Coca-Cola has its highest stock price in this timeperiod?
## 1973

## Around what ye did Coca-Cola has its lowest stock price in this time period?
## 1980


# Problem 2.2 - Visualizing Stock Dynamics
## Now, let's add the line for Procter & Gamble too. You can add a line to a 
## plot in R by using the lines function instead of the plot function. Keeping 
## the plot for Coca-Cola open, type in your R console:
lines(ProcterGamble$Date, ProcterGamble$StockPrice)
## Unfortunately, it's hard to tell which line is which. Let's fix this by 
## giving each line a color. First, re-run the plot command for Coca-Cola, but 
## add the argument col="red". You should see the plot for Coca-Cola show up 
## again, but this time in red. Now, let's add the Procter & Gamble line (using 
## the lines function like we did before), adding the argument col="blue". You 
## should now see in your plot the Coca-Cola stock price in red, and the 
## Procter & Gamble stock price in blue.
plot(CocaCola$Date, CocaCola$StockPrice, type = "l", col = "red")
lines(ProcterGamble$Date, ProcterGamble$StockPrice, col = "blue")
## As an alternative choice to changing the colors, you could instead change 
## the line type of the Procter & Gamble line by adding the argument lty=2. 
## This will make the Procter & Gamble line dashed.
plot(CocaCola$Date, CocaCola$StockPrice, type = "l", col = "red")
lines(ProcterGamble$Date, ProcterGamble$StockPrice, col = "blue", lty = 2)
## Using this plot, answer the following questions.

## Q.2.2: In March of 2000, the technology bubble burst, and a stock market 
## crash occurred. According to this plot, which company's stock dropped more?
## Procter & Gamble

## To answer this question and the ones that follow, you may find it useful to 
## draw a vertical line at a certain date. To do this, type the command
abline(v=as.Date(c("2000-03-01")), lwd=2, lty = 2)
## in your R console, with the plot still open. This generates a vertical line 
## at the date March 1, 2000. The argument lwd=2 makes the line a little 
## thicker. You can change the date in this command to generate the vertical 
## line in different locations.


# Problem 2.3 - Visualizing Stock Dynamics
## Answer these questions using the plot you generated in the previous problem.

## Q.2.3:

## Around 1983, the stock for one of these companies (Coca-Cola or Procter 
## and Gamble) was going up, while the other was going down. Which one was 
## going up?
## Coca-Cola

## In the time period shown in the plot, which stock generally has lower values?
## Coca-Cola


# Problem 3.1 - Visualizing Stock Dynamics 1995-2005
## Let's take a look at how the stock prices changed from 1995-2005 for all 
## five companies. In your R console, start by typing the following plot 
## command:
plot(CocaCola$Date[301:432], 
     CocaCola$StockPrice[301:432], 
     type="l", 
     col="red", 
     ylim=c(0,210))
## This will plot the CocaCola stock prices from 1995 through 2005, which are 
## the observations numbered from 301 to 432. The additional argument, 
## ylim=c(0,210), makes the y-axis range from 0 to 210. This will allow us to 
## see all of the stock values when we add in the other companies.
## Now, use the lines function to add in the other four companies, remembering 
## to only plot the observations from 1995 to 2005, or [301:432]. You don't 
## need the "type" or "ylim" arguments for the lines function, but remember to 
## make each company a different color so that you can tell them apart. Some 
## color options are "red", "blue", "green", "purple", "orange", and "black". 
## To see all of the color options in R, type colors() in your R console.
## (If you prefer to change the type of the line instead of the color, here are 
## some options for changing the line type: lty=2 will make the line dashed, 
## lty=3 will make the line dotted, lty=4 will make the line alternate between 
## dashes and dots, and lty=5 will make the line long-dashed.)
lines(ProcterGamble$Date[301:432], 
      ProcterGamble$StockPrice[301:432], 
      col = "blue")
lines(IBM$Date[301:432], 
      IBM$StockPrice[301:432], 
      col = "green")
lines(GE$Date[301:432], 
      GE$StockPrice[301:432], 
      col = "orange")
lines(Boeing$Date[301:432], 
      Boeing$StockPrice[301:432], 
      col = "black")

## Use this plot to answer the following four questions.

## Q.3.1: Which stock fell the most right after the technology bubble burst in 
## March 2000?
abline(v=as.Date(c("2000-03-01")), lty = 2)
## General Electric (GE)


# Problem 3.2 - Visualizing Stock Dynamics 1995-2005
## Q.3.2: Which stock reaches the highest value in the time period 1995-2005?
## IBM


# Problem 3.3 - Visualizing Stock Dynamics 1995-2005
## In October of 1997, there was a global stock market crash that was caused 
## by an economic crisis in Asia. 
## Q.3.3: Comparing September 1997 to November 1997, which companies saw a 
## decreasing trend in their stock price? (Select all that apply.)
abline(v=as.Date(c("1997-09-01")), lty = 2)
abline(v=as.Date(c("1997-10-01")), lty = 2)
abline(v=as.Date(c("1997-11-01")), lty = 2)
## Procter & Gamble, Boeing


# Problem 3.4 - Visualizing Stock Dynamics 1995-2005
## Q.3.4: In the last two years of this time period (2004 and 2005) which stock 
## seems to be performing the best, in terms of increasing stock price?
abline(v=as.Date(c("2004-01-01")), lty = 2)
## Boeing


# Problem 4.1 - Monthly Trends
## Lastly, let's see if stocks tend to be higher or lower during certain months. 
## Use the tapply command to calculate the mean stock price of IBM, sorted by 
## months. To sort by months, use
## months(IBM$Date)
## as the second argument of the tapply function.
## For IBM, compare the monthly averages to the overall average stock price. 
## In which months has IBM historically had a higher stock price (on average)? 
## Select all that apply.
tapply(IBM$StockPrice, months(IBM$Date), mean)
mean(IBM$StockPrice)
## April, February, January, March, May


# Problem 4.2 - Monthly Trends
## Repeat the tapply function from the previous problem for each of the other 
## four companies, and use the output to answer the remaining questions.
tapply(GE$StockPrice, months(GE$Date), mean)
tapply(ProcterGamble$StockPrice, months(ProcterGamble$Date), mean)
tapply(CocaCola$StockPrice, months(CocaCola$Date), mean)
tapply(Boeing$StockPrice, months(Boeing$Date), mean)

## Q.4.2: General Electric and Coca-Cola both have their highest average stock 
## price in the same month. Which month is this?
tapply(GE$StockPrice, months(GE$Date), mean)
tapply(CocaCola$StockPrice, months(CocaCola$Date), mean)
## April


# Problem 4.3 - Monthly Trends
## For the months of December and January, every company's average stock is 
## higher in one month and lower in the other. In which month are the stock 
## prices lower?
tapply(IBM$StockPrice, months(IBM$Date), mean)
tapply(GE$StockPrice, months(GE$Date), mean)
tapply(ProcterGamble$StockPrice, months(ProcterGamble$Date), mean)
tapply(CocaCola$StockPrice, months(CocaCola$Date), mean)
tapply(Boeing$StockPrice, months(Boeing$Date), mean)
