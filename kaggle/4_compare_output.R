# Load datasets to compare
outputFrom <- read.csv("./dataout/submission-20160614-1-late.csv")
outputTo <- read.csv("./dataout/submission-20160614-2-late.csv")

require(dplyr)
fromAndTo <- inner_join(outputFrom, outputTo, by = "USER_ID")
head(fromAndTo)
nrow(subset(fromAndTo, Predictions.x == Predictions.y))
## If nrow == 1392, then both datasets are equal
