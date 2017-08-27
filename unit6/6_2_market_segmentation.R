# MARKET SEGMENTATION FOR AIRLINES


# Problem 1.1 - Normalizing the Data
# Read the dataset AirlinesCluster.csv into R and call it "airlines".
airlines <- read.csv("./data_unit6_2/AirlinesCluster.csv")
# Looking at the summary of airlines, which TWO variables have (on average) the
# smallest values?
summary(airlines)
## BonusTrans and FlightTrans

# Which TWO variables have (on average) the largest values?
## Balance and BonusMiles


# Problem 1.2 - Normalizing the Data
# In this problem, we will normalize our data before we run the clustering 
# algorithms. Why is it important to normalize the data before clustering?
## If we don't normalize the data, the clustering will be dominated by the 
## variables that are on a larger scale.


# Problem 1.3 - Normalizing the Data
# Let's go ahead and normalize our data. You can normalize the variables in a 
# data frame by using the preProcess function in the "caret" package. You should
# already have this package installed from Week 4, but if not, go ahead and 
# install it with install.packages("caret"). Then load the package with 
# library(caret).
library(caret)
# Now, create a normalized data frame called "airlinesNorm" by running the 
# following commands:
preproc <- preProcess(airlines)
airlinesNorm <- predict(preproc, airlines)
# The first command pre-processes the data, and the second command performs the 
# normalization. If you look at the summary of airlinesNorm, you should see that
# all of the variables now have mean zero. You can also see that each of the 
# variables has standard deviation 1 by using the sd() function.
summary(airlinesNorm)
sd(airlinesNorm$Balance)
sd(airlinesNorm$FlightTrans)
# In the normalized data, which variable has the largest maximum value?
## FlightMiles

# In the normalized data, which variable has the smallest minimum value?
## DaysSinceEnroll


# Problem 2.1 - Hierarchical Clustering
# Compute the distances between data points (using euclidean distance) and then 
# run the Hierarchical clustering algorithm (using method="ward.D") on the 
# normalized data. It may take a few minutes for the commands to finish since 
# the dataset has a large number of observations for hierarchical clustering.
distAirlines <- dist(airlinesNorm, method = "euclidean")
hclustAirlines <- hclust(distAirlines, method = "ward.D")
# Then, plot the dendrogram of the hierarchical clustering process. Suppose the 
# airline is looking for somewhere between 2 and 10 clusters. According to the 
# dendrogram, which of the following is NOT a good choice for the number of 
# clusters?
plot(hclustAirlines)
## 6


# Problem 2.2 - Hierarchical Clustering
# Suppose that after looking at the dendrogram and discussing with the marketing
# department, the airline decides to proceed with 5 clusters. Divide the data 
# points into 5 clusters by using the cutree function. How many data points are
# in Cluster 1?
hclustGroups <- cutree(hclustAirlines, k = 5)
table(hclustGroups)
## 776


# Problem 2.3 - Hierarchical Clustering
# Now, use tapply to compare the average values in each of the variables for the
# 5 clusters (the centroids of the clusters). You may want to compute the 
# average values of the unnormalized data so that it is easier to interpret. You
# can do this for the variable "Balance" with the following command:
#    tapply(airlines$Balance, clusterGroups, mean)
str(airlines)
tapply(airlines$Balance, hclustGroups, mean)
tapply(airlines$QualMiles, hclustGroups, mean)
tapply(airlines$BonusMiles, hclustGroups, mean)
tapply(airlines$BonusTrans, hclustGroups, mean)
tapply(airlines$FlightMiles, hclustGroups, mean)
tapply(airlines$FlightTrans, hclustGroups, mean)
tapply(airlines$DaysSinceEnroll, hclustGroups, mean)
# Compared to the other clusters, Cluster 1 has the largest average values in 
# which variables (if any)? Select all that apply.
## DaysSinceEnroll

# How would you describe the customers in Cluster 1?
## Infrequent but loyal customers.


# Problem 2.4 - Hierarchical Clustering
# Compared to the other clusters, Cluster 2 has the largest average values in 
# which variables (if any)? Select all that apply.
## QualMiles, FlightMiles, FlightTrans

# How would you describe the customers in Cluster 2?
## Customers who have accumulated a large amount of miles, and the ones with the
## largest number of flight transactions.


# Problem 2.5 - Hierarchical Clustering
# Compared to the other clusters, Cluster 3 has the largest average values in 
# which variables (if any)? Select all that apply.
## Balance, BonusMiles, BonusTrans

# How would you describe the customers in Cluster 3?
## Customers who have accumulated a large amount of miles, mostly through 
## non-flight transactions.


# Problem 2.6 - Hierarchical Clustering
# Compared to the other clusters, Cluster 4 has the largest average values in 
# which variables (if any)? Select all that apply.
## None

# How would you describe the customers in Cluster 4?
## Relatively new customers who seem to be accumulating miles, mostly through 
## non-flight transactions. 


# Problem 2.7 - Hierarchical Clustering
# Compared to the other clusters, Cluster 5 has the largest average values in 
# which variables (if any)? Select all that apply.
## None

# How would you describe the customers in Cluster 5?
## Relatively new customers who don't use the airline very often.


# Problem 3.1 - K-Means Clustering
# Now run the k-means clustering algorithm on the normalized data, again 
# creating 5 clusters. Set the seed to 88 right before running the clustering 
# algorithm, and set the argument iter.max to 1000.
set.seed(88)
kmeansAirlines <- kmeans(airlinesNorm, centers = 5, iter.max = 1000)
str(kmeansAirlines)
# How many clusters have more than 1,000 observations?
table(kmeansAirlines$cluster)
## 2


# Problem 3.2 - K-Means Clustering
# Now, compare the cluster centroids to each other either by dividing the data 
# points into groups and then using tapply, or by looking at the output of 
# kmeansClust$centers, where "kmeansClust" is the name of the output of the 
# kmeans function. (Note that the output of kmeansClust$centers will be for the 
# normalized data. If you want to look at the average values for the 
# unnormalized data, you need to use tapply like we did for hierarchical 
# clustering.)
kmeansGroups <- kmeansAirlines$cluster
tapply(airlines$Balance, kmeansGroups, mean)
tapply(airlines$QualMiles, kmeansGroups, mean)
tapply(airlines$BonusMiles, kmeansGroups, mean)
tapply(airlines$BonusTrans, kmeansGroups, mean)
tapply(airlines$FlightMiles, kmeansGroups, mean)
tapply(airlines$FlightTrans, kmeansGroups, mean)
tapply(airlines$DaysSinceEnroll, kmeansGroups, mean)
# Do you expect Cluster 1 of the K-Means clustering output to necessarily be 
# similar to Cluster 1 of the Hierarchical clustering output?
##  No, because cluster ordering is not meaningful in either k-means clustering 
## or hierarchical clustering.
