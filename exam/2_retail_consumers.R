# UNDERSTANDING RETAIL CONSUMERS
# 
# In Unit 6, we saw how clustering can be used for market segmentation, the idea
# of dividing airline passengers into small, more similar groups, and then 
# designing a marketing strategy specifically for each group.  In this problem, 
# we'll see how this idea can be applied to retail consumer data.
# 
# In this problem, we'll use the dataset Households.csv, which contains data 
# collected over two years for a group of 2,500 households.  Each row 
# (observation) in our dataset represents a unique household.  The dataset 
# contains the following variables:
#     
# - NumVisits = the number of times the household visited the retailer 
# - AvgProdCount = the average number of products purchased per transaction
# - AvgDiscount = the average discount per transaction from coupon usage 
#                 (in %) - NOTE: Do not divide this value by 100!
# - AvgSalesValue = the average sales value per transaction
# - MorningPct = the percentage of visits in the morning (8am - 1:59pm)
# - AfternoonPct = the percentage of visits in the afternoon (2pm - 7:59pm)
# Note that some visits can occur outside of morning and afternoon hours.  That
# is, visits from 8pm - 7:59am are possible.
# 
# This dataset was derived from source files provided by dunnhumby, a customer 
# science company based in the United Kingdom.


# Problem 1 - Reading in the data
# Read the dataset Households.csv into R.
HouseHolds <- read.csv("./data_2/Households.csv")
str(HouseHolds)
summary(HouseHolds)
head(HouseHolds)
# How many households have logged transactions at the retailer only in the 
# morning?
nrow(subset(HouseHolds, MorningPct == 100))
## 4

# How many households have logged transactions at the retailer only in the 
# afternoon?
nrow(subset(HouseHolds, AfternoonPct == 100))
## 13


# Problem 2 - Descriptive statistics
# Of the households that spend more than $150 per transaction on average, what 
# is the minimum average discount per transaction?
min(subset(HouseHolds, AvgSalesValue > 150)$AvgDiscount)
## 15.64607

# Of the households who have an average discount per transaction greater than 
# 25%, what is the minimum average sales value per transaction?
min(subset(HouseHolds, AvgDiscount > 25)$AvgSalesValue)
## 50.1175

# In the dataset, what proportion of households visited the retailer at least 
# 300 times?
nrow(subset(HouseHolds, NumVisits >= 300)) / nrow(HouseHolds)
## 0.0592


# Problem 3 - Importance of Normalizing
# When clustering data, it is often important to normalize the variables so that
# they are all on the same scale. If you clustered this dataset without 
# normalizing, which variable would you expect to dominate in the distance 
# calculations?
summary(HouseHolds)
## NumVisits


# Problem 4 - Normalizing the Data
# Normalize all of the variables in the HouseHolds dataset by entering the 
# following commands in your R console: (Note that these commands assume that 
# your dataset is called "Households", and create the normalized dataset 
# "HouseholdsNorm". You can change the names to anything you want by editing the
# commands.)
# library(caret)
# 
# preproc = preProcess(Households)
# 
# HouseholdsNorm = predict(preproc, Households)
# 
library(caret)
preproc <- preProcess(HouseHolds)
HouseHoldsNorm <- predict(preproc, HouseHolds)
# (Remember that for each variable, the normalization process subtracts the mean
# and divides by the standard deviation. We learned how to do this in Unit 6.) 
# In your normalized dataset, all of the variables should have mean 0 and 
# standard deviation 1.
summary(HouseHoldsNorm)
# What is the maximum value of NumVisits in the normalized dataset?
## 10.2828

# What is the minimum value of AfternoonPct in the normalized dataset?
## -3.22843

# Run the following code to create a dendrogram of your data:
#     
# set.seed(200)
# distances <- dist(HouseholdsNorm, method = "euclidean")
# ClusterShoppers <- hclust(distances, method = "ward.D")
# plot(ClusterShoppers, labels = FALSE)
set.seed(200)
distances <- dist(HouseHoldsNorm, method = "euclidean")
ClusterShoppers <- hclust(distances, method = "ward.D")
plot(ClusterShoppers, labels = FALSE)


# Problem 5 - Interpreting the Dendrogram
# Based on the dendrogram, how many clusters do you think would be appropriate 
# for this problem? Select all that apply.
## 2, 3, 5


# Problem 6 - K-means Clustering
# Run the k-means clustering algorithm on your normalized dataset, selecting 10
# clusters. Right before using the kmeans function, type "set.seed(200)" in your
# R console.
set.seed(200)
k <- 10
KMC <- kmeans(HouseHoldsNorm, centers = k, iter.max = 1000)
str(KMC)
kmeansGroups <- KMC$cluster
table(kmeansGroups)

# How many observations are in the smallest cluster?
min(table(kmeansGroups))
## 51

# How many observations are in the largest cluster?
max(table(kmeansGroups))
## 504


# Problem 7 - Understanding the Clusters
# Now, use the cluster assignments from k-means clustering together with the 
# cluster centroids to answer the next few questions.
kmeans1 <- subset(HouseHolds, kmeansGroups == 1)
kmeans2 <- subset(HouseHolds, kmeansGroups == 2)
kmeans3 <- subset(HouseHolds, kmeansGroups == 3)
kmeans4 <- subset(HouseHolds, kmeansGroups == 4)
kmeans5 <- subset(HouseHolds, kmeansGroups == 5)
kmeans6 <- subset(HouseHolds, kmeansGroups == 6)
kmeans7 <- subset(HouseHolds, kmeansGroups == 7)
kmeans8 <- subset(HouseHolds, kmeansGroups == 8)
kmeans9 <- subset(HouseHolds, kmeansGroups == 9)
kmeans10 <- subset(HouseHolds, kmeansGroups == 10)

colMeans(kmeans1)
colMeans(kmeans2)
colMeans(kmeans3)
colMeans(kmeans4)
colMeans(kmeans5)
colMeans(kmeans6)
colMeans(kmeans7)
colMeans(kmeans8)
colMeans(kmeans9)
colMeans(kmeans10)

# Which cluster best fits the description "morning shoppers stopping in to make
# a quick purchase"?
## Cluster 4


# Problem 8 - Understanding the Clusters
# Which cluster best fits the description "shoppers with high average product 
# count and high average value per visit"?
## Cluster 2


# Problem 9 - Understanding the Clusters
# Which cluster best fits the description "frequent shoppers with low value per 
# visit"?
## Cluster 9


# Problem 10 - Random Behavior
# If we ran hierarchical clustering a second time without making any additional 
# calls to set.seed, we would expect:
## Identical results to the first hierarchical clustering

# If we ran k-means clustering a second time without making any additional calls to set.seed, we would expect:
## Different results from the first k-means clustering  

# If we ran k-means clustering a second time, again running the command 
# set.seed(200) right before doing the clustering, we would expect:
## Identical results to the first k-means clustering

# If we ran k-means clustering a second time, running the command set.seed(100) 
# right before doing the clustering, we would expect:
## Different results from the first k-means clustering  


# Problem 11 - The Number of Clusters
# Suppose the marketing department at the retail store decided that the 10 
# clusters were too specific, and they wanted more general clusters to describe 
# the consumer base. Would they want to increase or decrease the number of 
# clusters?
## Decrease the number of clusters


# Problem 12 - Increasing the Number of Clusters
# Run the k-means clustering algorithm again, this time selecting 5 clusters. 
# Right before the "kmeans" function, set the random seed to 5000.
set.seed(5000)
k <- 5
KMC.5 <- kmeans(HouseHoldsNorm, centers = k, iter.max = 1000)
str(KMC.5)
kmeansGroups.5 <- KMC.5$cluster
table(kmeansGroups.5)

# How many observations are in the smallest cluster?
min(table(kmeansGroups.5))
## 172

# How many observations are in the largest cluster?
max(table(kmeansGroups.5))
## 994


# Problem 13 - Describing the Clusters
# Using the cluster assignments from k-means clustering with 5 clusters, which 
# cluster best fits the description "frequent shoppers with low value per 
# visit"?
kmeans1.5 <- subset(HouseHolds, kmeansGroups.5 == 1)
kmeans2.5 <- subset(HouseHolds, kmeansGroups.5 == 2)
kmeans3.5 <- subset(HouseHolds, kmeansGroups.5 == 3)
kmeans4.5 <- subset(HouseHolds, kmeansGroups.5 == 4)
kmeans5.5 <- subset(HouseHolds, kmeansGroups.5 == 5)

colMeans(kmeans1.5)
colMeans(kmeans2.5)
colMeans(kmeans3.5)
colMeans(kmeans4.5)
colMeans(kmeans5.5)

## Cluster 4


# Problem 14 - Understanding Centroids
# Why do we typically use cluster centroids to describe the clusters?
## The cluster centroid captures the average behavior in the cluster, and can 
## be used to summarize the general pattern in the cluster.


# Problem 15 - Using a Visualization
# Which of the following visualizations could be used to observe the 
# distribution of NumVisits, broken down by cluster? Select all that apply.
## - A box plot of the variable NumVisits, subdivided by cluster 
## - ggplot with NumVisits on the x-axis and the cluster number on the y-axis, 
##   plotting with geom_point()