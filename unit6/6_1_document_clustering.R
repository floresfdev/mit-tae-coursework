# DOCUMENT CLUSTERING WITH DAILY KOS


# Problem 1.1 - Hierarchical Clustering
# Let's start by building a hierarchical clustering model. First, read the data
# set into R. Then, compute the distances (using method="euclidean"), and use 
# hclust to build the model (using method="ward.D"). You should cluster on all 
# of the variables.
dailykos <- read.csv("./data_unit6_1/dailykos.csv")
str(dailykos)
distances <- dist(dailykos, method = "euclidean")
head(distances)
str(distances)
write.csv(as.vector(distances), "./data_unit6_1/distances-backup.csv")
clusterKos <- hclust(distances, method = "ward.D")
str(clusterKos)
# Running the dist function will probably take you a while. Why? Select all that
# apply.
## - We have a lot of observations, so it takes a long time to compute the 
##   distance between each pair of observations.   
## - We have a lot of variables, so the distance computation is long. 


# Problem 1.2 - Hierarchical Clustering
# Plot the dendrogram of your hierarchical clustering model. Just looking at the
# dendrogram, which of the following seem like good choices for the number of 
# clusters? Select all that apply.
rm("distances")
ls()
plot(clusterKos)
## 2 and 3


# Problem 1.3 - Hierarchical Clustering
# In this problem, we are trying to cluster news articles or blog posts into 
# groups. This can be used to show readers categories to choose from when trying
# to decide what to read. Just thinking about this application, what are good 
# choices for the number of clusters? Select all that apply.
## 7 and 8


# Problem 1.4 - Hierarchical Clustering
# Let's pick 7 clusters. This number is reasonable according to the dendrogram, 
# and also seems reasonable for the application. Use the cutree function to 
# split your data into 7 clusters.
# Now, we don't really want to run tapply on every single variable when we have 
# over 1,000 different variables. Let's instead use the subset function to 
# subset our data by cluster. Create 7 new datasets, each containing the 
# observations from one of the clusters.
clusterGroups <- cutree(clusterKos, k = 7)
cluster1 <- subset(dailykos, clusterGroups == 1)
cluster2 <- subset(dailykos, clusterGroups == 2)
cluster3 <- subset(dailykos, clusterGroups == 3)
cluster4 <- subset(dailykos, clusterGroups == 4)
cluster5 <- subset(dailykos, clusterGroups == 5)
cluster6 <- subset(dailykos, clusterGroups == 6)
cluster7 <- subset(dailykos, clusterGroups == 7)
# How many observations are in cluster 3?
str(cluster3)
## 374

# Which cluster has the most observations?
table(clusterGroups)
## Cluster 1

# Which cluster has the fewest observations?
## Cluster 4


# Problem 1.5 - Hierarchical Clustering
# Instead of looking at the average value in each variable individually, we'll 
# just look at the top 6 words in each cluster. To do this for cluster 1, type 
# the following in your R console (where "HierCluster1" should be replaced with 
# the name of your first cluster subset):
tail(sort(colMeans(cluster1)))
# This computes the mean frequency values of each of the words in cluster 1, and
# then outputs the 6 words that occur the most frequently. The colMeans function
# computes the column (word) means, the sort function orders the words in 
# increasing order of the mean values, and the tail function outputs the last 6
# words listed, which are the ones with the largest column means.
# What is the most frequent word in this cluster, in terms of average value? 
# Enter the word exactly how you see it in the output:
## bush


# Problem 1.6 - Hierarchical Clustering
# Now repeat the command given in the previous problem for each of the other 
# clusters, and answer the following questions.
tail(sort(colMeans(cluster2)))
tail(sort(colMeans(cluster3)))
tail(sort(colMeans(cluster4)))
tail(sort(colMeans(cluster5)))
tail(sort(colMeans(cluster6)))
tail(sort(colMeans(cluster7)))

# Which words best describe cluster 2?
## november, poll, vote, challenge

# Which cluster could best be described as the cluster related to the Iraq war?
## Cluster 5

# In 2004, one of the candidates for the Democratic nomination for the President
# of the United States was Howard Dean, John Kerry was the candidate who won the
# democratic nomination, and John Edwards with the running mate of John Kerry 
# (the Vice President nominee). Given this information, which cluster best 
# corresponds to the democratic party?
## Cluster 7


# Problem 2.1 - K-Means Clustering
# Now, run k-means clustering, setting the seed to 1000 right before you run the
# kmeans function. Again, pick the number of clusters equal to 7. You don't need
# to add the iters.max argument.
# Subset your data into the 7 clusters (7 new datasets) by using the "cluster" 
# variable of your kmeans output.
set.seed(1000)
kmeansKos <- kmeans(dailykos, centers = 7)
str(kmeansKos)

kmeansGroups = kmeansKos$cluster
kmeans1 <- subset(dailykos, kmeansGroups == 1)
kmeans2 <- subset(dailykos, kmeansGroups == 2)
kmeans3 <- subset(dailykos, kmeansGroups == 3)
kmeans4 <- subset(dailykos, kmeansGroups == 4)
kmeans5 <- subset(dailykos, kmeansGroups == 5)
kmeans6 <- subset(dailykos, kmeansGroups == 6)
kmeans7 <- subset(dailykos, kmeansGroups == 7)

# How many observations are in Cluster 3?
table(kmeansGroups)
## 277

# Which cluster has the most observations?
## Cluster 4

# Which cluster has the fewest number of observations?
## Cluster 2


# Problem 2.2 - K-Means Clustering
# Now, output the six most frequent words in each cluster, like we did in the 
# previous problem, for each of the k-means clusters.
tail(sort(colMeans(kmeans1)))
tail(sort(colMeans(kmeans2)))
tail(sort(colMeans(kmeans3)))
tail(sort(colMeans(kmeans4)))
tail(sort(colMeans(kmeans5)))
tail(sort(colMeans(kmeans6)))
tail(sort(colMeans(kmeans7)))

# Which k-means cluster best corresponds to the Iraq War?
## Cluster 3

# Which k-means cluster best corresponds to the democratic party? (Remember that
## we are looking for the names of the key democratic party leaders.)
## Cluster 2


# Problem 2.3 - K-Means Clustering
# For the rest of this problem, we'll ask you to compare how observations were
# assigned to clusters in the two different methods. Use the table function to 
# compare the cluster assignment of hierarchical clustering to the cluster 
# assignment of k-means clustering.
table(clusterGroups, kmeansGroups)
# Which Hierarchical Cluster best corresponds to K-Means Cluster 2?
## Hierarchical Cluster 7


# Problem 2.4 - K-Means Clustering
# Which Hierarchical Cluster best corresponds to K-Means Cluster 3?
## Hierarchical Cluster 5


# Problem 2.5 - K-Means Clustering
# Which Hierarchical Cluster best corresponds to K-Means Cluster 7?
## No Hierarchical Cluster contains at least half of the points in K-Means 
## Cluster 7.


# Problem 2.6 - K-Means Clustering
# Which Hierarchical Cluster best corresponds to K-Means Cluster 6?
## Hierarchical Cluster 2