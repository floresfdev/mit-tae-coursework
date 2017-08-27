# VISUALIZING NETWORK DATA

# Problem 1.1 - Summarizing the Data
# Load the data from edges.csv into a data frame called edges, and load the 
# data from users.csv into a data frame called users.
edges <- read.csv("./data_unit7_2/edges.csv")
users <- read.csv("./data_unit7_2/users.csv")
# How many Facebook users are there in our dataset?
str(users)
## 59

# In our dataset, what is the average number of friends per user? Hint: this 
# question is tricky, and it might help to start by thinking about a small 
# example with two users who are friends.
head(edges)
edges[1,] # users 4019 and 4026 are friends
str(subset(edges, V1 == 4019)) # user 4019 has 2 connections as V1
str(subset(edges, V2 == 4019)) # user 4019 has 5 connections as V2
str(subset(edges, V1 == 4026)) # user 4026 has 1 connections as V1
str(subset(edges, V2 == 4026)) # user 4026 has 7 connections as V2

edges2 <- edges
edges2$PK <- row.names(edges2)
edges2
edges2melt <- melt(edges2, id = "PK")

str(edges2melt)

sum(table(edges2melt$value)) / nrow(users)
## 4.949153


# Problem 1.2 - Summarizing the Data
# Out of all the students who listed a school, what was the most common locale?
summary(users)
table(users$school, users$locale)
## Locale B


# Problem 1.3 - Summarizing the Data
# Is it possible that either school A or B is an all-girls or all-boys school?
table(users$gender, users$school)
## No


# Problem 2.1 - Creating a Network
# We will be using the igraph package to visualize networks; install and load 
# this package using the install.packages and library commands.
install.packages("igraph")
require(igraph)
# We can create a new graph object using the graph.data.frame() function. Based 
# on ?graph.data.frame, which of the following commands will create a graph g 
# describing our social network, with the attributes of each user correctly 
# loaded?
?graph.data.frame
g <- graph.data.frame(edges, FALSE, users)
g
## g = graph.data.frame(edges, FALSE, users)

# Note: A directed graph is one where the edges only go one way -- they point 
# from one vertex to another. The other option is an undirected graph, which 
# means that the relations between the vertices are symmetric.


# Problem 2.2 - Creating a Network
# Use the correct command from Problem 2.1 to load the graph g.
g <- graph.data.frame(edges, FALSE, users)
# Now, we want to plot our graph. By default, the vertices are large and have 
# text labels of a user's identifier. Because this would clutter the output, we
# will plot with no text labels and smaller vertices:
plot(g, vertex.size=5, vertex.label=NA)
# In this graph, there are a number of groups of nodes where all the nodes in 
# each group are connected but the groups are disjoint from one another, forming
# "islands" in the graph. Such groups are called "connected components," or 
# "components" for short. How many connected components with at least 2 nodes 
# are there in the graph?
## 4

# How many users are there with no friends in the network?
## 7


# Problem 2.3 - Creating a Network
# In our graph, the "degree" of a node is its number of friends. We have already
# seen that some nodes in our graph have degree 0 (these are the nodes with no 
# friends), while others have much higher degree. We can use degree(g) to 
# compute the degree of all the nodes in our graph g.
degree(g)
# How many users are friends with 10 or more other Facebook users in this 
# network?
sum(degree(g) >= 10)
## 9


# Problem 2.4 - Creating a Network
# In a network, it's often visually useful to draw attention to "important" 
# nodes in the network. While this might mean different things in different 
# contexts, in a social network we might consider a user with a large number of 
# friends to be an important user. From the previous problem, we know this is 
# the same as saying that nodes with a high degree are important users.

# To visually draw attention to these nodes, we will change the size of the 
# vertices so the vertices with high degrees are larger. To do this, we will 
# change the "size" attribute of the vertices of our graph to be an increasing 
# function of their degrees:
V(g)$size <- degree(g)/2+2
# Now that we have specified the vertex size of each vertex, we will no longer 
# use the vertex.size parameter when we plot our graph:
plot(g, vertex.label=NA)
# What is the largest size we assigned to any node in our graph?
max(V(g)$size)
## 11

#What is the smallest size we assigned to any node in our graph?
min(V(g)$size)
## 2


# Problem 3.1 - Coloring Vertices
# Thus far, we have changed the "size" attributes of our vertices. However, we
# can also change the colors of vertices to capture additional information about
# the Facebook users we are depicting.

# When changing the size of nodes, we first obtained the vertices of our graph 
# with V(g) and then accessed the the size attribute with V(g)$size. To change 
# the color, we will update the attribute V(g)$color.

# To color the vertices based on the gender of the user, we will need access to
# that variable. When we created our graph g, we provided it with the data frame
# users, which had variables gender, school, and locale. These are now stored as
# attributes V(g)$gender, V(g)$school, and V(g)$locale.

# We can update the colors by setting the color to black for all vertices, than 
# setting it to red for the vertices with gender A and setting it to gray for 
# the vertices with gender B:
V(g)$color = "black"
V(g)$color[V(g)$gender == "A"] = "red"
V(g)$color[V(g)$gender == "B"] = "gray"
# Plot the resulting graph. What is the gender of the users with the highest 
# degree in the graph?
plot(g, vertex.label=NA)
## Gender B


# Problem 3.2 - Coloring Vertices
# Now, color the vertices based on the school that each user in our network 
# attended.
table(V(g)$school)
V(g)$color = "black"
V(g)$color[V(g)$school == "A"] = "red"
V(g)$color[V(g)$school == "AB"] = "gray"
plot(g, vertex.label=NA)
# Are the two users who attended both schools A and B Facebook friends with 
# each other?
## Yes

# What best describes the users with highest degree?
##  Some, but not all, of the high-degree users attended school A


# Problem 3.3 - Coloring Vertices
# Now, color the vertices based on the locale of the user.
table(V(g)$locale)
V(g)$color = "black"
V(g)$color[V(g)$locale == "A"] = "red"
V(g)$color[V(g)$locale == "B"] = "gray"
plot(g, vertex.label=NA)
# The large connected component is most associated with which locale?
## Locale B

# The 4-user connected component is most associated with which locale?
## Locale A 


# Problem 4 - Other Plotting Options
# The help page is a helpful tool when making visualizations. Answer the 
# following questions with the help of ?igraph.plotting and experimentation in 
# your R console.
?igraph.plotting

# Which igraph plotting function would enable us to plot our graph in 3-D?
?rglplot
## rglplot

# What parameter to the plot() function would we use to change the edge width 
# when plotting g?
?plot.igraph
## edge.width