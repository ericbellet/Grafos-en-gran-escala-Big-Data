#install.packages("igraph")
#install.packages("statnet")
#install.packages("rgl")
#install.packages("tnet")
#install.packages("coda")
#install.packages("network")
setwd("C:/Users/eric.bellet/Desktop") 

library(coda)
library(statnet)
library(rgl)
library(tnet)
library(network)

# Make sure that network package is loaded 
data(package="network") 
# List available datasets in network package 
data(flo) 
# Load a built-in data set; see ?flo for more flo
list.files()
relations <- read.csv("relationalData.csv",header=FALSE,stringsAsFactors=FALSE) 
relations
relations <- as.matrix(relations)
nodeInfo <- read.csv("vertexAttributes.csv",header=TRUE,stringsAsFactors=FALSE)
nodeInfo
rownames(relations) <- nodeInfo$name 
colnames(relations) <- nodeInfo$name 
relations

nrelations<-network(relations,directed=FALSE) 
# Create a network object based on flo nrelations
# Get a quick description of the data 
nempty <- network.initialize(5) 
# Create an empty graph with 5 vertices
nempty 
# Compare with nrelations
summary(nrelations)
# Get an overall summary 
network.dyadcount(nrelations)
# How many dyads in nflo? 
network.edgecount(nrelations) 
# How many edges are present? 
network.size(nrelations) 
# How large is the network? 
as.sociomatrix(nrelations) 
# Show it as a sociomatrix 
nrelations[,] 
# Another way to do it

#####
plot(nrelations,displaylabels=T) 
# Plot with names 
plot(nrelations,displaylabels=T,mode="circle") 
# A less useful layout... 
library(sna) 
# Load the sna library 
gplot(nrelations)
gplot3d(nrelations)

g <- network.initialize(5) 
# Create an empty 
graph g[1,2] <- 1 
# Add an edge from 1 to 2 
g[2,] <- 1
# Add edges from 2 to everyone else
g 
# Examine the result 
m <- matrix(0, nrow=5, ncol=5) 
# Create an adjacency matrix 
m[3,4:5] <- 1 
# Add entries from 3 to 4 and 5 
g[m>0] <- 1 
# Add more entries g




