## Implementation of the kMeans Clustering algorithm. 
## Requires the name of the file, the number of clusters, and maximum number of iteration.

## Josh Tan
## CSCI 479
## 11/14/13

## use Lecture 13

## hints:
## 1. follow Algorithm 13.1 in the book
## 2. to choose k random centers, use the sample function in R

## INPUT VARIABLES
dataFile = "kMeansDataset/twoCircles.txt"  # input filename
k = 2                                        # number of clusters
maxIter = 20                                 # max iterations
epsilon = 0.00001                            # difference threshold

distToMeans <- function(X, m) {
        
    dist = apply(X, 1, function(x)
        sapply(1:ncol(m), function(y)
              dist(rbind(x, m[y]))))
    return(dist)
}

assignClusters <- function(meanDists) {

    clusterAndDist = sapply(1:n, function(x) list(which.min(meanDists[, x]), min(meanDists[, x]))) # TODO: rotate this output 
    return(clusterAndDist)
}

    

## read data
X = as.matrix(read.table(dataFile))

## plot data

plot(NULL, NULL, xlim = c(min(X[ ,1]), max(X[ ,1])), ylim = c(min(X[ ,2]), max(X[ ,2])), xlab = "X", ylab = "Y", main = "Original Data", type = "n")
points(X)

## determine number of points and dimensions in data
n = dim(X)[1]
d = dim(X)[2]

## select k random points for initial means
ids = sample(1:n, k)
means = X[ids, ]

## initialize cluster assignments for points
myCluster = matrix(0, 1, n)

invisible(readline(prompt="Press [enter] to begin clustering."))

for(iter in 1:maxIter) { # as long max number of iterations has been reached.
  
  cat("Iteration ", iter, ".\n", sep = "")
  
  ## for every point, find its distances to all the cluster means.
  meanDists  = distToMeans(X, means)
  clusterAndDist = assignClusters(meanDists)

  ## plot
  plot(NULL, NULL, xlim = c(min(X[ ,1]), max(X[ ,1])), ylim = c(min(X[ ,2]), max(X[ ,2])), xlab = "X", ylab = "Y", main = "Iteration" , type = "n")
  points(means, col="black", cex=5)
  points(X[which(clusterAndDist[1, ] == 1), ], col="red")
  points(X[which(clusterAndDist[1, ] == 2), ], col="blue")


  for (m in 1:k) {
      means[k, ] = colMeans(X[which(clusterAndDist[1, ] == k), ])
  }
  
#  for (p in 1:n) {
      
#  }   
  ## Assign every point to the closest cluster mean.

  
  ## for every cluster, calculate the new means,,
  
  ## See if the means of the clusters changed .
  ## Calculate the delta, the sum of the differences between the old means and the new means..

  ## Check if delta is less than epsilon, if so break from the loop..

  invisible(readline(prompt="Press [enter] to perform next iteration."))

}

## myCluster should have the final clustering assignments, which cluster every point is assigned to..
##plot(X,col = myCluster) 

## print output:
## 1. final mean and size for each cluster
## 2. final cluster assignment of all the points, along with which lcuster each point is assigned to
##    e.g. for 7 points with C1 = {p1,p4,p5} and C2 = {p2,p3,p6,p7}, the output should be
##    1,2,2,1,1,2,2
## 3. number of iterations, the final delta
