## Implementation of the kMeans Clustering algorithm. 
##
## Input requirements:   the name of the file, the number of clusters, and maximum number of iteration.
##   dataFile: filename of data to cluster
##   k:        number of clusters to use
##   maxIter:  maximum clustering iterations
##   epsilon:  threshold used to classify significant changes in cluster means
##
## Josh Tan
## CSCI 479
## 11/14/13

## INPUT VARIABLES
dataFile = "kMeansDataset/twoCircles.txt"
k = 3
maxIter = 20
epsilon = 0.001

centroidDists <- function(X, c) {
    ## what does
    ##
    ## Args:
    ##   X: what X
    ##   c: what c. 
    ##      centroids
    ## Returns:
    ##   What returned.
    dists = apply(X, 1, function(point)
        sapply(1:nrow(c), function(dim)
              dist(rbind(point, c[dim, ]))))
    return(t(dists))
}

assignClusters <- function(X, c) {
    ## what does
    ##
    ## Args:
    ##   X: what X
    ##   c: what c. 
    ##      centroids
    ## Returns:
    ##   What returned.
    cDists = centroidDists(X, c)
    clusterAndDist = sapply(1:n, function(x) which.min(cDists[x, ]))
    return(clusterAndDist)
}

## read data
X = as.matrix(read.table(dataFile))

## plot original data
plot(NULL, NULL, xlim = c(min(X[ ,1]), max(X[ ,1])),
     ylim = c(min(X[ ,2]), max(X[ ,2])),
     xlab = "X", ylab = "Y", main = "Original Data", type = "n")
points(X)

## determine number of points and dimensions in data
n = dim(X)[1]
d = dim(X)[2]

## select k random points for initial means
ids = sample(1:n, k)
centroids = X[ids, ] # centroids

## initialize cluster assignments for points
clusters = matrix(0, 1, n)

invisible(readline(prompt="Press [enter] to begin clustering."))

for(iter in 1:maxIter) { # as long max number of iterations has been reached.
  
  cat("Iteration ", iter, ".\n", sep = "")

  ## CLUSTER ASSIGNMENT STEP

  ## for every point, find its distances to all the cluster means and assign
  ## the point to its nearest cluster mean
  clusters = assignClusters(X, centroids)

  ## plot points (colored by cluster) and centroids
  plotTitle = sprintf("Iteration %d", iter)
  plot(NULL, NULL, xlim = c(min(X[ ,1]), max(X[ ,1])),
       ylim = c(min(X[ ,2]), max(X[ ,2])),
       xlab = "X", ylab = "Y", main = plotTitle , type = "n")
  points(X, col=clusters + 1)
  points(centroids, col="black", cex=5)

  ## CENTROID UPDATE STEP

  ## Using for loop
  ## for (c in 1:k) {
  ##     centroids[c, ] = colMeans(X[which(clusters == c), ])
  ## }

  ## for every cluster, calculate its new centroid
  newCentroids = t(sapply(1:k, function(c) colMeans(X[which(clusters == c), ])))

  ## CHECK IF CHANGE IN CENTROIDS IS SIGNIFICANT

  delta = sum((newCentroids - centroids) ^ 2)
  cat("Delta:", delta, "\n")

  if (delta > epsilon) {

      ## use new centroids for next iteration
      centroids = newCentroids

      ## Wait for user input (if need to)
      invisible(readline(prompt="Press [enter] to perform next iteration."))
  } else {
      break
  }

}

## plot final clustering
plot(NULL, NULL, xlim = c(min(X[ ,1]), max(X[ ,1])),
     ylim = c(min(X[ ,2]), max(X[ ,2])),
     xlab = "X", ylab = "Y", main = "Final Clustering" , type = "n")
points(X, col = clusters + 1)
points(centroids, col="black", cex=5)

## print output:
## 1. final mean and size for each cluster
## 2. final cluster assignment of all the points, along with which lcuster each point is assigned to
##    e.g. for 7 points with C1 = {p1,p4,p5} and C2 = {p2,p3,p6,p7}, the output should be
##    1,2,2,1,1,2,2
## 3. number of iterations, the final delta
