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
        sapply(1:ncol(c), function(dim)
              dist(rbind(point, c[dim, ]))))
    return(t(dists))
}

assignClusters <- function(cDists) {

    clusterAndDist = sapply(1:n, function(x) which.min(cDists[x, ]))
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
centroids = X[ids, ] # centroids

## initialize cluster assignments for points
clusters = matrix(0, 1, n)

invisible(readline(prompt="Press [enter] to begin clustering."))

for(iter in 1:maxIter) { # as long max number of iterations has been reached.
  
  cat("Iteration ", iter, ".\n", sep = "")

  ## CLUSTER ASSIGNMENT STEP

  ## Assign every point to the closest cluster mean.
  
  ## for every point, find its distances to all the cluster means.
  centDists  = centroidDists(X, centroids)
  clusters = assignClusters(centDists)

  ## plot
  plotTitle = sprintf("Iteration %d", iter)
  plot(NULL, NULL, xlim = c(min(X[ ,1]), max(X[ ,1])), ylim = c(min(X[ ,2]), max(X[ ,2])), xlab = "X", ylab = "Y", main = plotTitle , type = "n")
  points(centroids, col="black", cex=5)
  points(X[which(clusters == 1), ], col="red")
  points(X[which(clusters == 2), ], col="blue")


  ## CENTROID UPDATE STEP

  ## for every cluster, calculate the new means,,

  ## Using for loop
  ## for (c in 1:k) {
  ##     centroids[c, ] = colMeans(X[which(clusters == c), ])
  ## }

  centroids = t(sapply(1:k, function(c) colMeans(X[which(clusters == c), ])))

  ## CHECK IF CHANGE IN CENTROIDS IS SIGNIFICANT

  ## See if the means of the clusters changed .
  ## Calculate the delta, the sum of the differences between the old means and the new means..
  ## Check if delta is less than epsilon, if so break from the loop..
  ## if not, break from loop


  


  ## Wait for user input
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
