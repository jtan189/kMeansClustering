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
dataFile = "kMeansDataset/twoEllipses.txt"
k = 2
maxIter = 20
epsilon = 0.001

centroidDists <- function(X, c) {
    ## For each row in the input data, find its Euclidean distance to
    ## each of the centroids.
    ##
    ## Args:
    ##   X: input data matrix, where each row represents a point
    ##   c: centroid matrix, where each row represents a centroid
    ## Returns:
    ##   A matrix with the same number of rows as X and the same number
    ##   of columns as c. The rows correspond to the points in
    ##   X and the columns correspond to the centroids. The value
    ##   for a particular (row, col) is the Euclidean distance
    ##   for the corresponding (point, centroid).
    dists = apply(X, 1, function(point)
        sapply(1:nrow(c), function(dim)
              dist(rbind(point, c[dim, ]))))
    return(t(dists))
}

assignClusters <- function(X, c) {
    ## Assign the points in the input data to its closest centroid,
    ## using Euclidean distances.
    ##
    ## Args:
    ##   X: input data matrix, where each row represents a point
    ##   c: centroid matrix, where each row represents a centroid 
    ## Returns:
    ##   A vector with the same number of rows as X. The value for
    ##   a given index in this vector is indicates the cluster
    ##   centered at at c[index, ].
    cDists = centroidDists(X, c)
    clusterAndDist = sapply(1:n, function(x) which.min(cDists[x, ]))
    return(clusterAndDist)
}

plotData <- function(X, title, centroids = NULL, clusters = NULL) {
    ## Plot the data. The pairs() function is used if the data
    ## has more than 2 dimensions. Otherwise, the plot() function
    ## is used. If centroid and clustering data is provided,
    ## also draw these on the same plot(s).
    ##
    ## Args:
    ##   X: input data matrix, where each row represents a point
    ##   title: title to use for the plot
    ##   centroids: centroid matrix, where each row represents a centroid
    ##   clusters: vector of cluster assignments, corresponding to the
    ##             rows in centroids
    ## Returns:
    ##   Nothing.
    if (ncol(X) > 2) {
        ## should use scatterplot matrix
        if (!is.null(centroids) & !is.null(clusters)) {
            ## plot data colored by cluster, along with centroids
            pairsCol = append(clusters + 1,
                vector(mode = "numeric", length = nrow(centroids)) + 1)
            pairsCex = append(vector(mode = "numeric", length = length(clusters)) + 1,
                vector(mode = "numeric", length = nrow(centroids)) + 2)
            pairs(rbind(X, centroids), main = title, col = pairsCol, cex = pairsCex)
        } else {
            ## plot original data
            pairs(X, main = title)
        }
    } else {
        ## should use 2D plot
        plot(NULL, NULL, xlim = c(min(X[ ,1]), max(X[ ,1])),
             ylim = c(min(X[ ,2]), max(X[ ,2])),
             xlab = "X", ylab = "Y", main = title, type = "n")
        if (!is.null(centroids) & !is.null(clusters)) {
            ## plot data colored by cluster, along with centroids
            points(X, col = clusters + 1)
            points(centroids, col = "black", cex = 5)
        } else {
            ## plot original data
            points(X)
        }
    }
}

## read data
X = as.matrix(read.table(dataFile))

## plot original data
plotData(X, "Original Data")

## determine number of points and dimensions in data
n = dim(X)[1]
d = dim(X)[2]

## select k random points for initial means
ids = sample(1:n, k)
centroids = X[ids, ]

## initialize cluster assignments for points
clusters = matrix(0, 1, n)

invisible(readline(prompt = "Press [enter] to begin clustering."))

for(iter in 1:maxIter) {
  
  cat("Iteration ", iter, ".\n", sep = "")

  ## CLUSTER ASSIGNMENT STEP

  ## for every point, find its distances to all the cluster means and assign
  ## the point to its nearest cluster mean
  clusters = assignClusters(X, centroids)

  ## plot points (colored by cluster) and centroids
  plotTitle = sprintf("Iteration %d", iter)
  plotData(X, plotTitle, centroids, clusters)

  ## CENTROID UPDATE STEP

  ## for every cluster, calculate its new centroid
  newCentroids = t(sapply(1:k, function(c) colMeans(X[which(clusters == c), ])))

  ## check if change in centroids is significant
  delta = sum((newCentroids - centroids) ^ 2)
  cat("Delta:", delta, "\n")

  if (delta > epsilon) {
      ## use new centroids for next iteration
      centroids = newCentroids

      ## Wait for user input (if need to)
      if (iter != maxIter) {
          invisible(readline(prompt = "Press [enter] to perform next iteration."))
      }
  } else {
      break
  }
}

## plot final clustering
plotData(X, "Final Clustering", centroids, clusters)

## print output:
cat("\nClustering finished.\n\n")
for (c in 1:nrow(centroids)) {
    cat("Cluster ", c, ":\n", sep = "")
    cat("\tMean:", paste(centroids[c, ], collapse = ", "), "\n")
    cat("\tSize:", length(which(clusters == c)), "\n")
}

cat("\nIteration Count:", iter, "\n")
cat("Final Delta:", delta, "\n")

invisible(readline(prompt = "\nPress [enter] to print cluster assignments.\n"))
cat("Cluster Assignments:\n", clusters, "\n")
