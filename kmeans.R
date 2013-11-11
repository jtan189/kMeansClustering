## Implementation of the kMeans Clustering algorithm. 
## Requires the name of the file, the number of clusters, and maximum number of iteration.

## Author: Josh Tan
## Created: 2012-10-29

## input variables
dataFile = "twoCircles.txt"
K=2
maxIter=20
epsilon=0.00001
X=as.matrix(read.table(dataFile))

## Many ways to plot points..
plot(X)
plot(X[,1],X[,2])
plot(1,1,xlim=c(min(X[,1]),max(X[,1])), ylim=c(min(X[,2]),max(X[,2])), type="n")
points(X)


N=dim(X)[1]			
d=dim(X)[2]			
ids=sample(1:N,K)
C=X[ids,] # This is my initial means..
myCluster = matrix(0,1,N) # This is to hold the cluster assignment for each points..

for(iter in 1:maxIter){ # as long max number of iterations has been reached.
  
  
  ## for every point, find its distances to all the cluter means.
   
  ## Assign every point to the closest cluster mean.

  
  ## for every cluster, calculate the new means,,
  
  ## See if the means of the clusters changed .
  ## Calculate the delta, the sum of the differences between the old means and the new means..

  ## Check if delta is less than epsilon, if so break from the loop..  

}

## myCluster should have the final clustering assignments, which cluster every point is assigned to..
plot(X,col = myCluster) 
