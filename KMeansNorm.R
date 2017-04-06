# KMeansNorm.R
# Copyright 2017 by Ernst Henle

source("KMeansHelper.R") # ClusterPlot. Samples for observations, clusterCenters, and labels.
source("KMeans.R")

KMeansNorm <- function(observations = sampleObservations, clusterCenters = centersGuess, normD1 = F, normD2 = F)
{
  if (normD1)
  {
    # Determine mean and standard deviation of 1st dimension in observations
    mu1 <- mean(observations[,1])
    sd1 <- sd(observations[,1])
    # normalize 1st dimension of observations
    observations[,1] <- (observations[,1] - mu1)/sd1
    # normalize 1st dimension of clusterCenters
    centersGuess[,1] <- (centersGuess[,1] - mu1)/sd1 
  }
  if (normD2)
  {
    # Determine mean and standard deviation of 2nd dimension in observations
    mu2 <- mean(observations[,2])
    sd2 <- sd(observations[,2])
    # normalize 2nd dimension of observations
    observations[,2] <- (observations[,2] - mu2)/sd2
    # normalize 2nd dimension of clusterCenters
    centersGuess[,2] <- (centersGuess[,2] - mu2)/sd2
  }
  #clusterCenters <- KMeans(observations, clusterCenters)
  KMeansResult <- kmeans(x=observations, centers=clusterCenters)
  clusterCenters <- KMeansResult$centers
  ClusterPlot(observations=observations, centers=clusterCenters, labels=KMeansResult$cluster)
  if (normD1)
  {
    # denormalize in first dimension
    clusterCenters[,1] <- clusterCenters[,1]*sd1 + mu1
  } 
  if (normD2)
  {
    # denormalize in second dimension
    clusterCenters[,2] <- clusterCenters[,2]*sd2 + mu2
  } 
  return(clusterCenters)
}

# Q4a
# From the histograms, the second dimension of TestObservations has a much greater magnitude scale and range than the first,
# thus a wider distribution.

# Q4b
# By not normalizing any dimension, the clusters form along the y axis (2nd dimension) because the distances to the 
# centers are greater along the y axis than the x (1st dimension) by virtue of dimensional scales.

# Q4c
# By normalizing the 1st dimension (x), we amplify the issue in the first test. The distances to the
# centers is now much greater along the y axis than the x axis. The clusters form along the y axis.

# Q4d
# By normalizing the 2nd dimension (y), we reverse the issue. The x axis now has a much greater distances 
# the centers than on the y. The clusters form along the x axis. 

# Q4e
# By normalizing both dimensions, we now have a common scale (i.e z scores). This causes the clusters 
# to form along both the x and y axis.

# Q5
# Normalization is important in clustering because you want all your dimensions to share a common
# scale with a similar magnitude. Otherwise this distorts the distances to centers and misaligns 
# the clusters to the dimensions that have a the greatest magnitude.

# Q6
# You need to encode the categorical data as a set of binary or dummy variables.
# 

# Q7
# Clustering is unsupervised because it has no outcome variable that it tries to fit other datapoints to.
# It is simply taking observations and grouping similar ones together. 
