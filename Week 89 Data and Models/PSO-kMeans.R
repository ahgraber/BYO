library(xlsx);
library(ggplot2);
IPEDS.df <- read.xlsx("IPEDS Data.xlsx",sheetIndex = 1);

# Initialize parameters
k <- 2  # Number of clusters
p <- 20 # Number of particles
n <- 96 # Number of points

# Initialize empty arrays
centroids <- array(0,c(p,k,2))
centv <- array(0,c(p,k,2))
dist <- matrix(0,n,k)
x <- matrix(0,n,k)
fit <- rep(0,p)

# Initialize local best, global best, and personal best
local.best.fit <- Inf
global.best.fit <- Inf
pbest.fit <- rep(Inf,p)
pbest <- array(0,c(p,k,2))

# Generate random centroids
for (i in 1:p) {
  for (j in 1:k) {
    centroids[i,j,] <- c(runif(1,5,30),runif(1,60,100))
  }
}

for (iter in 1:100) {
  for (i in 1:p){
    
    # Calculate the fitness of the particle by calculating 
    # the distance from each point to each centroid then 
    # assigning each point to the closest centroid.  If
    # personal best, save it.
    for (j in 1:n) {
      for (kk in 1:k) {
        dist[j,kk] <- sqrt(sum((IPEDS.df[j,]-centroids[i,kk,])^2))
      }
      x[j,] <- rep(0,k)
      x[j,which.min(dist[j,])] <- 1
    }
    fit[i] <- sum(dist*x)
    if (fit[i] < pbest.fit[i]) {
      pbest.fit[i] <- fit[i]
      pbest[i,,] <- centroids[i,,]
    }
  }
  
  # If local or global best encountered, save it.
  local.best.fit <- min(fit)
  local.best <- centroids[which.min(fit),,]
  if ( global.best.fit > min(fit)) {
    global.best.fit <- min(fit)
    global.best <- centroids[which.min(fit),,]
    print(ggplot() + 
            geom_point(data=IPEDS.df,aes(x=StF_Ratio,y=Retention_Rate)) + 
            geom_point(data=as.data.frame(global.best),aes(x=V1,y=V2,color='red',shape=24)));
  }
  
  # Update the particles' velocities and locations
  for (i in 1:p) {
    uc <- runif(1,0,1)
    ud <- runif(1,0,1)
    centv[i,,] <- centv[i,,] + uc*(pbest[i,,]-centroids[i,,]) +
      ud*(global.best-centroids[i,,])
      # or ud*(local.best-centroids[i,,])
    centroids[i,,] <- centroids[i,,] + centv[i,,]
  }
}
