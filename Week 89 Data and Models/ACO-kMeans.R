library(xlsx);
library(ggplot2);
IPEDS.df <- read.xlsx("IPEDS Data.xlsx",sheetIndex = 1);

# Initialize parameters
k <- 2      # Number of clusters
a <- 40     # Number of ants
n <- 96     # Number of points
evap <- 0.3 # Evaporation quotient
Q <- 10     # Pheromone parameter
alpha <- 1
beta <- 10

# Initialize empty matrices
IPEDS.df$cluster <- rep(0,n)
assn <- matrix(0,a,n)
dist <- rep(0,a)
phero <- matrix(0,n,k)
dphero <- array(0,c(a,n,k))
probs <- array(0,c(a,n,k))
tempdist <- matrix(0,n,k)

for (iter in 1:100) {
  for (aa in 1:a) {
    
    # For each ant, generate a cluster assignment
    if (iter == 1) {
      assn[aa,] <- floor(runif(n,1,k+1))  # random initial cluster assignments
    } else {
      for (i in 1:n) {
        mcsum <- cumsum(probs[aa,i,]) 
        rno <- runif(1,0,1)
        for (kk in 1:k) {
          if (mcsum[kk] > rno) {
            assn[aa,i] <- kk
            break
          }
        }
      }
    }
    
    # Compute the centroids for each ant
    IPEDS.df$cluster <- assn[aa,]
    centroids <- aggregate(.~cluster,IPEDS.df,FUN=mean)
  
    # Evaluate the fitness function (total distance)
    dist[aa] <- 0.0
    for (i in 1:n) {
      centroid <- centroids[assn[aa,i],];
      dist[aa] <- dist[aa] + 0.5*sum( (IPEDS.df[i,1:2] - centroid[-1])^2 )
    }
    
    # Calculate pheromones from ant a on assigning point i to cluster j
    for (i in 1:n) {
      dphero[aa,i,] <- rep(0,k)
      dphero[aa,i,assn[aa,i]] <- Q/dist[aa]
    }
  }
  
  # Update pheromones
  for (i in 1:n) {
    for (j in 1:k) {
      phero[i,j] <- (1-evap)*phero[i,j] + sum(dphero[,i,j])
    }
  }
  
  # Update probabilities
  for (aa in 1:a) {
    for (i in 1:n) {
      for (j in 1:k) {
        centroid <- centroids[j,];
        tempdist[i,j] <- 0.5*sum( (IPEDS.df[i,1:2] - centroid[-1])^2 )
      }
    }
    temp <- (phero^alpha)*(tempdist^beta)
    temprs <- rowSums(temp)
    probs[aa,,] <- temp/temprs
  }
}

