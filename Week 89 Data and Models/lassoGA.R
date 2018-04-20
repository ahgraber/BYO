library(xlsx);
IPEDS.df <- read.xlsx("IPEDS Data.xlsx",sheetIndex = 1);

# A model that uses all of the variables
RetLM <- lm(Retention.Rate~.,data=IPEDS.df);
RetLM$coefficients
summary(RetLM)

#------------------------------------------
# Start of Genetic Algorithm
# Naive approach for Lasso problem with 
# binary feature selection from the IPEDS
# school data set

# Create 6 random initial solutions
curbin = matrix(0,6,9)
curAR = rep(0.0,6)
for (i in 1:6) {
  rcur <- runif(9,0,1);
  curbin[i,] <- ifelse(rcur<0.5,0,1)
  curindex <- which(curbin[i,]==1)+1
  cur.df <- IPEDS.df[c(1,curindex)];
  
  # Run the model only on this dataframe
  RetLM <- lm(Retention.Rate~.,data=cur.df);
  RetLM$coefficients
  summary(RetLM)
  curAR[i] <- summary(RetLM)$adj.r.squared
  bestLM <- curbin[i,]
  bestAR <- curAR[i]
}

# Set parameters
gens <- 100;

for (i in 1:gens) {
  
    # Pick the two fittest individuals as parents
    sbin <- sort(curAR,index.return=TRUE,decreasing=TRUE)
    p1 <- sbin$ix[1]
    p2 <- sbin$ix[2]
    basechild <- (curbin[p1,]+curbin[p2,])/2
    
    # Child 1
    ## Crossover
    crossrand <- runif(9,0,1)
    child1 <- ifelse(basechild==0.5,crossrand>0.5,basechild)
    ## Mutation
    mutaterand <- runif(9,0,1)
    child1 <- ifelse(mutaterand>0.9,1-child1,child1)

    # Child 2
    ## Crossover
    crossrand <- runif(9,0,1)
    child2 <- ifelse(basechild==0.5,crossrand>0.5,basechild)
    ## Mutation
    mutaterand <- runif(9,0,1)
    child2 <- ifelse(mutaterand>0.9,1-child2,child2)

    # Replace the bottom two fittest individuals
    # with the children
    rep3 <- sbin$ix[5]
    rep4 <- sbin$ix[6]
    curbin[rep3,] <- child1
    curbin[rep4,] <- child2
    
    # Record the children
    nindex <- which(child1==1)+1
    neigh.df <- IPEDS.df[c(1,nindex)];
    RetLM <- lm(Retention.Rate~.,data=neigh.df);
    RetLM$coefficients
    summary(RetLM)
    neighAR <- summary(RetLM)$adj.r.squared
    if (neighAR > bestAR) {
        bestAR <- neighAR
        bestLM <- child1
    }
    nindex <- which(child2==1)+1
    neigh.df <- IPEDS.df[c(1,nindex)];
    RetLM <- lm(Retention.Rate~.,data=neigh.df);
    RetLM$coefficients
    summary(RetLM)
    neighAR <- summary(RetLM)$adj.r.squared
    if (neighAR > bestAR) {
      bestAR <- neighAR
      bestLM <- child2
    }
}

bestAR
bestLM
