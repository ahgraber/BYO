library(xlsx);
setwd("~/Documents/_SCHOOL/_Drexel/OPR 620 - Operations Research I/Notes/Week 8/Week 8 Data and Models")
IPEDS.df <- read.xlsx("IPEDS Data.xlsx",sheetIndex = 1);

# A model that uses all of the variables
RetLM <- lm(Retention.Rate~.,data=IPEDS.df);
RetLM$coefficients
summary(RetLM)

#------------------------------------------
# Start of Simulated Annealing
# Naive approach for Lasso problem with binary feature selection from the 
# IPEDS school data set

# Create a random initial solution
rcur <- runif(9,0,1);
curbin <- ifelse(rcur<0.5,0,1)
curindex <- which(curbin==1)+1 # add one because dependent variable is col 1
cur.df <- IPEDS.df[c(1,curindex)];

# Run the model only on this dataframe
RetLM <- lm(Retention.Rate~.,data=cur.df);
RetLM$coefficients
summary(RetLM)
# curAR <- summary(RetLM)$adj.r.squared
curAR <- 100*summary(RetLM)$adj.r.squared - sum(curbin)  
  # for sparser solution:
  # max R2 while minimizing variables included
bestLM <- curbin
bestAR <- curAR
  
# Set parameters
T <- 10
r <- 0.9
L <- 5
frozen <- 0.001

while ( T > frozen ) {

  for (i in 1:L) {  
    # Pick a random index to modify
    rind <- floor(runif(1,1,10))  # pick random integer (column to pivot)
    nbin <- curbin                # create next binary vector
    nbin[rind] <- 1-nbin[rind]    # next binary is current with the column value swapped
    nindex <- which(nbin==1)+1
    neigh.df <- IPEDS.df[c(1,nindex)];
    
    # Run the model only on the neighbor dataframe
    RetLM <- lm(Retention.Rate~.,data=neigh.df);
    RetLM$coefficients
    summary(RetLM)
    # neighAR <- summary(RetLM)$adj.r.squared
    neighAR <- 100*summary(RetLM)$adj.r.squared - sum(nbin)
      # for sparser solution:
      # max R2 while minimizing variables included
    
    # If neighbor's adjusted R-squared is strictly better than the current one, 
    # accept the neighbor as the new solution.  
    # Otherwise, draw a random number and accept it with some probability.
    # print(neighAR)
    if (neighAR > curAR) {
      curAR <- neighAR
      curbin <- nbin
      if (neighAR > bestAR) {
        bestAR <- neighAR
        bestLM <- nbin
      }
    } else {
      delta <- curAR-neighAR
      print(delta)
      if (runif(1,0,1) < exp( -delta/T) ) {
        # probability < threshold-for-pivot
        curAR <- neighAR
        curbin <- nbin
      }
    }
  }
  T <- r*T

}
bestAR
bestLM
