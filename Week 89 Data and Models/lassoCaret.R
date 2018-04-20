library(caret)
IPEDS.df <- read.xlsx("IPEDS Data.xlsx",sheetIndex = 1);

# A model that uses all of the variables
RetLM <- lm(Retention.Rate~.,data=IPEDS.df);
RetLM$coefficients
summary(RetLM)

#---------------------------------------------
# Use the caret package and its built-in SA and
# GA algorithms to do feature selection on the 
# data set
# install.packages("caret")
library(caret)
ctrl <- safsControl(functions = caretSA)
# safs - simulated annealing feature selection
objSA <- safs(x = IPEDS.df[,2:10], 
            y = IPEDS.df[,1],
            iters = 10,
            safsControl = ctrl,
            method = "lm")
objSA

ctrl <- gafsControl(functions = caretGA,
                    verbose=TRUE)
# safs - genetic algorthithm feature selection
objGA <- gafs(x = IPEDS.df[,2:10], 
            y = IPEDS.df[,1],
            iters = 10,
            gafsControl = ctrl,
            method = "lm")
objGA

# The caret package's GA implementation is very slow.
# Define our own fitness function and use the GA package
# instead.
# install.packages("GA")
library(GA)

# Fitness Function
lmrsq <- function(x) {
  nindex <- which(x==1)+1
  neigh.df <- IPEDS.df[c(1,nindex)]
  RetLM <- lm(Retention.Rate~.,data=neigh.df)
  return(summary(RetLM)$adj.r.squared) 
}

gaRes <- ga(type="binary", nBits=9, fitness=lmrsq, popSize=10, maxiter=100, run=100)
gaRes@solution
summary(gaRes)
