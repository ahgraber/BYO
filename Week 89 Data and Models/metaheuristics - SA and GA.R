### OPR 620 - metaheuristics with kmeans

#-- Initialize ------------------------------------------------------------------------------------

library(tidyverse)
library(xlsx)
location <- "~/Documents/_SCHOOL/_Drexel/OPR 620 - Operations Research I/Notes/Week 89/Week 8 Data and Models"

#-- Fitness function ------------------------------------------------------------------------------
### fitness function: within-cluster sum-squared distances of provided cluster
  # df is all rows in a given cluster
  fitness <- function(df) {
    deviation <- function(x) {(mean(x) - x)}
    ss <- sum( apply(df, 2, deviation) ^2 )
    return (ss)
  }

#-- Cluster function ------------------------------------------------------------------------------
### Generates cluster identity for a single observation

  assignment <- function(k) {
    binary <- 0
    while (sum(binary) != 1) {
      randoms <- runif(k,0,1)
      binary <- ifelse(randoms<0.5, 0, 1)
    }
    rm(randoms)
    return(binary)
  }


#-- Data exploration ------------------------------------------------------------------------------

  # Read data in
  # data <- read.xlsx(paste(location,"IPEDS Data.xlsx", sep="/"),sheetIndex=1)
  data <- iris[,-c(1,2,5)]


  # scale data
  data <- as.data.frame(scale(data))

### run kmeans model for comparison
  # how many clusters?
  errors <- list()
  for (k in 1:10) {
    model <- kmeans(data, k, iter.max=100, nstart=100)
    errors <- append(errors, model$tot.withinss)
  }
  plot(1:10, errors)
  # looks like 3 is a good initial attempt for number of clusters

  k <- 3
  model <- kmeans(data, k, iter.max=100, nstart=100)
  # summary(model)
  model$totss           # 950
  model$tot.withinss    # 437.6343
  table(model$cluster)  # 22 / 18 / 56

  # find fitness of optimal solution from kmeans function (for comparison)
  k <- max(model$cluster) # redundant, but good check
  withinss <- vector(length=k)
  for (i in 1:k) {
    df <- data[c(which(model$cluster==i)),]
    withinss[i] <- fitness(df)
  }
  optErr <- sum(withinss)
  cat("Optimal error is: ", optErr)



#-- Simulated Annaling with kmeans ----------------------------------------------------------------

### Create a random initial solution of cluster assignments; one per data row.
  rows <- dim(data)[1]
  curClust <- as.data.frame(t(replicate(rows, assignment(k))))

### Save fitness of initial solution
  withinss <- vector(length=k)
  for (i in 1:k) {
    df <- data[c(which(curClust[,i]==1)),]
    withinss[i] <- fitness(df)
  }

  curErr <- sum(withinss)
  cat("Starting error is: ", curErr)
  bestClust <- curClust
  bestErr <- curErr

### Set parameters for Simulated Annealing
  Tmp <- 20   # initial temperature
  r <- 0.95    # cooling ratio
  L <- 7      # internal loops
  fz <- 0.001 # frozen threshold

  # # how many outer loops is this?
  # count <- 0
  # while (Tmp > fz) {
  #   count <- count+1
  #   Tmp <- r*Tmp
  # }

### Loop through annealing algorithm
  while ( Tmp > fz ) {
    for (i in 1:L) {
      index <- floor(runif(1,0,rows)) + 1 # Pick a random row to reassign
      newClust <- curClust                # Save current cluster solution to modify
      newClust[index,] <- assignment(k)    # Generate and save new assignment

      # Find fitness of neighbor
        withinss <- vector(length=k)
        for (i in 1:k) {
          df <- data[c(which(newClust[,i]==1)),]
          withinss[i] <- fitness(df)
        }
        nErr <- sum(withinss)

      # Search function
      # print(nErr)
      if (nErr < curErr) {                # If neighbor's error is better than the current one,
        curErr <- nErr                    # accept the neighbor as the new solution.
        curClust <- newClust

        if (nErr < bestErr) {
          bestErr <- nErr
          bestClust <- newClust
        } # end if

      } # end if
      else {                              # Otherwise, accept neighbor with some probability
        delta <- nErr-curErr              # based on a ratio of the delta and system temp
        # print(delta)
        if (runif(1,0,1) < exp( -delta/T) ) {
          # probability < threshold-for-pivot
          curErr <- nErr
          curClust <- newClust
        } # end if
      } # end else

    } # end for (L inner loops)

    Tmp <- r*Tmp                          # decrease system temperature by the ratio

  } # end while (system cooling)


### Report results
  cat("Least error is: ", bestErr)


#-- Genetic Algorithm with kmeans -----------------------------------------------------------------

### Create 6 random initial solutions (i.e., starting population)
  rows <- dim(data)[1]
  parents <- 6
  curPop <- replicate(parents, replicate(rows, assignment(k)))
    # curPop[x,y,z] => 1:k, 1:rows, # parents

### check and save fitness of starting population
  curErr <- vector(length=parents)
  for (j in 1:parents) {        # for each parent

    withinss <- vector(length=k)
    for (i in 1:k) {
      df <- data[c(which(curPop[i,,j]==1)),]
      withinss[i] <- fitness(df)
    }
    curErr[j] <- sum(withinss)  # find fitness

  }

### print starting point
  cat("Starting error is: ", sort(curErr, index.return=TRUE)$x[1])

### Set parameters for Genetic Algorithm
  gens <- 200;

### Loop through evolution
  for (i in 1:gens) {

    # Pick the two fittest individuals as parents
    sortErr <- sort(curErr, index.return=TRUE)
    p1 <- sortErr$ix[1]
    p2 <- sortErr$ix[2]
    baseChild <- (curPop[1:k,,p1]+curPop[1:k,,p2])/2

    # Save the current best solution
    withinss <- vector(length=k)
    for (i in 1:k) {
      df <- data[c(which(curPop[i,,p1]==1)),]
      withinss[i] <- fitness(df)
    }
    bestErr <- sum(withinss)  # find fitness
    bestClust <- curPop[1:k,,p1]

   ## Child 1
    # Crossover
    crossover <- runif(rows, 0,1)   # generate random chances if parent genes don't agree
    child1 <- ifelse(baseChild==0.5,
                     ifelse(crossover>0.5, curPop[1:k,,p1], curPop[1:k,,p2]),
                     baseChild)     # assign one parent's genes to the disagreed chromosome

    # Mutation
    mutation <- floor(runif(1,0,rows)) + 1          # generate random chance to mutate
    new <- assignment(k)                            # Randomly generate new assignments
    child1[,index] <- ifelse(mutation < 0.1, new, child1[,index])
      # if small probability, swap in new assignment


   ## Child 2
    # Crossover
    crossover <- runif(rows, 0,1)   # generate random chances if parent genes don't agree
    child2 <- ifelse(baseChild==0.5,
                     ifelse(crossover>0.5, curPop[1:k,,p1], curPop[1:k,,p2]),
                     baseChild)     # assign one parent's genes to the disagreed chromosome
    # Mutation
    mutation <- floor(runif(1,0,rows)) + 1          # generate random chance to mutate
    new <- assignment(k)                            # Randomly generate new assignments
    child2[,index] <- ifelse(mutation < 0.1, new, child2[,index])
      # if small probability, swap in new assignment

    # Replace the bottom two fittest individuals with the children
    p5 <- sortErr$ix[5]
    p6 <- sortErr$ix[6]
    curPop[1:k,,p5] <- child1
    curPop[1:k,,p6] <- child2

    # Check fitness of children
    withinss <- vector(length=k)
    for (i in 1:k) {
      df <- data[c(which(curPop[i,,p5]==1)),]
      withinss[i] <- fitness(df)
    }
    child1Err <- sum(withinss)

    withinss <- vector(length=k)
    for (i in 1:k) {
      df <- data[c(which(curPop[i,,p6]==1)),]
      withinss[i] <- fitness(df)
    }
    child2Err <- sum(withinss)


    # Save the current best solution
    if (child1Err < child2Err) {
      if (child1Err < bestErr) {
          bestErr <- child1Err
          bestClust <- child1
      } # end if
    } # end if
    else {
      if (child2Err < bestErr) {
          bestErr <- child2Err
          bestClust <- child2
      } # end if
    } # end else

  } # end for

### Report results
  cat("Least error is: ", bestErr)
  sum(bestClust[1,])
  sum(bestClust[2,])
  sum(bestClust[3,])
