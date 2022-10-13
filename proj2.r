# Group 12 Practical 2
## Group Members: Xin Chen (s2340094), Yicong Sun (s2445309), Yihong Zhao (s2331659)
## Address of the github repo: 
## Contributions: 

# Function Pone for estimating the success probability of a single prisoner
# Inputs:
## n -- number of trials allowed for each prisoner (2n prisoners)
## k -- The prisoner's number
## strategy -- three difference strategies:
### strategy==1 -- Start at the box with their number on it, if the card number k is
### not their number then go to the kth box for their next choice, repeat this
### process until the end of the game
### strategy==2 -- Start at a random box, then repeat strategy 1
### strategy==3 -- Choose n boxes at random
## nreps.nreps -- number of simulations to estimate success probability, with
## default value 10000
# The function Pone returns the estimated success probability for a single prisoner
# (based on simulations)
Pone <- function(n,k,strategy,nreps.nreps=10000) {
  choice <- rep(NA,n) ## Initialise a vector to store choices of the prisoner
  count <- 1 ## Initialise a counter for number of simulations
  s <- 0 ## Initialise a counter for number of successes
  ## Strategy 1
  if (strategy==1) {
    while (count <= nreps.nreps) {
      ## A vector to store card numbers, cards were put into different boxes randomly
      ## with equal probabilities
      u <- sample(1:(2*n),size=2*n)
      choice[1] <- u[k] ## Start at the box with their number on it
      ## Repeat strategy 1 until the end of the game
      for (i in 2:n) {
        choice[i] <- u[choice[i-1]]
      }
      ## Count number of successes
      if (is.element(k,choice)) {
        s <- s+1
      }
      count <- count+1 ## Update counter
    }
    prob_one <- s/nreps.nreps ## Estimated success probability
    return(prob_one) ## Return the estimated probability
  }
  ## Strategy 2
  else if (strategy==2) {
    while (count <= nreps.nreps) {
      ## A vector to store card numbers, cards were put into different boxes randomly
      ## with equal probabilities
      u <- sample(1:(2*n),size=2*n)
      choice[1] <- sample(1:(2*n),size=1) ## Start at a random box
      ## Repeat strategy 1
      for (i in 2:n) {
        choice[i] <- u[choice[i-1]]
      }
      ## Count number of successes
      if (is.element(k,choice)) {
        s <- s+1
      }
      count <- count+1 ## Update counter
    }
    prob_one <- s/nreps.nreps ## Estimated success probability
    return(prob_one) ## Return the estimated probability
  }
  ## Strategy 3
  else {
    while (count <= nreps.nreps) {
      ## A vector to store card numbers, cards were put into different boxes randomly
      ## with equal probabilities
      u <- sample(1:(2*n),size=2*n)
      choice <- sample(1:(2*n),size=n) ## Choose n boxes at random
      ## Count number of successes
      if (is.element(k,choice)) {
        s <- s+1
      }
      count <- count+1 ## Update counter
    }
  }
  prob_one <- s/nreps.nreps ## Estimated success probability
  return(prob_one) ## Return the estimated probability
}

# Function Pall for estimating the probability of prisoners getting released,
# i.e., all prisoners finding their number
Pall <- function(n,strategy,nreps.nreps=10000) {
  result <- rep(NA,2*n) ## Initialise a vector to store results of prisoners
  count <- 1 ## Initialise a counter for number of simulations
  s <- 0 ## Initialise a counter for total number of successes
  while (count <= nreps.nreps) {
    for (k in 1:(2*n)) {
      result[k] <- Pone(n,k,strategy,nreps.nreps=1)
    }
    if (sum(result)==(2*n)) {
      s <- s+1
    }
    count <- count+1 ## Update counter
    result <- rep(NA,2*n) ## Reset the result vector
  }
  prob_all <- s/nreps.nreps ## Estimated success probability of all prisoners
  return(prob_all) ## Return the estimated probability
}
