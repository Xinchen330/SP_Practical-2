# Group 12 Practical 2
## Group Members: Xin Chen (s2340094)
##                Yicong Sun (s2445309)
##                Yihong Zhao (s2331659)
## Address of the github repo: 
## Contributions: 

# Function trial_outcome to determine whether an individual prisoner 
# can find his/her card number following three different strategies
# Inputs:
## n -- number of trials allowed for each prisoner (2n prisoners)
## k -- The prisoner's number
## strategy -- three difference strategies:
### strategy==1 -- Start at the box with their number on it, if the card number 
### k is not their number then go to the kth box for their next choice, repeat 
### this process until the end of the game
### strategy==2 -- Start at a random box, then repeat strategy 1
### strategy==3 -- Choose n boxes at random
## cards -- an (1x2n) vector with shuffled card numbers
# The function trial_outcome returns 1 if the prisoner finds the number, and
# returns 0 if he/she fails
trial_outcome <- function(n,k,strategy,cards) {
  choice <- rep(NA,n) ## Initialise a vector to store choices of the prisoner
  ## Strategy 1
  if (strategy==1) {
    choice[1] <- cards[k] ## Start at the box with their number on it
    ## Repeat strategy 1 until the end of the game
    for (i in 2:n) {
      choice[i] <- cards[choice[i-1]]
    }
  }
  ## Strategy 2
  else if (strategy==2) {
    choice[1] <- sample(1:(2*n),size=1) ## Start at a random box
    for (i in 2:n) {
      choice[i] <- cards[choice[i-1]]
    }
  }
  ## Strategy 3
  else {
    choice <- sample(1:(2*n),size=n) ## Choose n boxes at random
  }
  ## Determine whether the trial is successful. 
  ## The trial was successful if the prisoner picked out k 
  ## (so that k is an element of the vector choice)
  if (is.element(k,choice)) {
    outcome <- 1
  }
  else {
    outcome <- 0
  }
  return (outcome)
}

# Function Pone for estimating the success probability of a single prisoner
# Inputs: Arguments (n,k,strategy) are the same as trial_outcome function
## nreps -- Number of simulations required to estimate success probability, 
## with default value 10000
# The function Pone returns the estimated success probability for a single 
# prisoner (based on simulations)
Pone <- function(n,k,strategy,nreps=10000) {
  count <- 1 ## Initialise a counter for number of simulations
  s <- 0 ## Initialise a counter for number of successes
  ## Create a vector to store card numbers, cards were put into different 
  ## boxes randomly with equal probabilities
  cards <- sample(1:(2*n),size=2*n)
  while (count <= nreps) {
    s <- s+trial_outcome(n,k,strategy,cards) # Count number of successes
    count <- count+1 ## Update counter
    # Reshuffle cards at the end of each simulation
    cards <- sample(1:(2*n),size=2*n)
  }
  prob_one <- s/nreps ## Estimated success probability
  return(prob_one) ## Return the estimated probability
}

# Function Pall for estimating the success probability of all prisoners,
# i.e., all prisoners find their numbers
# Inputs: Arguments are the same as Pone function, except k
# The function Pone returns the estimated the joint success probability for
# all prisoners
Pall <- function(n,strategy,nreps=10000) {
  count <- 1 ## Initialise a counter for number of simulations
  s <- 0 ## Initialise a counter for number of successes
  ## Create a vector to store card numbers, cards were put into different 
  ## boxes randomly with equal probabilities
  cards <- sample(1:(2*n),size=2*n)
  while (count <= nreps) {
    result <- rep(NA,2*n) ## Initialise a vector to store results of prisoners
    for (k in 1:(2*n)) {
      result[k] <- trial_outcome(n,k,strategy,cards)
    }
    ## All prisoners find their numbers if all elements of the result vector 
    ## are 1, in which case the sum of the elements would be 2n
    if (sum(result)==(2*n)) {
      s <- s+1
    }
    count <- count+1 ## Update counter
    cards <- sample(1:(2*n),size=2*n) # Reshuffle cards
  }
  prob_all <- s/nreps ## Estimated success probability of all prisoners
  return(prob_all) ## Return the estimated probability
}

card_loop <- function(cards) {
  ## Initialize a vector to record whether a card number has been picked out
  status <- rep(0,length(cards))
  l_lengths <- c() ## A vector to store loop lengths
  choice <- rep(0,length(cards))
  while (is.element(0,status)) {
    k <- min(which(status==0))
    choice[1] <- cards[k]
    if (choice[1]==k) {
      l <- choice[which(choice!=0)]
    }
    else {
      for (i in 2:length(cards)) {
        if (cards[choice[i-1]]!=k) {
          choice[i] <- cards[choice[i-1]]
        }
        else {
          choice[i] <- k
          l <- choice[which(choice!=0)]
          break
        }
      }
    }
    status[l] <- 1 ## Update status
    l_lengths <- append(l_lengths,length(l))
    choice <- rep(0,length(cards)) # Reset the choice vector
  }
  return (l_lengths)
}
