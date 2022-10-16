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

# Function card_loop for finding the number of loops and the length of each
# loop in a sequence of randomly shuffled cards
# Inputs:
## cards -- A sequence of 2n randomly shuffled cards
# The function card_loop returns a vector l_length which contains loop lengths
# of the given card sequence
card_loop <- function(cards) {
  ## Initialize a vector to record whether a card number has been picked out
  ## Initial state was set to be 0, once a card has been picked out, its state
  ## will be switched to 1
  state <- rep(0,length(cards))
  l_lengths <- c() ## Initialise a vector to store loop lengths
  ## Initialise a vector to store cards being picked out
  ## The length was set to be 2n mainly for speed considerations
  choice <- rep(0,length(cards))
  ## While there are cards yet to be picked out, we should keep counting loops
  while (is.element(0,state)) {
    ## Start from the smallest card number that hasn't been selected
    k <- min(which(state==0))
    choice[1] <- cards[k] ## Make the first choice
    ## If the first card happens to be k, the loop length will be 1
    if (choice[1]==k) {
      ## The loop is given by the non-zero elements of the choice vector
      l <- choice[which(choice!=0)]
    }
    ## Otherwise, we should keep drawing cards using strategy 1
    else {
      for (i in 2:length(cards)) {
        ## Repeating strategy 1 until we find the card number k
        if (cards[choice[i-1]]!=k) {
          choice[i] <- cards[choice[i-1]]
        }
        ## If our next card happens to be k
        else {
          choice[i] <- k ## Update the choice vector
          ## Pick out the loop by extracting non-zero elements of the choice 
          ## vector, and break the while loop
          l <- choice[which(choice!=0)]
          break
        }
      }
    }
    state[l] <- 1 ## Update states
    l_lengths <- append(l_lengths,length(l)) ## Record the loop lengths
    choice <- rep(0,length(cards)) # Reset the choice vector
  }
  return (l_lengths) ## Return loop lengths
}

# Function dloop for estimating the success probability of each loop length from
# 1 to 2n occurring at least once in a random shuffling
# Inputs: Arguments are the same as Pall function, except strategy
# The function dloop returns 2n-vector representing the probability of each loop
# length from 1 to 2n occurs at least once
dloop<-function(n,nreps=10000){
  loop<-rep(0,2*n) ## Initialise a vector to store the frequency of loop length
  prob<-rep(0,2*n) ## Initialise a vector to store the probability
  count<-1 ## Initialise a counter for number of simulations
 while (count<=nreps){
    u<-sample(1:(2*n),size = 2*n)
    ## Use function card_loop to find the lengths of all the loops in this
    ## simulation.
    len<-card_loop(u) 
    ## Use function unique to find the unique loop length
    unique_len<-unique(len)
    ## Count the length of the loop in this simulation
    for (j in 1:length(unique_len)){
      loop[unique_len[j]]=loop[unique_len[j]]+1
    }
    count<-count+1 ## Update counter
 }
  prob=loop/nreps  ## Estimated probability
  return(prob) ##Return the estimated probability
}
