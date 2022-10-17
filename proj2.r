# Group 12 Practical 2
## Group Members: Xin Chen (s2340094)
##                Yicong Sun (s2445309)
##                Yihong Zhao (s2331659)
## Address of the github repo: https://github.com/Xinchen330/SP_Practical-2.git
## Contributions: 

# Overview:
## Basic set-up: Suppose that there are 2n prisoners, with each prisoner 
## being labelled 1 to 2n. There is a room with 2n boxes. A total number 
## of 2n cards with a unique number between 1 to 2n printed are randomly 
## shuffled and placed in one of the boxes. Each prisoner is expected to 
## find their number with a maximum of n trials. If all 2n prisoners 
## successfully find their numbers, they are freed.
## The main purpose of this R script is to conduct a simulation study to 
## estimate the probability of both individual and joint success 
## probabilities of prisoners, under three different strategies (see below). 
## In fact, loops occur in the sequence of opened boxes, and we will see 
## that all prisoners will succeed if all loop lengths within the sequence 
## are less than or equal to n.

# Function trial_outcome to determine whether an individual prisoner 
# can find their card number following three different strategies
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
# returns 0 if they fail
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

## We provide example simulations below using n=5 and n=50. All simulations 
## repeated nreps=10000 times. When we estimate the individual success 
## probability, we assume that k=1 (the number one prisoner). However, this 
## simulation result should hold for all 2n prisoners. Note that the estimated
## probability might be slightly different if we re-run the code (since orders 
## of the randomly shuffled cards are not guaranteed to be the same), but the 
## probabilities should be similar.
# Example code using n=5
Pone(5,1,1)
Pone(5,1,2)
Pone(5,1,3)
## outcome: Strategy 1: 0.4998
## Strategy 2: 0.3977
## Strategy 3: 0.4957
Pall(5,1)
Pall(5,2)
Pall(5,3)
## outcome: Strategy 1: 0.3437
## Strategy 2: 1e-04
## Strategy 3: 0.0014

# Example code using n=50
Pone(50,1,1)
Pone(50,1,2)
Pone(50,1,3)
## outcome: Strategy 1: 0.5078
## Strategy 2: 0.3682
## Strategy 3: 0.4967
Pall(50,1)
Pall(50,2)
Pall(50,3)
## outcome: Strategy 1: 0.305
## Strategy 2: 0
## Strategy 3: 0

# Comments
## As we can see from our simulation above, the individual success probability 
## is almost the same for strategies 1 and 3, around 50%. The success 
## probability under strategy 2 is marginally lower than other strategies. 
## However, we can see that the joint success probability under strategy 1 
## is unexpectedly high, with around 35% with n=5 and 31% with n=50. In 
## contrast, prisoners are unlikely to succeed under the other two strategies. 
## This result is not surprising if we realise that prisoners' successes are 
## not independent events under strategy 1. Under strategy 3, prisoners' 
## successes are independent events, so the joint success probability is 
## (1/2)^n, which goes to 0 as n increases. However, as we will see in the 
## following simulation, loops occur in the sequence of opened boxes. All 
## prisoners will succeed if there is no loop longer than n.

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
    for (i in 1:length(unique_len)){
      loop[unique_len[i]]=loop[unique_len[i]]+1
    }
    count<-count+1 ## Update counter
  }
  prob=loop/nreps  ## Estimated probability
  return(prob) ##Return the estimated probability
}

# Example code using dloop to estimate the probabilities for n = 50 
# using dloop
n <- 50 ## Maximum trials allowed
## Probabilities that each loop length (from 1 to 2n) occurs at least once
dloop(n)

## Assessing the probability that all loop lengths are not greater than 50
s <- 0 ## Initialise a counter for number of successes
nreps <- 10000 ## Estimate the probability by 10000 simulations
count <- 1 ## Initialise a counter for number of simulations
while (count <= nreps) {
  ## Use dloop function with nreps=1 will return the loop lengths in one set
  ## of randomly shuffled cards, we can pick out lengths by extracting 
  ## non-zero elements
  ## Succeed if the maximum loop length is no greater than n (50 in this case)
  if (max(which(dloop(n,nreps=1)!=0)) <= n){
    s <- s+1
  }
  count <- count +1 ## Update counter
}
s/nreps ## Probability that there is no loop longer than 50
