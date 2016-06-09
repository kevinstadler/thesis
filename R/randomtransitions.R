# numerical simulation

# stochastically generate chains that start off in state 0/n and are in
# state n/n after chainlength generations
generate.transitioning.chains <- function(transitionmatrix, chainlength, numchains=1, initstate=0) {
  N <- dim(transitionmatrix)-1
  p <- markov.chain(transitionmatrix, chainlength)[chainlength+1,N+1]
  message("probability of being in state N/N after exactly ", chainlength, " generations is ", p)
  message("this means a chain will be found roughly every ", round(1/p), " attempts")
  sapply(1:numchains, function(chain) {
    attempts <- 0
    while (TRUE) {
      attempts <- attempts+1
      pop <- vector("numeric", chainlength+1)
      pop[1] <- initstate
      for (i in 2:(chainlength+1))
        pop[i] <- sample(0:N, 1, prob=transitionmatrix[1+pop[i-1],])
      if (pop[chainlength+1] == N) {
        message("found transition ", chain, " after ", attempts, " attempts")
        return(pop)
      }
    }
  })
}

# stochastically generate chains that start off in state 0/n and that first
# reach state n/n after EXACTLY chainlength generations
generate.transitioning.chains.exact <- function(transitionmatrix, chainlength, numchains=1, initstate=NULL) {
  N <- dim(transitionmatrix)-1
  p <- markov.chain.exactduration(transitionmatrix, chainlength)
  p <- p[chainlength+1,N+1] - p[chainlength,N+1]
  message("probability of first arriving in state N/N after exactly ", chainlength, " generations is at most ", p)
  message("this means a chain will be found roughly every ", round(1/p), " attempts")
  sapply(1:numchains, function(chain) {
    attempts <- 0
    while (TRUE) {
      attempts <- attempts+1
      pop <- vector("numeric", chainlength+1)
      pop[1] <- ifelse(is.null(initstate), sample(1:N, 1, prob=transitionmatrix[1,-1]), initstate)
      for (i in 2:(chainlength+1)) {
        if (pop[i-1] == N || pop[i-1] == 0)
          break
        pop[i] <- sample(0:N, 1, prob=transitionmatrix[1+pop[i-1],])
      }
      if (pop[chainlength+1] == N) {
        message("found transition ", chain, " after ", attempts, " attempts")
        return(pop)
      }
    }
  })
}

plotchains <- function(data, ...) {
  # plot circles for start+end conditioning
  plot(c(0, nrow(data)-1), range(data), xlab="generation", ylab="frequency", ...)
#  paste("frequency of variant 1 (out of ", max(data), ")", sep="")
  for (i in 1:ncol(data))
    lines(0:(nrow(data)-1), data[,i])
}
