suppressMessages(library(markovchain))

newchain <- function(unnormalisedmatrix, name, states=ifelse(sapply(colnames(unnormalisedmatrix), length, USE.NAMES=FALSE), colnames(unnormalisedmatrix), rownames(unnormalisedmatrix)))
  new("markovchain", name=name, states=states, transitionMatrix=unname(unnormalisedmatrix/rowSums(unnormalisedmatrix)))

# assume (deterministic) selection of the mean of the posterior distribution
bilm.transition.matrix.average <- function(N, alpha)
  new("markovchain", name=paste("BILM by averaging with alpha", alpha, sep="="), states=paste("x", 0:N, sep="="), transitionMatrix=t(sapply(0:N, function(x) dbinom(0:N, N, (x + alpha/2) / (N + alpha)))))

# assume (deterministic) maximum a posteriori calculation of theta
bilm.transition.matrix.map <- function(N, alpha)
  # catch abnormal modes: when x=0 (or x=N) then alpha<1 (or beta<1) and the
  # mode is simply 0 (or 1)
  new("markovchain", name=paste("BILM by averaging with alpha", alpha, sep="="), states=paste("x", 0:N, sep="="), transitionMatrix=t(sapply(0:N, function(x) dbinom(0:N, N, pmax(0, pmin(1, (x + alpha/2 - 1) / (N + alpha - 2)))))))

# assume sampling from the posterior Reali & Griffiths 2009 (p.321)
bilm.transition.matrix.sample <- function(N, alpha)
  new("markovchain", name=paste("BILM by sampling from the posterior with alpha", alpha, sep="="), states=paste("x", 0:N, sep="="), transitionMatrix=sapply(0:N, function(target)choose(N, target)*beta(0:N+target+alpha/2, 2*N-0:N-target+alpha/2) / beta(0:N + alpha/2, N - 0:N + alpha/2)))


# ps = binomial production probability in that state
binomialsampling.markov.matrix <- function(ps)
  t(sapply(ps, function(p) dbinom(0:(length(ps)-1), length(ps)-1, p)))
# transition probabilities out of a state organised in rows:
# x[i,j] := P(i -> j) where i, j in [1, length(ps)]
# also by definition rowSums(binomialsampling.markov.matrix(...)) == 1

repl.mut.matrix <- function (...)
  binomialsampling.markov.matrix(repl.mut.eq(...))

plotstationary <- function(markovchain, st=steadyStates(markovchain), absorbingstates=ifelse(missing(markovchain), 1, dim(st)[1]), xlab="x", names.arg=0:(dim(st)[2]-1), ...)
  barplot(st/absorbingstates, xlab=xlab, names.arg=names.arg, ...)

# compute the development of the Markov chain probability distribution for the
# specified number of iterations. initstate is a vector of length N and
# transitionmatrix an NxN matrix (with rows summing to 1)
markov.chain <- function(transitionmatrix, generations=2500, initstate=c(1, rep(0, dim(transitionmatrix)-1)), exactduration=FALSE) {
  if (exactduration) { # see below
    return(markov.chain.exactduration(transitionmatrix, generations))
  }
  out <- matrix(nrow=generations+1, ncol=length(initstate))
  out[1,] <- initstate
  for (i in 2:(generations+1))
    out[i,] <- out[i-1,] * transitionmatrix
  invisible(out)
}

# return per-generation completion probabilities for the given markov chain
completionprobabilities <- function(transitionmatrix, ...)
  diff(markov.chain(transitionmatrix, ...)[, dim(transitionmatrix)])

# return the mode and average duration of completions of this markov chain
completionstats <- function(transitionmatrix, ...) {
  data <- completionprobabilities(transitionmatrix, ...)
  c(mode=which.max(data), mean=weighted.mean(0:(length(data)-1), data))
}
#completionstats(bilm.transition.matrix.average(20, .05), 100)
#completionstats(bilm.transition.matrix.average(20, .05), 100, exactduration=TRUE)

plotcompletionprobabilities <- function(transitionmatrix, ...) {
  ps <- completionprobabilities(transitionmatrix, ...)
  par(pty="s", mfrow=c(1, 2))
  plot(ps, type="l", xaxs="i", yaxs="i", xlab="generation", ylab="probability of first transition completing")
  plot(cumsum(ps), type="l", xaxs="i", yaxs="i", ylim=0:1, xlab="generation", ylab="probability of having exhibited a transition")
  invisible(ps) # for postprocessing
}

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

# transition matrix fiddling
makestickytop <- function(m, N=dim(m)[1]-1)
  rbind(m[-(N+1),], c(rep(0, N), 1))

makestickybottom <- function(m, N=dim(m)[1]-1)
  rbind(c(1, rep(0, N)), m[-1,])

# construct markov chain probability matrix
markov.chain.exactduration <- function(transitionmatrix, generations) {
  # fixate once chain reaches N/N or reverts back to 0/N
  onetimetransitions <- transitionmatrix[] %>% makestickytop %>% makestickybottom %>% newchain
  # determine generation 2 start state (just after the initial pickup)
  transitionjustpickedup <- c(0, transitionmatrix[1,-1])
  data <- markov.chain(onetimetransitions, generations-1, initstate=transitionjustpickedup)
  # clear out all the chains that reverted back to the 0/N state
  data <- cbind(rep(0, generations), data[,-1])
  # prepend generation 0 initstate
  rbind(c(sum(transitionjustpickedup), rep(0, dim(transitionmatrix)-1)), data)
  # rowSums(data) isn't actually equal (and neither is
  # rowSums(apply.conditioning(data)), meaning the sticktop/bottom approach is
  # actually leeching? would actually *cutting* the state space by two states
  # be a more valid approach?
}

# stochastically generate chains that start off in state 0/n and that first
# reach state n/n after EXACTLY chainlength generations
generate.transitioning.chains.exact <- function(transitionmatrix, chainlength, numchains=1, initstate=0) {
  N <- dim(transitionmatrix)-1
  p <- markov.chain(transitionmatrix, chainlength)
  p <- p[chainlength+1,N+1] - p[chainlength,N+1]
  message("probability of first arriving in state N/N after exactly ", chainlength, " generations is at most ", p)
  message("this means a chain will be found just over once every ", round(1/p), " attempts")
  sapply(1:numchains, function(chain) {
    attempts <- 0
    while (TRUE) {
      attempts <- attempts+1
      pop <- vector("numeric", chainlength+1)
      pop[1] <- initstate
      for (i in 2:(chainlength+1)) {
        if (pop[i-1] == N)
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
  plot(c(0, nrow(data)-1), range(data), xlab="generation", ylab=paste("frequency of variant 1 (out of ", max(data), ")", sep=""), ...)
  for (i in 1:ncol(data))
    lines(0:(nrow(data)-1), data[,i])
}

# typesetting

formatalpha <- function(alpha)
  bquote(alpha/2 ~ "=" ~ .(alpha/2))

latextable <- function(m, caption=NULL, ...) {
  colnames(m) <- paste(substring(colnames(m), 1, 1), substring(colnames(m), 2), sep="'")
  if (!is.null(caption) & length(caption) == 1) {
    sep <- regexpr("[\\.,:]", caption)[1]
    if (sep != -1)
      caption <- c(caption, substr(caption, 1, sep-1))
  }
  xtable(m, caption=caption, ..., digits=4)
}
