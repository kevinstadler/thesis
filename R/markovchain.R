suppressMessages(library(markovchain))
library(magrittr)

newchain <- function(unnormalisedmatrix, name, states=ifelse(sapply(colnames(unnormalisedmatrix), length, USE.NAMES=FALSE), colnames(unnormalisedmatrix), rownames(unnormalisedmatrix)))
  new("markovchain", name=name, states=states, transitionMatrix=unname(unnormalisedmatrix/rowSums(unnormalisedmatrix)))

statenames <- function(N)
  as.character(0:N)

# ps = binomial production probability in that state
binomialsampling.markov.matrix <- function(ps)
  new("markovchain", name="Binomial sampling", states=statenames(length(ps)-1), transitionMatrix=t(sapply(ps, function(p) dbinom(0:(length(ps)-1), length(ps)-1, p))))
# transition probabilities out of a state are organised in rows:
# x[i,j] := P(i -> j) where i, j in [1, length(ps)]
# also by definition rowSums(binomialsampling.markov.matrix(...)) == 1

# assume (deterministic) selection of the mean of the posterior distribution
bilm.transition.matrix.average <- function(N, alpha)
  new("markovchain", name=paste("BILM by averaging with alpha", alpha, sep="="), states=statenames(N), transitionMatrix=t(sapply(0:N, function(x) dbinom(0:N, N, (x + alpha/2) / (N + alpha)))))

# assume (deterministic) maximum a posteriori calculation of theta
bilm.transition.matrix.map <- function(N, alpha)
  # catch abnormal modes: when x=0 (or x=N) then alpha<1 (or beta<1) and the
  # mode is simply 0 (or 1)
  new("markovchain", name=paste("BILM by averaging with alpha", alpha, sep="="), states=statenames(N), transitionMatrix=t(sapply(0:N, function(x) dbinom(0:N, N, pmax(0, pmin(1, (x + alpha/2 - 1) / (N + alpha - 2)))))))

# assume sampling from the posterior Reali & Griffiths 2009 (p.321)
bilm.transition.matrix.sample <- function(N, alpha)
  new("markovchain", name=paste("BILM by sampling from the posterior with alpha", alpha, sep="="), state=statenames(N), transitionMatrix=sapply(0:N, function(target)choose(N, target)*beta(0:N+target+alpha/2, 2*N-0:N-target+alpha/2) / beta(0:N + alpha/2, N - 0:N + alpha/2)))

# calculate the Wright-Fisher model mutation rate equivalent to the BILM's N
# and alpha. the bracketing in the paper itself is garbled and there's a '/'
# missing somewhere, the correct version of the transformation can be found on
# the top of page 5 of the supplementary material of Reali & Griffiths 2010
N.alpha.to.u <- function(N, alpha)
  alpha / (2 * (alpha + N))

# m0 and m1 are mutation probabilities of spontaneously generating variants.
# b is a replicator bias (fitness advantage of variant 1 over 0) in (-inf,inf)
repl.mut.eq <- function(N, alpha0=0, alpha1=alpha0, m0=N.alpha.to.u(N, alpha0), m1=N.alpha.to.u(N, alpha1), b=0, ks=0:N) {
  # apply selection (discrete replicator equation)
  # Wright-Fisher http://www.stats.ox.ac.uk/~etheridg/orsay/selection.pdf
  if (b < 0) {
    biasedks <- (N-ks) * (1 + abs(b))
    1 - (biasedks*(1-m0) + ks*m1 ) / (biasedks + ks)
  } else {
    biasedks <- ks * (1+b)
    (biasedks*(1-m0) + (N-ks)*m1 ) / (biasedks + N - ks)
  }
  # alternatively: Fermi-style selection coefficient: b in (-inf,inf)
  # (see also http://web.evolbio.mpg.de/~traulsen/paper/O5.pdf)
#  biasedks <- ks*exp(b)
  # limiting to [0,1] necessary because exp() causes overflow with negative b
#  pmax(0, pmin(1, ( biasedks*(1-m0) + (N-ks)*m1 ) / (biasedks + N - ks)))
}
#repl.mut.eq(5)
#repl.mut.eq(5, 0.1)
# a higher m0 means more pressure for x -> 0
#repl.mut.eq(5, 0.1, 0)

repl.mut.matrix <- function (...)
  binomialsampling.markov.matrix(repl.mut.eq(...))

plotstationary <- function(markovchain, st=steadyStates(markovchain), absorbingstates=ifelse(missing(markovchain), 1, dim(st)[1]), xlab="x", names.arg=0:(dim(st)[2]-1), ...)
  barplot(st/absorbingstates, xlab=xlab, names.arg=names.arg, ...)

# transition matrix fiddling
makestickytop <- function(m, N=dim(m)[1]-1)
  rbind(m[-(N+1),], c(rep(0, N), 1))

makestickybottom <- function(m, N=dim(m)[1]-1)
  rbind(c(1, rep(0, N)), m[-1,])

# compute the development of the Markov chain probability distribution for the
# specified number of iterations. initstate is a vector of length N and
# transitionmatrix an NxN matrix (with rows summing to 1)
markov.chain <- function(transitionmatrix, generations=2500, initstate=c(1, rep(0, dim(transitionmatrix)-1)), exactduration=FALSE) {
  if (exactduration) { # use actuation as initstate, see below
    return(markov.chain.exactduration(transitionmatrix, generations))
  }
  out <- matrix(nrow=generations+1, ncol=length(initstate))
  out[1,] <- initstate
  for (i in 2:(generations+1))
    out[i,] <- out[i-1,] * transitionmatrix
  invisible(out)
}

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

# return per-generation completion probabilities for the given markov chain
completionprobabilities <- function(transitionmatrix, exactduration=FALSE, ...) {
  data <- transitionmatrix %>%
  makestickytop %>% # only count first transitions
  newchain %>%
  # exactduration=TRUE causes markov.chain to use actuation as a start state
  markov.chain(exactduration=exactduration, ...)
  
  diff(data[, dim(transitionmatrix)[1]])
}

# return the mode and average duration of completions of this markov chain
completionstats <- function(transitionmatrix, ...) {
  data <- completionprobabilities(transitionmatrix, ...)
  c(mode=which.max(data), mean=weighted.mean(0:(length(data)-1), data))
}
#completionstats(bilm.transition.matrix.average(20, .05), 100)
#completionstats(bilm.transition.matrix.average(20, .05), 100, exactduration=TRUE)

plotcompletionprobabilities <- function(transitionmatrix, ylim=NULL, ...) {
  ps <- completionprobabilities(transitionmatrix, ...)
  tightmargin(pty="s", mfrow=c(1, 2))
  plot(ps, type="l", xaxs="i", yaxs="i", xlab="generation", ylab="probability of first transition completing", main="(i)", ylim=ylim)
  plot(cumsum(ps), type="l", xaxs="i", yaxs="i", ylim=0:1, xlab="generation", ylab="probability of having exhibited a transition", main="(ii)")
  invisible(ps) # for postprocessing
}

# numerically compute the probability of a chain succeeding from the given
# initial state (specified as an index of the transition/state matrix)
successprobability <- function(transitionmatrix, initstate, precision=.99, stepsize=500) {
  transitionmatrix %<>% makestickytop %>% makestickybottom %>% newchain
  population <- rep(0, dim(transitionmatrix))
  population[initstate] <- 1
  while (population[1] + population[length(population)] < precision)
    population <- markov.chain(transitionmatrix, generations=stepsize, initstate=population)[stepsize,]
  population[dim(transitionmatrix)]
}

# merge the absorbingstates of the transition matrix together into one absorbing state that only transitions to itself
mergeabsorbingstates <- function(m, absorbingstates) {
  if (length(absorbingstates) == 1)
    return(m)
  # take subset of transition matrix, only leave one state for all states
  ma <- m[-absorbingstates[-1], -absorbingstates[-1]]
  # merge (sum) the final absorbingrows columns together for every row
  ma[,absorbingstates[1]] <- rowSums(m[-absorbingstates[-1], absorbingstates])
  # make new merged absorbing state only transition to itself
  ma[absorbingstates[1],] <- 0
  ma[absorbingstates[1], absorbingstates[1]] <- 1
  return(ma)
}

plotcompletionprobabilitiesperstart <- function(transitionmatrix, maxmomentum=0, nstatestomerge=1 + 2*maxmomentum, freqs=0:(dim(transitionmatrix)/nstatestomerge-1), add=FALSE, ...) {
  # squash top/bottom states together into two absorbing states
  m <- mergeabsorbingstates(transitionmatrix[], 1:nstatestomerge)
  transitionmatrix <- newchain(mergeabsorbingstates(m, (nrow(m)-nstatestomerge+1):nrow(m)))
  if (maxmomentum == 0)
    startindices <- 1+freqs
  else
    # indices of states which have positive momentum
    startindices <- c(1, 1+freqs[-c(1, length(freqs))]*3, dim(transitionmatrix))
  ps <- sapply(startindices, function(i) successprobability(transitionmatrix, i))
  if (add)
    points(freqs, ps, ...)
  else
    plot(freqs, ps, xlab="initial frequency", ylab="probability of diffusion", ylim=0:1, ...)
    # abline(a=?, b=0, lty=3)
}

#graylevels=round(0.75*length(hmmargs$hmm$States)) / round(0.75*ncol(data))
plotchain <- function(data, xlab="generation", ylab="frequency", graylevels=24, ...)
  image(data, x=0:(nrow(data)-1), y=0:(ncol(data)-1), xlab=xlab, ylab=ylab, col=gray(graylevels:0/graylevels), breaks=c(0, 1.5^(-graylevels:0))*max(data), ...)


# typesetting

formatalpha <- function(alpha)
  bquote(alpha/2 ~ "=" ~ .(alpha/2))

latextable <- function(m, caption=NULL, floating.environment="table", ...) {
  rownames(m) <- paste("x", rownames(m), sep="=")
  colnames(m) <- paste("x'", colnames(m), sep="=")
  if (!is.null(caption) & length(caption) == 1) {
    sep <- regexpr("[\\.,:]", caption)[1]
    if (sep != -1)
      caption <- c(caption, substr(caption, 1, sep-1))
  }
  print(xtable(m, caption=caption, ..., digits=4), floating.environment=floating.environment)
}
