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
markov.chain <- function(transitionmatrix, generations=3000, initstate=c(1, rep(0, dim(transitionmatrix)-1))) {
  out <- matrix(nrow=generations+1, ncol=length(initstate))
  out[1,] <- initstate
  for (i in 2:(generations+1))
    out[i,] <- out[i-1,] * transitionmatrix
  invisible(out)
}

plotcompletionprobabilities <- function(chain, ...) {
  completionprobabilities <- diff(markov.chain(chain, ...)[,dim(chain)])
  par(pty="s", mfrow=c(1, 2))
  plot(completionprobabilities, type="l", xaxs="i", yaxs="i", xlab="generation", ylab="probability of new transition terminating")
  plot(cumsum(completionprobabilities), type="l", xaxs="i", yaxs="i", ylim=0:1, xlab="generation", ylab="probability of having exhibited a transition")
  invisible(completionprobabilities) # for postprocessing
}

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
