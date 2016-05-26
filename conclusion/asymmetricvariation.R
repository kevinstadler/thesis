# a Markov model of the replicator-mutator equation extended with momentum

# ps = binomial production probability in that state
binomialsampling.markov.matrix <- function(ps)
  t(sapply(ps, function(p) dbinom(0:(length(ps)-1), length(ps)-1, p)))
# transition probabilities out of a state organised in rows:
# x[i,j] := P(i -> j) where i, j in [1, length(ps)]
# also by definition rowSums(binomialsampling.markov.matrix(...)) == 1

# basic BILM transition matrix
bilm.eq <- function(N, alpha)
  (0:N + alpha/2) / (N + alpha)
# create the (N+1)x(N+1) Markov chain transition matrix for the given N and alpha
bilm.transition.matrix <- function(N, alpha)
  t(sapply(0:N, function(n)dbinom(0:N, N, (n + alpha / 2) / (N + alpha))))

# m0 and m1 are mutation probabilities of spontaneously generating variants
# sigma is a replicator bias (fitness advantage of variant 2 over 1)
repl.mut.eq <- function(N, m0=0, m1=m0, sigma=0) {
  # basic BILM transition matrix
  ps <- 0:N/N
  # apply selection (discrete replicator equation)
  # p.3 http://arxiv.org/pdf/0911.1763.pdf
  # equiv. Wright-Fisher http://www.stats.ox.ac.uk/~etheridg/orsay/selection.pdf

  # original formulation
#  ps <- (ps+sigma) / (1+sigma*ps)
  # this one is current but the impact of b is not symmetric around 0
#  ps <- ps / (1 + b*(ps-1))
  b <- sigma/2 # s in (2,2)
  ps <- ps*(1+b) / (1 + b*(2*ps-1))
  # s in (-inf,+inf) f(x1)=exp(s), f(x2) = exp(-s)
  # could also make it multiplicative, i.e.
  # TODO use Fermi function: function(x)1/(1+exp(-x)) (from Skyrms and also Harper: https://johncarlosbaez.wordpress.com/2015/03/24/stationary-stability-in-finite-populations/

  # http://myxo.css.msu.edu/ecoli/srvsrf.html
  # relative W = 1 + selection rate r / average malthusian growth

  # apply mutation
  ps*(1-m0) + (1-ps)*m1
}
#repl.mut.eq(5)
#repl.mut.eq(5, 0.1)
# a higher m0 means more pressure for x -> 0
#repl.mut.eq(5, 0.1, 0)

#repl.mut.eq(5)
# effect of sigma symmetric
#repl.mut.eq(5, sigma=1) + rev(repl.mut.eq(5, sigma=-1))

repl.mut.matrix <- function (...)
  binomialsampling.markov.matrix(repl.mut.eq(...))
#repl.mut.matrix(3, 0.1)

# for momentum matrix, explode state space from N+1 to 3*(N+1)
# column order of m: x0/m-1, x0/m0, x0/m1; x1/m-1, x1/m0, x1/m1; ...
# pass a sourcemomentum/targetmomentum matrix
momentum.matrix.pos <- function(sourcemomentum, targetmomentum, maxmomentum=1) {
  Nm <- 1+2*maxmomentum
  N <- max(nrow(sourcemomentum), nrow(targetmomentum))
  # column (target) offsets
#  col <- matrix(rep(1+3*0:(N-1), each=N), nrow=N) + targetmomentum
#  3*N*col + rep(2+3*0:(N-1), N)+sourcemomentum
  col <- matrix(rep(maxmomentum+Nm*0:(N-1), each=N), nrow=N) + targetmomentum
  Nm*N*col + rep(1+maxmomentum+Nm*0:(N-1), N)+sourcemomentum
}
# targetmomentum is working
#x[momentum.matrix.pos(0, matrix(1, nrow=3, ncol=3))] = 1
#x <- matrix(0, nrow=9, ncol=9)
#x[momentum.matrix.pos(matrix(-1, nrow=3, ncol=3), 1)] = diag(3)
#x
#x <- matrix(0, nrow=9, ncol=9)
#x[momentum.matrix.pos(0, upper.tri(diag(3))-lower.tri(diag(3)))] = 1
#x
#x <- matrix(0, nrow=9, ncol=9)
#x[momentum.matrix.pos(matrix(-1, nrow=3, ncol=3), 1)] = diag(3)
#x

# 'up/down' momentum, i.e. momentum term always one of -1, 0, 1
updown.momentum.matrix <- function(N, ..., b=0) {
  tm <- repl.mut.matrix(N, ...)
  m <- matrix(0, nrow=3*nrow(tm), ncol=3*nrow(tm))

  targetmomentums <- upper.tri(tm)-lower.tri(tm)
  # first take over all uninfluenced production probabilities
  m[momentum.matrix.pos(0, targetmomentums)] <- tm

  # calculate momentum-influenced production probabilities and add them
  for (n in 1:nrow(tm)) {
    posmtargets <- momentum.matrix.pos(1, targetmomentums)
    m[posmtargets] <- binomialsampling.markov.matrix(repl.mut.eq(N, ..., sigma=b))
    negmtargets <- momentum.matrix.pos(-1, targetmomentums)
    m[negmtargets] <- binomialsampling.markov.matrix(repl.mut.eq(N, ..., sigma=-b))
  }
  return(m)
}
#updown.momentum.matrix(3, .001)
#updown.momentum.matrix(3, .001, b=1)

rep.matrix <- function(m, times=1, each=1, times.row=times, times.col=times, each.row=each, each.col=each) {
  # replicate individual elements
  m <- matrix(rep(m, each=each.row), ncol=each.row*dim(m)[1], byrow=TRUE)
  m <- matrix(rep(m, each=each.col), ncol=each.col*dim(m)[1], byrow=TRUE)
  # replicate entire matrix
  do.call(cbind, replicate(times.col, do.call(rbind, replicate(times.row, m, simplify=FALSE)), simplify=FALSE))
}

# calculate stationary http://www.stat.berkeley.edu/~mgoldman/Section0220.pdf
stationarydistribution <- function(m, maxmomentum=0) {
  x <- cbind((m-diag(ncol(m)))[,-ncol(m)], rep(1, ncol(m)))
  colSums(matrix(solve(t(x), c(rep(0, nrow(m)-1), 1)), nrow=1+2*maxmomentum))
}

discretephasediagram <- function(m, maxmomentum) {
  N <- nrow(m)/(1+2*maxmomentum) # -1
  # calculate expected average difference between current state and next state
  # need this to weight (source) states during averaging!
  m <- m*stationarydistribution(m, 0)
#  splitmatrix <- rep.matrix(matrix(1:N**2, ncol=N), each.row=1+2*maxmomentum, each.col=1+2*maxmomentum)
  # group order is 0>0, 1>0, 2>0, ..., 0>1, 1>1, 2>1, ..., 0>2, ...
  print(matrix(rep(1:(N*(1+2*maxmomentum)), nrow(m)), nrow=nrow(m)))
  dx <- sapply(split(m, rep(1:(N*(1+2*maxmomentum)), nrow(m))), mean)
  print(dx)
  dx <- dx - rep(0:(N-1)/(N-1), each=1+2*maxmomentum)
  dx <- dx/20
  # normalise length in both directions
  xoffsets <- rep(0:(N-1)/(N-1), 1+2*maxmomentum)
  moffsets <- rep(-maxmomentum:maxmomentum, each=N)
  print(length(dx))
  print(length(xoffsets))
  print(length(moffsets))
#  moffsets <- matrix(-maxmomentum:maxmomentum, ncol=nrow(m), nrow=N+1, byrow=T)
  dm <- 0
  plot(0, N/2, xlim=c(-maxmomentum, maxmomentum), ylim=c(0, 1), xlab="m", ylab="x")
  arrows(moffsets-dm, xoffsets-dx, moffsets+dm, xoffsets+dx, length=0.1)
}
discretephasediagram(updown.momentum.matrix(5, 0.01), 1)


# compare stationary distributions
comparestationarydistributions <- function(m1, m2, n=min(nrow(m1), nrow(m2)), epsilon=1e-10)
  all(stationarydistribution(m1, as.integer(nrow(m1)/(2*n))) - stationarydistribution(m2, as.integer(nrow(m2)/(2*n))) < epsilon)

#comparestationarydistributions(repl.mut.matrix(5, .01), updown.momentum.matrix(5, .01))

stationarydistribution(repl.mut.matrix(5, 0.1, 0.2, 0.1))

foo <- function(N) {
  par(mfrow=c(4,1))
  barplot(stationarydistribution(updown.momentum.matrix(N, .01), 1))
  barplot(stationarydistribution(updown.momentum.matrix(N, .01, b=1.99), 1))
  barplot(stationarydistribution(updown.momentum.matrix(N, .01, .02), 1))
  barplot(stationarydistribution(updown.momentum.matrix(N, .01, .02, b=1.99), 1))
}
foo(25)

momentumstationarydistribution(updown.momentum.matrix(5, .001, .001))
momentumstationarydistribution(updown.momentum.matrix(5, .001, .002))
momentumstationarydistribution(updown.momentum.matrix(5, .001, .002, b=1.99))

momentumstationarydistribution(updown.momentum.matrix(10, .001))
momentumstationarydistribution(updown.momentum.matrix(10, .001, b=1))

x <- momentumstationarydistribution(updown.momentum.matrix(10, .001))

barplot(x)

# inspect durations of transitions

# compute the development of the Markov chain probability distribution for the
# specified number of iterations. initstate is a vector of length N and
# transitionmatrix an NxN matrix (with rows summing to 1)
markov.chain <- function(transitionmatrix, niterations, initstate=c(1, rep(0, ncol(transitionmatrix)-1))) {
  out <- matrix(nrow=niterations+1, ncol=length(initstate))
  out[1,] <- initstate
  for (i in 2:(niterations+1)) {
    out[i,] <- t(transitionmatrix) %*% out[i-1,]
  }
  return(out)
}

# merge the last #absorbingrows states of the transition matrix together into one absorbing state that only transitions to itself
setabsorbingstate <- function(m, absorbingrows) {
  ma <- m[1:(nrow(m)-absorbingrows),1:(nrow(m)-absorbingrows)]
  # merge (sum) the final absorbingrows columns together for every row
  ma <- cbind(ma, rowSums(m[1:nrow(ma),(1+nrow(m)-absorbingrows):nrow(m)]))
  # single absorbing state that only transitions to itself
  rbind(ma, c(rep(0, nrow(ma)), 1))
}

# discrete 'diff'-momentum, momentum term has 2*N+1 discrete levels in [-1, 1]
diff.momentum.matrix <- function(N, ..., b=0) {
  tm <- repl.mut.matrix(N, ...)
  # N+1 states, so momentum can go from -N to N, i.e. 2*N+1 levels
  m <- matrix(0, nrow=nrow(tm)*(1+2*N), ncol=nrow(tm)*(1+2*N))
  targetmomentums <- outer(5:1, 5:1, '-')
  # all uninfluenced production probabilities
  m[momentum.matrix.pos(0, targetmomentums, N)] <- tm

  # calculate momentum-influenced production probabilities and add them
  for (n in 1:nrow(tm)) {
    for (mon in -N:N) { # TODO don't redo 0 (or, actually, use it to check correctness)
      targets <- momentum.matrix.pos(n, targetmomentums)
      m[targets] <- binomialsampling.markov.matrix(repl.mut.eq(N, ..., sigma=b*n/N))
    }
  }
  # what are the corresponding resulting momentums for each of these transitions?

  return(m)
}
diff.momentum.matrix(3)
length(stationarydistribution(diff.momentum.matrix(10, .001))) 10*20?
momentumstationarydistribution(diff.momentum.matrix(10, .001), 10)


# four ways to compute bias at time of population turnover:
# 1. community collective memory (zero is definitive zero)
# 2. individual up/down/zero biases (wait does this require 3*n more states??)
# a. additive momentum bias
# b. multiplicative (diminishing return) momentum bias

