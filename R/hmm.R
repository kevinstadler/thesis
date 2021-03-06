library(HMM)

# transitionmatrix[] has to be organised row-wise
newhmm <- function(transitionmatrix, emissionProbs, startProbs=c(1, rep(0, dim(transitionmatrix)-1)))
  initHMM(States=states(transitionmatrix), Symbols=1:ncol(emissionProbs), startProbs=startProbs, transProbs=transitionmatrix[], emissionProbs=emissionProbs)

chain.mean <- function(markovchain)
  apply(markovchain, 1, function(row) weighted.mean(0:(length(row)-1), row))

# draw a b/w heatmap based on a matrix of positive numbers, with higher color resolution closer to 0
# the first argument is a list with elements 'hmm' and 'observation'

plotposterior <- function(hmmargs, addmean=TRUE, addmostlikely=FALSE, ...) {
  data <- t(do.call(posterior, hmmargs))
  plotchain(data, ...)
  if (addmean)
    lines(0:(nrow(data)-1), chain.mean(data), lty=2, col="white")
  if (addmostlikely)
    points(0:(nrow(data)-1), as.numeric(do.call(viterbi, hmmargs)), pch=".", col="white") # 4 for cross, 20 for small bullet
}

# all states emit the same symbol - posterior is same as running markov.chain()
noconditioning <- function(transitionmatrix, duration)
  list(hmm=newhmm(transitionmatrix, matrix(1, nrow=dim(transitionmatrix))), observation=rep(1, 1+duration))
#plotposterior(noconditioning(bilm.transition.matrix.average(50, .5), 100), addmostlikely=FALSE)

# all actuated trajectories without interruptions
#actuationconditioning <- function(transitionmatrix, duration)
#  list(hmm=newhmm(transitionmatrix, cbind(c(1, rep(0, dim(transitionmatrix)-1)), c(0, rep(1, dim(transitionmatrix)-1)))), observation=c(1, rep(2, duration)))

# gotta fiddle a little bit with this one to stop the model from avoiding the
# final state until the very last generation: every state has a chance of
# emitting one of two symbols (.99 vs .01), but the lower probability one of
# all the non-categorical states is never actually emitted
naiveconditioning <- function(transitionmatrix, duration)
  list(hmm=newhmm(transitionmatrix, cbind(rep(.99, dim(transitionmatrix)), c(rep(0, dim(transitionmatrix)-1), .01), c(rep(.01, dim(transitionmatrix)-1), 0))), observation=c(rep(1, duration), 2))
#plotposterior(naiveconditioning(bilm.transition.matrix.average(10, .01), 30))

# exact initiation doesn't matter, chain might stay at 0 for some time
completionconditioning <- function(transitionmatrix, duration)
  list(hmm=newhmm(transitionmatrix, cbind(c(rep(1, dim(transitionmatrix)-1), 0), c(rep(0, dim(transitionmatrix)-1), 1))), observation=c(rep(1, duration), 2))
#plotposterior(completionconditioning(bilm.transition.matrix.average(30, .01), 50))

# exact initiaton and termination
exactconditioning <- function(transitionmatrix, duration)
  list(hmm=newhmm(transitionmatrix, cbind(c(1, rep(0, dim(transitionmatrix)-1)), c(0, rep(1, dim(transitionmatrix)-2), 0), c(rep(0, dim(transitionmatrix)-1), 1))), observation=c(1, rep(2, duration-1), 3))
#plotposterior(exactconditioning(bilm.transition.matrix.average(50, .01), 100))
