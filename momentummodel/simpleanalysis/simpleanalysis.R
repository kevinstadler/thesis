#' ---
#' title: Momentum-based language change
#' author: Kevin Stadler
#' output:
#'   pdf_document:
#'     keep_tex: true
#'     fig_caption: true
#' classoption: a4paper
#' ---

#' ## Simple numerical investigation of the transitions

#+ setup, echo=FALSE
knitr::opts_chunk$set(echo=FALSE, cache=TRUE, fig.width=7, fig.height=7)
knitr::opts_knit$set(eval.after="fig.cap")
source("extract-data-simple.R")

simulationruns <- 24*nrow(allconditions)
overthreshold <- nrow(unique(x[c("run",parnames)]))
ntransitions <- nrow(unique(subset(x,success==1)[c("run",parnames)]))

#' Let us begin with a simple criterion of what a 'transition' is. Assuming a threshold (say 5\%), we can define the two extreme areas where the mean population usage level of the minority variant is below this threshold as the two regions of 'near-categorical use' of either variant. A transition, then, is the period in which the mean usage levels of the population cross from near-categorical use of one to near-categorical use of the other variant. Taking our grand total of $`r simulationruns`$ runs, which were all initialised with mean usage levels between 1 and 3\%, we find that only `r overthreshold` (`r 100*overthreshold/simulationruns`\%) ever exceeded the 5\% mark. And of those only an even smaller fraction, namely `r ntransitions` (`r 100*ntransitions/simulationruns`\%) ever exhibited a transition (i.e. crossed over into near-categorical usage of the other variant).

plottransitions <- function(transitions, dataindices=c(1,5), threshold=0.05, ...) {
  plot(0:1, 0:1, type="n", xlab="time (normalised)", ylab="x", xaxs="i", yaxs="i", ...)
  if (nrow(transitions) > 0) {
  for (j in 1:nrow(transitions)) {
    data <- read.table(paste(transitions$run[j], resultfilename(transitions[j,parnames]), sep="/"))
    for (dataindex in dataindices) {
      if (dataindex <= 4) {
        if (transitions$poslow[j] == 1) {
          lines(seq(0,1,1/(transitions$end[j] - transitions$start[j])), data[transitions$start[j]:transitions$end[j],dataindex], ...)
        } else {
          lines(seq(0,1,1/(transitions$end[j] - transitions$start[j])), 1-data[transitions$start[j]:transitions$end[j],dataindex], ...)
        }
      } else {
        maxmomentum <- maxdecaydifference(transitions$a[j], transitions$a[j]*transitions$m[j])
        lines(seq(0,1,1/(transitions$end[j] - transitions$start[j])), 0.5+sign(transitions$poslow[j]-0.5)*data[transitions$start[j]:transitions$end[j],dataindex]/(2*maxmomentum), col=gray(0.5, 0.5), ...)
      }
    }
  }}
  if (any(dataindices > 4)) {
    axis(4, c(0, 1/4, 1/2, 3/4, 1), c(-1, -0.5, 0, 0.5, 1), col="gray")
    mtext("m\'", side=4, 2, col="gray")
  }
  #plot(function(x) plogis((1-2*x)*qlogis(threshold)), col="red", lty=3, add=TRUE)
  return(nrow(transitions))
}

prepareplots <- function(nplots) {
  setwd("data")
  par(mfrow=c(nplots%/%2, if (nplots>1) 2 else 1), mar=c(4.1, 3.1, 3.1, 2.5), pty="s", xaxs="i", yaxs="i", mgp=c(2, 0.7, 0))
}

defaults <- c(N=10, b=2, m=2, T=5)

plotsubset <- function(par, val) {
  pars <- defaults
  pars[[par]] <- val
  plottransitions(subset(x, success & N==pars[["N"]] & b==pars[["b"]] & m==pars[["m"]] & T == pars[["T"]]), main=paste(par, val, sep="="))
}

cap <- function(par, values, ntransitions, desc="settings of parameter")
  paste("Successful transitions generated for ", length(values), " ", desc, " $", par, "$. The remaining parameters were held fixed at ", paste(sapply(setdiff(names(defaults), par), function(par) paste("$", par, "=", defaults[[par]], "$", sep="")), collapse=", "), ". The total number of attested transitions for each setting of $", par, "$ is ", paste(ntransitions, collapse=", "), ".", sep="")

#+ ttransitions, fig.cap=cap("T", t, ntransitions, "settings of the sample resolution parameter")
prepareplots(4)
ntransitions <- sapply(t, function(tlvl)plotsubset("T", tlvl))

#' This surely tells us something.

#+ mtransitions, fig.height=10, fig.cap=cap("m", m, ntransitions)
prepareplots(6)
ntransitions <- sapply(m, function(mlvl)plotsubset("m", mlvl))

#' This surely tells us something.

#+ btransitions, fig.height=10, fig.cap=cap("b", b, ntransitions, "different settings for the momentum bias strength")
prepareplots(6)
ntransitions <- sapply(b, function(blvl)plotsubset("b", blvl))

#' This surely tells us something.

#+ ntransitions, fig.height=10, fig.cap=cap("n", N, ntransitions, "different population sizes")
prepareplots(6)
ntransitions <- sapply(N, function(nlvl)plotsubset("N", nlvl))

#' The neutral evolution transitions are *really* rare. Here's *all* of them:

#+ neutralevolutiontransitions, fig.cap=paste(t1, t2)
prepareplots(2)
t1 <- nrow(plottransitions(subset(x, success & N==2 & b==0), main="N=2"))
t2 <- nrow(plottransitions(subset(x, success & N>2 & b==0), main="N>5"))

#par(mfrow=c(1,4))
#for (t in 2:5) {
#  overlayalltransitions(subset(data.frame(allconditions), N==10 & b==2 & m == 2 & T == t))
#}

#par(mfrow=c(1,4))
#for (thisb in c(1, 1.5, 2, 2.5)) {
#  overlayalltransitions(subset(data.frame(allconditions), N==10 & b==thisb & m == 2 & T == 4))
#}
