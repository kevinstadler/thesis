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

#+ setup, echo=FALSE, cache=TRUE
knitr::opts_chunk$set(echo=FALSE, cache=TRUE, fig.width=7, fig.height=7)
knitr::opts_knit$set(eval.after="fig.cap")
source("~/usm/R/extract-data-simple.R")

nruns <- length(unique(alltransitions$run))
simulationruns <- nruns*nrow(allconditions)
overthreshold <- nrow(unique(alltransitions[c("run",parnames)]))
ntransitions <- nrow(unique(completed[c("run",parnames)]))

#' Let us begin with a simple criterion of what a 'transition' is. Assuming a threshold (say 5\%), we can define the two extreme areas where the mean population usage level of the minority variant is below this threshold as the two regions of 'near-categorical use' of either variant. A transition, then, is the period in which the mean usage levels of the population cross from near-categorical use of one to near-categorical use of the other variant. Taking our grand total of $`r simulationruns`$ runs, which were all initialised with mean usage levels between 1 and 3\%, we find that only `r overthreshold` (~`r round(100*overthreshold/simulationruns)`\%) ever exceeded the 5\% mark. And of those only an even smaller fraction, namely `r ntransitions` (~`r round(100*ntransitions/simulationruns)`\%) ever exhibited a transition (i.e. crossed over into near-categorical usage of the other variant).

plottransitions <- function(transitions, dataindices=c("mrange", "m", "xrange", "x"), x=0, alpha=0.1, ...) {
  if (nrow(transitions) == 0) {
    newplot(0:1, "")
  } else {
    if (x == "normalised") {
      newplot(0:1, "time (normalised)", ...)
    } else if (x == "aligned") {
      # maximum plotting width we'd need for latter half of transitions
      t0offset <- max(transitions$end - transitions$start - transitions$t0)
      newplot(c(-t0offset, t0offset), "interactions/agent (relative to midpoint)", ...)
    } else {
      newplot(c(0, max(transitions$end - transitions$start)), "interactions/agent", ...)
    }
    if (any(dataindices > 4)) {
      addmomentumline()
    }
    for (j in 1:nrow(transitions)) {
      data <- readtrajectory(transitions$run[j], transitions[j,parnames])
      if (x == "normalised") {
        xs <- seq(0,1,1/(transitions$end[j] - transitions$start[j]))
      } else if (x == "aligned") {
        xs <- 0:(transitions$end[j]-transitions$start[j]) - transitions$t0[j]
      } else {
        xs <- 0:(transitions$end[j]-transitions$start[j])
      }
      addtrajectory(xs, data[transitions$start[j]:transitions$end[j],], maxdecaydifference(transitions$a[j], transitions$a[j]*transitions$m[j]), dataindices, transitions$poslow[j] != 1, alpha=alpha)
    }
  }
  if (any(dataindices > 4)) {
    addmomentumaxis()
  }
  #plot(function(x) plogis((1-2*x)*qlogis(threshold)), col="red", lty=3, add=TRUE)
  return(nrow(transitions))
}

defaults <- c(a=0.01, N=10, b=2, m=2, T=5)

plotsubset <- function(par, val) {
  pars <- defaults
  pars[[par]] <- val
  runs <- nruns*nrow(subset(allconditions, N==pars[["N"]] & b==pars[["b"]] & m==pars[["m"]] & T == pars[["T"]]))
  list(nruns=runs, n=plottransitions(subset(completed, N==pars[["N"]] & b==pars[["b"]] & m==pars[["m"]] & T == pars[["T"]]), x="aligned", main=paste(par, val, sep="=")))
}

cap <- function(par, values, ntransitions, desc="settings of parameter")
  paste("Successful transitions generated for ", length(values), " ", desc, " $", par, "$. The remaining parameters were held fixed at ", paste(sapply(setdiff(names(defaults), par), function(par) paste("$", par, "=", defaults[[par]], "$", sep="")), collapse=", "), ". The total number of attested transitions for each setting of $", par, "$ is ", paste(ntransitions[2,], collapse=", "), ", with ", ntransitions[1,1], " runs per condition.", sep="")

#+ ttransitions, fig.cap=cap("T", t, ntransitions, "settings of the sample resolution parameter")
prepareplots(4)
setwd("data")
ntransitions <- sapply(t, function(tlvl)plotsubset("T", tlvl))

#' This surely tells us something.

#+ mtransitions, fig.height=10, fig.cap=cap("m", m, ntransitions)
prepareplots(6)
setwd("data")
ntransitions <- sapply(m, function(mlvl)plotsubset("m", mlvl))

#' This surely tells us something.

#+ btransitions, fig.height=10, fig.cap=cap("b", b, ntransitions, "different settings for the momentum bias strength")
prepareplots(6)
setwd("data")
ntransitions <- sapply(b, function(blvl)plotsubset("b", blvl))

#' This surely tells us something.

#+ ntransitions, fig.height=10, fig.cap=cap("n", N, ntransitions, "different population sizes")
prepareplots(6)
setwd("data")
ntransitions <- sapply(N, function(nlvl)plotsubset("N", nlvl))

#+ x0transitions, fig.height=10, fig.cap=cap("x0", x0, ntransitions, "initial settings")
#prepareplots(4)
# essentially indistinguishable
#ntransitions <- sapply(x0, function(x0lvl)plotsubset("x0", x0lvl))

#' The neutral evolution transitions are *really* rare. Here's *all* of them:

#+ neutralevolutiontransitions, fig.cap=paste("All transitions generated in conditions with neutral evolution (momentum bias $b=0$) with a population of two agents (", t1, " in ? runs) and populations of 5 or more (", t2, " in ? runs)", sep="")
prepareplots(2)
setwd("data")
t1 <- nrow(plottransitions(subset(completed, N==2 & b==0), x="aligned", dataindices=c("m", "x"), main="N=2"))
t2 <- nrow(plottransitions(subset(completed, N>2 & b==0), x="aligned", dataindices=c("m", "x"), main="N>5"))

#+ interruptions, fig.cap="Highest proportion of incoming variant usage vs. highest momentum attested during interrupted transitions (trajectories which crossed the 5\\% threshold but did not make it to the completion threshold at 95\\%), for neutral evolution (left) vs. momentum biased runs (right). In the neutral evolution runs the momentum bias is ineffective, so the momentum levels < 0.2 indicate the levels of momentum that are typically reached by random walks. The right graph shows that the simple threshold method is probably inadequate: the cluster in the bottom left captures trajectories which walked into the region of 5-20\\% incoming variant usage without triggering runaway change. The cluster on the right side shows transitions with runaway change (as indicated by high maximum momentum), but did not reach the 95\\% mark. The true 'interrupted' transitions we are interested in lie in the area in between, with highest attested incoming variant usage between 30 and 80\\%."
interruptedneutral <- subset(interrupted, b==0)
interruptedmomentum <- subset(interrupted, b>=1)
prepareplots(2)
plot(interruptedneutral$xmax, abs(interruptedneutral$mmax), pch='.', xlim=0:1, ylim=0:1, xlab="highest incoming variant usage", ylab="highest momentum", main="b=0")
plot(interruptedmomentum$xmax, abs(interruptedmomentum$mmax), pch='.', xlim=0:1, ylim=0:1, xlab="highest incoming variant usage", ylab="highest momentum", main="b>=1")

#+ interruptionsperparameter, fig.cap="Frequency and turning point of interrupted changes per parameter"
# loads of walks just over the .05 threshold, takes looong to plot.
#lattice::densityplot(interrupted$xmax, groups=interrupted$b)
interrupted <- subset(interrupted, 0.3 < xmax & xmax < 0.8)
#lattice::densityplot(interrupted$xmax, groups=interrupted$b, auto.key=TRUE, from=0.3, to=0.8)
plotinterruptions <- function(var)
  plot(jitter(interrupted[[var]]), interrupted$xmax, pch=20, xlab=var, ylab="highest usage of incoming variant")

prepareplots(4)
plotinterruptions("b")
plotinterruptions("N")
plotinterruptions("m")
plotinterruptions("T")
#plotinterruptions("x0")

#+ completionratiopercondition
#allconditions$transitions <- apply(allconditions, 1, function(cd) nrow(subset(completed, a==cd[["a"]] & b==cd[["b"]] & T==cd[["T"]]  & m==cd[["m"]]  & x0==cd[["x0"]] & i==cd[["i"]] & N==cd[["N"]])))
#allconditions$interruptions <- apply(allconditions, 1, function(cd) nrow(subset(interrupted, a==cd[["a"]] & b==cd[["b"]] & T==cd[["T"]]  & m==cd[["m"]]  & x0==cd[["x0"]] & i==cd[["i"]] & N==cd[["N"]])))
#allconditions$interruptionratio <- allconditions$interruptions / allconditions$transitions
overhalf <- subset(alltransitions, xmax>=0.5)
nrow(subset(overhalf, b==0 & success==1))/nrow(subset(overhalf, b==0))
nrow(subset(overhalf, b>=1 & success==1))/nrow(subset(overhalf, b>=1))

#+ interruptionsamples, fig.cap=paste("Example of a successful and an interrupted transition created in runs with identical parameter settings: ", paste(parnames, interruptionsamples[1,parnames], sep="=", collapse=", "))
findtransitions <- function(data, proto)
  subset(data, b==proto[["b"]] & m==proto[["m"]] & N==proto[["N"]] & x0==proto[["x0"]])

interruptionsamples <- subset(interrupted, b>=2 & xmax > 0.5)
prepareplots(nrow(interruptionsamples))
setwd("data")
for (i in 1:1) {
  # find a matching successful transition
  plottransitions(findtransitions(completed, interruptionsamples[i,])[1,], alpha=0.5)
  plottransitions(interruptionsamples[i,], alpha=0.5)
}

#+ largeninterruptions, fig.cap=paste(parnames, largeninterruptions[1,parnames], sep="=", collapse=",")
largeninterruptions <- subset(interrupted, N>10 & b > 1 & xmax > 0.5)
prepareplots(2)
plottransitions(largeninterruptions[1,], alpha=0.5)
plottransitions(largeninterruptions[2,], alpha=0.5)

#par(mfrow=c(1,4))
#for (t in 2:5) {
#  overlayalltransitions(subset(data.frame(allconditions), N==10 & b==2 & m == 2 & T == t))
#}

#par(mfrow=c(1,4))
#for (thisb in c(1, 1.5, 2, 2.5)) {
#  overlayalltransitions(subset(data.frame(allconditions), N==10 & b==thisb & m == 2 & T == 4))
#}
