#'---
#'title: Awareness of a syntactic change in Shetland
#'author: 
#'output: pdf_document
#'classoption: a4paper,12pt
#'header-includes:
#'  - \usepackage{fullpage}
#'---
#'
#' Quantifying people's explicit knowledge about ongoing language changes and their directionality. The tested variables in question are:
#'
#'1. verb positioning in imperatives, moving from old VSO to SVO (is that right?).
#'2. a stable variable in negation ("didna" vs. "didnoo")
#'3. yes/no question syntax
#'4. wh question syntax

#+ echo=FALSE
knitr::opts_chunk$set(echo=FALSE)

d <- read.table("shetland.csv", header=TRUE, sep=" ")
lvls <- c("onlyout", "moreout", "both", "morein", "onlyin")
d$self <- factor(d$self, levels=lvls)
d$other <- factor(d$other, levels=lvls)
d$selfbehind <- as.numeric(d$self)<=as.numeric(d$other)
d$selfahead <- as.numeric(d$self)>=as.numeric(d$other)

d$oldervar <- factor(d$oldervar, levels=c("same", "in", "out"))
d$young <- factor(d$young, levels=lvls)
d$old <- factor(d$old, levels=lvls)

ages <- sort(aggregate(age ~ id, data=d, FUN=mean)$age)
medage <- median(ages)

imp <- subset(d, var == "imp")
neg <- subset(d, var == "neg")
ynq <- subset(d, var == "ynq")
whq <- subset(d, var == "whq")

#' # Knowledge of self+population usage frequencies
#' The following graphs show the speakers' own estimated usage frequency vs. <!-- (population average boxed in red, for the stable variable this box is the `correct' answer) --> their estimated usage frequency for the entire population.
#' Datapoints below the diagonal mean that people perceive themselves to be more old-fashioned/conservative than "the people around them".

plot.image <- function(counts, correct=NULL, ...) {
  mx <- max(counts)
  # or fields::image.plot(..., nlevel=mx+1, legend.shrink=0.7)
  image(1:dim(counts)[1], 1:dim(counts)[2], counts, col=gray(mx:0/mx), ...)
  if (!is.null(correct)) {
    rect(correct[1]-0.5, correct[length(correct)]-0.5, correct[1]+0.5, correct[length(correct)]+0.5, border="red")
  }
  # scatterplot
  # plot(jitter(as.numeric(d$young)), jitter(as.numeric(d$old)), pch=19, xlim=c(1,5), ylim=c(1,5), xlab='Young speakers', ylab='Old speakers')
  for (i in 1:dim(counts)[1]) {
    for (j in 1:dim(counts)[2]) {
      if (counts[i,j]>0) {
        text(i, j, counts[i,j], col="white")
      }
    }
  }
  abline(0, 1, col="red")
}

plot.usage.estimates <- function(counts, ...) {
  plot.image(counts, xlab="Self-use estimate", ylab="Population-use estimate", ...)
}

#par(pty="s", mfrow=c(1,2), mgp=c(2, 1, 0))
#plot.usage.estimates(table(neg$self, neg$other), main="Stable negation")
#plot.usage.estimates(table(imp$self, imp$other), main="Imperative")
#par(pty="s", mfrow=c(1,2))
#plot.usage.estimates(table(ynq$self, ynq$other), main="y/n questions")
#plot.usage.estimates(table(whq$self, whq$other), main="wh questions")

#' ## Stable negation
#' For the stable variable, the 'correct' answer is boxed in red: variable use in the population is constant at some use of "didnoo" (second line from the bottom) which almost everybody gets right, but then speakers of all ages also claim to never use didnoo themselves (this estimate is confirmed by the apparent time estimates below, where people report both older and younger speakers to use *some* didnoo).
#Could this pattern be indicative of social or geographic stratification, with the interviewed group considering themselves as outgroup from whoever uses some of the other variant?

plot.byage <- function(data, ...) {
  young <- subset(data, age<=medage)
  old <- subset(data, age>medage)
  par(pty="s", mfrow=c(1,2))
  plot.usage.estimates(table(young$self, young$other), main=paste("younger than", medage), ...)
  plot.usage.estimates(table(old$self, old$other), main=paste("older than", medage), ...)
}
plot.byage(neg, correct=2)
#' Both younger speakers reporting to use only "didnoo" are indeed from didnoo-saying Whalsay off the East coast (07H and 08H on the map)!
#' \begin{figure}[h!]\centering\includegraphics[width=0.5\textwidth]{shetlandpegs} \caption{Idyllic Shetland, with coloured pins in it} \end{figure}
#'
#' ## Changing variables
#' Usage is moving from quite frequent use of the incoming variant towards even more use of the incoming variant, so the correct answer for the population-wide usage should probably be the 4th line from the bottom. The 'correct' answer for their own use will obviously depend on the speaker: assuming that usage levels are mostly dependent on speaker age, we can split the informants into two equally sized groups. We would expect the younger speakers (left graphs) to produce more points *below* the diagonal (e.g. at 5,4) and older speakers (right graphs) more *above* the diagonal (e.g. 3,4).
#'
#' ### Imperative
plot.byage(imp)
#' ### y/n questions
plot.byage(ynq)
#' ### wh questions
plot.byage(whq)

#' # Explicit knowledge

#' What did people think was the 'older' variant of the two presented? This could be based on any or all of real time, connotations of variants being archaic, or apparent time (although up to this point in the questionnaire it was avoided to draw explicit attention to age grading).

counts <- table(d$oldervar, d$var)
barplot(counts, legend=rownames(counts), col=c("white", "green", "darkred"), main="Which variant is 'older', for all variables")
#library(lattice)
#barchart(response~vartype, d, groups=vartype, stack=TRUE)

#' People really know about the changing variables, but are either clueless or just guessing about the stable one. Fewer data collected for the two question types, plus one NA for the negation (which correctly pointed out that the two negation variants are a geographic feature rather than a temporal one!)
#'
#' # Apparent time (age-grading) knowledge

#' Would people's perception of usage in different age groups be accurate enough to draw conclusions about age grading/apparent time differences?
#' Below is the data for all four variables, points on the diagonal mean people perceive no change in use between younger/older speakers, points above the diagonal indicate perception of increased use among younger speakers.

plot.age.grading <- function(counts, correct=NULL, xlab="estimated old use", ylab="estimated young use", ...) {
  plot.image(counts, correct, xlab=xlab, ylab=ylab, ...)
}

par(pty="s", mfrow=c(1,2))
plot.age.grading(table(neg$old, neg$young), correct=2, main="Stable use of 'didnoo'")
plot.age.grading(table(imp$old, imp$young), main="Increasing use of 'Du mak..'")
par(pty="s", mfrow=c(1,2))
plot.age.grading(table(ynq$old, ynq$young), main="Increasing use of 'Does du ken..'")
plot.age.grading(table(whq$old, whq$young), main="Increasing use of 'Whit did du..'")

#' # Testing for primacy effects etc.
#' Check that randomisation order is not a good predictor of the responses to the 'self' and 'population' usage question (for the changing variable at least): while the outcome variable is ordinal with 5 levels, only two of those levels were actually selected for the yes-no questions ('only incoming' and 'more incoming') and three levels for wh questions ('only incoming' as well as 'more incoming' and 'more outgoing' which we will group together), which means we can fit a logistic model to a binomial outcome. The influence of the randomisation can be read off the `firstvaroutgoing` predictor variable in the individual models (which never reaches significance).
#'
#' ## Negation
summary(glm(self!="onlyout" ~ firstvar, binomial(), neg))
#summary(glm(other!="onlyout" ~ firstvar, binomial(), neg))
#' ## y/n questions
summary(glm(self!="onlyin" ~ age + firstvar, binomial(), ynq))
#summary(glm(other!="onlyin" ~ age + firstvar, binomial(), ynq))
#' ## wh questions
#' Age is an almost significant predictor of your own usage
summary(glm(self!="onlyin" ~ age + firstvar, binomial(), whq))
#summary(glm(other!="onlyin" ~ age + firstvar, binomial(), whq))

# Neither is a great predictor. How about the self-reported difference to the population mean?
# ## neg
## both ok
#summary(glm(selfbehind ~ age + firstvar, binomial(), neg))
#summary(glm(selfahead ~ age + firstvar, binomial(), neg))
# ## imp
## bothok
#summary(glm(selfbehind ~ age + firstvar, binomial(), imp))
#summary(glm(selfahead ~ age + firstvar, binomial(), imp))
# ## yn
## ok
#summary(glm(selfbehind ~ age + firstvar, binomial(), ynq))
# every single person considering themselves to be ahead or even with the change
#summary(glm(selfahead ~ age + firstvar, binomial(), ynq))
# ## wh
#summary(glm(selfbehind ~ age + firstvar, binomial(), whq))
# only one person not considering themselves to be ahead or even with the change
#summary(glm(selfahead ~ age + firstvar, binomial(), whq))
