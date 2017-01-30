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
#'1. verb positioning in imperatives
#'2. a stable variable in negation ("didna" vs. very localised used of "didnoo")
#'3. yes/no question syntax
#'4. wh question syntax

#+ echo=FALSE
knitr::opts_chunk$set(echo=FALSE)

source("shetland-data.R")

#' ## Comparing the demographics of the two samples
#' Two samples were collected:
#'
#' 1. a balanced sample of `r length(unique(da$id))` participants who filled out the questionnaire following a longer interview about the subjective grammaticality of the variables in question (about half of the participants only gave information on the first two variables) 
#' 2. a convenience sample of `r length(unique(do$id))` participants recruited online (information on all four variables)
#'
#' ## Gender distribution for the two samples
#plot(table(d$condition, d$gender))
counts <- table(participants$gender, participants$condition)
barplot(counts, legend=rownames(counts), col=topo.colors(2))
#' ## Age distribution for the two samples
#' The mean ages of the two samples are `r mean(subset(participants, condition=="paper")$age)` and `r mean(subset(participants, condition=="online")$age)` respectively, the medians `r median(subset(participants, condition=="paper")$age)` and `r median(subset(participants, condition=="online")$age)`.
lattice::histogram( ~ age | condition, data=d)

#' ## Geographical distribution of participants across the two samples
counts <- table(participants$loc, participants$condition)
barplot(counts, legend=rownames(counts), col=rainbow(length(levels(participants$loc))))

#' # Estimates of own & population usage frequencies
#' The following graphs show the speakers' own estimated usage frequency vs. <!-- (population average boxed in red, for the stable variable this box is the `correct' answer) --> their estimated usage frequency for the entire population.
#' Datapoints *below* the diagonal mean that people perceive themselves to use *more* of the incoming variant than "the people around them" do. The left graphs show the datapoints collected on location, the right graphs the data collected online.

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
        text(i, j, counts[i,j], col=gray(round(1-(mx-counts[i,j])/mx)))
      }
    }
  }
  abline(0, 1, col="red")
}

plot.usage.estimates <- function(counts, ...) {
  plot.image(counts, xlab="Self-use estimate", ylab="Population-use estimate", ...)
}

par(pty="s", mfrow=c(1,2), mgp=c(2, 1, 0))
plot.usage.estimates(table(variable("neg", da)$self, variable("neg", da)$other), main="Stable negation")
plot.usage.estimates(table(variable("neg", do)$self, variable("neg", do)$other), main="Stable negation")

#' Reports of the stable negation variable are in agreement with the known pattern: participants hardly use 'didnoo' for negation, but are aware that some people do use it. All four data points in the rightmost column reporting to always use 'didnoo' are indeed from the island of Whalsay where they like to say 'didnoo' all the time!
#plot.usage.estimates(table(variable("neg", subset(d, loc=='Whalsay'))$self, variable("neg", subset(d, loc=='Whalsay'))$other), main="Stable negation")

par(pty="s", mfrow=c(1,2), mgp=c(2, 1, 0))
plot.usage.estimates(table(variable("imp", da)$self, variable("imp", da)$other), main="Imperative")
plot.usage.estimates(table(variable("imp", do)$self, variable("imp", do)$other), main="Imperative")
par(pty="s", mfrow=c(1,2), mgp=c(2, 1, 0))
plot.usage.estimates(table(variable("ynq", da)$self, variable("ynq", da)$other), main="y/n questions")
plot.usage.estimates(table(variable("ynq", do)$self, variable("ynq", do)$other), main="y/n questions")
par(pty="s", mfrow=c(1,2), mgp=c(2, 1, 0))
plot.usage.estimates(table(variable("whq", da)$self, variable("whq", da)$other), main="wh questions")
plot.usage.estimates(table(variable("whq", do)$self, variable("whq", do)$other), main="wh questions")

#' ## Qualitative observations about the responses
#' What is interesting about all three changing variables is that the biggest group of people regard themselves as 'ahead of the curve' in terms of using the novel variant. Intuitively, one would think that at least half of the people would need to be behind (or at least exactly on average), so the data could indicate that people are quite likely to think of themselves to be ahead of a trend. It's quite likely that this is just an artefact of the question though - when estimating a single person's usage it is not unlikely that one might land on one of the extreme 'categorical use' options of the *ordinal scale*, whereas when asking about a community average even the thought of a single non-categorical group member could nudge the response to a 'mostly' or 'both equally' option.
#' This intuition is potentially reflected in the general distribution of responses for own vs population-usage across all variables (only the 3 changing variables are plotted, but the same 'edge-avoiding' pattern can be found in the stable variable):
changing <- subset(d, var != "neg")
par(pty="s", mfrow=c(1,2))
barplot(table(changing$self), main="Self-use estimates")
barplot(table(changing$other), main="Population-use estimates")

#' # Testing for primacy effects etc.
#' First we should check whether the randomised presentation order of the two variants (variable `firstvar`) can predict the responses to the 'self' and 'population' usage questions:
#'
#' ## Negation
#' ### Own usage
rms::orm(self ~ firstvar, data=variable("neg"))
#' ### Other (population) usage
rms::orm(other ~ firstvar, data=variable("neg"))

#' ## Imperatives
#' ### Own usage
rms::orm(self ~ firstvar, data=variable("imp"))
#' ### Other (population) usage
rms::orm(other ~ firstvar, data=variable("imp"))

#' ## Y/n questions
#' ### Own usage
rms::orm(self ~ firstvar, data=variable("ynq"))
#' Order of presentation might have had an effect here, with people more likely to overestimate the variant that was presented first:
lattice::histogram( ~ self | firstvar, data=variable("ynq"))

#' As can be seen from the plot the influence isn't dramatic, since only three of the five options were ever selected: mainly, participants are less likely to state that *only* the incoming variant is used when the outgoing variant was presented first.
#'
#' ### Other (population) usage
rms::orm(other ~ firstvar, data=variable("ynq"))

#' ## Wh questions
#' ### Own usage
rms::orm(self ~ firstvar, data=variable("whq"))
#' ### Other (population) usage
rms::orm(other ~ firstvar, data=variable("whq"))
# Again, people are potentially overestimating the variant that is presented first (in this case the outgoing variant)
#lattice::histogram( ~ other | firstvar, data=variable("whq"))

#' # What predicts people's 'own usage' estimate?
#' For the negation variable `location=Whalsay` should be best - the coefficient is indeed high, but so is the standard error due to data sparsity, so, that none of the variables come out as significant predictors for the stable negation variable:
rms::orm(self ~ loc+age+gender+firstvar+condition, data=variable("neg"))
#' For the imperatives, nothing sticks out:
rms::orm(self ~ loc+age+gender+firstvar+condition, data=variable("imp"))
#' For yes/no question there is still an effect of ordering, plus a possible effect of gender (which direction?)
rms::orm(self ~ loc+age+gender+firstvar+condition, data=variable("ynq"))
#' For wh questions age is a good predictor, but the effect size is tiny (see boxplot below, the effect is *not* just driven by the single 'moreout' response). Both Lerwick and the South seem to be doing something different from the rest too:
rms::orm(self ~ loc+age+gender+firstvar+condition, data=variable("whq"))
boxplot(age~self, data=variable("whq"), main="Age distribution per self-use response")

#' # People's perception of their own vs. community usage
#' This graph shows the distribution of people's 'own usage' estimates vs. their community estimates. As mentioned before, many more people see themselves 'ahead' of the community for the changing variables. The right graph breaks this down even more by looking at the absolute (ordinal) difference between people's own vs. community estimate:

par(mfrow=c(1,2), pty="s")
counts <- table(d$selfrelative, d$var)
barplot(counts, legend=rownames(counts), col=cm.colors(3))
counts <- table(d$selfdiff, d$var)
barplot(counts, legend=rownames(counts), col=cm.colors(5))

#' ## What predicts whether people think they are ahead or behind their peers?
#' ### Negation
rms::orm(selfdiff ~ loc+age+gender+firstvar+condition, data=variable("neg"), maxit=20)
#' Whalsayers (as well as Northerners) are more likely to think that they are 'ahead' of the 'people around them' in their use of 'didnoo' (which indicates they took the formulation of the "people around you" question to mean Shetland as a whole) - interestingly Whalsayers seem generally more likely to think they're ahead for the three other changes as well, as seen in the right graph:
par(mfrow=c(1,2), pty="s")
counts <- table(variable("neg")$selfdiff, variable("neg")$loc=="Whalsay")
dimnames(counts)[[2]] <- c("RoS", "Whalsay")
barplot(counts, legend=rownames(counts), col=cm.colors(5), main="Being ahead for negation")
counts <- table(d$selfdiff, d$loc=="Whalsay")
dimnames(counts)[[2]] <- c("RoS", "Whalsay")
barplot(counts, legend=rownames(counts), col=cm.colors(5), main="Being ahead for all variables")

#' ### Imperatives
rms::orm(selfdiff ~ loc+age+gender+firstvar+condition, data=variable("imp"))
#' ### Yes/no questions
#' For yes/no questions, age is again significant, but see the boxplot below.
rms::orm(selfdiff ~ loc+age+gender+firstvar+condition, data=variable("ynq"))
boxplot(age~selfdiff, data=variable("ynq"), main="Age distribution per being-ahead response")
#' ### Wh questions
# Folk from Whalsay, the South and Lerwick (and maybe also Bressay) are more likely to think they're ahead of "the people around them" (see barplot below):
rms::orm(selfdiff ~ loc+age+gender+firstvar+condition, data=variable("whq"))
#counts <- table(variable("whq")$selfdiff, variable("whq")$loc)
#barplot(counts, legend=rownames(counts), col=cm.colors(5), main="Perception of 'being ahead' of people around oneself")

#' # Explicit knowledge about variant age

#' Moving on from what we can extrapolate from the 'self' vs. 'population use' estimates, what did people explicitly *say* was the 'older' variant of the two presented? This could be based on any or all of real time, connotations of variants being archaic, or apparent time (although up to this point in the questionnaire it was avoided to draw explicit attention to age grading).

counts <- table(d$oldervar, d$var)
barplot(counts, legend=rownames(counts), col=c("white", "green", "darkred"), main="Which variant is 'older', for all variables")

#' People really know about the changing variables, but are either clueless or just guessing about the stable one.
library(party)
model <- ctree(oldervar=="out" ~ loc+age+gender+firstolder+condition+var, na.omit(d))
plot(model)

#' In the paper-based condition fewer data points collected for the two question types, plus one `NA` for the negation which was correctly pointed out to be a geographic variation rather than a temporal one. Whether participants know the directionality (for the three changing variables pooled together) is not predicted well by any of the participant variables (also not when taken individually, particularly for gender and age):
rms::lrm(oldervar=="out" ~ loc+age+gender+firstolder+condition, data=subset(d, var!="neg"))

#'
#' # Apparent time (age-grading) knowledge
#' Would people's perception of usage in different age groups be accurate enough to draw conclusions about age grading/apparent time differences?
#' Below is the data for all four variables, points on the diagonal mean people perceive no change in use between younger/older speakers, points above the diagonal indicate perception of increased use among younger speakers. Online and paper-based data are pooled together for these graphs.

plot.age.grading <- function(counts, correct=NULL, xlab="estimated older sp. use", ylab="estimated younger sp. use", ...)
  plot.image(counts, correct, xlab=xlab, ylab=ylab, ...)

par(pty="s", mfrow=c(1,2), mgp=c(2, 1, 0))
plot.age.grading(table(variable("neg")$old, variable("neg")$young), correct=2, main="Stable use of 'didnoo'")
plot.age.grading(table(variable("imp")$old, variable("imp")$young), main="Increasing use of 'Du mak..'")

par(pty="s", mfrow=c(1,2), mgp=c(2, 1, 0))
plot.age.grading(table(variable("ynq")$old, variable("ynq")$young), main="Increasing use of 'Does du ken..'")
plot.age.grading(table(variable("whq")$old, variable("whq")$young), main="Increasing use of 'Whit did du..'")

#' The following graph shows the distribution of people's 'older' vs. 'younger speaker' estimates relative to one another (i.e. whether younger speakers are 'ahead' of older ones in novel variant use). The right graph breaks this down even more by looking at the absolute (ordinal) difference between the younger and older speaker estimates - this is essentially the 'distance' from the diagonal in the graphs above:
par(mfrow=c(1,2), pty="s")
counts <- table(d$apparentrelative, d$var)
barplot(counts, legend=rownames(counts), col=cm.colors(3))
counts <- table(d$apparentdiff, d$var)
barplot(counts, legend=rownames(counts), col=cm.colors(9))

#' ## What predicts whether people see a trend in 'apparent time'?
#' This is again ordinal logistic regression modelling, this time the predicted variable is the absolute ordinal difference between the younger+older speaker estimates (i.e. the data on the right in the graph above).
#'
#' ### Negation
rms::orm(apparentdiff ~ loc+age+gender+firstvar+condition, data=variable("neg"))
#boxplot(age~apparentdiff, data=variable("neg"), main="Age distribution per apparent-time-diff")
#' Interestingly there seems to be a gender effect in reporting an apparent time difference in negation use. Also speakers from Whalsay see a difference between younger and older speakers that the other communities don't, namely that 'didnoo' usage is mostly found in older speakers! Keep in mind that there are only 4 datapoints from Whalsay, distributed like this (the respective ages of those speakers are `r subset(participants, loc=="Whalsay")$age`):
par(pty="s")
plot.age.grading(table(subset(d, var=="neg" & loc=="Whalsay")$old, subset(d, var=="neg" & loc=="Whalsay")$young), main="Apparent time perception of 'didnoo' by speakers from Whalsay")

#counts <- table(d$apparentdiff, d$loc=="Whalsay")
#barplot(counts, legend=rownames(counts), col=topo.colors(5))
#' ### Imperatives
rms::orm(apparentdiff ~ loc+age+gender+firstvar+condition, data=variable("imp"))
#plot(ctree(apparentdiff ~ loc+age+gender+firstvar+condition, variable("imp")))
#' ### Yes/no questions
rms::orm(apparentdiff ~ loc+age+gender+firstvar+condition, data=variable("ynq"))
#plot(ctree(apparentdiff ~ loc+age+gender+firstvar+condition, variable("ynq")))
#' ### Wh questions
#' Lerwickers more likely to report apparent time differences, plus maybe an effect of order of variant presentation:
rms::orm(apparentdiff ~ loc+age+gender+firstvar+condition, data=variable("whq"))
#plot(ctree(apparentdiff ~ loc+age+gender+firstvar+condition, variable("whq")))

# 30/04/2015 supervision questions:

# does age predict whether people are good at knowing the directionality of change?
# -> real time: no
# -> apparent time: no
#rms::orm(apparentrelative ~ age, data=subset(d, var!="neg"))
#rms::orm(apparentdiff ~ age, data=subset(d, var!="neg"))

# are older people more reliable at knowing they're behind the curve?
# -> apparent time: ?

# does being from whalsay predict what you think about the age of 'didnoo'?
# -> real time: no
#d$whalsay <- d$loc == "Whalsay"
#rms::lrm(oldervar ~ whalsay+age+gender, data=variable("neg"))

#' # Agreement between grammaticality judgments and self-use estimates

par(mfrow=c(2,2))
plot.judgment(subset(jdg, incoming==TRUE), "self")
plot.judgment(subset(jdg, incoming==FALSE), "self")
plot.judgment(subset(jdg, incoming==TRUE), "other")
plot.judgment(subset(jdg, incoming==FALSE), "other")

par(mfrow=c(2,2))
plot.judgment(subset(jdg, incoming==TRUE), "selfdiff")
plot.judgment(subset(jdg, incoming==FALSE), "selfdiff")
plot.judgment(subset(jdg, incoming==TRUE), "oldervar")
plot.judgment(subset(jdg, incoming==FALSE), "oldervar")

cor(as.numeric(jdginc$judgment), as.numeric(jdginc$other), use="complete.obs", method="spearman")
