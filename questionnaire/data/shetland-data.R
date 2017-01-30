vars <- c("imp", "ynq", "whq", "neg")
varcolors <- c("orange", "blue", "green", "darkgrey")
names(varcolors) <- vars
changingcolors <- varcolors[1:3]
longvars <- c(imp="imperatives", ynq="yes/no questions", whq="wh questions", neg="negation")
vardesc <- c(imp="imperatives", ynq="yes/no questions", whq="wh questions", neg="negation (stable control)")

lvls <- c("onlyout", "moreout", "both", "morein", "onlyin")
collapsedlvls <- c("fewin", "morein", "onlyin")
collapsedmapping <- c("fewin", "fewin", "fewin", "morein", "onlyin")
rellvls <- c("behind", "level", "ahead")
adddiff <- function(d, v1, v2="other", name=v1) {
  diffname <- paste(name, "diff", sep="")
  diffs <- as.numeric(d[[v1]]) - as.numeric(d[[v2]])
  d[[paste(name, "relative", sep="")]] <- factor(rellvls[2+sign(diffs)], levels=rellvls, ordered=TRUE)
#  d[[diffname]] <- factor(diffs, levels=do.call(seq, as.list(range(diffs))), ordered=TRUE)
  d[[diffname]] <- factor(diffs, levels=-4:4, ordered=TRUE)
  levels(d[[diffname]])[6:9] <- sprintf("%+i", 1:4)
  return(d)
}

nicelvls <- c("only out", "more out", "both", "more in", "only in")
helmertvars <- c("whq", "question", "notchanging")
helmertlevels <- function(var) contr.helmert(4)[5 - as.numeric(var),]
arrangedata <- function(d) {
  d$gender <- factor(c(F="female", M="male")[d$gender], levels=c("female", "male"))

  d$self <- factor(d$self, levels=lvls, ordered=TRUE)
  d$jitteredself <- jitter(as.numeric(d$self))
  d$other <- factor(d$other, levels=lvls, ordered=TRUE)
  d$var <- factor(d$var, levels=vars)
  d$longvar <- factor(longvars[d$var], levels=longvars)
  # helmert coding (not reverse, as returned by contr.helmert())
  d[,helmertvars] <- helmertlevels(d$var)
  d$oldervar <- factor(d$oldervar, levels=c("out", "same", "in")) # TODO consider this ordered or not?
  d$young <- factor(d$young, levels=lvls, ordered=TRUE)
  d$old <- factor(d$old, levels=lvls, ordered=TRUE)
  levels(d$self) <- nicelvls
  levels(d$other) <- nicelvls
  levels(d$young) <- nicelvls
  levels(d$old) <- nicelvls

  # collapse first three levels to avoid empty cells in ordered logit model
#  flattenedlevels <- c("lessin", "lessin", "lessin", "morein", "onlyin")
#  d$selftrunc <- factor(flattenedlevels[d$self], levels=unique(flattenedlevels), ordered=TRUE)
#  d$othertrunc <- factor(flattenedlevels[d$other], levels=unique(flattenedlevels), ordered=TRUE)

  # dummy variable for between-changing/stable tests
  d$stable <- d$var == "neg"

  # swap "in" and "out" for the stable "neg" variable so that "in"/"out"
  # responses can be meaningfully interpreted as 'majority'/'minority' variant
  d[d$var=="neg","oldervar"] <- levels(d$oldervar)[4-as.numeric(d[d$var=="neg","oldervar"])]
  for (col in c("firstvar", "self", "other", "firstolder", "young", "old")) {
    lvls <- levels(d[[col]])
    d[d$var=="neg",col] <- lvls[1+length(lvls)-as.numeric(d[d$var=="neg",col])]
  }

  # identical measures (under different labels)
  #d$selffreq <- factor(d$self, levels=freqlvls)
  d$oldervar <- factor(c("outgoing older", "always both", "incoming older")[as.numeric(d$oldervar)], levels=c("outgoing older", "always both", "incoming older"))
  d$oldervarfreq <- factor(c("minority older", "always both", "majority older")[as.numeric(d$oldervar)], levels=c("minority older", "always both", "majority older"))

  # derivative measures
  d <- adddiff(d, "self")
  d <- adddiff(d, "old")
  d <- adddiff(d, "young")
  d <- adddiff(d, "young", "old", "apparent")
  d$apparentdiffN <- as.numeric(d$apparentdiff)-5
#  d$apparentdiff <- as.numeric(d$young)-as.numeric(d$old)
#  d$apparentrelative <- factor(rellvls[2+sign(d$apparentdiff)], levels=rellvls, ordered=TRUE)
#  d$apparentdiff <- factor(d$apparentdiff, levels=difflvls(d$apparentdiff), ordered=TRUE)
  return(d)
}

da <- arrangedata(read.table("shetland.csv", header=TRUE, sep=" "))
da$condition <- as.factor("paper")
do <- arrangedata(read.csv("shetland-online.csv"))
do$id <- factor(do$id)
do$condition <- as.factor("online")
d <- rbind(do,da)

#d$loc <- factor(d$loc, levels=c("Central", "Lerwick", "South", "West", "Bressay", "North", "Whalsay"))
d$loc <- relevel(relevel(d$loc, "Bressay"), "Lerwick")

#agecats <- c("young", "middle", "old")
agecats <- c("age <= 32", "age > 32")
# split participants in 3 evenly sized categories: <=27 (N=26), 28-49 (N=26), >=50 (N=25)
#d$agecat <- factor(agecats[2 - (d$age <= 27) + (d$age>=50)], levels=agecats, ordered=TRUE)
d$agecat <- factor(agecats[1+(d$age>32)], levels=agecats)
participants <- unique(d[c("id", "gender", "age", "agecat", "loc", "condition")])
#agecatmeans <- sapply(agecats, function(a) mean(subset(participants, agecat == a)$age))
#agecatmeans <- aggregate(participants$age, by=list(participants$agecat), FUN=mean)$x

variable <- function(v, data=d) # "imp", "neg", "whq", "ynq"
  subset(data, var == v)

changing <- subset(d, var != "neg")
changing$var <- factor(changing$var)
neg <- subset(d, var == "neg")
imp <- subset(d, var == "imp")
whq <- subset(d, var == "whq")
ynq <- subset(d, var == "ynq")

# given the two distribution of responses to younger/older, what is the
# baseline distribution of differences between them that could have occurred by
# chance?

diffchancelevel <- function(p1, p2, diff) {
  nlevels <- length(p1)
  sum(sapply((1 + max(0, -diff)) : (nlevels - max(0, diff)), function(first) p1[[first]] * p2[[first+diff]]))
}

diffchancelevels <- function(p1, p2=p1) {
  nlevels <- length(p1)
  sapply((-nlevels+1):(nlevels-1), function(diff) diffchancelevel(p1, p2, diff))
}
# plot baseline distribution of derived responses assuming uniform responses
#barplot(diffchancelevels(rep(0.2, 5)), col=temp.colors(9))

#diffchancelevels(rep(0.2, 5), rep(0.2, 5))
#diffchancelevels(c(0,0,1,0,0), rep(0.2, 5))

# grammaticality judgments

ids <- as.character(unique(da$id))
suppressMessages(library(foreach))
jdg <- read.csv("judgments.csv", sep="\t")
jdg <- foreach (i = 1:nrow(jdg), .combine=rbind) %do%
  data.frame(id=ids, var=as.character(jdg$var[i]), verb=as.character(jdg$verb[i]), incoming=jdg$variant[i]=="incoming", judgment=as.numeric(jdg[i,paste("X", ids, sep="")]), stringsAsFactors=FALSE)
# make var a factor (with identical ordering as the other dataset)
jdg$var <- factor(jdg$var, levels(d$var))

# filter single NA
jdg <- jdg[complete.cases(jdg),]

# we have to do some sort of aggregation first because not every verb was
# presented in both the new and old variant context. here we just take the mean
# across all verbs contexts, but could filter down to only those presented with
# both variants etc.
meanjudgments <- aggregate(jdg$judgment, by=list(id=jdg$id, var=jdg$var, incoming=jdg$incoming), FUN=mean)
# now put incoming+outgoing judgments on same row
meanjudgments <- aggregate(meanjudgments$x, by=list(id=meanjudgments$id, var=meanjudgments$var), FUN=c)
# positive values -> incoming variant rated more highly
meanjudgments$absdiff <- meanjudgments$x[,2] - meanjudgments$x[,1]
meanjudgments$reldiff <- meanjudgments$x[,2] / meanjudgments$x[,1]
# merge grammaticality judgments and usage level estimates by speaker "id" and "var": intersect(names(meanjudgments), names(da))

# approach #2: only average ratings given for verbs rated in both
# incoming+outgoing context
matchedjudgments <- merge(subset(jdg, incoming)[-4], subset(jdg, !incoming)[-4], by=c("id", "var", "verb"))
colnames(matchedjudgments)[4:5] <- c("incoming", "outgoing")

meanmatchedjudgments <- aggregate(matchedjudgments[c("incoming", "outgoing")], by=list(id=matchedjudgments$id, var=matchedjudgments$var), FUN=mean)
meanmatchedjudgments$absdiff <- meanmatchedjudgments$incoming - meanmatchedjudgments$outgoing
meanmatchedjudgments$reldiff <- meanmatchedjudgments$incoming / meanmatchedjudgments$outgoing

matchedjudgments$absdiff <- matchedjudgments$incoming - matchedjudgments$outgoing
matchedjudgments$reldiff <- matchedjudgments$incoming / matchedjudgments$outgoing
#aggregate(jdg$judgment, by=list(id=jdg$id, var=jdg$var, verb=jdg$verb), FUN=c)$x

plotjudgmentcor <- function(judgments, c1, c2, render=c1, xlab=paste("estimated usage level (", c1, ")", sep=""), ylab=paste("relative acceptability (incoming", if (c2=="absdiff") "-" else "/", "outgoing)"), main=NULL, ...) {
  d <- merge(judgments, da)
  d$var <- factor(d$var)
  # R's core cor.test can't do p values for ties
#  print(cor(as.numeric(d[[c1]]), d[[c2]], method="kendall"))
#  kendall <- cor.test(as.numeric(d[[c1]]), d[[c2]], method="kendall")
  # rpudplus can (rpucor.test()), but requires a license for the test
#  print(rpud::rpucor(cbind(as.numeric(d[[c1]]), d[[c2]]), method="kendall", use="pairwise"))
  # calculate 95% confidence interval of tau (tau slightly different here)
#  print(DescTools::KendallTauB(as.numeric(d[[c1]]), d[[c2]], 0.95))
  # apparently the best package (with p values accounting for ties) is pvrank:
  kendall <- pvrank::rankor(as.numeric(d[[c1]]), d[[c2]], "kendall", print=FALSE, type="greater")
  plot(as.numeric(d[[render]]), d[[c2]], xlim=c(0.8, 5.2), xlab=xlab, ylab=ylab, main=if (is.null(main)) bquote(tau[B] == .(round(kendall$Value, 3)) ~ "(p" == .(paste(round(kendall$Cpv, 3), ")", sep=""))) else main, col=changingcolors[d$var], xaxt="n", pch=4, pty="s", ...)
  axis(1, 1:5, levels(d[[c1]]), las=3)
  legend("topleft", levels(d$var), fill=changingcolors)
  # plot relative judgment ('equal acceptability') baseline
  abline(h=if(c2=="absdiff") 0 else 1, lty=2)
}

# usageestimate could be "self", "other" or "selfdiff"
# for the 'judgments' argument, see below
plotjudgments <- function(judgments=meanmatchedjudgments, usageestimate="jitteredself", ...) {
  par(mfrow=c(1,2))
  plotjudgmentcor(judgments, usageestimate, "absdiff", ...)
  plotjudgmentcor(judgments, usageestimate, "reldiff", ...)
}
# this is the plot averaged over all lexical items
#plotjudgments(meanjudgments)
# averaged only over matched lexical items (slightly higher)
#plotjudgments(meanmatchedjudgments)
#plotjudgments(meanmatchedjudgments, "other")
# this plot over all individual items (strong lexical effects)
#plotjudgments(matchedjudgments)

# intensity in (0,1]
temp.colors <- function(mn, mx=NULL, intensity=1) {
  if (is.null(mx)) {
    mx <- floor(mn/2)
    mn <- ceiling(-mn/2)
  }
  hsv(c(rep(0.65, abs(mn)), FALSE, rep(0, abs(mx))), intensity*abs(mn:mx)/max(abs(c(mn,mx))))
}
