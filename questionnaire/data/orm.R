# utility functions for mixed effects ordered logistic regression
# for documentation, see orderedlogisticregression.pdf in this directory

format.p.value <- function(p) {
  p[which(p <= 0)] <- NA # bogus Chi2 tests with df=0
  ifelse(p < .001, "< .001", sub("^0", "", round(p, 3)))
}

library(ordinal)
# TODO need to fix this...
dummymdl <- clm(self ~ age, data=d)
clme <- function(..., attempts=10) {
  clmm2call <- match.call()
  clmm2call[[1]] <- quote(clmm2)
  if (is.null(clmm2call$data))
    clmm2call$data <- d
  clmm2call$Hess <- TRUE
#  clmm2call$control <- clmm2.control(innerCtrl="giveError")
  attempt <- 0
  warning <- TRUE
  while (!is.null(warning) && attempt < attempts) {
    warning <- NULL
    attempt <- attempt+1
    mdl <- withCallingHandlers(eval.parent(clmm2call),
      warning=function(w) {
        if (w$message != "design appears to be rank-deficient, so dropping some coefs") {
          warning <<- w
          invokeRestart("muffleWarning")
        }
      })
  }
  if (!is.null(warning)) {
    stop(paste("clmm2 fit failed in all", attempt, "attempts:", warning))
  }
  # stargazer() supports clm but no clmm models
  mdl$call <- dummymdl$call
  mdl$alpha <- mdl$Alpha

  return(mdl)
}

library(stargazer, quietly=TRUE) # l before #?
ormtable <- function(..., of="", title=paste("Ordered logistic regression model (coefficients and standard errors)", of), table.layout="=!#-!t-!s=n", showstats=TRUE, omit.table.layout=if (!showstats) "s=", ord.intercepts=TRUE, table.placement="htbp", dep.var="") {
  stargazer(..., ord.intercepts=ord.intercepts, title=title, no.space=TRUE, star.cutoffs=c(0.05, 0.01, 0.001), table.layout=table.layout, omit.table.layout=omit.table.layout, table.placement=table.placement, dep.var.caption=dep.var)
}

# performs pairwise model comparison
modelcomparison <- function(..., modellabels=NULL) {
  models <- list(...)
  if (length(models) < 2)
    stop('Require at least two models for comparison')
  if (is.null(modellabels))
    modellabels <- 0:(length(models)-1)
  models <- models[order(sapply(models, function(x) x$df.residual), decreasing = TRUE)]
  if (any(!sapply(models, inherits, "clm2")))
    stop('not all objects are of class "clm2"')
  ns <- sapply(models, function(x) length(x$fitted.values))
  if(any(ns != ns[1]))
    stop("models were not all fitted to the same size of dataset")
  rsp <- unique(sapply(models, function(x) {
                       tmp <- attr(x$location, "terms")
                       class(tmp) <- "formula"
                       paste(tmp[2]) } ))
  mds <- sapply(models, function(x) {
      modelname <- gsub(" * ", ":", deparse(attr(x$location, "terms")[[3]]), fixed=TRUE)#⨯
#      if (!is.null(x$stDev))
#        modelname <- paste(modelname, " + (1|", names(varmdl$stDev), ")", sep="")
      if (!is.null(x$scale))
        modelname <- paste(modelname, "|", attr(x$scale, "terms")[[2]])
      if (!is.null(x$nominal))
        modelname <- paste(modelname, "|", attr(x$nominal, "terms")[[2]])
      modelname
    })
  lls <- sapply(models, function(x) -2*x$logLik)
  dfs <- sapply(models, function(x) x$df.residual)
  # find the next consecutive model with lower residual df
  baseline <- 1+length(dfs)-apply(outer(dfs, dfs, FUN=">"), 2, function(col) match(TRUE, rev(col)))
  df <- dfs[baseline] - dfs
  x2 <- lls[baseline] - lls
  pr <- c(NA, 1 - pchisq(x2[-1], df[-1]))
  out <- data.frame(Model = mds, Resid.df = dfs, '-2logLik' = lls, Test=ifelse(is.na(baseline), NA, paste(modellabels[baseline], "vs", modellabels)), Df = df, LRtest = x2, Prob = pr)
  rownames(out) <- paste("(", modellabels, ")", sep="")
  names(out) <- c("Model", "Res. df", "-2LL", "Test", "df", "LR", "P(>Chi)")
  class(out) <- c("Anova", "data.frame")
  attr(out, "heading") <- c("Likelihood ratio tests of cumulative link models\n", paste("Response:", rsp))
  out
}

modelcomparisontable <- function(anova, of="", title=paste("Pairwise model comparison", of), label=NULL, decmodelnumbers=TRUE, ...) {
#  if (decmodelnumbers)
#    rownames(anova) <- 0:(nrow(anova)-1)
  # p value will be in the last column (hopefully)
  anova[,ncol(anova)] <- format.p.value(anova[,ncol(anova)])
  stargazer(anova[,!(names(anova) %in% c("BIC"))], summary=FALSE, title=title, label=label)
}

ormtables <- function(nullmdl, ..., label=NULL, of=NULL, title=paste("Ordered logistic regression model (coefficients and standard errors)", of), ord.intercepts=TRUE) {
  ormtable(..., showstats=FALSE, of=of, title=title, ord.intercepts=ord.intercepts, label=label)
  modelcomparisontable(do.call("modelcomparison", if (is.null(nullmdl)) list(...) else c(list(nullmdl), list(...))), decmodelnumbers=!is.null(nullmdl), title=paste("Pairwise comparison of the models in Table~\\ref{", label, "}.", sep=""), label=if (!is.null(label)) paste(label, "comparison", sep=""))
}
