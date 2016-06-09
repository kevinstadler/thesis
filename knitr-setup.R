library(knitr)
knit_hooks$set(crop = hook_pdfcrop)
knit_hooks$set(plotsetup = function(before, options, envir) {
    if (before) par(family="serif", font.main=1, mgp=c(2.3, 1, 0))
})

# fullpage's 453pt = 6.29in
# normal textwidth 390pt = 5.4in
fullwidth <- 5.4
smallwidth <- 3.5

opts_chunk$set(echo=FALSE, message=FALSE, results="asis", fig.align="center", fig.pos="htbp", fig.width=fullwidth, fig.height=fullwidth, cache=TRUE, crop=TRUE, plotsetup=TRUE, dev.args=list(pointsize=10)) # colormodel="cmyk"

opts_knit$set(eval.after=c("fig.cap", "fig.subcap"))

tightmargin <- function(...)
  par(mar=c(3, 3.3, 2, 0.8), ...) # b l t r

library(xtable)

mathematise <- function(...)
  paste("$", ..., "$", sep="")

options(xtable.sanitize.text.function = identity,
  xtable.sanitize.rownames.function = mathematise,
  xtable.sanitize.colnames.function = mathematise,
  xtable.table.placement = "htbp")#, xtable.booktabs=TRUE)
