library(knitr)
knit_hooks$set(crop = hook_pdfcrop)

# fullpage's 453pt = 6.29in
opts_chunk$set(echo=FALSE, message=FALSE, results="asis", fig.align="center", fig.pos="htbp", fig.width=6.29, fig.height=6.29, crop=TRUE, cache=TRUE)

opts_knit$set(eval.after=c("fig.cap", "fig.subcap"))

tightmargin <- function(...)
  par(mgp=c(2, 1, 0), mar=c(3, 3.3, 2, 0.8), ...) # b l t r

library(magrittr)
library(xtable)

mathematise <- function(...)
  paste("$", ..., "$", sep="")

options(xtable.sanitize.text.function = identity,
  xtable.sanitize.rownames.function = mathematise,
  xtable.sanitize.colnames.function = mathematise,
  xtable.table.placement = "htbp")#, xtable.booktabs=TRUE)

par(mgp=c(2.3, 1, 0), font.main=1)
