library(knitr)

defaultpar <- function(...)
  par(font.main=1, mgp=c(2.1, 0.8, 0), ...) # family="serif"

knit_hooks$set(plotsetup = function(before=TRUE, options, envir) if (before) defaultpar())
knit_hooks$set(crop = hook_pdfcrop)

# delay switch to scientific format number printing - default is 10^4, increase
options(scipen=2) # digits=

# fullpage's 453pt = 6.29in
# normal textwidth 390pt = 5.4in
fullwidth <- 5.4
smallwidth <- 3.5

opts_chunk$set(echo=FALSE, message=FALSE, results="asis", fig.align="center", fig.pos="htbp", fig.width=fullwidth, fig.height=fullwidth, cache=TRUE, crop=TRUE, plotsetup=TRUE, dev.args=list(pointsize=10, family="serif", colormodel="cmyk"))
library(lattice)
trellis.par.set(fontsize=list(text=10))
#trellis.par.get("fontsize")

opts_knit$set(eval.after=c("fig.cap", "fig.subcap", "fig.scap"))

tightmargin <- function(...)
  defaultpar(mar=c(3.1, 3.3, 2, 0.8), ...) # b l t r

library(xtable)

mathematise <- function(...)
  paste("$", ..., "$", sep="")

options(xtable.sanitize.text.function = identity,
  xtable.sanitize.rownames.function = mathematise,
  xtable.sanitize.colnames.function = mathematise,
  xtable.table.placement = "htbp")#, xtable.booktabs=TRUE)

temp.colors <- function(mn, mx=NULL, intensity=1) {
  if (is.null(mx)) {
    mx <- floor(mn/2)
    mn <- ceiling(-mn/2)
  }
  hsv(c(rep(0.65, abs(mn)), FALSE, rep(0, abs(mx))), intensity*abs(mn:mx)/max(abs(c(mn,mx))))
}
