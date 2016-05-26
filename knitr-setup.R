library(knitr)
knit_hooks$set(crop = hook_pdfcrop)

# fullpage's 453pt = 6.29in
opts_chunk$set(echo=FALSE, message=FALSE, results="asis", fig.align="center", fig.pos="htbp", fig.width=6.29, fig.height=6.29, crop=TRUE, cache=TRUE)

opts_knit$set(eval.after='fig.subcap')

library(xtable)
options(xtable.sanitize.text.function=identity, xtable.sanitize.rownames.function=identity, xtable.sanitize.colnames.function=identity)#, xtable.booktabs=TRUE)
