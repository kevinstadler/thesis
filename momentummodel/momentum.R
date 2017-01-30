library(functional)

last <- function(l)
  tail(l,n=1)
lplot <- function(...) plot(..., type='l')
lplot2 <- function(...) lplot(..., lty=3) # dotted lines as default alternative, 2 for dashed

# plot multiple curves in one plot using different line styles. length(pars) should not be more than 6.
# passing ... to both plot() AND curve() will inadvertently cause warnings, but it's the only way to
# get plot options like xlim/ylim applied to the initial plot
# TODO FIXME make explicitly past xlim override c(from,to)
curves <- function(fun, pars, from=0, to=1, parname=NULL, legendpos="topleft", ...) {
  plot(0, xlim=c(from, to), xaxs='i', ..., type='n')
  ltys = rep_len(1:5, length(pars))
  mapply(function(p, l) curve(fun(p, x), add=TRUE, lty=l, from=from, to=to, ...), pars, ltys)
  if (!is.null(parname))
    legend(legendpos, sapply(pars, function(x) paste(parname, '=', x, sep='')), lty=ltys)
}

#par(mar=c(0,0,0,0), pty='s') #, xaxs='i', yaxs='i') # axis spacing types do not seem to work globally

lambdatoalpha <- function(lambda) lambda/(1+lambda)
alphatolambda <- function(alpha) alpha/(1-alpha)

#curve(alphatolambda, from=0, to=1, xlab=expression(alpha), ylab=expression(lambda))
#curve(alphatolambda, from=0, to=1, xlab=expression(alpha), ylab=expression(lambda), log='y')

# x is the old average, y the new data point
ewma <- function(alpha, x, y) {
  alpha*y+(1-alpha)*x
}

expdecay <- function(alpha, t, x0=1, y=0) {
  y+(x0-y)*exp(-alpha*t)
}

# returns the timestep/iteration at which the difference between the
# two exponential decays alpha and gamma is greatest
stepstomaxdifference <- function(alpha, gamma) {
  log(alpha/gamma)/(alpha-gamma)
}

# returns the maximum decay difference
# y will not make a difference to when the maximum is reached, but to the absolute amplitude of the maximum
maxdecaydifference <- function(alpha, gamma, x0=1, y=0) {
  t <- stepstomaxdifference(alpha, gamma) # rounding optional!
  expdecay(alpha,t,x0=x0,y=y)-expdecay(gamma,t,x0=x0,y=y)
}

normaliseb <- function(b, alpha, gamma) b/maxdecaydifference(alpha, gamma)
limit <- function(x) pmin(1,pmax(x,0))

# demo simulations pass the current interaction no. i to the production function to allow for fine control

# dataprodfun(..., i, x) returns a single value between 0 and 1, the very first call will be for i=2
singlepositiveoutlier <- function(i, x) if (i==12) 1 else 0
singlenegativeoutlier <- function(i, x) if (i==12) 0 else 1
#singlesingleoutlier <- function(T, i, x) if (i==2) 1/T else 0
singleoutlieranalytic <- function(i, x) if (i==12) 1 else x # this is equivalent to an infinite T
singleoutlierthenstochastic <- function(T, i, x) if (i==12) 1 else rbinom(1, T, x)/T


applybias <- function(normalisedb, agent, data) {
  if (data == 0 || data == 1)
    data
  else
    limit(data+normalisedb*(agent[3]-agent[2]))
}

#biaseverywhere <- function(normalisedb, momentum, data) {
#  data+normalisedb*momentum
#}
#biasmiddle <- function(normalisedb, momentum, data) {
#  if (data == 0 || data == 1) data else data+normalisedb*momentum
#}

# updatefun(..., <agentrow>, data) returns three values, 1. the updated x (based on f(y)), 2. whatever is fed to the momentum-EWMAs 3. f(y)
# normalisedb will normally be computed by normaliseb(b, beta, gamma)
# open question: bias amplification for 0/T data points as well???
objectivemomentum <- function(normalisedb, alpha, beta, gamma, agent, data) {
  fdata = if (data == 0 || data == 1) data else limit(data+normalisedb*(agent[3]-agent[2]))
  c(ewma(alpha, agent[1], fdata), ewma(beta, agent[2], data), ewma(gamma, agent[3], data), fdata)
}
perceivedmomentum <- function(normalisedb, alpha, beta, gamma, agent, data) {
  fdata = if (data == 0 || data == 1) data else limit(data+normalisedb*(agent[3]-agent[2]))
  c(ewma(alpha, agent[1], fdata), ewma(beta, agent[2], fdata), ewma(gamma, agent[3], fdata), fdata)
}
selfmomentum <- function(normalisedb, alpha, beta, gamma, agent, data) {
  fdata = if (data == 0 || data == 1) data else limit(data+normalisedb*(agent[3]-agent[2]))
  updated = ewma(alpha, agent[1], fdata)
  c(updated, ewma(beta, agent[2], updated), ewma(gamma, agent[3], updated), fdata)
}

# NEW WAY TO DO IT
#betafun(prevagent, data, fdata, newx)
copynewx <- function(prevthis, data, fdata, newx) {
  newx
}
objectivemomentum <- function(alpha, prevthis, data, fdata, newx) {
  ewma(alpha, prevthis, data)
}
perceivedmomentum <- function(alpha, prevthis, data, fdata, newx) {
  ewma(alpha, prevthis, fdata)
}
selfmomentum <- function(alpha, prevthis, data, fdata, newx) {
  ewma(alpha, prevthis, newx)
}

# the individual simulation is mainly for demonstration purposes and therefore passes the iteration number to the prodfun as well
# the output matrix has 4 rows: 1 = x, 2 = ewmab, 3 = ewmag, 4 = y (biased target)
# all learning rates go straight into the update function
individualsimulation <- function(dataprodfun, alpha, biasfun, betafun, gammafun, interactions, x0) {
  pop <- matrix(nrow=4, ncol=interactions)
  pop[,1] <- c(x0, x0, x0, NA)
  for (i in 2:interactions) {
  	data <- dataprodfun(i, pop[1,i-1])
  	fdata <- biasfun(pop[,i-1], data)
  	newx <- ewma(alpha, pop[1,i-1], fdata)
    pop[,i] <- c(newx, betafun(pop[2,i-1], data, fdata, newx), gammafun(pop[3,i-1], data, fdata, newx), fdata)
  }
  pop
}

# lty's
# 1: x solid black line, left axis
ALPHA=1
X=1
# 2: m dotted line, right axis
GAMMA=3
MOMENTUMAXIS=3
# 3: f(x) red dashed line, left axis
FY=2
MOMENTUM=2
plotindividualsimulation <- function(data, alpha, beta, gamma, add=FALSE, col='black') {
  if (add)
    par(new=T)
  else
    # mar: bottom, left, top, right - defaults 5.1, 4.1, 4.1, 2.1 - increase right to fit 2nd y axis label
    par(mar=c(5.1, 4.1, 3.1, 4.1))

  lplot(data[1,], lty=ALPHA, yaxs='i', ylim=0:1, axes=!add, xlab=if (add) '' else paste('interactions, t=', round(1/alphatolambda(alpha)^2), sep=''), ylab=if (add) '' else 'frequency')

  # plot (biased) target!
  if (dim(data)[1] > 3) {
    lines(data[4,], type='l', lty=GAMMA, col='red')
  }

  if (dim(data)[1] > 1) {
    # plot dotted line in the model to show momentum zero point
    abline(h=0.5, lty=GAMMA, col='grey')
    par(new=T)
    maxdiff = maxdecaydifference(beta, gamma)
    lplot(apply(data, 2, function(x) x[3]-x[2]), col=col, yaxs='i', lty=MOMENTUM, axes=F, xlab=NA, ylab=NA, ylim=c(-maxdiff, maxdiff)) # ylim=c(-1,1)
    if (!add) {
      axis(4, col='gray')
      mtext("momentum", side=4, line=3) # or line 2
    }
  }
}

objectivemomentumbs = c(1, 5, 200)
perceivedmomentumbs = c(1, 2, 3)
selfmomentumbs = c(0.12, 0.15, 1)

# single outlier
comparemomentumssingleoutlier <- function(alpha=0.01, beta=alpha, gamma=2*beta, x0s=0:2/20, interactions=200, onefigure=TRUE) {
  if (onefigure) par(mfrow=c(3,3))
  # run 'true' case with bias only applied in the middle only once?
  for (i in 1:3) {
  	# TODO check symmetry
  	for (x0 in x0s)
      plotindividualsimulation(individualsimulation(singlepositiveoutlier, alpha, Curry(applybias, normaliseb(objectivemomentumbs[i], beta, gamma)), Curry(objectivemomentum, beta), Curry(objectivemomentum, gamma), interactions, x0), alpha, beta, gamma, add=(x0!=0))
    title(paste("objective, b=", objectivemomentumbs[i], sep=''))
  	for (x0 in x0s)
      plotindividualsimulation(individualsimulation(singlepositiveoutlier, alpha, Curry(applybias, normaliseb(perceivedmomentumbs[i], beta, gamma)), Curry(perceivedmomentum, beta), Curry(perceivedmomentum, gamma), interactions, x0), alpha, beta, gamma, add=(x0!=0))
    title(paste("perceived, b=", perceivedmomentumbs[i], sep=''))
    # selfmomentum is weird re: maximum values
    # selfmomentum is SUPERdirectional (i.e. can become completely immune to external input based on internal momentum bias)
    for (x0 in x0s)
      plotindividualsimulation(individualsimulation(singlepositiveoutlier, alpha, Curry(applybias, normaliseb(selfmomentumbs[i], beta, gamma)/alpha), Curry(selfmomentum, beta), Curry(selfmomentum, gamma), interactions, x0), alpha, beta, gamma, add=(x0!=0))
    title(paste("self, b=", selfmomentumbs[i], sep=''))
  }
}

# comparemomentumssingleoutlier()

analyticobjectivemomentumbs = c(0.25, 0.5, 1)
analyticobjperceivedmomentumbs = c(0.1, 0.15, 0.25)
analyticperceivedmomentumbs = c(0.25, 0.5, 0.75)
analyticperceivedobjmomentumbs = c(1, 5, 100)
analyticselfmomentumbs = c(0.02, 0.1, 0.2)

# single outlier+analytic
comparemomentumssingleoutlieranalytic <- function(alpha=0.01, beta=alpha, gamma=2*beta, x0s=0, interactions=900, onefigure=TRUE) {
  if (onefigure) par(mfcol=c(5,3))
  for (i in 1:3) {
    for (x0 in x0s)
      plotindividualsimulation(individualsimulation(singleoutlieranalytic, alpha, Curry(applybias, normaliseb(analyticobjectivemomentumbs[i], beta, gamma)), Curry(objectivemomentum, beta), Curry(objectivemomentum, gamma), interactions, x0), alpha, beta, gamma, add=(x0!=0))
    title(paste("objective, b=", analyticobjectivemomentumbs[i], sep=''))
    for (x0 in x0s)
      plotindividualsimulation(individualsimulation(singleoutlieranalytic, alpha, Curry(applybias, normaliseb(analyticobjperceivedmomentumbs[i], beta, gamma)), Curry(objectivemomentum, beta), Curry(perceivedmomentum, gamma), interactions, x0), alpha, beta, gamma, add=(x0!=0))
    title(paste("obj-perceived, b=", analyticobjperceivedmomentumbs[i], sep=''))
    for (x0 in x0s)
      plotindividualsimulation(individualsimulation(singleoutlieranalytic, alpha, Curry(applybias, normaliseb(analyticperceivedobjmomentumbs[i], beta, gamma)), Curry(perceivedmomentum, beta), Curry(objectivemomentum, gamma), interactions, x0), alpha, beta, gamma, add=(x0!=0))
    title(paste("perceived-obj, b=", analyticperceivedobjmomentumbs[i], sep=''))
    for (x0 in x0s)
      plotindividualsimulation(individualsimulation(singleoutlieranalytic, alpha, Curry(applybias, normaliseb(analyticperceivedmomentumbs[i], beta, gamma)), Curry(perceivedmomentum, beta), Curry(perceivedmomentum, gamma), interactions, x0), alpha, beta, gamma, add=(x0!=0))
    title(paste("perceived, b=", analyticperceivedmomentumbs[i], sep=''))
    # selfmomentum is weird re: maximum values
    for (x0 in x0s)
      plotindividualsimulation(individualsimulation(singleoutlieranalytic, alpha, Curry(applybias, normaliseb(analyticselfmomentumbs[i], beta, gamma)/alpha), Curry(selfmomentum, beta), Curry(selfmomentum, gamma), interactions, x0), alpha, beta, gamma, add=(x0!=0))
    title(paste("self, b=", analyticselfmomentumbs[i], sep=''))
  }
}

#pdf("singleoutlieranalytic.pdf")
#comparemomentumssingleoutlieranalytic(onefigure=TRUE)
#dev.off()

stochasticprod <- function(T, x) rbinom(1, T, x)/T
# biased variation/mutation
# mutationprod <- 

epsilon <- 0.02
fullnetworksimulation <- function(dataprodfun, alpha, biasfun, betafun, gammafun, n, interactionsperagent, x0fun) {
  pop <- matrix(nrow=3, ncol=n) # row 1 = x, row 2 = ewmab, row 3 = ewmag (4 = dummy last f(y) they incorporated)
  pop[1,] = pop[2,] = pop[3,] = replicate(n, x0fun())
  # measures = meanx meanm, stdx stdm, minx minm, maxx, maxm
  measures <- matrix(nrow=8, ncol=interactionsperagent+1)
  measures[,1] <- c(mean(pop[1,]), 0, sd(pop[1,]), 0, min(pop[1,]), 0, max(pop[1,]), 0)
  for (i in 2:(interactionsperagent*n+1)) {
    a1 <- sample(n,1)
    a2 <- ((a1+sample(n-1,1))%%(n-1))+1
    # feed them each other's data
    d1 <- dataprodfun(pop[1,a1])
    d2 <- dataprodfun(pop[1,a2])
    dfor2 <- biasfun(pop[,a2], d1)
    dfor1 <- biasfun(pop[,a1], d2)
    new2 <- ewma(alpha, pop[1,a2], dfor2)
    new1 <- ewma(alpha, pop[1,a1], dfor1)
    pop[,a1] = c(new1, betafun(pop[2,a1], d2, dfor1, new1), gammafun(pop[3,a1], d2, dfor1, new1))
    pop[,a2] = c(new2, betafun(pop[2,a2], d1, dfor2, new2), gammafun(pop[3,a2], d1, dfor2, new2))
    if ((i %% n)==1) {
      momentums = pop[3,]-pop[2,]
      measures[,(i%/%n)+1] = c(mean(pop[1,]), mean(momentums), sd(pop[1,]), sd(momentums), min(pop[1,]), min(momentums), max(pop[1,]), max(momentums))
      # call range(pop[1,]) and range(momentums) instead
      if (measures[1,(i%/%n)+1] <= epsilon || measures[1,(i%/%n)+1]+epsilon >= 1) {
      	# break early due to convergence
        return(measures[,1:(i%/%n)+1])
      }
    }
  }
  measures
}

# glm fit to inflection point (t0) conversion
inflectionpoint <- function(fit) {
  -coef(fit)[1]/coef(fit)[2]
}

# helper function to convert the glm log-odds into logistic growth rate+inflection point format
glmtort0 <- function (generalisedlinearmodel) {
	r <- coef(generalisedlinearmodel)['t']
	list(r=r, t0=coef(generalisedlinearmodel)['(Intercept)']/-r)
}

logistic <- function (r, t0, t) {
	1/(1+exp(-r*(t-t0)))
}

logisticmodel <- x ~ 1/(1+exp(-r*(t-t0)))

# sane lower bounds for the parameter fitting with "port"
lowerbounds <- c(-1000, 0.0001) # .Machine$double.xmin is way too small

#print(fitlogistic(logistic(0.1, 1000, 1:1050)))
# returns: t0, r, stderr(t0), stderr(r), logLik, isconverged?
fitlogistic <- function(data, guessedt0=NULL, guessedr=NULL, ignorenonconvergence=FALSE) {
  # if doing nonlinear fit then calculate a guessedrate and guessedinflectionpoint first
  if (is.null(guessedt0)) {
    # since there might be earlier unsuccessful transitions, find last datapoint <= 50%
    guessedt0 <- tail(which(data <= 0.5), n=1)
  }
  if (is.null(guessedr)) {
    # guess slope at inflection point
    if (data[guessedt0] < 0.2) { # this should only happen for ongoing curves which are hopeless for fitting
#     print("not even attempting logistic fit")
      cat("*")
      return(c(rep(NA, 5), FALSE))
    }
    pret0 <- tail(which(data <= data[guessedt0]-0.1), n=1)
    guessedr <- 4*(0.1/(guessedt0-pret0))
  }
  if (is.na(guessedr)) {
    cat('b') # bail
    return(c(rep(NA, 5), FALSE))
  }
#  start <- c(-guessedr*guessedt0, guessedr)
#  model <- glm(cbind(x,nx) ~ t, data=list(t=1:length(data), x=data, nx=1-data), family=binomial(logit), start=start)
#  glmtort0(model)
  model <- NULL
  try(model <- nls(logisticmodel, data=list(t=1:length(data), x=data), start=c(t0=guessedt0, r=guessedr), algorithm="port", lower=lowerbounds, control=nls.control(warnOnly=TRUE)), silent=TRUE)
  if (is.null(model)) {
    cat("-")
    return(c(rep(NA, 4), FALSE))
  }
  if (!model$convInfo$isConv) {
    if (ignorenonconvergence) {
      cat("~")
    } else {
      cat("!") # unexpected non-convergence
 #     write(data, "results/failed", ncolumns=1)
      # run once again for trace
      try(nls(logisticmodel, data=list(t=1:length(data), x=data), start=c(t0=guessedt0, r=guessedr), algorithm="port", lower=lowerbounds, trace=TRUE, control=nls.control(warnOnly=TRUE)))
    }
    # summary() on non-converged objects might fail, supply NAs for parameter stderrs
    cs <- c(coef(model), NA, NA)
  } else {
    cat("+")
    cs <- summary(model)$coefficients[,1:2]
  }
  c(cs, logLik(model), model$convInfo$isConv)
}

# rows of data: meanx meanm, minx minm, maxx maxm
plotsimulation <- function(data, alpha, beta, gamma, add=FALSE, col='black') {
  if (add)
    par(new=T)
  else
    # mar: bottom, left, top, right - defaults 5.1, 4.1, 4.1, 2.1 - increase right to fit 2nd y axis label
    par(mar=c(5.1, 4.1, 3.1, 4.1))

  extracap <- ''
  coefs <- c()
  if (data[1,dim(data)[2]] + epsilon >= 1) {
    model <- fitlogistic(data[1,])
    coefs <- glmtort0(model)
    extracap <- paste(', r=', round(coef(model)[2], digits=3), sep='')
  }

  lplot(data[1,], lty=ALPHA, yaxs='i', ylim=0:1, axes=!add, xlab=if (add) '' else paste('interactions/individual, t=', round(1/alphatolambda(alpha)^2), extracap, sep=''), ylab=if (add) '' else 'frequency')

  if (length(coefs)) {
    curve(logistic(coefs[1], coefs[2], x), from=1, to=dim(data)[2], col="blue", add=TRUE)
  }

  # plot min/max interval
  if (dim(data)[1] > 2) {
    lines(data[3,], type='l', lty=ALPHA)
    lines(data[5,], type='l', lty=ALPHA)
  }

  # plot the analytical biased target
  lines(limit(data[1,]+data[2,]/maxdecaydifference(beta, gamma)), col='red')

  # plot dotted line in the model to show momentum zero point
  abline(h=0.5, lty=GAMMA, col='grey')
  par(new=T)
  maxdiff = maxdecaydifference(beta, gamma)
#  lplot(apply(data, 2, function(x) x[3]-x[2]), col=col, yaxs='i', lty=MOMENTUM, axes=F, xlab=NA, ylab=NA, ylim=c(-maxdiff, maxdiff)) # ylim=c(-1,1)
  lplot(data[2,], col=col, yaxs='i', lty=MOMENTUM, axes=F, xlab=NA, ylab=NA, ylim=c(-maxdiff, maxdiff)) # ylim=c(-1,1)
  if (dim(data)[1] > 2) {
    lines(data[4,], type='l', lty=MOMENTUM)
    lines(data[6,], type='l', lty=MOMENTUM)
  }
  if (!add) {
    axis(4, col='gray')
    mtext("momentum", side=4, line=3) # or line 2
  }
}

alpha=0.01
beta=alpha
gamma=0.02

n=10
interactionsperagent=6000
b=2
T=2
x0=0.05
#x0=1-x0

plotseveral <- function(trials=3) {
  par(mfcol=c(5,trials))
  replicate(trials, {
    plotsimulation(fullnetworksimulation(Curry(stochasticprod, T), alpha, Curry(applybias, normaliseb(b, beta, gamma)), Curry(objectivemomentum, beta), Curry(objectivemomentum, gamma), n, interactionsperagent, function()x0), alpha, beta, gamma)
    title(paste("objective", sep=''))
    plotsimulation(fullnetworksimulation(Curry(stochasticprod, T), alpha, Curry(applybias, normaliseb(b, beta, gamma)), Curry(objectivemomentum, beta), Curry(perceivedmomentum, gamma), n, interactionsperagent, function()x0), alpha, beta, gamma)
    title(paste("obj-perceived", sep=''))
    plotsimulation(fullnetworksimulation(Curry(stochasticprod, T), alpha, Curry(applybias, normaliseb(b, beta, gamma)), Curry(perceivedmomentum, beta), Curry(objectivemomentum, gamma), n, interactionsperagent, function()x0), alpha, beta, gamma)
    title(paste("perceived-obj", sep=''))
    plotsimulation(fullnetworksimulation(Curry(stochasticprod, T), alpha, Curry(applybias, normaliseb(b, beta, gamma)), Curry(perceivedmomentum, beta), Curry(perceivedmomentum, gamma), n, interactionsperagent, function()x0), alpha, beta, gamma)
    title(paste("perceived-perceived", sep=''))
    plotsimulation(fullnetworksimulation(Curry(stochasticprod, T), alpha, Curry(applybias, normaliseb(b, beta, gamma)/alpha), Curry(selfmomentum, beta), Curry(selfmomentum, gamma), n, interactionsperagent, function()x0), alpha, beta, gamma)
    title(paste("self", sep=''))
#    plotsimulation(fullnetworksimulation(Curry(stochasticprod, T), Curry(perceivedmomentum, normaliseb(b, beta, gamma), alpha, beta, gamma), n, interactionsperagent, function()x0), alpha, beta, gamma)
#    plotsimulation(fullnetworksimulation(Curry(stochasticprod, T), Curry(selfmomentum, normaliseb(b, beta, gamma)/alpha, alpha, beta, gamma), n, interactionsperagent, function()x0), alpha, beta, gamma)
  })
}
#plotseveral(6)

# 5: txmax and the 4 x data points
maxxdata <- function(data) {
  txmax <- which.max(data[1,])
  c(txmax, data[1:4,txmax])
}

# 9: tmmax, the 4 m data points and 4 x datapoints at tmmax
maxmomentumdata <- function(data) {
  tmmax <- which.max(data[2,])
  c(tmmax, data[5:8,tmmax], data[1:4,tmmax])
}

# returns the time span within which the bulk of the change happens.
# the default returns the duration it takes to go from 0.1 to 0.9
bulkofchange <- function(r, cutoff=0.95) {
#  (log(1/cutoff)-log(1/(1-cutoff)))/r
  log(cutoff/(1-cutoff))/r
}
# bulkofchange(0.1, cutoff=0.9)
# [1] 21.97225

# logistic(0.1, 0, 21.97225) - logistic(0.1, 0, -21.97225)
# [1] 0.8000001

xnames <- c("x", "xstd", "xmin", "xmax") # 4 (txmax optional, since it can be at the end)
momentumnames <- c("tmmax", "mmax", "mmaxstd", "mmaxmin", "mmaxmax", paste("mmax", xnames, sep='')) # 9
logisticnames <- c("t0", "r", "t0stderr", "rstderr", "loglik", "conv")

# columns written out into the various files for later analysis:
returnednames <- c("tend", "txmax", paste("xmax", xnames, sep=''), momentumnames)
# length(returnednames) = 15
diffusednames <- c("tend", momentumnames, logisticnames, paste("symm", logisticnames, sep=''), paste("fst", logisticnames, sep=''), paste("snd", logisticnames, sep=''))
# length(diffusednames) = 34
ongoingnames <- c("tend", paste("end", xnames, sep=''), "txmax", paste("xmax", xnames, sep=''), momentumnames, logisticnames)
# length(ongoingnames) = 25
# returned <- c() # vector of interactions/agent until transitioned to 0
# diffused <- c() # vector of logistic fits?
# ongoing <- c() # vector of mean values at the end of the period
runbatch <- function (n, simfun, fileprefix="") {
  returnedf <- paste(fileprefix, "returned", sep='')
  diffusedf <- paste(fileprefix, "diffused", sep='')
  ongoingf <- paste(fileprefix, "ongoing", sep='')
  for (i in 1:n) {
    data <- simfun()
    tend <- dim(data)[2]
    result <- data[1,tend]
    if (result <= epsilon) {
      cat(".")
#      returned <- c(returned, dim(data)[2])
      # 1 + 5 + 9 = 15
      write(c(tend, maxxdata(data), maxmomentumdata(data)), returnedf, append=TRUE, ncolumns=length(returnednames))
    } else if (result + epsilon >= 1) {
      cat("S")
      model <- fitlogistic(data[1,])
      # model with symmetric amount of data before and after inflection point
      # approximate number of datapoints between inflection point and end
      # ndata <- dim(data)[2] - model[1] # bad idea, tail might be indefinitely long
      ndata <- bulkofchange(model[2])
      symstart <- max(1,round(model[1]-ndata))
      symend <- min(dim(data)[2],round(model[1]+ndata))
      symmdata <- data[1,symstart:symend]
      symmmodel <- fitlogistic(symmdata, guessedt0=round(ndata), guessedr=model[2])
      # see if different rates are fitted to the first or second half
      firstmodel <- fitlogistic(head(symmdata, n=round(ndata)), guessedt0=round(ndata), guessedr=symmmodel[2])
      secondmodel <- fitlogistic(tail(symmdata, n=round(ndata)), guessedt0=0, guessedr=symmmodel[2])
      # offset t0 of the second model (relative to the data size) so that it is comparable across datasets
      symmmodel[1] <- symmmodel[1] + symstart - 1
      firstmodel[1] <- firstmodel[1] + symstart - 1
      secondmodel[1] <- secondmodel[1] + symstart -1 + round(ndata)
      # 1 + 9 + (5 + 5 + 5 + 5)
      write(c(tend, maxmomentumdata(data), model, symmmodel, firstmodel, secondmodel), diffusedf, append=TRUE, ncolumns=length(diffusednames))
      if (interactive()) {
        plot(1:dim(data)[2], data[1,], ylim=0:1, type='l')
        curve(logistic(model[2], model[1], x), add=TRUE, col='red') # model
        curve(logistic(symmmodel[2], symmmodel[1], x), add=TRUE, col='green', from=symstart, to=symend)
        curve(logistic(firstmodel[2], firstmodel[1], x), add=TRUE, col='blue', from=symstart, to=symstart+round(ndata)) # splitmodel
        curve(logistic(secondmodel[2], secondmodel[1], x), add=TRUE, col='blue', from=symstart+round(ndata), to=symend) # splitmodel
        # TODO plot momentum alongside it
      }
#      diffused = c(diffused, coef(model)) # write m_max, m_min, t_mmax, r, t0, loglik, rsymm, t0symm, logliksymm, rfirst, t0first, loglikfirst, rsecond, t0second, logliksecond
    } else {
      # 1 + 4 + 5 + 9 + 5 = 24
      cat("?")
      write(c(tend, data[1:4,tend], maxxdata(data), maxmomentumdata(data), fitlogistic(data[1,], ignorenonconvergence=TRUE)), ongoingf, append=TRUE, ncolumns=length(ongoingnames))
    }
  }
}

#runbatch(10, Curry(fullnetworksimulation, Curry(stochasticprod, T), , alpha, beta, gamma), n, interactionsperagent, function()x0))

constructfilename <- function(interactionsperagent, n, alpha, gamma, x0, b, T) {
  paste("results/", interactionsperagent, 'n', n, 'a', alpha, 'g', gamma, 'x', x0, 'b', b, 'T', T, sep='')
}

Ts <- 2:4
bs <- 1:3
x0s<- c(0.05, 0.1, 0.15) #0.025
ns <- c(2,10,20)
alphas <- c(0.01,0.005)
gammamults <- c(1.5, 2, 2.5)
interactionsperagent <- 10000
runconditions <- function(runs) {
  i <- 1
  nconditions <- length(Ts)*length(bs)*length(x0s)*length(ns)*length(alphas)*length(gammamults)
  for (alpha in alphas) {
    for (T in Ts) {
      for (n in ns) {
        for (gamma in alpha*gammamults) {
          for (x0 in x0s) {
            for (b in bs) {
              biasfun <- Curry(applybias, normaliseb(b, alpha, gamma))
              betafun <- copynewx # Curry(perceivedmomentum, beta)
              gammafun <- Curry(perceivedmomentum, gamma)
              fileprefix <- constructfilename(interactionsperagent, n, alpha, gamma, x0, b, T)
              print(paste("condition", i, "of", nconditions, "-", fileprefix, "for", runs))
              time <- system.time(runbatch(runs, Curry(fullnetworksimulation, Curry(stochasticprod, T), alpha, biasfun, betafun, gammafun, n, interactionsperagent, function()x0), fileprefix))
              cat("\n")
              print(time)
              i <- i+1
            }
          }
        }
      }
    }
  }
}

runs <- 500

if (!interactive()) {
  runconditions(runs)
}
