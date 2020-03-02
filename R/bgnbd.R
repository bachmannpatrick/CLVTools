################################################## BG/NBD estimation, visualization functions

library(hypergeo)

bgnbd.cbs.LL <- function(params, vX, vT_x, vT_cal) {

  dc.check.model.params(c("log.r", "log.alpha", "log.a", "log.b"), exp(params), "bgnbd.cbs.LL")

  tryCatch(x <- vX, error = function(e) stop("Error in bgnbd.cbs.LL: cal.cbs must have a frequency column labelled \"x\""))
  tryCatch(t.x <- vT_x, error = function(e) stop("Error in bgnbd.cbs.LL: cal.cbs must have a recency column labelled \"t.x\""))
  tryCatch(T.cal <- vT_cal, error = function(e) stop("Error in bgnbd.cbs.LL: cal.cbs must have a column for length of time observed labelled \"T.cal\""))

  # if ("custs" %in% colnames(cal.cbs)) {
  #
  #   many_rows = function(vec, nreps) {
  #     return(rep(1, nreps) %*% t.default(vec))
  #   }
  #
  #   custs <- cal.cbs[, "custs"]
  #   logvec = (1:length(custs)) * (custs > 1)
  #   logvec = logvec[logvec > 0]
  #   M = sum(logvec > 0)
  #   for (i in 1:M) {
  #     cal.cbs = rbind(cal.cbs, many_rows(cal.cbs[logvec[i], ], custs[logvec[i]] -
  #                                          1))
  #   }
  #   x = vX
  #   t.x = vT_x
  #   T.cal = vT_cal
  # }
  result <- sum(-bgnbd.LL(params, x, t.x, T.cal))
  return(result)
}

bgnbd.LL <- function(params, x, t.x, T.cal) {

  beta.ratio = function(a, b, x, y) {
    exp(lgamma(a) + lgamma(b) - lgamma(a + b) - lgamma(x) - lgamma(y) + lgamma(x +
                                                                                 y))
  }

  max.length <- max(length(x), length(t.x), length(T.cal))

  if (max.length%%length(x))
    warning("Maximum vector length not a multiple of the length of x")
  if (max.length%%length(t.x))
    warning("Maximum vector length not a multiple of the length of t.x")
  if (max.length%%length(T.cal))
    warning("Maximum vector length not a multiple of the length of T.cal")

  dc.check.model.params(c("log.r", "log.alpha", "log.a", "log.b"), exp(params), "bgnbd.LL")

  if (any(x < 0) || !is.numeric(x))
    stop("x must be numeric and may not contain negative numbers.")
  if (any(t.x < 0) || !is.numeric(t.x))
    stop("t.x must be numeric and may not contain negative numbers.")
  if (any(T.cal < 0) || !is.numeric(T.cal))
    stop("T.cal must be numeric and may not contain negative numbers.")

  r = exp(params[1])
  alpha = exp(params[2])
  a = exp(params[3])
  b = exp(params[4])

  x <- rep(x, length.out = max.length)
  t.x <- rep(t.x, length.out = max.length)
  T.cal <- rep(T.cal, length.out = max.length)

  A = r * log(alpha) + lgamma(r + x) - lgamma(r) - (r + x) * log(alpha + t.x)
  B = beta.ratio(a, b + x, a, b) * ((alpha + t.x)/(alpha + T.cal))^(r + x) + as.numeric((x >
                                                                                           0)) * beta.ratio(a + 1, b + x - 1, a, b)
  LL = sum(A + log(B))

  return(LL)
}

bgnbd.compress.cbs <- function(cbs, rounding = 3) {

  if (!("x" %in% colnames(cbs)))
    stop("Error in bgnbd.compress.cbs: cbs must have a frequency column labelled \"x\"")
  if (!("t.x" %in% colnames(cbs)))
    stop("Error in bgnbd.compress.cbs: cbs must have a recency column labelled \"t.x\"")
  if (!("T.cal" %in% colnames(cbs)))
    stop("Error in bgnbd.compress.cbs: cbs must have a column for length of time observed labelled \"T.cal\"")

  orig.rows <- nrow(cbs)

  if (!("custs" %in% colnames(cbs))) {
    custs <- rep(1, nrow(cbs))
    cbs <- cbind(cbs, custs)
  }

  other.colnames <- colnames(cbs)[!(colnames(cbs) %in% c("x", "t.x", "T.cal"))]

  ## Round x, t.x and T.cal to the desired level
  cbs[, c("x", "t.x", "T.cal")] <- round(cbs[, c("x", "t.x", "T.cal")], rounding)

  ## Aggregate every column that is not x, t.x or T.cal by those columns. Do this by
  ## summing entries which have the same x, t.x and T.cal.
  cbs <- as.matrix(aggregate(cbs[, !(colnames(cbs) %in% c("x", "t.x", "T.cal"))],
                             by = list(x = cbs[, "x"], t.x = cbs[, "t.x"], T.cal = cbs[, "T.cal"]), sum))

  colnames(cbs) <- c("x", "t.x", "T.cal", other.colnames)
  final.rows <- nrow(cbs)
  message("Data reduced from ", orig.rows, " rows to ", final.rows, " rows.")
  return(cbs)
}

bgnbd.EstimateParameters <- function(cal.cbs, par.start = c(1, 3, 1, 3), max.param.value = 10000) {

  dc.check.model.params(c("r", "alpha", "a", "b"), par.start, "bgnbd.EstimateParameters")

  bgnbd.eLL <- function(params, cal.cbs, max.param.value) {
    params <- exp(params)
    params[params > max.param.value] = max.param.value
    return(-1 * bgnbd.cbs.LL(params, cal.cbs))
  }
  logparams = log(par.start)
  results = optim(logparams, bgnbd.eLL, cal.cbs = cal.cbs, max.param.value = max.param.value,
                  method = "L-BFGS-B")
  estimated.params <- exp(results$par)
  estimated.params[estimated.params > max.param.value] <- max.param.value
  return(estimated.params)
}

bgnbd.pmf <- function(params, t, x) {
  max.length <- max(length(t), length(x))
  if (max.length%%length(t))
    warning("Maximum vector length not a multiple of the length of t")
  if (max.length%%length(x))
    warning("Maximum vector length not a multiple of the length of x")
  dc.check.model.params(c("r", "alpha", "a", "b"), params, "bgnbd.pmf")

  if (any(t < 0) || !is.numeric(t))
    stop("t must be numeric and may not contain negative numbers.")
  if (any(x < 0) || !is.numeric(x))
    stop("x must be numeric and may not contain negative numbers.")

  t. <- rep(t, length.out = max.length)
  x <- rep(x, length.out = max.length)

  return(bgnbd.pmf.General(params, 0, t, x))
}

bgnbd.pmf.General <- function(params, t.start, t.end, x) {

  max.length = max(length(t.start), length(t.end), length(x))

  if (max.length%%length(t.start))
    warning("Maximum vector length not a multiple of the length of t.start")
  if (max.length%%length(t.end))
    warning("Maximum vector length not a multiple of the length of t.end")
  if (max.length%%length(x))
    warning("Maximum vector length not a multiple of the length of x")

  dc.check.model.params(c("r", "alpha", "a", "b"), params, "bgnbd.pmf.General")

  if (any(t.start < 0) || !is.numeric(t.start))
    stop("t.start must be numeric and may not contain negative numbers.")
  if (any(t.end < 0) || !is.numeric(t.end))
    stop("t.end must be numeric and may not contain negative numbers.")
  if (any(x < 0) || !is.numeric(x))
    stop("x must be numeric and may not contain negative numbers.")

  t.start = rep(t.start, length.out = max.length)
  t.end = rep(t.end, length.out = max.length)
  x = rep(x, length.out = max.length)

  if (any(t.start > t.end)) {
    stop("Error in bgnbd.pmf.General: t.start > t.end.")
  }
  r <- params[1]
  alpha <- params[2]
  a <- params[3]
  b <- params[4]
  equation.part.0 <- rep(0, max.length)
  t = t.end - t.start
  term3 = rep(0, max.length)
  term1 = beta(a, b + x)/beta(a, b) * gamma(r + x)/gamma(r)/factorial(x) * ((alpha/(alpha +
                                                                                      t))^r) * ((t/(alpha + t))^x)

  for (i in 1:max.length) {
    if (x[i] > 0) {
      ii = c(0:(x[i] - 1))
      summation.term = sum(gamma(r + ii)/gamma(r)/factorial(ii) * ((t[i]/(alpha +
                                                                            t[i]))^ii))
      term3[i] = 1 - (((alpha/(alpha + t[i]))^r) * summation.term)
    }
  }
  term2 = as.numeric(x > 0) * beta(a + 1, b + x - 1)/beta(a, b) * term3

  return(term1 + term2)
}


bgnbd.ConditionalExpectedTransactions <- function(params, T.star, x, t.x, T.cal) {

  h2f1 <- function(a, b, c, z) {
    lenz <- length(z)
    j = 0
    uj <- 1:lenz
    uj <- uj/uj
    y <- uj
    lteps <- 0

    while (lteps < lenz) {
      lasty <- y
      j <- j + 1
      uj <- uj * (a + j - 1) * (b + j - 1)/(c + j - 1) * z/j
      y <- y + uj
      lteps <- sum(y == lasty)
    }
    return(y)
  }

  max.length <- max(length(T.star), length(x), length(t.x), length(T.cal))

  if (max.length%%length(T.star))
    warning("Maximum vector length not a multiple of the length of T.star")
  if (max.length%%length(x))
    warning("Maximum vector length not a multiple of the length of x")
  if (max.length%%length(t.x))
    warning("Maximum vector length not a multiple of the length of t.x")
  if (max.length%%length(T.cal))
    warning("Maximum vector length not a multiple of the length of T.cal")

  dc.check.model.params(c("r", "alpha", "a", "b"), params, "bgnbd.ConditionalExpectedTransactions")

  if (any(T.star < 0) || !is.numeric(T.star))
    stop("T.star must be numeric and may not contain negative numbers.")
  if (any(x < 0) || !is.numeric(x))
    stop("x must be numeric and may not contain negative numbers.")
  if (any(t.x < 0) || !is.numeric(t.x))
    stop("t.x must be numeric and may not contain negative numbers.")
  if (any(T.cal < 0) || !is.numeric(T.cal))
    stop("T.cal must be numeric and may not contain negative numbers.")

  x <- rep(x, length.out = max.length)
  t.x <- rep(t.x, length.out = max.length)
  T.cal <- rep(T.cal, length.out = max.length)

  r = params[1]
  alpha = params[2]
  a = params[3]
  b = params[4]
  term1 <- ((a + b + x - 1)/(a - 1))
  term2 <- 1 - ((alpha + T.cal)/(alpha + T.cal + T.star))^(r + x) * h2f1(r + x,
                                                                         b + x, a + b + x - 1, T.star/(alpha + T.cal + T.star))
  term3 <- 1 + as.numeric(x > 0) * (a/(b + x - 1)) * ((alpha + T.cal)/(alpha +
                                                                         t.x))^(r + x)
  out <- term1 * term2/term3
  return(out)
}

bgnbd.PAlive <- function(params, x, t.x, T.cal) {

  max.length <- max(length(x), length(t.x), length(T.cal))

  if (max.length%%length(x))
    warning("Maximum vector length not a multiple of the length of x")
  if (max.length%%length(t.x))
    warning("Maximum vector length not a multiple of the length of t.x")
  if (max.length%%length(T.cal))
    warning("Maximum vector length not a multiple of the length of T.cal")

  dc.check.model.params(c("r", "alpha", "a", "b"), params, "bgnbd.PAlive")

  if (any(x < 0) || !is.numeric(x))
    stop("x must be numeric and may not contain negative numbers.")
  if (any(t.x < 0) || !is.numeric(t.x))
    stop("t.x must be numeric and may not contain negative numbers.")
  if (any(T.cal < 0) || !is.numeric(T.cal))
    stop("T.cal must be numeric and may not contain negative numbers.")

  x <- rep(x, length.out = max.length)
  t.x <- rep(t.x, length.out = max.length)
  T.cal <- rep(T.cal, length.out = max.length)

  r = params[1]
  alpha = params[2]
  a = params[3]
  b = params[4]
  term1 = (a/(b + x - 1)) * ((alpha + T.cal)/(alpha + t.x))^(r + x)
  return(1/(1 + as.numeric(x > 0) * term1))
}

bgnbd.Expectation <- function(params, t) {

  h2f1 <- function(a, b, c, z) {
    lenz <- length(z)
    j = 0
    uj <- 1:lenz
    uj <- uj/uj
    y <- uj
    lteps <- 0

    while (lteps < lenz) {
      lasty <- y
      j <- j + 1
      uj <- uj * (a + j - 1) * (b + j - 1)/(c + j - 1) * z/j
      y <- y + uj
      lteps <- sum(y == lasty)
    }
    return(y)
  }

  if (any(t < 0) || !is.numeric(t))
    stop("t must be numeric and may not contain negative numbers.")
  r = params[1]
  alpha = params[2]
  a = params[3]
  b = params[4]

  dc.check.model.params(c("r", "alpha", "a", "b"), params, "bgnbd.Expectation")

  term1 = (a + b - 1)/(a - 1)
  term2 = (alpha/(alpha + t))^r
  term3 = h2f1(r, b, a + b - 1, t/(alpha + t))
  output = term1 * (1 - term2 * term3)

  return(output)
}

bgnbd.PlotFrequencyInCalibration <- function(params, cal.cbs, censor, plotZero = TRUE,
                                             xlab = "Calibration period transactions", ylab = "Customers", title = "Frequency of Repeat Transactions") {

  tryCatch(x <- cal.cbs[, "x"], error = function(e) stop("Error in bgnbd.PlotFrequencyInCalibration: cal.cbs must have a frequency column labelled \"x\""))
  tryCatch(T.cal <- cal.cbs[, "T.cal"], error = function(e) stop("Error in bgnbd.PlotFrequencyInCalibration: cal.cbs must have a column for length of time observed labelled \"T.cal\""))

  dc.check.model.params(c("r", "alpha", "a", "b"), params, "bgnbd.PlotFrequencyInCalibration")
  if (censor > max(x))
    stop("censor too big (> max freq) in PlotFrequencyInCalibration.")

  x = cal.cbs[, "x"]
  T.cal = cal.cbs[, "T.cal"]
  n.x <- rep(0, max(x) + 1)
  ncusts = nrow(cal.cbs)
  for (ii in unique(x)) {
    # Get number of customers to buy n.x times, over the grid of all possible n.x
    # values (no censoring)
    n.x[ii + 1] <- sum(ii == x)
  }
  n.x.censor <- sum(n.x[(censor + 1):length(n.x)])
  n.x.actual <- c(n.x[1:censor], n.x.censor)  # This upper truncates at censor (ie. if censor=7, 8 categories: {0, 1, ..., 6, 7+}).
  T.value.counts <- table(T.cal)  # This is the table of counts of all time durations from customer birth to end of calibration period.
  T.values <- as.numeric(names(T.value.counts))  # These are all the unique time durations from customer birth to end of calibration period.
  n.T.values <- length(T.values)  # These are the number of time durations we need to consider.
  n.x.expected <- rep(0, length(n.x.actual))  # We'll store the probabilities in here.
  n.x.expected.all <- rep(0, max(x) + 1)  # We'll store the probabilities in here.

  for (ii in 0:max(x)) {
    # We want to run over the probability of each transaction amount.
    this.x.expected = 0
    for (T.idx in 1:n.T.values) {
      # We run over all people who had all time durations.
      T = T.values[T.idx]
      if (T == 0)
        next
      n.T = T.value.counts[T.idx]  # This is the number of customers who had this time duration.
      prob.of.this.x.for.this.T = bgnbd.pmf(params, T, ii)
      expected.given.x.and.T = n.T * prob.of.this.x.for.this.T
      this.x.expected = this.x.expected + expected.given.x.and.T
    }
    n.x.expected.all[ii + 1] = this.x.expected
  }
  n.x.expected[1:censor] = n.x.expected.all[1:censor]
  n.x.expected[censor + 1] = sum(n.x.expected.all[(censor + 1):(max(x) + 1)])

  col.names <- paste(rep("freq", length(censor + 1)), (0:censor), sep = ".")
  col.names[censor + 1] <- paste(col.names[censor + 1], "+", sep = "")
  censored.freq.comparison <- rbind(n.x.actual, n.x.expected)
  colnames(censored.freq.comparison) <- col.names
  cfc.plot <- censored.freq.comparison
  if (plotZero == FALSE)
    cfc.plot <- cfc.plot[, -1]
  n.ticks <- ncol(cfc.plot)
  if (plotZero == TRUE) {
    x.labels <- 0:(n.ticks - 1)
    x.labels[n.ticks] <- paste(n.ticks - 1, "+", sep = "")
  }
  ylim <- c(0, ceiling(max(cfc.plot) * 1.1))
  barplot(cfc.plot, names.arg = x.labels, beside = TRUE, ylim = ylim, main = title,
          xlab = xlab, ylab = ylab, col = 1:2)
  legend("topright", legend = c("Actual", "Model"), col = 1:2, lwd = 2)
  return(censored.freq.comparison)
}


bgnbd.PlotFreqVsConditionalExpectedFrequency <- function(params, T.star, cal.cbs,
                                                         x.star, censor, xlab = "Calibration period transactions", ylab = "Holdout period transactions",
                                                         xticklab = NULL, title = "Conditional Expectation") {
  tryCatch(x <- cal.cbs[, "x"], error = function(e) stop("Error in bgnbd.PlotFreqVsConditionalExpectedFrequency: cal.cbs must have a frequency column labelled \"x\""))
  tryCatch(t.x <- cal.cbs[, "t.x"], error = function(e) stop("Error in bgnbd.PlotFreqVsConditionalExpectedFrequency: cal.cbs must have a recency column labelled \"t.x\""))
  tryCatch(T.cal <- cal.cbs[, "T.cal"], error = function(e) stop("Error in bgnbd.PlotFreqVsConditionalExpectedFrequency: cal.cbs must have a column for length of time observed labelled \"T.cal\""))

  dc.check.model.params(c("r", "alpha", "a", "b"), params, "bgnbd.PlotFreqVsConditionalExpectedFrequency")
  if (censor > max(x))
    stop("censor too big (> max freq) in PlotFreqVsConditionalExpectedFrequency.")

  if (any(T.star < 0) || !is.numeric(T.star))
    stop("T.star must be numeric and may not contain negative numbers.")
  if (any(x.star < 0) || !is.numeric(x.star))
    stop("x.star must be numeric and may not contain negative numbers.")

  n.bins = censor + 1
  transaction.actual = rep(0, n.bins)
  transaction.expected = rep(0, n.bins)
  bin.size = rep(0, n.bins)
  for (cc in 0:censor) {
    if (cc != censor) {
      this.bin = which(cc == x)
    } else if (cc == censor) {
      this.bin = which(x >= cc)
    }
    n.this.bin = length(this.bin)
    bin.size[cc + 1] = n.this.bin
    transaction.actual[cc + 1] = sum(x.star[this.bin])/n.this.bin
    transaction.expected[cc + 1] = sum(bgnbd.ConditionalExpectedTransactions(params,
                                                                             T.star, x[this.bin], t.x[this.bin], T.cal[this.bin]))/n.this.bin
  }
  col.names = paste(rep("freq", length(censor + 1)), (0:censor), sep = ".")
  col.names[censor + 1] = paste(col.names[censor + 1], "+", sep = "")
  comparison = rbind(transaction.actual, transaction.expected, bin.size)
  colnames(comparison) = col.names
  if (is.null(xticklab) == FALSE) {
    x.labels = xticklab
  }
  if (is.null(xticklab) != FALSE) {
    if (censor < ncol(comparison)) {
      x.labels = 0:(censor)
      x.labels[censor + 1] = paste(censor, "+", sep = "")
    }
    if (censor >= ncol(comparison)) {
      x.labels = 0:(ncol(comparison))
    }
  }
  actual = comparison[1, ]
  expected = comparison[2, ]
  ylim = c(0, ceiling(max(c(actual, expected)) * 1.1))
  plot(actual, type = "l", xaxt = "n", col = 1, ylim = ylim, xlab = xlab, ylab = ylab,
       main = title)
  lines(expected, lty = 2, col = 2)
  axis(1, at = 1:ncol(comparison), labels = x.labels)
  legend("topleft", legend = c("Actual", "Model"), col = 1:2, lty = 1:2, lwd = 1)
  return(comparison)
}

bgnbd.PlotRecVsConditionalExpectedFrequency <- function(params, cal.cbs, T.star,
                                                        x.star, xlab = "Calibration period recency", ylab = "Holdout period transactions",
                                                        xticklab = NULL, title = "Actual vs. Conditional Expected Transactions by Recency") {

  dc.check.model.params(c("r", "alpha", "a", "b"), params, "bgnbd.PlotRecVsConditionalExpectedFrequency")

  if (any(T.star < 0) || !is.numeric(T.star))
    stop("T.star must be numeric and may not contain negative numbers.")
  if (any(x.star < 0) || !is.numeric(x.star))
    stop("x.star must be numeric and may not contain negative numbers.")

  tryCatch(x <- cal.cbs[, "x"], error = function(e) stop("Error in bgnbd.PlotRecVsConditionalExpectedFrequency: cal.cbs must have a frequency column labelled \"x\""))
  tryCatch(t.x <- cal.cbs[, "t.x"], error = function(e) stop("Error in bgnbd.PlotRecVsConditionalExpectedFrequency: cal.cbs must have a recency column labelled \"t.x\""))
  tryCatch(T.cal <- cal.cbs[, "T.cal"], error = function(e) stop("Error in bgnbd.PlotRecVsConditionalExpectedFrequency: cal.cbs must have a column for length of time observed labelled \"T.cal\""))

  t.values <- sort(unique(t.x))
  n.recs <- length(t.values)
  transaction.actual <- rep(0, n.recs)
  transaction.expected <- rep(0, n.recs)
  rec.size <- rep(0, n.recs)

  for (tt in 1:n.recs) {
    this.t.x <- t.values[tt]
    this.rec <- which(t.x == this.t.x)
    n.this.rec <- length(this.rec)
    rec.size[tt] <- n.this.rec
    transaction.actual[tt] <- sum(x.star[this.rec])/n.this.rec
    transaction.expected[tt] <- sum(bgnbd.ConditionalExpectedTransactions(params,
                                                                          T.star, x[this.rec], t.x[this.rec], T.cal[this.rec]))/n.this.rec
  }

  comparison <- rbind(transaction.actual, transaction.expected, rec.size)
  colnames(comparison) <- round(t.values, 3)

  bins <- seq(1, ceiling(max(t.x)))
  n.bins <- length(bins)
  actual <- rep(0, n.bins)
  expected <- rep(0, n.bins)
  bin.size <- rep(0, n.bins)

  x.labels <- NULL
  if (is.null(xticklab) == FALSE) {
    x.labels <- xticklab
  } else {
    x.labels <- 1:(n.bins)
  }
  point.labels <- rep("", n.bins)
  point.y.val <- rep(0, n.bins)
  for (ii in 1:n.bins) {
    if (ii < n.bins) {
      this.bin <- which(as.numeric(colnames(comparison)) >= (ii - 1) & as.numeric(colnames(comparison)) <
                          ii)
    } else if (ii == n.bins) {
      this.bin <- which(as.numeric(colnames(comparison)) >= ii - 1)
    }
    actual[ii] <- sum(comparison[1, this.bin])/length(comparison[1, this.bin])
    expected[ii] <- sum(comparison[2, this.bin])/length(comparison[2, this.bin])
    bin.size[ii] <- sum(comparison[3, this.bin])
  }

  ylim <- c(0, ceiling(max(c(actual, expected)) * 1.1))
  plot(actual, type = "l", xaxt = "n", col = 1, ylim = ylim, xlab = xlab, ylab = ylab,
       main = title)
  lines(expected, lty = 2, col = 2)

  axis(1, at = 1:n.bins, labels = x.labels)
  legend("topleft", legend = c("Actual", "Model"), col = 1:2, lty = 1:2, lwd = 1)

  return(rbind(actual, expected, bin.size))
}

bgnbd.PlotTransactionRateHeterogeneity <- function(params, lim = NULL) {
  dc.check.model.params(c("r", "alpha", "a", "b"), params, "bgnbd.PlotTransactionRateHeterogeneity")
  shape <- params[1]
  rate <- params[2]
  rate.mean <- round(shape/rate, 4)
  rate.var <- round(shape/rate^2, 4)
  if (is.null(lim)) {
    lim = qgamma(0.99, shape = shape, rate = rate)
  }
  x.axis.ticks <- seq(0, lim, length.out = 100)
  heterogeneity <- dgamma(x.axis.ticks, shape = shape, rate = rate)
  plot(x.axis.ticks, heterogeneity, type = "l", xlab = "Transaction Rate", ylab = "Density",
       main = "Heterogeneity in Transaction Rate")
  mean.var.label <- paste("Mean:", rate.mean, "    Var:", rate.var)
  mtext(mean.var.label, side = 3)
  return(rbind(x.axis.ticks, heterogeneity))
}

bgnbd.PlotDropoutRateHeterogeneity <- function(params, lim = NULL) {
  dc.check.model.params(c("r", "alpha", "a", "b"), params, "bgnbd.PlotDropoutRateHeterogeneity")
  alpha_param = params[3]
  beta_param = params[4]
  beta_param.mean = round(alpha_param/(alpha_param + beta_param), 4)
  beta_param.var = round(alpha_param * beta_param/((alpha_param + beta_param)^2)/(alpha_param +
                                                                                    beta_param + 1), 4)
  if (is.null(lim)) {
    # get right end point of grid
    lim = qbeta(0.99, shape1 = alpha_param, shape2 = beta_param)
  }
  x.axis.ticks = seq(0, lim, length.out = 100)
  heterogeneity = dbeta(x.axis.ticks, shape1 = alpha_param, shape2 = beta_param)
  plot(x.axis.ticks, heterogeneity, type = "l", xlab = "Dropout Probability p",
       ylab = "Density", main = "Heterogeneity in Dropout Probability")
  mean.var.label = paste("Mean:", beta_param.mean, "    Var:", beta_param.var)
  mtext(mean.var.label, side = 3)
  return(rbind(x.axis.ticks, heterogeneity))
}

bgnbd.ExpectedCumulativeTransactions <- function(params, T.cal, T.tot, n.periods.final) {

  dc.check.model.params(c("r", "alpha", "s", "beta"), params, "bgnbd.ExpectedCumulativeTransactions")

  if (any(T.cal < 0) || !is.numeric(T.cal))
    stop("T.cal must be numeric and may not contain negative numbers.")

  if (length(T.tot) > 1 || T.tot < 0 || !is.numeric(T.tot))
    stop("T.cal must be a single numeric value and may not be negative.")
  if (length(n.periods.final) > 1 || n.periods.final < 0 || !is.numeric(n.periods.final))
    stop("n.periods.final must be a single numeric value and may not be negative.")

  intervals <- seq(T.tot/n.periods.final, T.tot, length.out = n.periods.final)

  cust.birth.periods <- max(T.cal) - T.cal

  expected.transactions <- sapply(intervals, function(interval) {
    if (interval <= min(cust.birth.periods))
      return(0)
    sum(bgnbd.Expectation(params, interval - cust.birth.periods[cust.birth.periods <=
                                                                  interval]))
  })

  return(expected.transactions)
}


bgnbd.PlotTrackingCum <- function(params, T.cal, T.tot, actual.cu.tracking.data,
                                  xlab = "Week", ylab = "Cumulative Transactions", xticklab = NULL, title = "Tracking Cumulative Transactions") {

  dc.check.model.params(c("r", "alpha", "a", "b"), params, "bgnbd.Plot.PlotTrackingCum")

  if (any(T.cal < 0) || !is.numeric(T.cal))
    stop("T.cal must be numeric and may not contain negative numbers.")
  if (any(actual.cu.tracking.data < 0) || !is.numeric(actual.cu.tracking.data))
    stop("actual.cu.tracking.data must be numeric and may not contain negative numbers.")

  if (length(T.tot) > 1 || T.tot < 0 || !is.numeric(T.tot))
    stop("T.cal must be a single numeric value and may not be negative.")

  actual <- actual.cu.tracking.data
  expected <- bgnbd.ExpectedCumulativeTransactions(params, T.cal, T.tot, length(actual))

  cu.tracking.comparison <- rbind(actual, expected)

  ylim <- c(0, max(c(actual, expected)) * 1.05)
  plot(actual, type = "l", xaxt = "n", xlab = xlab, ylab = ylab, col = 1, ylim = ylim,
       main = title)
  lines(expected, lty = 2, col = 2)
  if (is.null(xticklab) == FALSE) {
    if (ncol(cu.tracking.comparison) != length(xticklab)) {
      stop("Plot error, xticklab does not have the correct size")
    }
    axis(1, at = 1:ncol(cu.tracking.comparison), labels = xticklab)
  } else {
    axis(1, at = 1:length(actual), labels = 1:length(actual))
  }
  abline(v = max(T.cal), lty = 2)

  legend("bottomright", legend = c("Actual", "Model"), col = 1:2, lty = 1:2, lwd = 1)

  return(cu.tracking.comparison)
}

bgnbd.PlotTrackingInc <- function(params, T.cal, T.tot, actual.inc.tracking.data,
                                  xlab = "Week", ylab = "Transactions", xticklab = NULL, title = "Tracking Weekly Transactions") {

  dc.check.model.params(c("r", "alpha", "a", "b"), params, "bgnbd.Plot.PlotTrackingCum")

  if (any(T.cal < 0) || !is.numeric(T.cal))
    stop("T.cal must be numeric and may not contain negative numbers.")
  if (any(actual.inc.tracking.data < 0) || !is.numeric(actual.inc.tracking.data))
    stop("actual.inc.tracking.data must be numeric and may not contain negative numbers.")

  if (length(T.tot) > 1 || T.tot < 0 || !is.numeric(T.tot))
    stop("T.cal must be a single numeric value and may not be negative.")

  actual <- actual.inc.tracking.data
  expected <- dc.CumulativeToIncremental(bgnbd.ExpectedCumulativeTransactions(params,
                                                                              T.cal, T.tot, length(actual)))

  ylim <- c(0, max(c(actual, expected)) * 1.05)
  plot(actual, type = "l", xaxt = "n", xlab = xlab, ylab = ylab, col = 1, ylim = ylim,
       main = title)
  lines(expected, lty = 2, col = 2)
  if (is.null(xticklab) == FALSE) {
    if (length(actual) != length(xticklab)) {
      stop("Plot error, xticklab does not have the correct size")
    }
    axis(1, at = 1:length(actual), labels = xticklab)
  } else {
    axis(1, at = 1:length(actual), labels = 1:length(actual))
  }
  abline(v = max(T.cal), lty = 2)

  legend("topright", legend = c("Actual", "Model"), col = 1:2, lty = 1:2, lwd = 1)

  return(rbind(actual, expected))
}

################################################################################ Functions for Manipulating Data

library(Matrix)

dc.ElogToCbsCbt <- function(elog, per = "week", T.cal = max(elog$date), T.tot = max(elog$date),
                            merge.same.date = TRUE, cohort.birth.per = T.cal, dissipate.factor = 1, statistic = "freq") {

  dc.WriteLine("Started making CBS and CBT from the ELOG...")

  elog <- dc.FilterCustByBirth(elog, cohort.birth.per)
  if (nrow(elog) == 0)
    stop("error caused by customer birth filtering")

  elog <- elog[elog$date <= T.tot, ]
  if (nrow(elog) == 0)
    stop("error caused by holdout period end date")

  elog <- dc.DissipateElog(elog, dissipate.factor)
  if (nrow(elog) == 0)
    stop("error caused by event long dissipation")

  if (merge.same.date) {
    elog <- dc.MergeTransactionsOnSameDate(elog)
    if (nrow(elog) == 0)
      stop("error caused by event log merging")
  }

  calibration.elog <- elog[elog$date <= T.cal, ]
  holdout.elog <- elog[elog$date > T.cal, ]

  split.elog.list <- dc.SplitUpElogForRepeatTrans(calibration.elog)

  repeat.transactions.elog <- split.elog.list$repeat.trans.elog
  cust.data <- split.elog.list$cust.data


  dc.WriteLine("Started Building CBS and CBT for calibration period...")
  cbt.cal <- dc.BuildCBTFromElog(calibration.elog, statistic)
  cbt.cal.rep.trans <- dc.BuildCBTFromElog(repeat.transactions.elog, statistic)
  cbt.cal <- dc.MergeCustomers(cbt.cal, cbt.cal.rep.trans)

  dates <- data.frame(cust.data$birth.per, cust.data$last.date, T.cal)

  cbs.cal <- dc.BuildCBSFromCBTAndDates(cbt.cal, dates, per, cbt.is.during.cal.period = TRUE)

  dc.WriteLine("Finished building CBS and CBT for calibration period.")

  cbt.holdout <- NULL
  cbs.holdout <- NULL
  if (nrow(holdout.elog) > 0) {
    dc.WriteLine("Started building CBS and CBT for holdout period...")
    cbt.holdout <- dc.BuildCBTFromElog(holdout.elog, statistic)

    dates <- c((T.cal + 1), T.tot)
    cbs.holdout <- dc.BuildCBSFromCBTAndDates(cbt.holdout, dates, per, cbt.is.during.cal.period = FALSE)
    cbt.holdout <- dc.MergeCustomers(cbt.cal, cbt.holdout)
    cbs.holdout <- dc.MergeCustomers(cbs.cal, cbs.holdout)
    dc.WriteLine("Finished building CBS and CBT for holdout.")
    dc.WriteLine("...Finished Making All CBS and CBT")
    return(list(cal = list(cbs = cbs.cal, cbt = cbt.cal), holdout = list(cbt = cbt.holdout,
                                                                         cbs = cbs.holdout), cust.data = cust.data))
  }

  dc.WriteLine("...Finished Making All CBS and CBT")
  return(list(cal = list(cbs = cbs.cal, cbt = cbt.cal), holdout = list(cbt = cbt.holdout,
                                                                       cbs = cbs.holdout), cust.data = cust.data))
}

dc.FilterCustByBirth <- function(elog, cohort.birth.per) {
  L = length(cohort.birth.per)
  if (L > 2) {
    stop("Invalid cohort.birth.per argument")
  }
  if (L == 0) {
    return(elog)
  }
  if (L == 1) {
    start.date <- min(elog$date)
    end.date <- cohort.birth.per
  } else if (length(cohort.birth.per) == 2) {
    start.date <- min(cohort.birth.per)
    end.date <- max(cohort.birth.per)
  }
  cbt <- dc.CreateFreqCBT(elog)
  custs.first.transaction.indices <- dc.GetFirstPurchasePeriodsFromCBT(cbt)
  custs.first.transaction.dates <- as.Date(colnames(cbt)[custs.first.transaction.indices])
  custs.in.birth.period.indices <- which(custs.first.transaction.dates >= start.date &
                                           custs.first.transaction.dates <= end.date)
  custs.in.birth.period <- rownames(cbt)[custs.in.birth.period.indices]
  elog <- elog[elog$cust %in% custs.in.birth.period, ]
  dc.WriteLine("Finished filtering out customers not in the birth period.")
  return(elog)
}

dc.DissipateElog <- function(elog, dissipate.factor) {
  if (dissipate.factor > 1) {
    x <- rep(FALSE, dissipate.factor)
    x[1] <- TRUE
    keptIndices <- rep(x, length.out = nrow(elog))
    elog <- elog[keptIndices, ]
    elog$cust <- factor(elog$cust)
    dc.WriteLine("Finished filtering out", dissipate.factor - 1, "of every",
                 dissipate.factor, "transactions.")
  } else {
    dc.WriteLine("No dissipation requested.")
  }
  return(elog)
}

dc.MergeTransactionsOnSameDate <- function(elog) {
  dc.WriteLine("Started merging same-date transactions...")
  elog <- cbind(elog, 1:nrow(elog) * (!duplicated(elog[, c("cust", "date")])))

  aggr.elog <- aggregate(elog[, !(colnames(elog) %in% c("cust", "date"))], by = list(cust = elog[,
                                                                                                 "cust"], date = elog[, "date"]), sum)
  aggr.elog <- aggr.elog[order(aggr.elog[, ncol(aggr.elog)]), ][, -ncol(aggr.elog)]
  dc.WriteLine("... Finished merging same-date transactions.")
  return(aggr.elog)
}

dc.SplitUpElogForRepeatTrans <- function(elog) {
  dc.WriteLine("Started Creating Repeat Purchases")
  unique.custs <- unique(elog$cust)
  first.trans.indices <- rep(0, length(unique.custs))
  last.trans.indices <- rep(0, length(unique.custs))
  count <- 0
  for (cust in unique.custs) {
    count <- count + 1
    cust.indices <- which(elog$cust == cust)
    # Of this customer's transactions, find the index of the first one
    first.trans.indices[count] <- min(cust.indices[which(elog$date[cust.indices] ==
                                                           min(elog$date[cust.indices]))])

    # Of this customer's transactions, find the index of the last one
    last.trans.indices[count] <- min(cust.indices[which(elog$date[cust.indices] ==
                                                          max(elog$date[cust.indices]))])
  }
  repeat.trans.elog <- elog[-first.trans.indices, ]

  first.trans.data <- elog[first.trans.indices, ]
  last.trans.data <- elog[last.trans.indices, ]


  # [-1] is because we don't want to change the column name for custs
  names(first.trans.data)[-1] <- paste("first.", names(first.trans.data)[-1], sep = "")
  names(first.trans.data)[which(names(first.trans.data) == "first.date")] <- "birth.per"
  names(last.trans.data) <- paste("last.", names(last.trans.data), sep = "")

  # [-1] is because we don't want to include two custs columns
  cust.data <- data.frame(first.trans.data, last.trans.data[, -1])
  names(cust.data) <- c(names(first.trans.data), names(last.trans.data)[-1])

  dc.WriteLine("Finished Creating Repeat Purchases")
  return(list(repeat.trans.elog = repeat.trans.elog, cust.data = cust.data))
}



##' functions, so it is better to convert the date column to date
dc.BuildCBTFromElog <- function(elog, statistic = "freq") {
  dc.WriteLine("Started Building CBT...")
  if (statistic == "freq") {
    return(dc.CreateFreqCBT(elog))
  } else if (statistic == "reach") {
    return(dc.CreateReachCBT(elog))
  } else if (statistic == "total.spend") {
    return(dc.CreateSpendCBT(elog))
  } else if (statistic == "average.spend") {
    return(dc.CreateSpendCBT(elog, is.avg.spend = TRUE))
  } else {
    stop("Invalid cbt build (var: statistic) specified.")
  }
}

dc.CreateFreqCBT <- function(elog) {
  # Factoring is so that when xtabs sorts customers, it does so in the original
  # order It doesn't matter that they're factors; rownames are stored as characters
  elog$cust <- factor(elog$cust, levels = unique(elog$cust))
  xt <- xtabs(~cust + date, data = elog)
  dc.WriteLine("...Completed Freq CBT")
  return(xt)
}

dc.CreateReachCBT <- function(elog) {
  # Factoring is so that when xtabs sorts customers, it does so in the original
  # order It doesn't matter that they're factors; rownames are stored as characters
  elog$cust <- factor(elog$cust, levels = unique(elog$cust))
  xt <- xtabs(~cust + date, data = elog)
  xt[xt > 1] <- 1
  dc.WriteLine("...Completed Reach CBT")
  return(xt)
}

dc.CreateSpendCBT <- function(elog, is.avg.spend = FALSE) {
  # Factoring is so that when xtabs sorts customers, it does so in the original
  # order It doesn't matter that they're factors; rownames are stored as characters
  elog$cust <- factor(elog$cust, levels = unique(elog$cust))
  sales.xt <- xtabs(sales ~ cust + date, data = elog)
  if (is.avg.spend) {
    suppressMessages(freq.cbt <- dc.CreateFreqCBT(elog))
    sales.xt <- sales.xt/freq.cbt
    # For the cases where there were no transactions
    sales.xt[which(!is.finite(sales.xt))] <- 0
  }
  dc.WriteLine("...Completed Spend CBT")
  return(sales.xt)
}

dc.MakeRFmatrixSkeleton <- function(n.periods) {
  ## note: to access the starting i'th t.x element use (i>0): i*(i-1)/2 + 2, ...
  ## this yields the sequence: 2, 3, 5, 8, ... there are n*(n+1)/2 + 1 elements in
  ## this table
  n <- n.periods
  rf.mx.skeleton <- matrix(0, n * (n + 1)/2 + 1, 2)
  colnames(rf.mx.skeleton) <- c("x", "t.x")
  for (ii in 1:n) {
    ith.t.index <- 2 + ii * (ii - 1)/2
    t.vector <- rep(ii, ii)
    x.vector <- c(1:ii)
    rf.mx.skeleton[ith.t.index:(ith.t.index + (ii - 1)), 1] <- x.vector
    rf.mx.skeleton[ith.t.index:(ith.t.index + (ii - 1)), 2] <- t.vector
  }
  return(rf.mx.skeleton)
}

dc.MakeRFmatrixHoldout <- function(holdout.cbt) {

  holdout.length <- ncol(holdout.cbt)
  matrix.skeleton <- dc.MakeRFmatrixSkeleton(holdout.length)
  n.combinations <- nrow(matrix.skeleton)
  n.star <- rep(holdout.length, n.combinations)
  final.transactions <- dc.GetLastPurchasePeriodsFromCBT(holdout.cbt)
  custs <- rep(0, n.combinations)
  for (ii in 1:n.combinations) {
    custs.with.freq <- which(rowSums(holdout.cbt) == matrix.skeleton[ii, 1])
    custs.with.rec <- which(final.transactions == matrix.skeleton[ii, 2])
    custs[ii] <- length(intersect(custs.with.freq, custs.with.rec))
  }
  rf.holdout.matrix <- cbind(matrix.skeleton, n.star, custs)
  colnames(rf.holdout.matrix) <- c("x.star", "t.x.star", "n.star", "custs")
  return(rf.holdout.matrix)
}

dc.BuildCBSFromCBTAndDates <- function(cbt, dates, per, cbt.is.during.cal.period = TRUE) {
  if (cbt.is.during.cal.period == TRUE) {
    dc.WriteLine("Started making calibration period CBS...")
    custs.first.dates <- dates[, 1]
    custs.last.dates <- dates[, 2]
    T.cal <- dates[, 3]
    if (length(custs.first.dates) != length(custs.last.dates)) {
      stop("Invalid dates (different lengths) in BuildCBSFromFreqCBTAndDates")
    }

    f <- rowSums(cbt)
    r <- as.numeric(difftime(custs.last.dates, custs.first.dates, units = "days"))
    T <- as.numeric(difftime(T.cal, custs.first.dates, units = "days"))
    x <- switch(per, day = 1, week = 7, month = 365/12, quarter = 365/4, year = 365)
    r = r/x
    T = T/x
    cbs = cbind(f, r, T)
    # cbs <- data.frame(f=f, r=r/x, T=T/x)
    rownames(cbs) <- rownames(cbt)
    colnames(cbs) <- c("x", "t.x", "T.cal")
  } else {
    ## cbt is during holdout period
    dc.WriteLine("Started making holdout period CBS...")
    date.begin.holdout.period <- dates[1]
    date.end.holdout.period <- dates[2]
    f <- rowSums(cbt)
    T <- as.numeric(difftime(date.end.holdout.period, date.begin.holdout.period,
                             units = "days")) + 1
    x <- switch(per, day = 1, week = 7, month = 365/12, quarter = 365/4, year = 365)
    T = T/x
    cbs = cbind(f, T)
    # cbs <- data.frame( f=f, T=T/x)
    rownames(cbs) <- rownames(cbt)
    colnames(cbs) <- c("x.star", "T.star")
  }

  dc.WriteLine("Finished building CBS.")
  return(cbs)
}

dc.MergeCustomers <- function(data.correct, data.to.correct) {

  ## Initialize a new data frame
  data.to.correct.new <- matrix(0, nrow = nrow(data.correct), ncol = ncol(data.to.correct))
  # data.to.correct.new <- data.frame(data.to.correct.new.size)
  orig.order <- 1:nrow(data.correct)
  orig.order <- orig.order[order(rownames(data.correct))]
  data.correct.ordered <- data.correct[order(rownames(data.correct)), ]
  ## obscure code: handles boundary case when data.correct has one column and
  ## coerces data.correct.ordered to be a vector
  if (is.null(nrow(data.correct.ordered))) {
    # data.correct.ordered <- data.frame(data.correct.ordered)
    rownames(data.correct.ordered) <- rownames(data.correct)[order(rownames(data.correct))]
    colnames(data.correct.ordered) <- colnames(data.correct)
  }

  data.to.correct <- data.to.correct[order(rownames(data.to.correct)), ]
  rownames(data.to.correct.new) <- rownames(data.correct.ordered)
  colnames(data.to.correct.new) <- colnames(data.to.correct)

  ## Initialize the two iterators ii.correct, ii.to.correct
  ii.correct <- 1
  ii.to.correct <- 1

  ## Grab the data to hold the stopping conditions
  max.correct.iterations <- nrow(data.correct.ordered)
  max.to.correct.iterations <- nrow(data.to.correct)

  ## Grab the lists of customers from the data frames and convert them to optimize
  ## the loop speed
  cust.list.correct <- rownames(data.correct.ordered)
  cust.list.to.correct <- rownames(data.to.correct)

  cust.correct.indices <- c()
  cust.to.correct.indices <- c()


  while (ii.correct <= max.correct.iterations & ii.to.correct <= max.to.correct.iterations) {
    cur.cust.correct <- cust.list.correct[ii.correct]
    cur.cust.to.correct <- cust.list.to.correct[ii.to.correct]
    if (cur.cust.correct < cur.cust.to.correct) {
      ii.correct <- ii.correct + 1
    } else if (cur.cust.correct > cur.cust.to.correct) {
      ii.to.correct <- ii.to.correct + 1
    } else if (cur.cust.correct == cur.cust.to.correct) {
      ## data.to.correct.new[ii.correct, ] = data.to.correct[ii.to.correct, ]
      cust.correct.indices <- c(cust.correct.indices, ii.correct)
      cust.to.correct.indices <- c(cust.to.correct.indices, ii.to.correct)

      ii.correct <- ii.correct + 1
      ii.to.correct <- ii.to.correct + 1
    } else {
      stop("Array checking error in MergeCustomers")
    }
  }
  data.to.correct.new[cust.correct.indices, ] <- data.to.correct
  data.to.correct.new <- data.to.correct.new[order(orig.order), ]
  return(data.to.correct.new)
}

dc.RemoveTimeBetween <- function(elog, day1, day2, day3, day4) {
  if (day1 > day2 || day2 > day3 || day3 > day4) {
    stop("Days are not input in increasing order.")
  }
  elog1 <- elog[which(elog$date >= day1 & elog$date <= day2), ]
  elog2 <- elog[which(elog$date >= day3 & elog$date <= day4), ]
  time.between.periods <- as.numeric(day3 - day2)

  elog2timeErased <- elog2
  elog2timeErased$date <- elog2$date - time.between.periods
  elog3 = rbind(elog1, elog2timeErased)

  elogsToReturn = list()
  elogsToReturn$elog1 <- elog1
  elogsToReturn$elog2 <- elog2
  elogsToReturn$elog3 <- elog3
  return(elogsToReturn)
}

dc.GetFirstPurchasePeriodsFromCBT <- function(cbt) {
  cbt <- as.matrix(cbt)
  num.custs <- nrow(cbt)
  num.periods <- ncol(cbt)
  first.periods <- c(num.custs)

  ## loops through the customers and periods and locates the first purchase periods
  ## of each customer. Records them in first.periods
  for (ii in 1:num.custs) {
    curr.cust.transactions <- as.numeric(cbt[ii, ])
    transaction.index <- 1
    made.purchase <- FALSE
    while (made.purchase == FALSE & transaction.index <= num.periods) {
      if (curr.cust.transactions[transaction.index] > 0) {
        made.purchase <- TRUE
      } else {
        transaction.index <- transaction.index + 1
      }
    }
    if (made.purchase == FALSE) {
      first.periods[ii] <- 0
    } else {
      first.periods[ii] <- transaction.index
    }
  }
  return(first.periods)
}

dc.GetLastPurchasePeriodsFromCBT <- function(cbt) {
  cbt <- as.matrix(cbt)
  num.custs <- nrow(cbt)
  num.periods <- ncol(cbt)
  last.periods <- c(num.custs)

  ## loops through the customers and periods and locates the first purchase periods
  ## of each customer. Records them in last.periods
  for (ii in 1:num.custs) {
    curr.cust.transactions <- as.numeric(cbt[ii, ])
    transaction.index <- num.periods
    made.purchase <- FALSE
    while (made.purchase == FALSE & transaction.index >= 1) {
      if (curr.cust.transactions[transaction.index] > 0) {
        made.purchase <- TRUE
      } else {
        transaction.index <- transaction.index - 1
      }
    }
    if (made.purchase == FALSE) {
      last.periods[ii] <- 0
    } else {
      last.periods[ii] <- transaction.index
    }
  }
  return(last.periods)
}


dc.MakeRFmatrixCal <- function(frequencies, periods.of.final.purchases, num.of.purchase.periods,
                               holdout.frequencies = NULL) {

  if (!is.numeric(periods.of.final.purchases)) {
    stop("periods.of.final.purchases must be numeric")
  }
  if (length(periods.of.final.purchases) != length(frequencies)) {
    stop(paste("number of customers in frequencies is not equal", "to the last purchase period vector"))
  }
  ## initializes the data structures to later be filled in with counts
  rf.mx.skeleton <- dc.MakeRFmatrixSkeleton(num.of.purchase.periods)
  if (is.null(holdout.frequencies)) {
    RF.matrix <- cbind(rf.mx.skeleton, num.of.purchase.periods, 0)
    colnames(RF.matrix) <- c("x", "t.x", "n.cal", "custs")
  } else {
    RF.matrix <- cbind(rf.mx.skeleton, num.of.purchase.periods, 0, 0)
    colnames(RF.matrix) <- c("x", "t.x", "n.cal", "custs", "x.star")
  }


  ## create a matrix out of the frequencies & periods.of.final.purchases
  rf.n.custs <- cbind(frequencies, periods.of.final.purchases, holdout.frequencies)
  ## count all the pairs with zero for frequency and remove them
  zeroes.rf.subset <- which(rf.n.custs[, 1] == 0)  ##(which x == 0)
  RF.matrix[1, 4] <- length(zeroes.rf.subset)
  if (!is.null(holdout.frequencies)) {
    RF.matrix[1, 5] <- sum(holdout.frequencies[zeroes.rf.subset])
  }
  rf.n.custs <- rf.n.custs[-zeroes.rf.subset, ]

  ## sort the count data by both frequency and final purchase period
  rf.n.custs <- rf.n.custs[order(rf.n.custs[, 1], rf.n.custs[, 2]), ]

  ## formula: (x-1) + 1 + tx*(tx-1)/2 + 1 keep count of duplicates once different,
  ## use formula above to place count into the RF table.
  current.pair <- c(rf.n.custs[1, 1], rf.n.custs[1, 2])

  same.item.in.a.row.counter <- 1
  if (!is.null(holdout.frequencies)) {
    x.star.total <- rf.n.custs[1, 3]
  }
  num.count.points <- nrow(rf.n.custs)
  for (ii in 2:num.count.points) {
    last.pair <- current.pair
    current.pair <- c(rf.n.custs[ii, 1], rf.n.custs[ii, 2])
    if (identical(last.pair, current.pair)) {
      same.item.in.a.row.counter <- same.item.in.a.row.counter + 1
      if (!is.null(holdout.frequencies)) {
        x.star.total <- x.star.total + rf.n.custs[ii, 3]
      }
    } else {
      x <- last.pair[1]
      t.x <- last.pair[2]
      corresponding.rf.index <- (x - 1) + 1 + t.x * (t.x - 1)/2 + 1
      RF.matrix[corresponding.rf.index, 4] <- same.item.in.a.row.counter
      same.item.in.a.row.counter <- 1
      if (!is.null(holdout.frequencies)) {
        RF.matrix[corresponding.rf.index, 5] <- x.star.total
        x.star.total <- rf.n.custs[ii, 3]
      }
    }
    if (ii == num.count.points) {
      x <- current.pair[1]
      t.x <- current.pair[2]
      corresponding.rf.index <- (x - 1) + 1 + t.x * (t.x - 1)/2 + 1
      RF.matrix[corresponding.rf.index, 4] <- same.item.in.a.row.counter
      same.item.in.a.row.counter <- NULL
      if (!is.null(holdout.frequencies)) {
        RF.matrix[corresponding.rf.index, 5] <- x.star.total
        x.star.total = NULL
      }
    }
  }
  return(RF.matrix)
}


dc.WriteLine <- function(...) {
  message(...)
  flush.console()
}

addLogs <- function(loga, logb) {
  return(logb + log(exp(loga - logb) + 1))
}

subLogs <- function(loga, logb) {
  return(logb + log(exp(loga - logb) - 1))
}

dc.PlotLogLikelihoodContours <- function(loglikelihood.fcn, predicted.params, ...,
                                         n.divs = 2, multiple.screens = FALSE, num.contour.lines = 10, zoom.percent = 0.9,
                                         allow.neg.params = FALSE, param.names = c("param 1", "param 2", "param 3", "param 4")) {
  permutations <- combn(length(predicted.params), 2)
  num.permutations <- ncol(permutations)
  contour.plots <- list()

  if (multiple.screens == FALSE) {
    dev.new()
    plot.window.num.cols <- ceiling(num.permutations/2)
    plot.window.num.rows <- 2
    par(mfrow = c(plot.window.num.rows, plot.window.num.cols))
  }

  for (jj in 1:num.permutations) {
    vary.or.fix.param <- rep("fix", 4)
    vary.or.fix.param[permutations[, jj]] <- "vary"
    contour.plots[[jj]] <- dc.PlotLogLikelihoodContour(loglikelihood.fcn, vary.or.fix.param,
                                                       predicted.params, ..., n.divs = n.divs, new.dev = multiple.screens, num.contour.lines = num.contour.lines,
                                                       zoom.percent = zoom.percent, allow.neg.params = allow.neg.params, param.names = param.names)
  }

  if (multiple.screens == FALSE) {
    par(mfrow = c(1, 1))
  }

  return(contour.plots)
}

dc.PlotLogLikelihoodContour <- function(loglikelihood.fcn, vary.or.fix.param, predicted.params,
                                        ..., n.divs = 3, new.dev = FALSE, num.contour.lines = 10, zoom.percent = 0.9,
                                        allow.neg.params = FALSE, param.names = c("param 1", "param 2", "param 3", "param 4")) {
  if (new.dev) {
    dev.new()
  }
  idx.par.vary <- which(vary.or.fix.param == "vary")

  if (length(idx.par.vary) != 2) {
    stop("vary.or.fix.param must have exactly two elements: \"vary\" ")
  }

  values.par.vary <- predicted.params[idx.par.vary]
  v1 <- values.par.vary[1]
  v2 <- values.par.vary[2]
  par1.ticks <- c(v1 - (n.divs:1) * zoom.percent, v1, v1 + (1:n.divs) * zoom.percent)
  par2.ticks <- c(v2 - (n.divs:1) * zoom.percent, v2, v2 + (1:n.divs) * zoom.percent)

  param.names.vary <- param.names[idx.par.vary]

  if (!allow.neg.params) {
    par1.ticks <- par1.ticks[par1.ticks > 0]
    par2.ticks <- par2.ticks[par2.ticks > 0]
  }
  n.par1.ticks = length(par1.ticks)
  n.par2.ticks = length(par2.ticks)

  ll <- sapply(0:(n.par1.ticks * n.par2.ticks - 1), function(e) {
    i <- (e%%n.par1.ticks) + 1
    j <- (e%/%n.par1.ticks) + 1
    current.params <- predicted.params
    current.params[idx.par.vary] <- c(par1.ticks[i], par2.ticks[j])
    loglikelihood.fcn(current.params, ...)
  })

  loglikelihood.contours <- matrix(ll, nrow = n.par1.ticks, ncol = n.par2.ticks)

  if (FALSE) {
    for (ii in 1:n.par1.ticks) {
      for (jj in 1:n.par2.ticks) {
        current.params <- predicted.params
        current.params[idx.par.vary] <- c(par1.ticks[ii], par2.ticks[jj])
        loglikelihood.contours[ii, jj] <- loglikelihood.fcn(current.params,
                                                            ...)
        ## cat('finished', (ii-1)*2*n.divs+jj, 'of', 4*n.divs*n.divs, fill=TRUE)
      }
    }
  }
  contour.plot <- contour(x = par1.ticks, y = par2.ticks, z = loglikelihood.contours,
                          nlevels = num.contour.lines)
  # label.varying.params <- paste(idx.par.vary, collapse=', ')

  contour.plot.main.label <- paste("Log-likelihood contour of", param.names.vary[1],
                                   "and", param.names.vary[2])
  abline(v = values.par.vary[1], h = values.par.vary[2], col = "red")

  title(main = contour.plot.main.label, xlab = param.names.vary[1], ylab = param.names.vary[2])

}

dc.ReadLines <- function(csv.filename, cust.idx, date.idx, sales.idx = -1) {
  dc.WriteLine("Started reading file. Progress:")
  elog.file <- file(csv.filename, open = "r")
  elog.lines <- readLines(elog.file)
  n.lines <- length(elog.lines) - 1
  cust <- rep("", n.lines)
  date <- rep("", n.lines)
  if (sales.idx != -1) {
    sales <- rep(0, n.lines)
  }

  for (ii in 2:(n.lines + 1)) {
    ## splitting each line by commas
    split.string <- strsplit(elog.lines[ii], ",")
    ## assigning the comma delimited values to our vector
    this.cust <- split.string[[1]][cust.idx]
    this.date <- split.string[[1]][date.idx]
    if (is.na(this.cust) | is.na(this.date)) {
      next
    }
    cust[ii - 1] <- this.cust
    date[ii - 1] <- this.date
    if (sales.idx != -1) {
      sales[ii - 1] <- split.string[[1]][sales.idx]
    }
    ## Progress bar:
    if (ii%%1000 == 0) {
      dc.WriteLine(ii, "/", n.lines)
    }
  }

  elog <- cbind(cust, date)
  elog.colnames <- c("cust", "date")

  if (sales.idx != -1) {
    elog <- cbind(elog, sales)
    elog.colnames <- c(elog.colnames, "sales")
  }

  elog <- data.frame(elog, stringsAsFactors = FALSE)
  colnames(elog) <- elog.colnames

  if (sales.idx != -1) {
    elog$sales <- as.numeric(elog$sales)
  }
  close(elog.file)
  dc.WriteLine("File successfully read.")
  return(elog)
}

dc.check.model.params <- function(printnames, params, func) {
  if (length(params) != length(printnames)) {
    stop("Error in ", func, ": Incorrect number of parameters; there should be ",
         length(printnames), ".", call. = FALSE)
  }
  if (!is.numeric(params)) {
    stop("Error in ", func, ": parameters must be numeric, but are of class ",
         class(params), call. = FALSE)
  }
  if (any(params < 0)) {
    stop("Error in ", func, ": All parameters must be positive. Negative parameters: ",
         paste(printnames[params < 0], collapse = ", "), call. = FALSE)
  }
}

dc.CumulativeToIncremental <- function(cu) {
  inc <- cu - c(0, cu)[-(length(cu) + 1)]
  return(inc)
}
