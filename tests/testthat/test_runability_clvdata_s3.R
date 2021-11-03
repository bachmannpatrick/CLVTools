skip_on_cran()

# Load required data -----------------------------------------------------------------------------------
data("apparelTrans")
data("apparelStaticCov")
data("apparelDynCov")
apparelDynCov[, Cov.Date := as.Date(Cov.Date)] # otherwise warnings when setting dyncov
apparelDynCov <- apparelDynCov[Cov.Date > "2005-01-01" ] # otherwise warnings when setting dyncov

fct.helper.test.runability.clv.data.summary <- function(clv.data){
  test_that("summary works",{
    expect_silent(summary(clv.data))
  })

  test_that("summary prints",{
    skip_on_cran()
    expect_output(print(summary(clv.data)))
  })
}


fct.helper.test.runability.clv.data.trackingplot <- function(clv.data){
  test_that("plot, no options", {
    skip_on_cran()
    expect_message(plot(clv.data), regexp = "Plotting")
  })

  test_that("plot, explicit", {
    skip_on_cran()
    expect_silent(plot(clv.data, which="tracking", verbose=FALSE))
  })

  test_that("plot, cumulative = FALSE", {
    skip_on_cran()
    expect_message(plot(clv.data, cumulative=FALSE), regexp = "Plotting")
  })

  test_that("plot, cumulative = TRUE", {
    skip_on_cran()
    expect_message(plot(clv.data, cumulative=TRUE), regexp = "Plotting")
  })

  test_that("plot, plot = FALSE, repeat trans = 0", {
    skip_on_cran()
    expect_message(dt.plot <- plot(clv.data, plot=FALSE), regexp = "Plotting")
    expect_s3_class(dt.plot, "data.table")
    expect_true(isTRUE(all.equal(unlist(dt.plot[period.until == min(period.until), 2]),
                                 0, check.attributes = FALSE)))
  })

  test_that("plot, verbose = TRUE", {
    skip_on_cran()
    expect_message(plot(clv.data, verbose=TRUE), regexp = "Plotting")
  })

  test_that("plot, verbose = FALSE", {
    skip_on_cran()
    expect_silent(plot(clv.data, verbose=FALSE))
  })
}

fct.helper.test.runability.clv.data.plotfrequency <- function(clv.data){
  test_that("plot - frequency, trans.bins", {
    skip_on_cran()
    # real and integer vec, and single
    expect_silent(plot(clv.data, which="frequency", trans.bins=3, verbose=FALSE))
    expect_silent(plot(clv.data, which="frequency", trans.bins=c(1,2,3), verbose=FALSE))
    expect_silent(plot(clv.data, which="frequency", trans.bins=3:250, verbose=FALSE))
    # works from 0 and 1
    expect_silent(plot(clv.data, which="frequency", trans.bins=0:10, count.repeat.trans=TRUE, verbose=FALSE))
    expect_silent(plot(clv.data, which="frequency", trans.bins=1:10, count.repeat.trans=FALSE, verbose=FALSE))
  })

  test_that("plot - frequency, count.repeat.trans", {
    skip_on_cran()
    expect_silent(plot(clv.data, which="frequency", count.repeat.trans=TRUE, verbose=FALSE))
    # bins starting from 1
    expect_silent(plot(clv.data, which="frequency", count.repeat.trans=FALSE, trans.bins=1:10, verbose=FALSE))
  })

  test_that("plot - frequency, count.remaining", {
    skip_on_cran()
    expect_silent(plot(clv.data, which="frequency", count.remaining=TRUE, verbose=FALSE))
    # bins starting from 1
    expect_silent(plot(clv.data, which="frequency", count.remaining=FALSE, verbose=FALSE))
  })

  test_that("plot - frequency, count.remaining", {
    skip_on_cran()
    expect_silent(plot(clv.data, which="frequency", count.remaining=TRUE, label.remaining="abc", verbose=FALSE))
  })

  test_that("plot - frequency, sample", {
    skip_on_cran()
    expect_silent(plot(clv.data, which="frequency", sample="estimation", verbose=FALSE))
    expect_silent(plot(clv.data, which="frequency", sample="full", verbose=FALSE))
    if(clv.data.has.holdout(clv.data)){
      expect_silent(plot(clv.data, which="frequency", sample="holdout", verbose=FALSE))
    }
  })

  test_that("plot - frequency, extra arguments do not cause error", {
    skip_on_cran()
    expect_silent(plot(clv.data, which="frequency", color="blue", verbose=FALSE))
  })

  test_that("plot - frequency, plot=FALSE", {
    skip_on_cran()
    expect_silent(dt.plot <- plot(clv.data, which="frequency", plot=FALSE, verbose=FALSE))
    expect_s3_class(dt.plot, "data.table")
    expect_setequal(colnames(dt.plot), c("num.transactions", "num.customers"))

    # expect_type does not get factor
    expect_true(is.factor(dt.plot$num.transactions))
    expect_true(is.integer(dt.plot$num.customers))
    expect_false(anyNA(dt.plot))
    expect_true(length(unique(levels(dt.plot$num.transactions))) ==
                  length(levels(dt.plot$num.transactions)))

  })
}

fct.helper.test.runability.clv.data.plotspending <- function(clv.data){
  test_that("plot - spending, mean.spending=T", {
    skip_on_cran()
    expect_silent(plot(clv.data, which="spending", mean.spending=TRUE))
  })

  test_that("plot - spending, mean.spending=F", {
    skip_on_cran()
    expect_silent(plot(clv.data, which="spending", mean.spending=FALSE))
  })

  test_that("plot - spending, sample", {
    skip_on_cran()
    expect_silent(plot(clv.data, which="spending", sample="estimation", verbose=FALSE))
    expect_silent(plot(clv.data, which="spending", sample="full", verbose=FALSE))
    if(clv.data.has.holdout(clv.data)){
      expect_silent(plot(clv.data, which="spending", sample="holdout", verbose=FALSE))
    }
  })

  test_that("plot - spending, extra arguments", {
    skip_on_cran()
    # others and ... args
    expect_silent(plot(clv.data, which="spending", color="yellow", geom="point", size=0.1, verbose=FALSE))
  })

  test_that("plot - spending, plot=FALSE", {
    skip_on_cran()
    expect_silent(dt.plot <- plot(clv.data, which="spending", mean.spending=TRUE, plot=FALSE))
    expect_s3_class(dt.plot, "data.table")
    expect_setequal(colnames(dt.plot), c("Id", "Spending"))
    # always returns Id
    expect_silent(dt.plot <- plot(clv.data, which="spending", mean.spending=FALSE, plot=FALSE))
    expect_setequal(colnames(dt.plot), c("Id", "Spending"))
  })
}



fct.helper.test.runability.clv.data.plotinterpurchasetime <- function(clv.data){
  test_that("plot - interpurchasetime, extra arguments", {
    skip_on_cran()
    expect_silent(plot(clv.data, which="interpurchasetime", size=0.02, color="green", geom="point", verbose=FALSE))
  })

  test_that("plot - interpurchasetime, sample", {
    skip_on_cran()
    expect_silent(plot(clv.data, which="interpurchasetime", sample="estimation", verbose=FALSE))
    expect_silent(plot(clv.data, which="interpurchasetime", sample="full", verbose=FALSE))
    if(clv.data.has.holdout(clv.data)){
      expect_silent(plot(clv.data, which="interpurchasetime", sample="holdout", verbose=FALSE))
    }
  })

  test_that("plot - interpurchasetime, plot=FALSE", {
    skip_on_cran()
    # others and ... args
    expect_silent(dt.plot <- plot(clv.data, which="interpurchasetime", plot=FALSE, verbose=FALSE))
    expect_s3_class(dt.plot, "data.table")
    expect_setequal(colnames(dt.plot), c("Id", "mean.interpurchase.time"))
  })
}

fct.helper.test.runability.clv.data.others3 <- function(clv.data){
  test_that("nobs works", {
    expect_silent(nobs(clv.data))
    expect_true(is.integer(nobs(clv.data)))
  })

  test_that("print works", {
    expect_output(print(clv.data))
  })

  test_that("show works", {
    expect_output(show(clv.data))
  })

  test_that("as.data.frame works", {
    expect_true(is.data.frame(as.data.frame(clv.data)))
    expect_true(is.data.frame(as.data.frame(clv.data, sample="estimation")))
    if(clv.data.has.holdout(clv.data)){
      expect_true(is.data.frame(as.data.frame(clv.data, sample="holdout")))
      }
  })

  test_that("as.data.table works", {
    expect_true(is.data.table(as.data.table(clv.data)))
    expect_true(is.data.table(as.data.table(clv.data, sample="estimation")))
    if(clv.data.has.holdout(clv.data)){
      expect_true(is.data.table(as.data.table(clv.data, sample="holdout")))
    }
  })

  test_that("subset works", {
    expect_true(is.data.table(subset(clv.data, sample="estimation")))
    expect_true(is.data.table(subset(clv.data, sample="full")))
    # random Date, returns empty if not found
    expect_true(is.data.table(subset(clv.data, Date>="1900-01-01", sample="estimation")))
    if(clv.data.has.holdout(clv.data)){
      expect_true(is.data.table(subset(clv.data, Date>="1900-01-01", sample="holdout")))
    }
  })
}

# This all falls under the context of runability for the fitted models
context("Runability - clvdata - S3")

# Create with and withouth holdout, with and withouth static covariates
expect_silent(apparel.holdout    <- clvdata(apparelTrans, date.format = "ymd", time.unit = "w", estimation.split = 39))
expect_silent(apparel.no.holdout <- clvdata(apparelTrans, date.format = "ymd", time.unit = "w"))

expect_silent(apparel.holdout.static.cov     <- SetStaticCovariates(clv.data = apparel.holdout,
                                                                    data.cov.life = apparelStaticCov,  names.cov.life = "Gender",
                                                                    data.cov.trans = apparelStaticCov, names.cov.trans = "Gender"))
expect_silent(apparel.no.holdout.static.cov  <- SetStaticCovariates(clv.data = apparel.no.holdout,
                                                             data.cov.life = apparelStaticCov,  names.cov.life = "Gender",
                                                             data.cov.trans = apparelStaticCov, names.cov.trans = "Gender"))


expect_silent(apparel.holdout.dyn.cov     <- SetDynamicCovariates(clv.data = apparel.holdout,
                                                                  data.cov.life = apparelDynCov,
                                                                  data.cov.trans = apparelDynCov,
                                                                  names.cov.life = c("Channel", "Marketing", "Gender"),
                                                                  names.cov.trans = c("Channel", "Marketing", "Gender"),
                                                                  name.date = "Cov.Date"))

expect_silent(apparel.no.holdout.dyn.cov     <- SetDynamicCovariates(clv.data = apparel.no.holdout,
                                                                      data.cov.life = apparelDynCov,
                                                                      data.cov.trans = apparelDynCov,
                                                                      names.cov.life = c("Channel", "Marketing", "Gender"),
                                                                      names.cov.trans = c("Channel", "Marketing", "Gender"),
                                                                      name.date = "Cov.Date"))


fct.helper.test.runability.clv.data.runall <- function(clv.data){
  fct.helper.test.runability.clv.data.trackingplot(clv.data)
  fct.helper.test.runability.clv.data.plotfrequency(clv.data)
  fct.helper.test.runability.clv.data.plotspending(clv.data)
  fct.helper.test.runability.clv.data.summary(clv.data)
  fct.helper.test.runability.clv.data.others3(clv.data)
  fct.helper.test.runability.clv.data.plotinterpurchasetime(clv.data)
}


fct.helper.test.runability.clv.data.runall(apparel.holdout)
fct.helper.test.runability.clv.data.runall(apparel.no.holdout)
fct.helper.test.runability.clv.data.runall(apparel.holdout.static.cov)
fct.helper.test.runability.clv.data.runall(apparel.no.holdout.static.cov)
fct.helper.test.runability.clv.data.runall(apparel.holdout.dyn.cov)
fct.helper.test.runability.clv.data.runall(apparel.no.holdout.dyn.cov)

