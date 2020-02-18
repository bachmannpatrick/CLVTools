# Load required data -----------------------------------------------------------------------------------
data("apparelTrans")
data("apparelDemographics")
data("apparelDynCov")
apparelDynCov[, Cov.Date := as.Date(Cov.Date)] # otherwise warnings when setting dyncov

fct.helper.test.runability.clv.data.summary <- function(clv.data){
  test_that("summary works",{
    expect_silent(summary(clv.data))
  })

  test_that("summary prints",{
    expect_output(print(summary(clv.data)))
  })
}


fct.helper.test.runability.clv.data.plot <- function(clv.data){
  test_that("plot, no options", {
    expect_message(plot(clv.data), regexp = "Plotting")
  })

  test_that("plot, cumulative = FALSE", {
    expect_message(plot(clv.data, cumulative=FALSE), regexp = "Plotting")
  })

  test_that("plot, cumulative = TRUE", {
    expect_message(plot(clv.data, cumulative=TRUE), regexp = "Plotting")
  })

  test_that("plot, plot = FALSE", {
    expect_message(dt.plot <- plot(clv.data, plot=FALSE), regexp = "Plotting")
    expect_s3_class(dt.plot, "data.table")
  })

  test_that("plot, verbose = TRUE", {
    expect_message(plot(clv.data, verbose=TRUE), regexp = "Plotting")
  })

  test_that("plot, verbose = FALSE", {
    expect_silent(plot(clv.data, verbose=FALSE))
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
}

# This all falls under the context of runability for the fitted models
context("Runability - clvdata - S3")

# Create with and withouth holdout, with and withouth static covariates
expect_message(apparel.holdout    <- clvdata(apparelTrans, date.format = "ymd", time.unit = "w", estimation.split = 39), regexp = "ignored")
expect_message(apparel.no.holdout <- clvdata(apparelTrans, date.format = "ymd", time.unit = "w"), regexp = "ignored")

expect_silent(apparel.holdout.static.cov     <- SetStaticCovariates(clv.data = apparel.holdout,
                                                                    data.cov.life = apparelDemographics,  names.cov.life = "Gender",
                                                                    data.cov.trans = apparelDemographics, names.cov.trans = "Gender"))
expect_silent(apparel.no.holdout.static.cov  <- SetStaticCovariates(clv.data = apparel.no.holdout,
                                                             data.cov.life = apparelDemographics,  names.cov.life = "Gender",
                                                             data.cov.trans = apparelDemographics, names.cov.trans = "Gender"))


expect_silent(apparel.holdout.dyn.cov     <- SetDynamicCovariates(clv.data = apparel.holdout,
                                                                  data.cov.life = apparelDynCov,
                                                                  data.cov.trans = apparelDynCov,
                                                                  names.cov.life = c("Gender", "DM", "High.Season"),
                                                                  names.cov.trans = c("Gender", "DM", "High.Season"),
                                                                  name.date = "Cov.Date"))

expect_silent(apparel.no.holdout.dyn.cov     <- SetDynamicCovariates(clv.data = apparel.no.holdout,
                                                                      data.cov.life = apparelDynCov,
                                                                      data.cov.trans = apparelDynCov,
                                                                      names.cov.life = c("Gender", "DM", "High.Season"),
                                                                      names.cov.trans = c("Gender", "DM", "High.Season"),
                                                                      name.date = "Cov.Date"))



fct.helper.test.runability.clv.data.plot(apparel.holdout)
fct.helper.test.runability.clv.data.plot(apparel.no.holdout)
fct.helper.test.runability.clv.data.plot(apparel.holdout.static.cov)
fct.helper.test.runability.clv.data.plot(apparel.no.holdout.static.cov)
fct.helper.test.runability.clv.data.plot(apparel.holdout.dyn.cov)
fct.helper.test.runability.clv.data.plot(apparel.no.holdout.dyn.cov)


fct.helper.test.runability.clv.data.summary(apparel.holdout)
fct.helper.test.runability.clv.data.summary(apparel.no.holdout)
fct.helper.test.runability.clv.data.summary(apparel.holdout.static.cov)
fct.helper.test.runability.clv.data.summary(apparel.no.holdout.static.cov)
fct.helper.test.runability.clv.data.summary(apparel.holdout.dyn.cov)
fct.helper.test.runability.clv.data.summary(apparel.no.holdout.dyn.cov)

fct.helper.test.runability.clv.data.others3(apparel.holdout)
fct.helper.test.runability.clv.data.others3(apparel.no.holdout)
fct.helper.test.runability.clv.data.others3(apparel.holdout.static.cov)
fct.helper.test.runability.clv.data.others3(apparel.no.holdout.static.cov)
fct.helper.test.runability.clv.data.others3(apparel.holdout.dyn.cov)
fct.helper.test.runability.clv.data.others3(apparel.no.holdout.dyn.cov)




