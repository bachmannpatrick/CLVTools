.fct.helper.s3.fitted.coef <- function(clv.fitted, full.names){

  expect_silent(res.coef <- coef(clv.fitted))

  test_that("Named numeric vector", {
    expect_type(res.coef, "double")
    expect_named(res.coef, full.names, ignore.order = TRUE)
  })

  test_that("Same length as during optimization", {
    expect_length(res.coef, ncol(coef(clv.fitted@optimx.estimation.output)))
  })

  test_that("Names have same order as vcov()", {
    skip_on_cran()
    expect_silent(res.vcov <- vcov(clv.fitted))
    expect_named(res.coef, rownames(vcov(clv.fitted)), ignore.case = FALSE, ignore.order = FALSE)
    expect_named(res.coef, colnames(vcov(clv.fitted)), ignore.case = FALSE, ignore.order = FALSE)
  })

  test_that("Same order as coef(summary())", {
    skip_on_cran()
    expect_named(res.coef, rownames(coef(summary(clv.fitted))), ignore.case = FALSE, ignore.order = FALSE)
  })

  test_that("No NAs", {
    expect_false(anyNA(res.coef))
  })

  # **Todo: model specific: coef() same as exp(coef(optimx))
}

.fct.helper.s3.fitted.vcov <- function(clv.fitted, full.names){
  expect_silent(res.vcov <- vcov(clv.fitted))

  test_that("Named numeric matrix", {
    res.attr <- attributes(res.vcov)
    expect_named(res.attr, c("dim", "dimnames"))
    expect_length(res.attr$dimnames, 2)
    expect_equal(res.attr$dim, c(length(full.names), length(full.names)))
    expect_equal(res.attr$dimnames[[1]], names(coef(clv.fitted)))
    expect_equal(res.attr$dimnames[[2]], names(coef(clv.fitted)))
  })
  test_that("Names same order as coef()", {
    skip_on_cran()
    expect_named(coef(clv.fitted), rownames(res.vcov), ignore.case = FALSE, ignore.order = FALSE)
    expect_named(coef(clv.fitted), colnames(res.vcov), ignore.case = FALSE, ignore.order = FALSE)
  })
  test_that("Names same order as coef(summary())", {
    skip_on_cran()
    expect_equal(rownames(coef(summary(clv.fitted))), rownames(res.vcov), ignore.case = FALSE, ignore.order = FALSE)
    expect_equal(rownames(coef(summary(clv.fitted))), colnames(res.vcov), ignore.case = FALSE, ignore.order = FALSE)
  })
  test_that("No NAs",{
    expect_false(anyNA(res.vcov))
  })
}


.fct.helper.s3.fitted.confint <- function(clv.fitted, full.names){
  test_that("Confint works with different alphas", {
    expect_silent(ci.99 <- confint(clv.fitted, level = 0.99))
    expect_silent(ci.95 <- confint(clv.fitted, level = 0.95))
    expect_silent(ci.90 <- confint(clv.fitted, level = 0.90))
    expect_silent(ci.70 <- confint(clv.fitted, level = 0.70))

    # Level works and provides different values
    expect_false(isTRUE(all.equal(ci.99,ci.95,check.attributes=FALSE)))
    expect_false(isTRUE(all.equal(ci.95,ci.90,check.attributes=FALSE)))
    expect_false(isTRUE(all.equal(ci.90,ci.70,check.attributes=FALSE)))

    # Rightly named, all same
    expect_setequal(rownames(ci.99), full.names)
    expect_setequal(rownames(ci.99), rownames(ci.95))
    expect_setequal(rownames(ci.95), rownames(ci.90))
    expect_setequal(rownames(ci.90), rownames(ci.70))

    # Also title label correct
    expect_equal(colnames(ci.95), c("2.5 %", "97.5 %"))
    expect_equal(colnames(ci.90), c("5 %", "95 %"))
    expect_equal(colnames(ci.99), c("0.5 %", "99.5 %"))
  })

  test_that("Confint works with character param", {
    skip_on_cran()
    # Single
    for(p in full.names)
      expect_equal(rownames(confint(clv.fitted, parm = p)), expected = p)
    # Multiple
    p <- full.names[1:3]
    expect_setequal(rownames(confint(clv.fitted, parm = p)), expected = p)
    p <- full.names[1:2]
    expect_setequal(rownames(confint(clv.fitted, parm = p)), expected = p)
    # All - excplicitely
    p <- full.names
    expect_setequal(rownames(confint(clv.fitted, parm = p)), expected = p)
    # All - implicitely (ie none given)
    expect_setequal(rownames(confint(clv.fitted)), expected = p)
  })

  test_that("Confint works with integer param", {
    skip_on_cran()
    p <- full.names
    # Single
    expect_equal(rownames(confint(clv.fitted, parm = 2)), expected = p[2])
    expect_equal(rownames(confint(clv.fitted, parm = 4)), expected = p[4])
    # Sequence
    expect_setequal(rownames(confint(clv.fitted, parm = 1:3)), expected = p[1:3])
    expect_setequal(rownames(confint(clv.fitted, parm =c(1,2,4))), expected = p[c(1,2,4)])
    # All - excplicitely
    expect_setequal(rownames(confint(clv.fitted, parm = seq(length(p)))), expected = p)
    # All - implicitely (ie none given)
    expect_setequal(rownames(confint(clv.fitted)), expected = p)

    # Minus removes
    expect_setequal(rownames(confint(clv.fitted, parm = -2)), expected = p[-2])
    expect_setequal(rownames(confint(clv.fitted, parm = -c(2,4))), expected = p[-c(2,4)])
    # Remove all
    expect_null(rownames(confint(clv.fitted, parm = -seq(length(p)))))
  })
  # same behavior as lm
  test_that("confint NA if unknown parm", {
    skip_on_cran()
    # Unknown character
    expect_true(all(is.na( confint(clv.fitted, parm = "abc") )))
    expect_true(all(is.na( confint(clv.fitted, parm = c("abc", "zcgd")) )))
    # Wrong indices
    expect_true(all(is.na( confint(clv.fitted, parm = 50:100))))
    expect_true(all(is.na( confint(clv.fitted, parm = 99) )))
    # Part of it are known
    expect_true(all(full.names %in%
                      rownames(confint(clv.fitted, parm = 1:100))))
    expect_true(!all(is.na( confint(clv.fitted, parm = 1:100))))
  })
}


.fct.helper.s3.fitted.summary <- function(clv.fitted){

  expect_silent(res.sum <- summary(clv.fitted))

  test_that("Basic summary structure", {
    expect_is(res.sum, "summary.clv.fitted")
    expect_true(is.list(res.sum))
    expect_true(all(c("call", "name.model", "tp.estimation.start","tp.estimation.end",
                      "time.unit", "coefficients", "AIC", "BIC","kkt1", "kkt2","additional.options") %in%
                      names(res.sum)))
    expect_is(res.sum$call, "call")
    expect_is(res.sum$name.model, "character")
    expect_true(lubridate::is.Date(res.sum$tp.estimation.start) | lubridate::is.POSIXct(res.sum$tp.estimation.start))
    expect_true(lubridate::is.Date(res.sum$tp.estimation.end) | lubridate::is.POSIXct(res.sum$tp.estimation.end))
    expect_is(res.sum$time.unit, "character")
    expect_is(res.sum$coefficients, "matrix")
    expect_is(res.sum$AIC, "numeric")
    expect_is(res.sum$BIC, "numeric")
    expect_is(res.sum$kkt1, "logical")
    expect_is(res.sum$kkt2, "logical")
    expect_is(res.sum$additional.options, "list")
  })

  test_that("Correct coef structure", {
    # Basic correct
    sum.coef <- coef(res.sum)
    expect_true(ncol(sum.coef) == 4)
    expect_true(all(colnames(sum.coef) != ""))

    # Same order as vcov()
    expect_equal(rownames(sum.coef), rownames(vcov(clv.fitted)))
    expect_equal(rownames(sum.coef), colnames(vcov(clv.fitted)))

    # Same order as coef()
    expect_equal(rownames(sum.coef), names(coef(clv.fitted)))
  })

  test_that("summary() prints", {
    expect_output(res.show <- show(res.sum))
    expect_null(res.show)

    expect_output(res.print <- print(res.sum))
    expect_equal(res.print, res.sum)
  })
}

.fct.helper.s3.fitted.print <- function(clv.fitted){
  test_that("Prints in different ways", {
    # Just that they work and return their input
    expect_output(res <- show(clv.fitted))
    # expect_null(res)

    expect_output(res <- print(clv.fitted))
    expect_identical(res, clv.fitted)
  })
}

.fct.helper.s3.fitted.nobs <- function(clv.fitted){
  test_that("has correct format",{
    expect_silent(res.nobs <- nobs(clv.fitted))
    expect_is(res.nobs, "integer")
    expect_equal(res.nobs, nrow(clv.fitted@cbs))
  })
}

.fct.helper.s3.fitted.logLik <- function(clv.fitted){
  test_that("has correct format", {
    expect_silent(res.loglik <- logLik(clv.fitted))
    expect_s3_class(res.loglik, "logLik")
    res.attr <- attributes(res.loglik)
    expect_named(res.attr, expected = c("nall", "nobs", "df", "class"))
    expect_equal(res.attr$df,   length(coef(clv.fitted)))
    expect_equal(res.attr$df,   ncol(coef(clv.fitted@optimx.estimation.output)))
    # **TOOD: Ask Jeff
    # expect_equal(res.attr$nall, nrow())?
    # expect_equal(res.attr$nobs, nobs(clv.fitted))
  })
}

.fct.helper.clvfitted.all.s3 <- function(clv.fitted, full.names){
  .fct.helper.s3.fitted.coef(clv.fitted = clv.fitted, full.names = full.names)

  .fct.helper.s3.fitted.vcov(clv.fitted = clv.fitted, full.names = full.names)

  .fct.helper.s3.fitted.confint(clv.fitted = clv.fitted, full.names = full.names)

  .fct.helper.s3.fitted.summary(clv.fitted = clv.fitted)

  .fct.helper.s3.fitted.print(clv.fitted = clv.fitted)

  .fct.helper.s3.fitted.nobs(clv.fitted = clv.fitted)

  .fct.helper.s3.fitted.logLik(clv.fitted = clv.fitted)
}


fct.helper.clvfittedtransactions.all.s3 <- function(clv.fitted, full.names,
                                                    clv.newdata.nohold, clv.newdata.withhold,
                                                    DERT.not.implemented){

  .fct.helper.clvfitted.all.s3(clv.fitted = clv.fitted, full.names = full.names)

  fct.testthat.runability.clvfittedtransactions.plot(clv.fitted = clv.fitted, clv.newdata.nohold=clv.newdata.nohold,
                                                     clv.newdata.withhold=clv.newdata.withhold)

  fct.testthat.runability.clvfittedtransactions.predict(fitted.transactions = clv.fitted, clv.newdata.nohold=clv.newdata.nohold,
                                                        clv.newdata.withhold=clv.newdata.withhold, DERT.not.implemented=DERT.not.implemented)

}


fct.helper.clvfittedspending.all.s3 <- function(clv.fitted, full.names,
                                                clv.newdata.nohold, clv.newdata.withhold){

  .fct.helper.clvfitted.all.s3(clv.fitted = clv.fitted, full.names = full.names)

  fct.testthat.runability.clvfittedspending.plot(fitted.spending = clv.fitted)

  fct.testthat.runability.clvfittedspending.predict(fitted.spending = clv.fitted,
                                                    clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold)
}

# plot with different ways of naming
# plot with predict.end=NULL same as predict.end=holdout.end and predict.end=holdout.period.in.tu
# correct that label = model name same as no label
