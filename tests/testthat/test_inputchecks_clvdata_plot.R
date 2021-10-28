data("cdnow")
skip_on_cran()
context("Inputchecks - clvdata - plot")

clv.cdnow.nohold <- fct.helper.create.clvdata.cdnow(cdnow, estimation.split = NULL)

# shared args
.fct.helper.inputchecks.single.logical(fct = plot, l.std.args = list(clv.cdnow.nohold), name.param = "plot")
.fct.helper.inputchecks.single.logical(fct = plot, l.std.args = list(clv.cdnow.nohold), name.param = "verbose")
# tracking specific args
.fct.helper.inputchecks.single.logical(fct = plot, l.std.args = list(clv.cdnow.nohold, which="tracking"),
                                       name.param = "cumulative")
# spending specifc args
.fct.helper.inputchecks.single.logical(fct = plot, l.std.args = list(clv.cdnow.nohold, which="spending"),
                                       name.param = "mean.spending")

test_that("which - only allowed args", {
  skip_on_cran()
  expect_error(plot(clv.cdnow.nohold, which=NULL), regexp = "cannot be NULL")
  expect_error(plot(clv.cdnow.nohold, which=NA_character_), regexp = "may not contain")
  expect_error(plot(clv.cdnow.nohold, which="trking"), regexp = "choose one of the following")
  expect_error(plot(clv.cdnow.nohold, which="spnding"), regexp = "choose one of the following")
  expect_error(plot(clv.cdnow.nohold, which=""), regexp = "choose one of the following")
})

test_that("tracking - Stops for unneeded parameters", {
  # only unneeded for tracking plot!
  skip_on_cran()
  expect_error(plot(clv.cdnow.nohold, which="tracking", abc=123), regexp = "not needed")
  expect_error(plot(clv.cdnow.nohold, which="tracking", transactions=TRUE), regexp = "not needed")
})

test_that("spending - specific args",{
  skip_on_cran()
  # sample
  expect_error(plot(clv.cdnow.nohold, which="spending", sample=NA_character_), regexp = "may not contain")
  expect_error(plot(clv.cdnow.nohold, which="spending", sample=NULL), regexp = "cannot be")
  expect_error(plot(clv.cdnow.nohold, which="spending", sample="estmation"), regexp = "choose one of the following")
  expect_error(plot(clv.cdnow.nohold, which="spending", sample=""), regexp = "choose one of the following")
})

test_that("spending - cannot select holdout data if none present", {
  skip_on_cran()
  expect_error(plot(clv.cdnow.nohold, which="spending", sample="holdout"), regexp = "has no holdout data")
})

test_that("interpurchasetime - cannot select holdout data if none present", {
  skip_on_cran()
  expect_error(plot(clv.cdnow.nohold, which="interpurchasetime", sample="holdout"), regexp = "has no holdout data")
})

