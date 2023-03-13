data("cdnow")
skip_on_cran()
context("Inputchecks - clvdata - plot")

clv.cdnow.nohold <- fct.helper.create.clvdata.cdnow(cdnow, estimation.split = NULL)

# . shared args ------------------------------------------------------------------------------------
# shared args
.fct.helper.inputchecks.single.logical(fct = plot, l.std.args = list(clv.cdnow.nohold), name.param = "plot")
.fct.helper.inputchecks.single.logical(fct = plot, l.std.args = list(clv.cdnow.nohold), name.param = "verbose")

# . which ------------------------------------------------------------------------------------
test_that("which - only allowed args", {
  skip_on_cran()
  expect_error(plot(clv.cdnow.nohold, which=NULL), regexp = "cannot be NULL")
  expect_error(plot(clv.cdnow.nohold, which=NA_character_), regexp = "may not contain")
  expect_error(plot(clv.cdnow.nohold, which="trking"), regexp = "choose one of the following")
  expect_error(plot(clv.cdnow.nohold, which="spnding"), regexp = "choose one of the following")
  expect_error(plot(clv.cdnow.nohold, which=""), regexp = "choose one of the following")
})


# . tracking specific args --------------------------------------------------------------------------------

.fct.helper.inputchecks.single.logical(fct = plot, l.std.args = list(clv.cdnow.nohold, which="tracking"),
                                       name.param = "cumulative")

test_that("tracking - Stops for unneeded parameters", {
  # only unneeded for tracking plot!
  skip_on_cran()
  expect_error(plot(clv.cdnow.nohold, which="tracking", abc=123), regexp = "not needed")
  expect_error(plot(clv.cdnow.nohold, which="tracking", transactions=TRUE), regexp = "not needed")
})


# . frequency specific args -------------------------------------------------------------------------------
.fct.helper.inputchecks.single.logical(fct = plot, l.std.args = list(clv.cdnow.nohold, which="frequency"),
                                       name.param = "count.repeat.trans")
.fct.helper.inputchecks.single.logical(fct = plot, l.std.args = list(clv.cdnow.nohold, which="frequency"),
                                       name.param = "count.remaining")

test_that("frequency - label.remaining is single character", {
  expect_error(plot(clv.cdnow.nohold, which="frequency", label.remaining=NA_character_), regexp = "may not contain")
  expect_error(plot(clv.cdnow.nohold, which="frequency", label.remaining=NULL), regexp = "of type character")
  expect_error(plot(clv.cdnow.nohold, which="frequency", label.remaining=character(0)), regexp = "single element")
  expect_error(plot(clv.cdnow.nohold, which="frequency", label.remaining=c("10+", ">10")), regexp = "single element")
})

test_that("frequency - trans.bins is valid", {
  # no foolish input
  expect_error(plot(clv.cdnow.nohold, which="frequency", trans.bins=NULL), regexp = "cannot")
  expect_error(plot(clv.cdnow.nohold, which="frequency", trans.bins=c(1,2, NA_integer_)), regexp = "contain any NA")
  expect_error(plot(clv.cdnow.nohold, which="frequency", trans.bins=numeric(0)), regexp = "has to contain")
  expect_error(plot(clv.cdnow.nohold, which="frequency", trans.bins=integer(0)), regexp = "has to contain")
  expect_error(plot(clv.cdnow.nohold, which="frequency", trans.bins="123"), regexp = "vector of integer numbers")

  # cannot be negative, ever
  expect_error(plot(clv.cdnow.nohold, which="frequency", trans.bins=c(-1,1,2), count.repeat.trans=TRUE), regexp = "positive")
  expect_error(plot(clv.cdnow.nohold, which="frequency", trans.bins=c(-1,1,2), count.repeat.trans=FALSE), regexp = "positive")
  # cannot be zero if not doing repeat
  expect_error(plot(clv.cdnow.nohold, which="frequency", trans.bins=c(0,1,2), count.repeat.trans=FALSE), regexp = "strictly positive")
  # has to be integers, always
  expect_error(plot(clv.cdnow.nohold, which="frequency", trans.bins=c(1.1,2,3), count.repeat.trans=FALSE), regexp = "integer")
  expect_error(plot(clv.cdnow.nohold, which="frequency", trans.bins=c(1,2,3.3), count.repeat.trans=TRUE), regexp = "integer")
})


# . spending specific args --------------------------------------------------------------------------------
.fct.helper.inputchecks.single.logical(fct = plot, l.std.args = list(clv.cdnow.nohold, which="spending"),
                                       name.param = "mean.spending")

test_that("spending - sample",{
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


# . interpurchasetime specific args -------------------------------------------------------------------------
test_that("interpurchasetime - cannot select holdout data if none present", {
  skip_on_cran()
  expect_error(plot(clv.cdnow.nohold, which="interpurchasetime", sample="holdout"), regexp = "has no holdout data")
})


# . timings specific args -------------------------------------------------------------------------
test_that("timings - annotate.ids is valid", {
  skip_on_cran()
  expect_error(plot(clv.cdnow.nohold, which="timings", annotate.ids=NA), regexp = "cannot be")
  expect_error(plot(clv.cdnow.nohold, which="timings", annotate.ids=NULL), regexp = "of type logical")
  expect_error(plot(clv.cdnow.nohold, which="timings", annotate.ids=""), regexp = "of type logical")
  expect_error(plot(clv.cdnow.nohold, which="timings", annotate.ids=c("1", "2")), regexp = "of type logical")
})

test_that("timings - Ids is valid", {
  skip_on_cran()
  expect_error(plot(clv.cdnow.nohold, which="timings", Ids=NA), regexp = "may not contain")
  expect_error(plot(clv.cdnow.nohold, which="timings", Ids=-1), regexp = "strictly positive")
  expect_error(plot(clv.cdnow.nohold, which="timings", Ids=0), regexp = "strictly positive")
  expect_error(plot(clv.cdnow.nohold, which="timings", Ids=1:10), regexp = "single number")
  expect_error(plot(clv.cdnow.nohold, which="timings", Ids=""), regexp = "empty text")
  expect_error(plot(clv.cdnow.nohold, which="timings", Ids=c("1", "", "2")), regexp = "empty text")
})

