data("cdnow")

context("Inputchecks - clvdata - plot")

clv.cdnow <- fct.helper.create.clvdata.cdnow(estimation.split = NULL)

l.std.args <- list(clv.cdnow)
.fct.helper.inputchecks.single.logical(fct = plot, l.std.args = l.std.args, name.param = "cumulative")
.fct.helper.inputchecks.single.logical(fct = plot, l.std.args = l.std.args, name.param = "plot")
.fct.helper.inputchecks.single.logical(fct = plot, l.std.args = l.std.args, name.param = "verbose")

test_that("Stops for unneeded parameters", {
  expect_error(plot(clv.cdnow, abc=123), regexp = "not needed")
  expect_error(plot(clv.cdnow, transactions=TRUE), regexp = "not needed")
})
