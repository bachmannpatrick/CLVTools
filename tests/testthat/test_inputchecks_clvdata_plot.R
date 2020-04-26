data("cdnow")

context("Inputchecks - clvdata - plot")

expect_silent(clv.cdnow <- clvdata(cdnow, date.format = "ymd", time.unit = "w"))

l.std.args <- list(clv.cdnow)
.fct.helper.inputchecks.single.logical(fct = plot, l.std.args = l.std.args, name.param = "cumulative")
.fct.helper.inputchecks.single.logical(fct = plot, l.std.args = l.std.args, name.param = "plot")
.fct.helper.inputchecks.single.logical(fct = plot, l.std.args = l.std.args, name.param = "verbose")

test_that("Stops for unneeded parameters", {
  expect_error(plot(clv.cdnow, abc=123), regexp = "not needed")
  expect_error(plot(clv.cdnow, transactions=TRUE), regexp = "not needed")
})
