skip_on_cran()

gg.cdnow <- fit.cdnow(model = gg)

l.std.args <- list(x = gg.cdnow)
fct.helper.inputcheck.single.numeric(fct = plot, l.std.args = l.std.args, name.param = "n")
.fct.helper.inputchecks.single.logical(fct = plot, l.std.args = l.std.args, name.param = "verbose", null.allowed = FALSE)


test_that("Stop if unnecessary inputs given in ellipsis", {
  expect_error(plot(gg.cdnow, abc = 123), "further parameters")
  expect_error(plot(gg.cdnow, use.cor = TRUE), "further parameters")
  expect_error(plot(gg.cdnow, prediction.end = 6), "further parameters")
})
