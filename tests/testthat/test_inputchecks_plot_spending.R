fct.testthat.inputchecks.clvfittedspending.plot.ellipsis <- function(fitted.spending){
  test_that("Stop if unnecessary inputs given in ellipsis", {
    expect_error(plot(fitted.spending, abc = 123), "further parameters")
    expect_error(plot(fitted.spending, use.cor = TRUE), "further parameters")
    expect_error(plot(fitted.spending, prediction.end = 6), "further parameters")
  })
}



fct.testthat.inputchecks.clvfittedspending.plot <- function(data.cdnow){

  expect_silent(gg.cdnow <- gg(fct.helper.create.clvdata.cdnow(data.cdnow), verbose = FALSE))
  l.std.args <- list(x = gg.cdnow)

  fct.helper.inputcheck.single.numeric(fct = plot, l.std.args = l.std.args, name.param = "n")

  .fct.helper.inputchecks.single.logical(fct = plot, l.std.args = l.std.args, name.param = "verbose", null.allowed = FALSE)

  fct.testthat.inputchecks.clvfittedspending.plot.ellipsis(fitted.spending = gg.cdnow)

}


skip_on_cran()

data("cdnow")
data("apparelTrans")
data("apparelStaticCov")
fct.testthat.inputchecks.clvfittedspending.plot(data.cdnow = cdnow)

