fct.testthat.inputchecks.staticcov.fails.for.start.params.subzero <- function(method, clv.data.no.holdout, clv.data.with.holdout, l.start.params.model){
  test_that("Fails for start params <= 0", {
    lapply(l.start.params.model, fct.testthat.inputchecks.helper.expect.error.for.params, method = method, clv.data = clv.data.no.holdout)
    lapply(l.start.params.model, fct.testthat.inputchecks.helper.expect.error.for.params, method = method, clv.data = clv.data.with.holdout)
  })
}

