fct.testthat.runability.common.works.with.cor <- function(method, clv.data.holdout, clv.newdata.nohold, clv.newdata.withhold,
                                                          names.params.model, DERT.not.implemented){
  test_that("Works with use.cor=T", {
    skip_on_cran()
    skip_on_ci()
    skip_on_covr()

    l.args <- list(clv.data = clv.data.holdout, use.cor=TRUE, verbose=FALSE)
    expect_silent(m.cor <- do.call(what = method, args = l.args))

    full.names <- c(names.params.model, m.cor@clv.model@name.correlation.cor)
    if(is(clv.data.holdout, "clv.data.static.covariates"))
      full.names <- c(full.names, m.cor@names.prefixed.params.free.life, m.cor@names.prefixed.params.free.trans)

    fct.helper.clvfittedtransactions.all.s3(m.cor, full.names = full.names, DERT.not.implemented = DERT.not.implemented,
                                            clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold)
  })
}

fct.testthat.runability.common.works.with.cor.start.params <- function(method, clv.data.holdout, clv.newdata.nohold, clv.newdata.withhold,
                                                                       names.params.model, DERT.not.implemented = DERT.not.implemented){
  test_that("Works with use.cor=T and start.params", {
    skip_on_cran()
    skip_on_ci()
    skip_on_covr()

    l.args <- list(clv.data = clv.data.holdout, use.cor=TRUE, start.param.cor = 0.0, verbose=FALSE)
    expect_silent(m.cor <- do.call(what = method, args = l.args))

    full.names <- c(names.params.model, m.cor@clv.model@name.correlation.cor)
    if(is(clv.data.holdout, "clv.data.static.covariates"))
      full.names <- c(full.names, m.cor@names.prefixed.params.free.life, m.cor@names.prefixed.params.free.trans)

    fct.helper.clvfittedtransactions.all.s3(m.cor, full.names = full.names, DERT.not.implemented = DERT.not.implemented,
                                            clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold)
  })
}
