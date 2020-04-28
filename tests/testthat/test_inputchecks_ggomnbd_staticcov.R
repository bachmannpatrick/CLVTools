# Load required data -----------------------------------------------------------------------------------
data("apparelTrans")
data("apparelStaticCov")

expect_silent(clv.data.apparel.no.holdout   <- clvdata(apparelTrans, date.format = "ymd", time.unit = "w"))
expect_silent(clv.data.apparel.with.holdout <- clvdata(apparelTrans, date.format = "ymd", time.unit = "w"))

expect_silent(clv.data.apparel.no.holdout <- SetStaticCovariates(clv.data = clv.data.apparel.no.holdout,
                                                                 data.cov.life = apparelStaticCov, names.cov.life = "Gender",
                                                                 data.cov.trans = apparelStaticCov, names.cov.trans = "Gender"))
expect_silent(clv.data.apparel.with.holdout <- SetStaticCovariates(clv.data = clv.data.apparel.with.holdout,
                                                                   data.cov.life = apparelStaticCov, names.cov.life = "Gender",
                                                                   data.cov.trans = apparelStaticCov, names.cov.trans = "Gender"))
l.std.args.noholdout   <- list(clv.data=clv.data.apparel.no.holdout)
l.std.args.withholdout <- list(clv.data=clv.data.apparel.with.holdout)


# Covariate specific parameters --------------------------------------------------------------------------
#fct.helper.inputchecks.check.all.static.cov.model(fct.model = ggomnbd, l.std.args = l.std.args.noholdout, name.model = "ggomnbd staticcov")
#fct.helper.inputchecks.check.all.static.cov.model(fct.model = ggomnbd, l.std.args = l.std.args.withholdout, name.model = "ggomnbd staticcov")

context("Inputchecks - GGompertz / NBD staticcov - Model specific")
test_that("Fails for start params <= 0", {

  expect_error(ggomnbd(clv.data.apparel.no.holdout, start.params.model = c(r = 0, alpha = 1, beta = 1, b = 1, s = 1)),
               regexp = "greater")
  expect_error(ggomnbd(clv.data.apparel.no.holdout, start.params.model = c(r = 1, alpha = 0, beta = 1, b = 1, s = 1)),
               regexp = "greater")
  expect_error(ggomnbd(clv.data.apparel.no.holdout, start.params.model = c(r = 1, alpha = 1, beta = 0, b = 1, s = 1)),
               regexp = "greater")
  expect_error(ggomnbd(clv.data.apparel.no.holdout, start.params.model = c(r = 1, alpha = 1, beta = 1, b = 0, s = 1)),
               regexp = "greater")
  expect_error(ggomnbd(clv.data.apparel.no.holdout, start.params.model = c(r = 1, alpha = 1, beta = 1, b = 1, s = 0)),
               regexp = "greater")

  expect_error(ggomnbd(clv.data.apparel.with.holdout, start.params.model = c(r = 0, alpha = 1, beta = 1, b = 1, s = 1)),
               regexp = "greater")
  expect_error(ggomnbd(clv.data.apparel.with.holdout, start.params.model = c(r = 1, alpha = 0, beta = 1, b = 1, s = 1)),
               regexp = "greater")
  expect_error(ggomnbd(clv.data.apparel.with.holdout, start.params.model = c(r = 1, alpha = 1, beta = 0, b = 1, s = 1)),
               regexp = "greater")
  expect_error(ggomnbd(clv.data.apparel.with.holdout, start.params.model = c(r = 1, alpha = 1, beta = 1, b = 0, s = 1)),
               regexp = "greater")
  expect_error(ggomnbd(clv.data.apparel.with.holdout, start.params.model = c(r = 1, alpha = 1, beta = 1, b = 1, s = 0)),
               regexp = "greater")
})
