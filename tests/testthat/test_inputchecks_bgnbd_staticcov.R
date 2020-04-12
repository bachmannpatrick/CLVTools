# Load required data -----------------------------------------------------------------------------------
data("apparelTrans")
data("apparelDemographics")

expect_message(clv.data.apparel.no.holdout   <- clvdata(apparelTrans, date.format = "ymd", time.unit = "w"), regexp = "ignored")
expect_message(clv.data.apparel.with.holdout <- clvdata(apparelTrans, date.format = "ymd", time.unit = "w"), regexp = "ignored")

expect_silent(clv.data.apparel.no.holdout <- SetStaticCovariates(clv.data = clv.data.apparel.no.holdout,
                                                                 data.cov.life = apparelDemographics, names.cov.life = "Gender",
                                                                 data.cov.trans = apparelDemographics, names.cov.trans = "Gender"))
expect_silent(clv.data.apparel.with.holdout <- SetStaticCovariates(clv.data = clv.data.apparel.with.holdout,
                                                                   data.cov.life = apparelDemographics, names.cov.life = "Gender",
                                                                   data.cov.trans = apparelDemographics, names.cov.trans = "Gender"))
l.std.args.noholdout   <- list(clv.data=clv.data.apparel.no.holdout)
l.std.args.withholdout <- list(clv.data=clv.data.apparel.with.holdout)


# Covariate specific parameters --------------------------------------------------------------------------
fct.helper.inputchecks.check.all.static.cov.model(fct.model = bgnbd, l.std.args = l.std.args.noholdout, name.model = "bgnbd staticcov")
fct.helper.inputchecks.check.all.static.cov.model(fct.model = bgnbd, l.std.args = l.std.args.withholdout, name.model = "bgnbd staticcov")

context("Inputchecks - bgnbd staticcov - Model specific")
test_that("Fails for start params <= 0", {
  expect_error(bgnbd(clv.data.apparel.no.holdout, start.params.model = c(r=1, alpha=0, a=1, b=1)),
               regexp = "greater")
  expect_error(bgnbd(clv.data.apparel.no.holdout, start.params.model = c(r=1, alpha=-1, a=1, b=1)),
               regexp = "greater")
  expect_error(bgnbd(clv.data.apparel.no.holdout, start.params.model = c(r=0, alpha=1, a=1, b=1)),
               regexp = "greater")

  expect_error(bgnbd(clv.data.apparel.with.holdout, start.params.model = c(r=1, alpha=0, a=1, b=1)),
               regexp = "greater")
  expect_error(bgnbd(clv.data.apparel.with.holdout, start.params.model = c(r=1, alpha=-1, a=1, b=1)),
               regexp = "greater")
  expect_error(bgnbd(clv.data.apparel.with.holdout, start.params.model = c(r=0, alpha=1, a=1, b=1)),
               regexp = "greater")
})
