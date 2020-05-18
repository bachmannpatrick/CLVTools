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

correct.params = c(r=1, alpha=3, a=1, b=3)
param.names = c("r", "alpha", "a", "b")

# Covariate specific parameters --------------------------------------------------------------------------
fct.helper.inputchecks.check.all.static.cov.model(fct.model = bgnbd,
                                                  l.std.args = l.std.args.noholdout,
                                                  name.model = "BGNBD static cov",
                                                  correct.params = correct.params,
                                                  param.names = param.names)
fct.helper.inputchecks.check.all.static.cov.model(fct.model = bgnbd,
                                                  l.std.args = l.std.args.withholdout,
                                                  name.model = "BGNBD static cov",
                                                  correct.params = correct.params,
                                                  param.names = param.names)

context("Inputchecks - BGNBD staticcov - Model specific")

l.start.params.model <- list(c(r = 0, alpha = 1, a = 1, b = 1),
                             c(r = 1, alpha = 0, a = 1, b = 1),
                             c(r = 1, alpha = 1, a = 0, b = 1),
                             c(r = 1, alpha = 1, a = 1, b = 0),
                             c(r = -1, alpha = 1, a = 1, b = 1),
                             c(r = 1, alpha = -1, a = 1, b = 1),
                             c(r = 1, alpha = 1, a = -1, b = 1),
                             c(r = 1, alpha = 1, a = 1, b = -1))

fct.testthat.inputchecks.staticcov.fails.for.start.params.subzero(method = bgnbd,
                                                                  clv.data.no.holdout = clv.data.apparel.no.holdout,
                                                                  clv.data.with.holdout = clv.data.apparel.with.holdout,
                                                                  l.start.params.model = l.start.params.model)
