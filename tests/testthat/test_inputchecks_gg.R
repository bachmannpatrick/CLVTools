# Load required data -----------------------------------------------------------------------------------
data("cdnow")

expect_silent(clv.data.cdnow.no.holdout   <- clvdata(cdnow, date.format = "ymd", time.unit = "w"))
expect_silent(clv.data.cdnow.with.holdout <- clvdata(cdnow, date.format = "ymd", time.unit = "w"))

l.std.args.noholdout <- list(clv.data=clv.data.cdnow.no.holdout)
l.std.args.withholdout <- list(clv.data=clv.data.cdnow.with.holdout)

# Parameter clv.data ------------------------------------------------------------------------------------
fct.helper.inputchecks.check.all.no.cov.model(fct.model = gg, l.std.args = l.std.args.noholdout,   name.model="Gamma/Gamma")
fct.helper.inputchecks.check.all.no.cov.model(fct.model = gg, l.std.args = l.std.args.withholdout, name.model="Gamma/Gamma")

context("Checkinputs - Gamma/Gamma - Model specific")

l.start.params.model <- list(c(p = 0, q = 1, gamma = 1),
                             c(p = 1, q = 0, gamma = 1),
                             c(p = 1, q = 1, gamma = 0),
                             c(p = -1, q = 1, gamma = 1),
                             c(p = 1, q = -1, gamma = 1),
                             c(p = 1, q = 1, gamma = -1))

fct.testthat.inputchecks.nocov.fails.for.start.params.subzero(method = gg,
                                                              clv.data.no.holdout = clv.data.cdnow.no.holdout,
                                                              clv.data.with.holdout = clv.data.cdnow.with.holdout,
                                                              l.start.params.model = l.start.params.model)

# This test case does not make sense, because this model depends on spending data
#fct.testthat.inputchecks.nocov.cannot.predict.without.spending(method = gg,
#                                                               cdnow = cdnow)

