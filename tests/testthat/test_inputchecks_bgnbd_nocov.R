# Load required data -----------------------------------------------------------------------------------
data("cdnow")

expect_silent(clv.data.cdnow.no.holdout   <- clvdata(cdnow, date.format = "ymd", time.unit = "w"))
expect_silent(clv.data.cdnow.with.holdout <- clvdata(cdnow, date.format = "ymd", time.unit = "w"))

l.std.args.noholdout <- list(clv.data=clv.data.cdnow.no.holdout)
l.std.args.withholdout <- list(clv.data=clv.data.cdnow.with.holdout)

# Parameter clv.data ------------------------------------------------------------------------------------
fct.helper.inputchecks.check.all.no.cov.model(fct.model = bgnbd, l.std.args = l.std.args.noholdout,   name.model="bgnbd nocov")
fct.helper.inputchecks.check.all.no.cov.model(fct.model = bgnbd, l.std.args = l.std.args.withholdout, name.model="bgnbd nocov")

context("Checkinputs - BGNBD nocov - Model specific")

l.start.params.model <- list(c(r = 0, alpha = 1, a = 1, b = 1),
                             c(r = 1, alpha = 0, a = 1, b = 1),
                             c(r = 1, alpha = 1, a = 0, b = 1),
                             c(r = 1, alpha = 1, a = 1, b = 0),
                             c(r = -1, alpha = 1, a = 1, b = 1),
                             c(r = 1, alpha = -1, a = 1, b = 1),
                             c(r = 1, alpha = 1, a = -1, b = 1),
                             c(r = 1, alpha = 1, a = 1, b = -1))

fct.testthat.inputchecks.nocov.fails.for.start.params.subzero(method = bgnbd,
                                                              clv.data.no.holdout = clv.data.cdnow.no.holdout,
                                                              clv.data.with.holdout = clv.data.cdnow.with.holdout,
                                                              l.start.params.model = l.start.params.model)

fct.testthat.inputchecks.nocov.cannot.predict.without.spending(method = bgnbd,
                                                               cdnow = cdnow)
