# Load required data -----------------------------------------------------------------------------------
data("cdnow")

expect_silent(clv.data.cdnow.no.holdout   <- clvdata(cdnow, date.format = "ymd", time.unit = "w"))
expect_silent(clv.data.cdnow.with.holdout <- clvdata(cdnow, date.format = "ymd", time.unit = "w"))

l.std.args.noholdout <- list(clv.data=clv.data.cdnow.no.holdout)
l.std.args.withholdout <- list(clv.data=clv.data.cdnow.with.holdout)


# Parameter clv.data ------------------------------------------------------------------------------------
fct.helper.inputchecks.check.all.no.cov.model(fct.model = pnbd, l.std.args = l.std.args.noholdout,   name.model="pnbd nocov")
fct.helper.inputchecks.check.all.no.cov.model(fct.model = pnbd, l.std.args = l.std.args.withholdout, name.model="pnbd nocov")

context("Inputchecks - pnbd nocov - Model specific")

clv.data.cdnow.no.holdout
clv.data.cdnow.with.holdout

l.start.params.model <- list(c(alpha=0, beta=1, r=1, s=1),
                            c(alpha=-1, beta=1, r=1, s=1),
                            c(alpha=1, beta=1, r=0, s=1))

fct.testthat.inputchecks.nocov.fails.for.start.params.subzero(method = pnbd,
                                                              clv.data.no.holdout = clv.data.cdnow.no.holdout,
                                                              clv.data.with.holdout = clv.data.cdnow.with.holdout,
                                                              l.start.params.model = l.start.params.model)

fct.testthat.inputchecks.nocov.cannot.predict.without.spending(method = pnbd,
                                                               cdnow = cdnow)
