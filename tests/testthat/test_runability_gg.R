# Setup ------------------------------------------------------------------------------------------------------------------
data("cdnow")

context("Runability - Gamma/Gamma nocov - Basic runability")

gg.param.names = c("p", "q", "gamma")

expect_silent(clv.data.cdnow.noholdout <- clvdata(data.transactions = cdnow, date.format = "ymd", time.unit = "W"))
expect_silent(clv.data.cdnow.withholdout <- clvdata(data.transactions = cdnow, date.format = "ymd", time.unit = "W",
                                                    estimation.split = 37))

# Newdata clv data object to test plot/predict
#   Create with new fake data and generally other names
set.seed(0xcaffe) # hipster seed

expect_silent(dt.newdata.trans <- rbindlist(lapply(LETTERS, function(cid){
  data.table(cust.id = cid,
             trans.date = seq.Date(from = cdnow[, min(Date)], to = cdnow[, max(Date)],
                                   length.out = sample.int(n=5, size = 1, replace=FALSE)))
})))
expect_silent(dt.newdata.trans[, trans.date := format(trans.date, "%Y:%d:%m")])
expect_silent(clv.newdata.nohold <- clvdata(data.transactions = dt.newdata.trans, date.format = "ydm", time.unit = "w",
                                            estimation.split = NULL, name.id = "cust.id", name.date = "trans.date",
                                            name.price = NULL))
expect_silent(clv.newdata.withhold <- clvdata(data.transactions = dt.newdata.trans, date.format = "ydm", time.unit = "w",
                                              estimation.split = 37, name.id = "cust.id", name.date = "trans.date",
                                              name.price = NULL))

# Works out of the box without holdout
clv.fitted.nohold <- gg(clv.data = clv.data.cdnow.noholdout, verbose = FALSE)

.fct.helper.s3.fitted.coef(clv.fitted = clv.fitted.nohold, full.names = gg.param.names)

.fct.helper.s3.fitted.vcov(clv.fitted = clv.fitted.nohold, full.names = gg.param.names)

.fct.helper.s3.fitted.confint(clv.fitted = clv.fitted.nohold, full.names = gg.param.names)

.fct.helper.s3.fitted.summary(clv.fitted = clv.fitted.nohold)

.fct.helper.s3.fitted.print(clv.fitted = clv.fitted.nohold)

.fct.helper.s3.fitted.nobs(clv.fitted = clv.fitted.nohold)

.fct.helper.s3.fitted.logLik(clv.fitted = clv.fitted.nohold)

.fct.helper.s3.fitted.predict(clv.fitted = clv.fitted.nohold, clv.newdata.nohold=clv.newdata.nohold,
                              clv.newdata.withhold=clv.newdata.withhold, DERT.not.implemented=TRUE, model.depends.on.price = TRUE)

# Works out of the box with holdout
clv.fitted.withhold <- gg(clv.data = clv.data.cdnow.withholdout, verbose = FALSE)

.fct.helper.s3.fitted.coef(clv.fitted = clv.fitted.withhold, full.names = gg.param.names)

.fct.helper.s3.fitted.vcov(clv.fitted = clv.fitted.withhold, full.names = gg.param.names)

.fct.helper.s3.fitted.confint(clv.fitted = clv.fitted.withhold, full.names = gg.param.names)

.fct.helper.s3.fitted.summary(clv.fitted = clv.fitted.withhold)

.fct.helper.s3.fitted.print(clv.fitted = clv.fitted.withhold)

.fct.helper.s3.fitted.nobs(clv.fitted = clv.fitted.withhold)

.fct.helper.s3.fitted.logLik(clv.fitted = clv.fitted.withhold)

.fct.helper.s3.fitted.predict(clv.fitted = clv.fitted.withhold, clv.newdata.nohold=clv.newdata.nohold,
                              clv.newdata.withhold=clv.newdata.withhold, DERT.not.implemented=TRUE, model.depends.on.price = TRUE)

fct.testthat.runability.common.custom.model.start.params(method = gg,
                                                         start.params.model = c(p = 2, q = 2, gamma = 2),
                                                         clv.data.noholdout = clv.data.cdnow.noholdout,
                                                         clv.data.withholdout = clv.data.cdnow.withholdout)

fct.testthat.runability.nocov.custom.optimx.args(method = gg,
                                                 clv.data.noholdout = clv.data.cdnow.noholdout,
                                                 clv.data.withholdout = clv.data.cdnow.withholdout)

# This test does not make sense, as spending data is the basis of this model
#fct.testthat.runability.nocov.without.spending.data(method = gg,
#                                                    data.transactions = cdnow)

# This test does not make sense, as this model needs the price column for the cbs creation
#fct.testthat.runability.nocov.predict.newdata.spending(method = gg,
#                                                       data.transactions = cdnow,
#                                                       needs.price = TRUE)

fct.testthat.runability.common.all.optimization.methods(method = gg,
                                                        clv.data.noholdout = clv.data.cdnow.noholdout,
                                                        expected.message = "replaced by maximum positive value|Gradient not computable after method nlm|unused control arguments ignored|Estimation failed with NA coefs|Hessian could not be derived")


# Hourly tests
# Filter out suitable range
cdnow.early <- cdnow[Id %in% cdnow[, .(last.trans = max(Date)), by="Id"][last.trans <= "1997-03-01"]$Id]
cdnow.early <- cdnow.early[Id %in% cdnow[, .(first.trans = min(Date)), by="Id"][first.trans <= "1997-02-01"]$Id]
l.args <- list(clv.data = clvdata(data.transactions = cdnow.early, date.format = "ymd", time.unit = "h",
                                  estimation.split = 1000), verbose = FALSE, optimx.args=list(itnmax=40000),
               start.params.model = c(p = 1, q = 1, gamma = 1))

# can fit
expect_silent(hours <- do.call(what = gg, args = l.args))
# can predict
expect_silent(predict(hours, verbose=FALSE, predict.spending=TRUE))
