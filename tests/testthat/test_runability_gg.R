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

fct.testthat.runability.common.out.of.the.box.no.hold(method = gg,
                                                      clv.data.noholdout = clv.data.cdnow.noholdout,
                                                      clv.newdata.withhold = clv.newdata.withhold,
                                                      clv.newdata.nohold = clv.newdata.nohold,
                                                      param.names = gg.param.names,
                                                      DERT.not.implemented = TRUE,
                                                      model.depends.on.price = TRUE)

fct.testthat.runability.common.out.of.the.box.with.hold(method = gg,
                                                        clv.data.withholdout = clv.data.cdnow.withholdout,
                                                        clv.newdata.nohold = clv.newdata.nohold,
                                                        clv.newdata.withhold = clv.newdata.withhold,
                                                        param.names = gg.param.names,
                                                        DERT.not.implemented = TRUE,
                                                        model.depends.on.price = TRUE)

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

fct.testthat.runability.nocov.hourly.data(method = gg,
                                          data.cdnow = cdnow,
                                          start.params.model = c(p = 1, q = 1, gamma = 1))
