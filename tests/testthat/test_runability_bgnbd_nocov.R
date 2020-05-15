# Setup ------------------------------------------------------------------------------------------------------------------
data("cdnow")

context("Runability - BGNBD nocov - Basic runability")

bgnbd.param.names = c("r", "alpha", "a", "b")

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

fct.testthat.runability.common.out.of.the.box.no.hold(method = bgnbd,
                                                      clv.data.noholdout = clv.data.cdnow.noholdout,
                                                      clv.newdata.withhold = clv.newdata.withhold,
                                                      clv.newdata.nohold = clv.newdata.nohold,
                                                      param.names = bgnbd.param.names,
                                                      DERT.not.implemented = TRUE)

fct.testthat.runability.common.out.of.the.box.with.hold(method = bgnbd,
                                                        clv.data.withholdout = clv.data.cdnow.withholdout,
                                                        clv.newdata.withhold = clv.newdata.withhold,
                                                        clv.newdata.nohold = clv.newdata.nohold,
                                                        param.names = bgnbd.param.names,
                                                        DERT.not.implemented = TRUE)

fct.testthat.runability.common.custom.model.start.params(method = bgnbd,
                                                         start.params.model = c(r = 1, alpha = 4, a = 1, b = 4),
                                                         clv.data.noholdout = clv.data.cdnow.noholdout,
                                                         clv.data.withholdout = clv.data.cdnow.withholdout)

fct.testthat.runability.nocov.custom.optimx.args(method = bgnbd,
                                                 clv.data.noholdout = clv.data.cdnow.noholdout,
                                                 clv.data.withholdout = clv.data.cdnow.withholdout)

fct.testthat.runability.nocov.without.spending.data(method = bgnbd,
                                                    data.transactions = cdnow)

fct.testthat.runability.nocov.predict.newdata.spending(method = bgnbd,
                                                       data.transactions = cdnow)

fct.testthat.runability.nocov.hourly.data(method = bgnbd,
                                          data.cdnow = cdnow,
                                          start.params.model = c(r = 1, alpha = 3, a = 1, b = 3))
