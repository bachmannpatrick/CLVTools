# Setup ------------------------------------------------------------------------------------------------------------------
data("cdnow")

context("Runability - PNBD nocov - Basic runability")

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

fct.testthat.runability.nocov.out.of.the.box(method = pnbd, clv.data.withholdout = clv.data.cdnow.withholdout,
                                         clv.data.noholdout = clv.data.cdnow.noholdout,
                                         clv.newdata.withhold = clv.newdata.withhold,
                                         clv.newdata.nohold = clv.newdata.nohold)

fct.testthat.runability.nocov.custom.model.start.params(method = pnbd, start.params.model = c(r=1, alpha = 2, s = 1, beta = 2), clv.data.cdnow.noholdout, clv.data.cdnow.withholdout)

fct.testthat.runability.nocov.custom.optimx.args(method = pnbd,
                                         clv.data.noholdout = clv.data.cdnow.noholdout,
                                         clv.data.withholdout = clv.data.cdnow.withholdout)

fct.testthat.runability.nocov.all.optimization.methods(method = pnbd,
                                         clv.data.noholdout = clv.data.cdnow.noholdout)

fct.testthat.runability.nocov.multiple.optimization.methods(method = pnbd,
                                                        clv.data.noholdout = clv.data.cdnow.noholdout,
                                                        clv.newdata.nohold = clv.newdata.nohold,
                                                        clv.newdata.withhold = clv.newdata.withhold)

fct.testthat.runability.nocov.without.spending.data(method = pnbd, data.transactions = cdnow)

fct.testthat.runability.nocov.predict.newdata.spending(method = pnbd, data.transactions = cdnow)

fct.testthat.runability.nocov.hourly.data(method = pnbd, data.cdnow = cdnow, start.params.model = c(r = 0.63177, alpha = 4451.331, s = 0.000002, beta = 0.5166))
