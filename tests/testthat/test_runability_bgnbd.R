# Setup ------------------------------------------------------------------------------------------------------------------
data("cdnow")

context("Runability - BG/NBD nocov - Basic runability")


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

# Basic runability -------------------------------------------------------------------------------------------------------

test_that("Works out-of-the box, without additional params", {
  expect_silent(p.hold    <- bgnbd(clv.data=clv.data.cdnow.withholdout, verbose=FALSE))
  expect_silent(p.no.hold <- bgnbd(clv.data=clv.data.cdnow.noholdout, verbose=FALSE))
  fct.helper.fitted.all.s3(clv.fitted = p.hold,     full.names = names(p.hold@clv.model@names.original.params.model),
                           clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold)
  fct.helper.fitted.all.s3(clv.fitted = p.no.hold,  full.names = names(p.no.hold@clv.model@names.original.params.model),
                           clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold)
})

test_that("Works with custom model.start.params", {
  skip_on_cran()
  expect_silent(bgnbd(clv.data=clv.data.cdnow.noholdout,   start.params.model = c(r=1, alpha = 3, a = 1, b = 1), verbose=FALSE))
  expect_silent(bgnbd(clv.data=clv.data.cdnow.withholdout, start.params.model = c(r=1, alpha = 3, b = 1, b = 1), verbose=FALSE))
})

# optimx.args
test_that("Works with custom optimx.args", {
  skip_on_cran()
  # dont do trace, spams the output
  expect_message(bgnbd(clv.data=clv.data.cdnow.noholdout, optimx.args = list(itnmax=40000)))
  expect_message(bgnbd(clv.data=clv.data.cdnow.withholdout, optimx.args = list(itnmax=40000)))
})

test_that("Works without spending data",{
  skip_on_cran()
  expect_silent(clv.bgnbd.nospending <- bgnbd(clvdata(cdnow, name.price = NULL, date.format = "ymd", time.unit = "w", estimation.split = 37),
                                            verbose = FALSE))
  # predict still works outof the box
  expect_silent(predict(clv.bgnbd.nospending, verbose=FALSE))
  # Predict fails if spending should sill be predicted
  expect_error(predict(clv.bgnbd.nospending, predict.spending=TRUE),regexp = "there is no spending data")
})

test_that("No spending fit can predict on newdata that has spending", {
  skip_on_cran()
  # No spending fit
  expect_silent(clv.bgnbd.nospending <- bgnbd(clvdata(cdnow, name.price = NULL, date.format = "ymd", time.unit = "w", estimation.split = 37),
                                            verbose = FALSE))
  # Data with spending
  expect_silent(clv.cdnow.spending <- clvdata(cdnow, name.price = "Price", date.format = "ymd", time.unit = "w", estimation.split = 37))
  expect_silent(dt.pred <- predict(clv.bgnbd.nospending, newdata=clv.cdnow.spending, verbose=FALSE, predict.spending=TRUE))
  expect_true(all(c("predicted.Spending","predicted.CLV") %in% colnames(dt.pred)))
})

test_that("Works with hourly data", {
  skip_on_cran()
  # Filter out suitable range
  cdnow.early <- cdnow[Id %in% cdnow[, .(last.trans = max(Date)), by="Id"][last.trans <= "1997-03-01"]$Id]
  cdnow.early <- cdnow.early[Id %in% cdnow[, .(first.trans = min(Date)), by="Id"][first.trans <= "1997-02-01"]$Id]
  # can fit
  expect_silent(bgnbd.hours <- bgnbd(clvdata(data.transactions = cdnow.early, date.format = "ymd", time.unit = "h",
                                           estimation.split = 1000), verbose = FALSE, optimx.args=list(itnmax=40000),
                                   start.params.model = c(r = 1, alpha = 3, a = 1, b = 3)))
  # can predict
  expect_silent( predict(bgnbd.hours, verbose=FALSE, predict.spending=TRUE))
  # can plot
  expect_silent(plot(bgnbd.hours, verbose=FALSE))
})


