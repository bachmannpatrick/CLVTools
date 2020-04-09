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

# Basic runability -------------------------------------------------------------------------------------------------------

test_that("Works out-of-the box, without additional params", {
  expect_silent(p.hold    <- pnbd(clv.data=clv.data.cdnow.withholdout, verbose=FALSE))
  expect_silent(p.no.hold <- pnbd(clv.data=clv.data.cdnow.noholdout, verbose=FALSE))
  fct.helper.fitted.all.s3(clv.fitted = p.hold,     full.names = names(p.hold@clv.model@names.original.params.model),
                           clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold)
  fct.helper.fitted.all.s3(clv.fitted = p.no.hold,  full.names = names(p.no.hold@clv.model@names.original.params.model),
                           clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold)
})

test_that("Works with custom model.start.params", {
  skip_on_cran()
  expect_silent(pnbd(clv.data=clv.data.cdnow.noholdout,   start.params.model = c(r=1, alpha = 2, s = 1, beta = 2), verbose=FALSE))
  expect_silent(pnbd(clv.data=clv.data.cdnow.withholdout, start.params.model = c(r=1, alpha = 2, s = 1, beta = 2), verbose=FALSE))
})

# optimx.args
test_that("Works with custom optimx.args", {
  skip_on_cran()
  # dont do trace, spams the output
  expect_message(pnbd(clv.data=clv.data.cdnow.noholdout, optimx.args = list(itnmax=40000)))
  expect_message(pnbd(clv.data=clv.data.cdnow.withholdout, optimx.args = list(itnmax=40000)))
})

test_that("Works with multiple optimization methods",{
  skip_on_cran()
  expect_message(p.no.hold <- pnbd(clv.data=clv.data.cdnow.noholdout, optimx.args = list(method = c("Nelder-Mead", "L-BFGS-B"),
                                                                                         control=list(follow.on=TRUE))))
  fct.helper.fitted.all.s3(clv.fitted = p.no.hold,  full.names = names(p.no.hold@clv.model@names.original.params.model),
                           clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold)
})

test_that("Works without spending data",{
  skip_on_cran()
  expect_silent(clv.pnbd.nospending <- pnbd(clvdata(cdnow, name.price = NULL, date.format = "ymd", time.unit = "w", estimation.split = 37),
                                            verbose = FALSE))
  # predict still works outof the box
  expect_silent(predict(clv.pnbd.nospending, verbose=FALSE))
  # Predict fails if spending should sill be predicted
  expect_error(predict(clv.pnbd.nospending, predict.spending=TRUE),regexp = "there is no spending data")
})

test_that("No spending fit can predict on newdata that has spending", {
  skip_on_cran()
  # No spending fit
  expect_silent(clv.pnbd.nospending <- pnbd(clvdata(cdnow, name.price = NULL, date.format = "ymd", time.unit = "w", estimation.split = 37),
                                            verbose = FALSE))
  # Data with spending
  expect_silent(clv.cdnow.spending <- clvdata(cdnow, name.price = "Price", date.format = "ymd", time.unit = "w", estimation.split = 37))
  expect_silent(dt.pred <- predict(clv.pnbd.nospending, newdata=clv.cdnow.spending, verbose=FALSE, predict.spending=TRUE))
  expect_true(all(c("predicted.Spending","predicted.CLV") %in% colnames(dt.pred)))
})

test_that("Works with hourly data", {
  skip_on_cran()
  # Filter out suitable range
  cdnow.early <- cdnow[Id %in% cdnow[, .(last.trans = max(Date)), by="Id"][last.trans <= "1997-03-01"]$Id]
  cdnow.early <- cdnow.early[Id %in% cdnow[, .(first.trans = min(Date)), by="Id"][first.trans <= "1997-02-01"]$Id]
  # can fit
  expect_silent(pnbd.hours <- pnbd(clvdata(data.transactions = cdnow.early, date.format = "ymd", time.unit = "h",
                                           estimation.split = 1000), verbose = FALSE, optimx.args=list(itnmax=40000),
                                   start.params.model = c(r = 0.63177, alpha = 4451.331, s = 0.000002, beta = 0.5166)))
  # can predict
  expect_silent( predict(pnbd.hours, verbose=FALSE, predict.spending=TRUE))
  # can plot
  expect_silent(plot(pnbd.hours, verbose=FALSE))
})

# w/ correlation -------------------------------------------------------------------------------------------------------
# context("Runability - PNBD nocov - w/ correlation")
# test_that("Without holdout - with correlation", {
#   skip_on_cran
#
#   # expect_silent(pcdnow.c <- pnbd(clv.data=clv.data.cdnow.withholdout, use.cor=TRUE, verbose=FALSE))
#   # fct.helper.fitted.all.s3(clv.fitted = pcdnow.c,  full.names = c(names(pcdnow.c@clv.model@names.original.params.model),pcdnow.c@name.correlation.cor))
#
#   # expect_warning(pcdnow.c <- pnbd(clv.data.cdnow.noholdout, use.cor = TRUE, verbose = FALSE),
#   #                regexp = "Gradient not computable")
#   # Vcov does not work but other S3 that do not need vcov() should still work
#   # .fct.helper.s3.fitted.coef(clv.fitted = pcdnow.c, full.names = c("r", "alpha", "s", "beta", pscc@name.correlation.cor))
#   # expect_warning(summary(pcdnow.c), regexp = "For some parameters the standard error could not be calculated.")
#   # .fct.helper.s3.fitted.print(clv.fitted = pcdnow.c)
#   # .fct.helper.s3.fitted.nobs(clv.fitted = pcdnow.c)
#   # .fct.helper.s3.fitted.logLik(clv.fitted = pcdnow.c)
#   # .fct.helper.s3.fitted.plot(clv.fitted = pcdnow.c)
#   # .fct.helper.s3.fitted.predict(clv.fitted = pcdnow.c)
# })

# test_that("Works with use.cor=T and start.params", {
#   skip_on_cran()
#   expect_message(pnbd(clv.data=clv.data.cdnow.noholdout, use.cor=TRUE, start.param.cor = 0.25, verbose=FALSE))
#   expect_message(pnbd(clv.data=clv.data.cdnow.withholdout, use.cor=TRUE, start.param.cor = 0.25, verbose=FALSE))
# })

