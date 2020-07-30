fct.testthat.runability.nocov.without.spending.data <- function(method, data.transactions){
  test_that("Works without spending data",{
    skip_on_cran()
    skip_on_ci()
    skip_on_covr()
    l.args <- list(clvdata(data.transactions = data.transactions, name.price = NULL, date.format = "ymd", time.unit = "w", estimation.split = 37),
                   verbose = FALSE)
    expect_silent(clv.nospending <- do.call(what = method, args = l.args))
    # predict still works out of the box
    expect_silent(predict(clv.nospending, verbose=FALSE))
    # predict fails if spending should sill be predicted
    expect_error(predict(clv.nospending, predict.spending=TRUE), regexp = "there is no spending data")
  })
}

fct.testthat.runability.nocov.predict.fit.no.spending.but.newdata.spending <- function(method, data.transactions){
  test_that("Fit without spending can predict on newdata that has spending", {
    skip_on_cran()
    skip_on_ci()
    skip_on_covr()
    l.args <- list(clvdata(data.transactions = data.transactions, name.price = NULL, date.format = "ymd", time.unit = "w", estimation.split = 37),
                   verbose = FALSE)
    # No spending fit
    expect_silent(clv.nospending <- do.call(what = method, args = l.args))
    # Data with spending
    expect_silent(clv.cdnow.spending <- clvdata(data.transactions, name.price = "Price", date.format = "ymd", time.unit = "w", estimation.split = 37))
    expect_silent(dt.pred <- predict(clv.nospending, newdata=clv.cdnow.spending, verbose=FALSE, predict.spending=TRUE))
    expect_true(all(c("predicted.mean.spending") %in% colnames(dt.pred)))
  })
}


fct.helper.create.fake.transdata <- function(data){
  set.seed(0xcaffe) # hipster seed
  expect_silent(dt.newdata.trans <- rbindlist(lapply(LETTERS, function(cid){
    dt <- data.table(cust.id = cid,
                     trans.date = seq.Date(from = data[, min(Date)], to = data[, max(Date)],
                                           length.out = sample.int(n=5, size = 1, replace=FALSE)))
    dt[, Price := sample(1:1000, size = .N)]
    return(dt)
  })))
  expect_silent(dt.newdata.trans[, trans.date := format(trans.date, "%Y:%d:%m")])
}

fct.helper.create.fake.newdata.nocov <- function(data, estimation.split){
  # to test plot/predict
  # Create with new fake data and generally other names
  dt.newdata.trans <- fct.helper.create.fake.transdata(data = data)
  expect_silent(clv.newdata <- clvdata(data.transactions = dt.newdata.trans, date.format = "ydm", time.unit = "w",
                                       estimation.split = estimation.split, name.id = "cust.id", name.date = "trans.date",
                                       name.price = "Price"))
  return(clv.newdata)
}


fct.testthat.runability.nocov <- function(name.model, method, cdnow,
                                          has.DERT, has.cor,
                                          start.params.model,
                                          failed.optimization.methods.expected.message){


  context(paste0("Runability - ",name.model," nocov - Basic runability"))

  expect_silent(clv.data.cdnow.noholdout   <- clvdata(data.transactions = cdnow, date.format = "ymd", time.unit = "W"))
  expect_silent(clv.data.cdnow.withholdout <- clvdata(data.transactions = cdnow, date.format = "ymd", time.unit = "W",
                                                      estimation.split = 37))
  clv.newdata.nohold   <- fct.helper.create.fake.newdata.nocov(data = cdnow, estimation.split = NULL)
  clv.newdata.withhold <- fct.helper.create.fake.newdata.nocov(data = cdnow, estimation.split = 37)

  param.names <- names(start.params.model)
  l.args.test.all.s3 <- list(full.names = param.names, clv.newdata.nohold = clv.newdata.nohold,
                             clv.newdata.withhold = clv.newdata.withhold, DERT.not.implemented = !has.DERT)

  # Common tests ------------------------------------------------------------------------------------------------------------
  fct.testthat.runability.clvfitted.out.of.the.box.no.hold(method = method, clv.data.noholdout = clv.data.cdnow.noholdout,
                                                           l.args.test.all.s3 = l.args.test.all.s3, fct.test.all.s3=fct.helper.clvfittedtransactions.all.s3)

  fct.testthat.runability.clvfitted.out.of.the.box.with.hold(method = method, clv.data.withholdout = clv.data.cdnow.withholdout,
                                                           l.args.test.all.s3 = l.args.test.all.s3, fct.test.all.s3=fct.helper.clvfittedtransactions.all.s3)

  fct.testthat.runability.clvfitted.custom.model.start.params(method = method, start.params.model = start.params.model, clv.data = clv.data.cdnow.noholdout)
  fct.testthat.runability.clvfitted.custom.model.start.params(method = method, start.params.model = start.params.model, clv.data = clv.data.cdnow.withholdout)

  # fct.testthat.runability.clvfitted.all.optimization.methods(method = method, clv.data = clv.data.cdnow.noholdout,
  #                                                         expected.message = failed.optimization.methods.expected.message)
  #
  fct.testthat.runability.clvfitted.multiple.optimization.methods(method = method, clv.data= clv.data.cdnow.noholdout,
                                                                  l.args.test.all.s3 = l.args.test.all.s3, fct.test.all.s3=fct.helper.clvfittedtransactions.all.s3)

  # **TODO: fix ggomnbd?
  # fct.testthat.runability.clvfitted.hourly.data(method = method, data.cdnow = cdnow)


  # Nocov tests ------------------------------------------------------------------------------------------------------------
  fct.testthat.runability.clvfitted.custom.optimx.args(method = method, clv.data = clv.data.cdnow.noholdout)

  fct.testthat.runability.nocov.without.spending.data(method = method, data.transactions = cdnow)

  fct.testthat.runability.nocov.predict.fit.no.spending.but.newdata.spending(method = method, data.transactions = cdnow)


  if(has.cor){
    fct.testthat.runability.common.works.with.cor(method = method,
                                                  clv.data.holdout = clv.data.cdnow.withholdout,
                                                  clv.newdata.nohold = clv.newdata.nohold,
                                                  clv.newdata.withhold = clv.newdata.withhold,
                                                  DERT.not.implemented = !has.DERT,
                                                  names.params.model = names(start.params.model))

    fct.testthat.runability.common.works.with.cor.start.params(method = method,
                                                               clv.data.holdout = clv.data.cdnow.withholdout,
                                                               clv.newdata.nohold = clv.newdata.nohold,
                                                               clv.newdata.withhold = clv.newdata.withhold,
                                                               DERT.not.implemented = !has.DERT,
                                                               names.params.model = names(start.params.model))
  }
}
