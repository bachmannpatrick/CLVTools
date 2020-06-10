fct.testthat.runability.nocov.custom.optimx.args <- function(method, clv.data.noholdout, clv.data.withholdout, optimx.args){
  test_that("Works with custom optimx.args", {
    skip_on_cran()
    l.args.no.hold <- list(clv.data = clv.data.noholdout, optimx.args = optimx.args)
    l.args.hold <- list(clv.data = clv.data.withholdout, optimx.args = optimx.args)
    expect_message(do.call(what = method, args = l.args.no.hold))
    expect_message(do.call(what = method, args = l.args.hold))
  })
}

fct.testthat.runability.nocov.without.spending.data <- function(method, data.transactions){
  test_that("Works without spending data",{
    skip_on_cran()
    l.args <- list(clvdata(data.transactions = data.transactions, name.price = NULL, date.format = "ymd", time.unit = "w", estimation.split = 37),
                 verbose = FALSE)
    expect_silent(clv.nospending <- do.call(what = method, args = l.args))
    # predict still works out of the box
    expect_silent(predict(clv.nospending, verbose=FALSE))
    # predict fails if spending should sill be predicted
    expect_error(predict(clv.nospending, predict.spending=TRUE), regexp = "there is no spending data")
  })
}

fct.testthat.runability.nocov.predict.newdata.spending <- function(method, data.transactions){
  test_that("No spending fit can predict on newdata that has spending", {
    skip_on_cran()
    l.args <- list(clvdata(data.transactions = data.transactions, name.price = NULL, date.format = "ymd", time.unit = "w", estimation.split = 37),
                   verbose = FALSE)
    # No spending fit
    expect_silent(clv.nospending <- do.call(what = method, args = l.args))
    # Data with spending
    expect_silent(clv.cdnow.spending <- clvdata(data.transactions, name.price = "Price", date.format = "ymd", time.unit = "w", estimation.split = 37))
    expect_silent(dt.pred <- predict(clv.nospending, newdata=clv.cdnow.spending, verbose=FALSE, predict.spending=TRUE))
    expect_true(all(c("predicted.Spending","predicted.CLV") %in% colnames(dt.pred)))
  })
}

fct.testthat.runability.nocov.hourly.data <- function(method, data.cdnow, start.params.model){
  test_that("Works with hourly data", {
    skip_on_cran()
    # Filter out suitable range
    cdnow.early <- data.cdnow[Id %in% data.cdnow[, .(last.trans = max(Date)), by="Id"][last.trans <= "1997-03-01"]$Id]
    cdnow.early <- cdnow.early[Id %in% data.cdnow[, .(first.trans = min(Date)), by="Id"][first.trans <= "1997-02-01"]$Id]
    l.args <- list(clv.data = clvdata(data.transactions = cdnow.early, date.format = "ymd", time.unit = "h",
                                      estimation.split = 1000), verbose = FALSE)

    # can fit
    expect_silent(hours <- do.call(what = method, args = l.args))
    # can predict
    expect_silent(predict(hours, verbose=FALSE, predict.spending=TRUE))
    # can plot
    expect_silent(plot(hours, verbose=FALSE))
  })
}



fct.testthat.runability.nocov <- function(name.model, method, cdnow,
                                          has.DERT, has.cor,
                                          start.params.model,
                                          failed.optimization.methods.expected.message, custom.optimx.args){


  context(paste0("Runability - ",name.model," nocov - Basic runability"))

  # Data objects: normal data
  expect_silent(clv.data.cdnow.noholdout   <- clvdata(data.transactions = cdnow, date.format = "ymd", time.unit = "W"))
  expect_silent(clv.data.cdnow.withholdout <- clvdata(data.transactions = cdnow, date.format = "ymd", time.unit = "W",
                                                      estimation.split = 37))

  # Data objects: newdata
  # to test plot/predict
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


  # Common tests ------------------------------------------------------------------------------------------------------------
  param.names <- names(start.params.model)
  fct.testthat.runability.common.out.of.the.box.no.hold(method = method, clv.data.noholdout = clv.data.cdnow.noholdout,
                                                        clv.newdata.withhold = clv.newdata.withhold, clv.newdata.nohold = clv.newdata.nohold,
                                                        full.param.names = param.names, DERT.not.implemented = !has.DERT)

  fct.testthat.runability.common.out.of.the.box.with.hold(method = method, clv.data.withholdout = clv.data.cdnow.withholdout,
                                                          clv.newdata.withhold = clv.newdata.withhold, clv.newdata.nohold = clv.newdata.nohold,
                                                          full.param.names = param.names, DERT.not.implemented = !has.DERT)
  #
  fct.testthat.runability.common.custom.model.start.params(method = method, start.params.model = start.params.model,
                                                          clv.data.noholdout = clv.data.cdnow.noholdout, clv.data.withholdout = clv.data.cdnow.withholdout)

  # fct.testthat.runability.common.all.optimization.methods(method = method, clv.data.noholdout = clv.data.cdnow.noholdout,
  #                                                         expected.message = failed.optimization.methods.expected.message)
  #
  # fct.testthat.runability.common.multiple.optimization.methods(method = method, param.names = param.names,
  #                                                              DERT.not.implemented = !has.DERT,
  #                                                              clv.data.noholdout = clv.data.cdnow.noholdout,
  #                                                              clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold)



  # Nocov tests ------------------------------------------------------------------------------------------------------------
  fct.testthat.runability.nocov.custom.optimx.args(method = method, optimx.args = custom.optimx.args,
                                                   clv.data.noholdout = clv.data.cdnow.noholdout, clv.data.withholdout = clv.data.cdnow.withholdout)

  fct.testthat.runability.nocov.without.spending.data(method = method, data.transactions = cdnow)

  fct.testthat.runability.nocov.predict.newdata.spending(method = method, data.transactions = cdnow)

  fct.testthat.runability.nocov.hourly.data(method = method, data.cdnow = cdnow)


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
