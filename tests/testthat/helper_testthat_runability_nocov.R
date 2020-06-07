fct.testthat.runability.nocov.custom.optimx.args <- function(method, clv.data.noholdout, clv.data.withholdout, optimx.args = list(itnmax=40000)){
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
                                      estimation.split = 1000), verbose = FALSE, optimx.args=list(itnmax=40000),
                   start.params.model = start.params.model)

    # can fit
    expect_silent(hours <- do.call(what = method, args = l.args))
    # can predict
    expect_silent(predict(hours, verbose=FALSE, predict.spending=TRUE))
    # can plot
    expect_silent(plot(hours, verbose=FALSE))
  })
}
