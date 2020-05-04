fct.testthat.runability.nocov.out.of.the.box <- function(method, clv.data.withholdout, clv.data.noholdout, clv.newdata.withhold,
                                                   clv.newdata.nohold) {
  test_that("Works out-of-the box, without additional params", {
    l.args.hold <- list(clv.data = clv.data.withholdout, verbose=FALSE)
    l.args.no.hold <- list(clv.data = clv.data.noholdout, verbose=FALSE)

    expect_silent(p.hold    <- do.call(what = method, args = l.args.hold))
    expect_silent(p.no.hold <- do.call(what = method, args = l.args.no.hold))
    fct.helper.fitted.all.s3(clv.fitted = p.hold,     full.names = names(p.hold@clv.model@names.original.params.model),
                           clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold)
    fct.helper.fitted.all.s3(clv.fitted = p.no.hold,  full.names = names(p.no.hold@clv.model@names.original.params.model),
                           clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold)
  })
}


fct.testthat.runability.nocov.custom.model.start.params <- function(method, start.params.model, clv.data.noholdout, clv.data.withholdout){
  test_that("Works with custom model.start.params", {
    skip_on_cran()
    l.args.no.hold <- list(clv.data = clv.data.noholdout,   start.params.model = start.params.model, verbose=FALSE)
    l.args.hold <- list(clv.data = clv.data.withholdout, start.params.model = start.params.model, verbose=FALSE)

    expect_silent(do.call(what = method, args = l.args.no.hold))
    expect_silent(do.call(what = method, args = l.args.hold))
  })
}

fct.testthat.runability.nocov.custom.optimx.args <- function(method, clv.data.noholdout, clv.data.withholdout, optimx.args = list(itnmax=40000)){
  test_that("Works with custom optimx.args", {
    skip_on_cran()
    l.args.no.hold <- list(clv.data = clv.data.noholdout, optimx.args = optimx.args)
    l.args.hold <- list(clv.data = clv.data.withholdout, optimx.args = optimx.args)
    expect_message(do.call(what = method, args = l.args.no.hold))
    expect_message(do.call(what = method, args = l.args.hold))
  })
}

fct.testthat.runability.nocov.all.optimization.methods <- function(method, clv.data.noholdout){
  test_that("Works for all optimx optimization methods", {
    skip_on_cran()
    l.args <- list(clv.data = clv.data.noholdout, optimx.args = list(control=list(all.methods=TRUE)), verbose=FALSE)
    expect_warning(do.call(what = method, args = l.args),
                 regexp = "replaced by maximum positive value|Gradient not computable after method nlm|unused control arguments ignored|Estimation failed with NA coefs|Hessian could not be derived", all=TRUE)
  })
}

fct.testthat.runability.nocov.multiple.optimization.methods <- function(method, clv.data.noholdout, clv.newdata.nohold, clv.newdata.withhold){
  test_that("Works fully with multiple optimization methods", {
    skip_on_cran()
    l.args <- list(clv.data = clv.data.noholdout, optimx.args = list(method = c("BFGS", "L-BFGS-B", "Nelder-Mead")), verbose=FALSE)

    expect_silent(p.no.hold <- do.call(what = method, args = l.args))
    fct.helper.fitted.all.s3(clv.fitted = p.no.hold,  full.names = names(p.no.hold@clv.model@names.original.params.model),
                           clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold)
  })
}

fct.testthat.runability.nocov.without.spending.data <- function(method, data.transactions){
  test_that("Works without spending data",{
    skip_on_cran()
    l.args <- list(clvdata(data.transactions = cdnow, name.price = NULL, date.format = "ymd", time.unit = "w", estimation.split = 37),
                 verbose = FALSE)
    expect_silent(clv.nospending <- do.call(what = method, args = l.args))
    # predict still works outof the box
    expect_silent(predict(clv.nospending, verbose=FALSE))
    # predict fails if spending should sill be predicted
    expect_error(predict(clv.nospending, predict.spending=TRUE),regexp = "there is no spending data")
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
