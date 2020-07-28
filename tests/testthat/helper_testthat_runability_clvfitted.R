fct.testthat.runability.clvfitted.out.of.the.box.no.hold <- function(method, clv.data.noholdout, fct.test.all.s3, l.args.test.all.s3){
  test_that("Works out-of-the box, without additional params (no holdout)", {
    l.args.no.hold <- list(clv.data = clv.data.noholdout, verbose=FALSE)

    expect_silent(m.no.hold <- do.call(what = method, args = l.args.no.hold))
    l.args.test.all.s3 <- modifyList(l.args.test.all.s3, list(clv.fitted = m.no.hold))
    do.call(fct.test.all.s3, l.args.test.all.s3)
  })
}

fct.testthat.runability.clvfitted.out.of.the.box.with.hold <- function(method, clv.data.withholdout, fct.test.all.s3, l.args.test.all.s3){
  test_that("Works out-of-the box, without additional params (with holdout)", {
    skip_on_cran()
    l.args.hold <- list(clv.data = clv.data.withholdout, verbose=FALSE)

    expect_silent(m.hold <- do.call(what = method, args = l.args.hold))
    l.args.test.all.s3   <- modifyList(l.args.test.all.s3, list(clv.fitted = m.hold))
    do.call(fct.test.all.s3, l.args.test.all.s3)
  })
}

fct.testthat.runability.clvfitted.custom.model.start.params <- function(method, start.params.model, clv.data){
  test_that("Works with custom model.start.params", {
    skip_on_cran()
    skip_on_ci()
    skip_on_covr()

    l.args <- list(clv.data = clv.data,   start.params.model = start.params.model, verbose=FALSE)

    expect_silent(do.call(what = method, args = l.args))
  })
}

fct.testthat.runability.clvfitted.custom.optimx.args <- function(method, clv.data){
  test_that("Works with custom optimx.args", {
    skip_on_cran()
    skip_on_ci()
    skip_on_covr()
    # output although verbose = FALSE
    l.args <- list(clv.data = clv.data, verbose = FALSE, optimx.args = list(control = list(trace=6)))
    expect_output(do.call(what = method, args = l.args))
  })
}


fct.testthat.runability.clvfitted.all.optimization.methods <- function(method, clv.data, expected.message){
  test_that("Works for all optimx optimization methods", {
    skip_on_cran()
    skip_on_ci()
    skip_on_covr()

    l.args <- list(clv.data = clv.data, optimx.args = list(control=list(all.methods=TRUE)), verbose=FALSE)
    expect_warning(do.call(what = method, args = l.args), regexp = expected.message, all=TRUE)
  })
}

fct.testthat.runability.clvfitted.multiple.optimization.methods <- function(method, clv.data, fct.test.all.s3, l.args.test.all.s3){
  test_that("Works fully with multiple optimization methods", {
    skip_on_cran()
    skip_on_ci()
    skip_on_covr()

    l.args <- list(clv.data = clv.data, optimx.args = list(method = c("BFGS", "L-BFGS-B", "Nelder-Mead")), verbose=FALSE)

    expect_silent(m.fit <- do.call(what = method, args = l.args))
    l.args.test.all.s3 <- modifyList(l.args.test.all.s3, list(clv.fitted = m.fit))
    do.call(fct.test.all.s3, l.args.test.all.s3)
  })
}


fct.testthat.runability.clvfitted.hourly.data <- function(method, data.cdnow, start.params.model, fct.test.all.s3, l.args.test.all.s3){
  test_that("Works with hourly data", {
    skip_on_cran()
    # Filter out suitable range
    cdnow.early <- data.cdnow[Id %in% data.cdnow[, .(last.trans = max(Date)), by="Id"][last.trans <= "1997-03-01"]$Id]
    cdnow.early <- cdnow.early[Id %in% data.cdnow[, .(first.trans = min(Date)), by="Id"][first.trans <= "1997-02-01"]$Id]
    l.args <- list(clv.data = clvdata(data.transactions = cdnow.early, date.format = "ymd", time.unit = "h",
                                      estimation.split = 1000), verbose = FALSE)

    expect_silent(fitted.hours <- do.call(what = method, args = l.args))
    l.args.test.all.s3 <- modifyList(l.args.test.all.s3, list(clv.fitted = fitted.hours))
    do.call(fct.test.all.s3, l.args.test.all.s3)

    # # can predict
    # expect_silent(predict(hours, verbose=FALSE, predict.spending=TRUE))
    # # can plot
    # expect_silent(plot(hours, verbose=FALSE))
  })
}
