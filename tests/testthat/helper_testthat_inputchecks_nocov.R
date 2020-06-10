
fct.testthat.inputchecks.nocov.fails.for.start.params.subzero <- function(method, clv.data.no.holdout, clv.data.with.holdout, l.start.params.model)
test_that("Fails for start params <= 0", {
  lapply(l.start.params.model, fct.testthat.inputchecks.helper.expect.error.for.params, method = method, clv.data = clv.data.no.holdout)
  lapply(l.start.params.model, fct.testthat.inputchecks.helper.expect.error.for.params, method = method, clv.data = clv.data.with.holdout)
})

fct.testthat.inputchecks.helper.expect.error.for.params <- function(start.params.model, method, clv.data){
  l.args <- list(clv.data = clv.data, start.params.model = start.params.model)

  expect_error(do.call(what = method, args = l.args), regexp = "greater")
}

fct.testthat.inputchecks.nocov.cannot.predict.without.spending <- function(method, cdnow, start.params.model)
test_that("Spending fit cannot predict on newdata that has no spending", {
  l.args <- list(clv.data = clvdata(cdnow, name.price = "Price", date.format = "ymd", time.unit = "w", estimation.split = 37),
                 verbose = FALSE)
  # Spending fit
  expect_silent(clv.spending <- do.call(what = method, args = l.args))
  # Data without spending
  expect_silent(clv.cdnow.nospending <- clvdata(cdnow, name.price = NULL, date.format = "ymd", time.unit = "w", estimation.split = 37))
  expect_error(predict(clv.spending, newdata=clv.cdnow.nospending, verbose=FALSE, predict.spending=TRUE),
               regexp = "there is no spending data")

  # but works without spending
  expect_silent(dt.pred <- predict(clv.spending, newdata=clv.cdnow.nospending, predict.spending=FALSE, verbose=FALSE))
  expect_false(any(c("predicted.Spending","predicted.CLV") %in% colnames(dt.pred)))
})


fct.testthat.inputchecks.nocov <- function(name.method, method, start.params.model, l.illegal.start.params.model,
                                           has.cor, data.cdnow){

  expect_silent(clv.data.cdnow.no.holdout   <- clvdata(data.cdnow, date.format = "ymd", time.unit = "w"))
  expect_silent(clv.data.cdnow.with.holdout <- clvdata(data.cdnow, date.format = "ymd", time.unit = "w"))

  l.std.args.noholdout   <- list(clv.data=clv.data.cdnow.no.holdout)
  l.std.args.withholdout <- list(clv.data=clv.data.cdnow.with.holdout)

  context(paste0("Inputchecks - ", name.method," nocov - Parameter start.params.model"))
  .fct.helper.inputchecks.startparamsmodel(fct.model = method,
                                           l.std.args = l.std.args.noholdout,
                                           correct.params = start.params.model,
                                           names.params = names(start.params.model))
  .fct.helper.inputchecks.startparamsmodel(fct.model = method,
                                           l.std.args = l.std.args.withholdout,
                                           correct.params = start.params.model,
                                           names.params = names(start.params.model))


  context(paste0("Inputchecks - ", name.method," nocov - Parameter optimx.args"))
  .fct.helper.inputchecks.optimxargs(fct.model = method,
                                     l.std.args = l.std.args.noholdout)
  .fct.helper.inputchecks.optimxargs(fct.model = method,
                                     l.std.args = l.std.args.withholdout)

  context(paste0("Inputchecks - ", name.method," nocov - Parameter ..."))
  .fct.helper.inputchecks.nocov...(fct.model = method,
                                   l.std.args = l.std.args.noholdout)
  .fct.helper.inputchecks.nocov...(fct.model = method,
                                   l.std.args = l.std.args.withholdout)

  fct.testthat.inputchecks.nocov.cannot.predict.without.spending(method = method, cdnow = data.cdnow)


  context(paste0("Inputchecks - ",name.method," nocov - Model specific"))

  fct.testthat.inputchecks.nocov.fails.for.start.params.subzero(method = method,
                                                                clv.data.no.holdout = clv.data.cdnow.no.holdout,
                                                                clv.data.with.holdout = clv.data.cdnow.with.holdout,
                                                                l.start.params.model = l.illegal.start.params.model)


  if(has.cor){
    context(paste0("Inputchecks - ",name.method," nocov - Parameter use.cor"))
    .fct.helper.inputchecks.usecor(fct.model = method, l.std.args = l.std.args.noholdout,   correct.param = TRUE)
    .fct.helper.inputchecks.usecor(fct.model = method, l.std.args = l.std.args.withholdout, correct.param = TRUE)

    context(paste0("Inputchecks - ",name.method," nocov - Parameter start.param.cor"))
    .fct.helper.inputchecks.startparamcor(fct.model = method, l.std.args = l.std.args.noholdout,   correct.param = 0.5)
    .fct.helper.inputchecks.startparamcor(fct.model = method, l.std.args = l.std.args.withholdout, correct.param = 0.5)
  }


}
