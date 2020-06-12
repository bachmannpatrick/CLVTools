fct.testthat.inputchecks.startparamsmodel <- function(method, l.std.args, correct.params, names.params){

  test_that("Fails if not vector", {
    expect_error(do.call(method, modifyList(l.std.args,
                                               list(start.params.model = as.list(correct.params)))),
                 regexp = "numeric vector")
    expect_error(do.call(method, modifyList(l.std.args,
                                               list(start.params.model = as.data.frame(correct.params)))),
                 regexp = "numeric vector")
  })

  test_that("Fails if not numeric", {
    expect_error(do.call(method, modifyList(l.std.args,list(start.params.model =
                                                                 setNames(as.character(correct.params), names.params)))),
                 regexp = "numeric vector")

    expect_error(do.call(method, modifyList(l.std.args,list(start.params.model =
                                                                 setNames(as.factor(as.character(correct.params)), names.params)))),
                 regexp = "numeric vector")
  })

  test_that("Fails if not named", {
    expect_error(do.call(method, modifyList(l.std.args,list(start.params.model = unname(correct.params)))),
                 regexp = "Please provide a named")
  })
  test_that("Fails if named wrongly", {
    expect_error(do.call(method, modifyList(l.std.args,list(start.params.model =
                                                                 setNames(correct.params, rep("a", length(names.params)))))),
                 regexp = "Please provide the model")
  })
  test_that("Fails if single name missing", {
    expect_error(do.call(method, modifyList(l.std.args,list(start.params.model =
                                                                 setNames(correct.params, c("",names.params[-1]))))),
                 regexp = "Please provide the model")
  })
  test_that("Fails if duplicate params", {
    expect_error(do.call(method, modifyList(l.std.args,list(start.params.model =
                                                                 setNames(c(1, correct.params), c(names.params[1],names.params))))),
                 regexp = "Please provide exactly")
  })

  test_that("Fails if unneded param", {
    expect_error(do.call(method, modifyList(l.std.args,list(start.params.model =
                                                                 setNames(c(1, correct.params), c("abc",names.params))))),
                 regexp = "Please provide exactly")
  })

  test_that("Fails if any param NA", {
    expect_error(do.call(method, modifyList(l.std.args,list(start.params.model =
                                                                 setNames(c(NA_real_, correct.params[-1]),
                                                                          names.params)))),
                 regexp = "no NAs ")
  })
}


fct.testthat.inputchecks.usecor <- function(method, l.std.args, correct.param){
  test_that("Fails if not logical", {

    expect_error(do.call(method, modifyList(l.std.args,
                                               list(use.cor = NULL), keep.null = TRUE)),
                 regexp = "type logical")
    expect_error(do.call(method, modifyList(l.std.args,
                                               list(use.cor = as.list(correct.param)))),
                 regexp = "type logical")
    expect_error(do.call(method, modifyList(l.std.args,
                                               list(use.cor = as.character(correct.param)))),
                 regexp = "type logical")
    expect_error(do.call(method, modifyList(l.std.args,
                                               list(use.cor = as.factor(correct.param)))),
                 regexp = "type logical")
  })

  test_that("Fails if more than 1", {
    expect_error(do.call(method, modifyList(l.std.args,
                                               list(use.cor = rep(correct.param,2)))),
                 regexp = "single element")
  })

  test_that("Fails if is NA", {
    expect_error(do.call(method, modifyList(l.std.args,
                                               list(use.cor = NA))),
                 regexp = "NA")
  })
}


fct.testthat.inputchecks.startparamcor <- function(method, l.std.args, correct.param){

  l.std.args <- modifyList(l.std.args, list(use.cor=TRUE))

  test_that("Fails if not numeric vector", {
    expect_error(do.call(method, modifyList(l.std.args,
                                               list(start.param.cor = as.list(correct.param)))),
                 regexp = "numeric")
    expect_error(do.call(method, modifyList(l.std.args,
                                               list(start.param.cor = as.data.frame(correct.param)))),
                 regexp = "numeric")

    expect_error(do.call(method, modifyList(l.std.args,
                                               list(start.param.cor = as.character(correct.param)))),
                 regexp = "numeric")

    expect_error(do.call(method, modifyList(l.std.args,
                                               list(start.param.cor = as.factor(correct.param)))),
                 regexp = "numeric")

    expect_error(do.call(method, modifyList(l.std.args,
                                               list(start.param.cor = TRUE))),
                 regexp = "numeric")
  })

  test_that("Fails if more than 1", {
    expect_error(do.call(method, modifyList(l.std.args,
                                               list(start.param.cor = rep(correct.param,2)))),
                 regexp = "single")
  })

  test_that("Fails if is NA", {
    expect_error(do.call(method, modifyList(l.std.args,
                                               list(start.param.cor = NA_real_))),
                 regexp = "NA")
  })

  test_that("Fails if is out of [-1, 1]", {
    expect_error(do.call(method, modifyList(l.std.args,
                                               list(start.param.cor = 42))),
                 regexp = "interval")
    expect_error(do.call(method, modifyList(l.std.args,
                                               list(start.param.cor = -42))),
                 regexp = "interval")
  })

  test_that("Warning if given but use.cor = FALSE", {
    expect_error(do.call(method, modifyList(l.std.args,
                                               list(start.param.cor = correct.param,
                                                    use.cor=FALSE))),
                 regexp = "no correlation")
  })
}

fct.testthat.inputchecks.optimxargs <- function(method, l.std.args){

  test_that("Fails if not list", {
    expect_error(do.call(method, modifyList(l.std.args, list(optimx.args=data.frame(kkt=TRUE)))),
                 regexp = "list")
    expect_error(do.call(method, modifyList(l.std.args, list(optimx.args=c(kkt=TRUE)))),
                 regexp = "list")
  })
  test_that("Fails if NULL/NA", {
    expect_error(do.call(method, modifyList(l.std.args, list(optimx.args=NULL), keep.null = TRUE)),
                 regexp = "NULL")
    expect_error(do.call(method, modifyList(l.std.args, list(optimx.args=NA))),
                 regexp = "list")
  })

  test_that("Fails if unnamed elements", {
    expect_error(do.call(method, modifyList(l.std.args, list(optimx.args=list(TRUE)))),
                 regexp = "named")
    expect_error(do.call(method, modifyList(l.std.args, list(optimx.args=list("Nelder-Mead")))),
                 regexp = "named")
    expect_error(do.call(method, modifyList(l.std.args, list(optimx.args=list(method="Nelder-Mead", TRUE)))),
                 regexp = "names for every element")
  })
  test_that("Fails if top level names which are not in optimx", {
    expect_error(do.call(method, modifyList(l.std.args, list(optimx.args=list(methods="Nelder-Mead")))),
                 regexp = "valid input to optimx()")
    expect_error(do.call(method, modifyList(l.std.args, list(optimx.args=list(method="Nelder-Mead", kkk=TRUE)))),
                 regexp = "valid input to optimx()")
  })
}

fct.testthat.inputchecks.nocov... <- function(method, l.std.args){

  test_that("Stop if unnecessary inputs given in ellipsis", {
    # stuff that is for covariate models only

    expect_error(do.call(method, modifyList(l.std.args, list(names.cov.trans="Gender"))),
                 regexp = "not need")
    expect_error(do.call(method, modifyList(l.std.args, list(names.cov.life="Gender"))),
                 regexp = "not need")

    expect_error(do.call(method, modifyList(l.std.args, list(start.params.life=c(Gender=0.5)))),
                 regexp = "not needed")
    expect_error(do.call(method, modifyList(l.std.args, list(start.params.trans=c(Gender=0.5)))),
                 regexp = "not need")

    expect_error(do.call(method, modifyList(l.std.args, list(names.cov.constr="Gender"))),
                 regexp = "not need")
    expect_error(do.call(method, modifyList(l.std.args, list(start.params.constr=c(Gender=0.5)))),
                 regexp = "not need")

    expect_error(do.call(method, modifyList(l.std.args, list(reg.lambdas=c(trans=10, life=10)))),
                 regexp = "not need")
  })

  test_that("Stop if anything else in ellipsis", {
    expect_error(do.call(method, modifyList(l.std.args, list(abc=10))),
                 regexp = "not needed")
  })
}

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
  fct.testthat.inputchecks.startparamsmodel(method = method,
                                           l.std.args = l.std.args.noholdout,
                                           correct.params = start.params.model,
                                           names.params = names(start.params.model))
  fct.testthat.inputchecks.startparamsmodel(method = method,
                                           l.std.args = l.std.args.withholdout,
                                           correct.params = start.params.model,
                                           names.params = names(start.params.model))


  context(paste0("Inputchecks - ", name.method," nocov - Parameter optimx.args"))
  fct.testthat.inputchecks.optimxargs(method = method,
                                     l.std.args = l.std.args.noholdout)
  fct.testthat.inputchecks.optimxargs(method = method,
                                     l.std.args = l.std.args.withholdout)

  context(paste0("Inputchecks - ", name.method," nocov - Parameter ..."))
  fct.testthat.inputchecks.nocov...(method = method,
                                   l.std.args = l.std.args.noholdout)
  fct.testthat.inputchecks.nocov...(method = method,
                                   l.std.args = l.std.args.withholdout)

  fct.testthat.inputchecks.nocov.cannot.predict.without.spending(method = method, cdnow = data.cdnow)


  context(paste0("Inputchecks - ",name.method," nocov - Model specific"))

  fct.testthat.inputchecks.nocov.fails.for.start.params.subzero(method = method,
                                                                clv.data.no.holdout = clv.data.cdnow.no.holdout,
                                                                clv.data.with.holdout = clv.data.cdnow.with.holdout,
                                                                l.start.params.model = l.illegal.start.params.model)


  if(has.cor){
    context(paste0("Inputchecks - ",name.method," nocov - Parameter use.cor"))
    fct.testthat.inputchecks.usecor(method = method, l.std.args = l.std.args.noholdout,   correct.param = TRUE)
    fct.testthat.inputchecks.usecor(method = method, l.std.args = l.std.args.withholdout, correct.param = TRUE)

    context(paste0("Inputchecks - ",name.method," nocov - Parameter start.param.cor"))
    fct.testthat.inputchecks.startparamcor(method = method, l.std.args = l.std.args.noholdout,   correct.param = 0.5)
    fct.testthat.inputchecks.startparamcor(method = method, l.std.args = l.std.args.withholdout, correct.param = 0.5)
  }


}
