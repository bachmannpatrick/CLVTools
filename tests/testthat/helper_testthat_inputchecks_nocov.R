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


fct.testthat.inputchecks.usecor <- function(method, l.std.args){
  .fct.helper.inputchecks.single.logical(fct=method, l.std.args = l.std.args,
                                         name.param = "use.cor", null.allowed = FALSE)
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
    skip_on_cran()
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
    skip_on_cran()
    expect_error(do.call(method, modifyList(l.std.args, list(abc=10))),
                 regexp = "not needed")
  })
}

fct.testthat.inputchecks.fails.for.start.params.subzero <- function(method, l.std.args, l.illegal.start.params.model){
  test_that("Fails for start params <= 0", {
    skip_on_cran()
    for(single.illegal.start.params.model in l.illegal.start.params.model){
      l.args <- modifyList(l.std.args, list(start.params.model = single.illegal.start.params.model))
      expect_error(do.call(what = method, args = l.args), regexp = "greater")
    }
  })
}

fct.testthat.inputchecks.cannot.fit.without.spending <- function(method, data.cdnow){
  test_that("Cannot fit without spending data", {
    skip_on_cran()
    expect_silent(clv.cdnow.no.spending <- clvdata(data.cdnow, name.price = NULL, date.format = "ymd", time.unit = "w",
                                                   estimation.split = 37))
    expect_error(do.call(method, list(clv.data = clv.cdnow.no.spending, verbose=FALSE)),
                 regexp = "spending data")
  })
}

fct.testthat.inputchecks.cannot.predict.without.spending <- function(method, data.cdnow, is.spending.model){
  test_that("Spending fit cannot predict on newdata that has no spending", {
    skip_on_cran()

    # Fit with spending
    l.args <- list(clv.data = clvdata(data.cdnow, name.price = "Price", date.format = "ymd", time.unit = "w", estimation.split = 37),
                   verbose = FALSE)
    expect_silent(clv.spending <- do.call(what = method, args = l.args))

    # Data without spending
    expect_silent(clv.cdnow.nospending <- clvdata(data.cdnow, name.price = NULL, date.format = "ymd", time.unit = "w", estimation.split = 37))

    if(is.spending.model){
      expect_error(predict(clv.spending, newdata=clv.cdnow.nospending, verbose=FALSE),
                   regexp = "needs to contain spending data")
    }else{
      # Have to manually force predict.spending
      expect_error(predict(clv.spending, newdata=clv.cdnow.nospending, verbose=FALSE, predict.spending=TRUE),
                   regexp = "there is no spending data")
      # but works without spending
      expect_silent(dt.pred <- predict(clv.spending, newdata=clv.cdnow.nospending, predict.spending=FALSE, verbose=FALSE))
      expect_false(any(c("predicted.mean.spending","predicted.CLV") %in% colnames(dt.pred)))
    }
  })
}



fct.testthat.inputchecks.remove.first.transaction <- function(method, l.std.args){
  .fct.helper.inputchecks.single.logical(fct=method, l.std.args = l.std.args,
                                         name.param = "remove.first.transaction", null.allowed = FALSE)
}


.fct.testthat.inputchecks.clvfitted <- function(name.method, method,
                                                l.std.args.noholdout, l.std.args.withholdout,
                                                start.params.model, l.illegal.start.params.model, data.cdnow){


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
  fct.testthat.inputchecks.optimxargs(method = method, l.std.args = l.std.args.noholdout)
  fct.testthat.inputchecks.optimxargs(method = method, l.std.args = l.std.args.withholdout)

  context(paste0("Inputchecks - ", name.method," nocov - Parameter ..."))
  fct.testthat.inputchecks.nocov...(method = method, l.std.args = l.std.args.noholdout)
  fct.testthat.inputchecks.nocov...(method = method, l.std.args = l.std.args.withholdout)


  context(paste0("Inputchecks - ",name.method," nocov - Model specific"))
  fct.testthat.inputchecks.fails.for.start.params.subzero(method = method, l.std.args = l.std.args.noholdout,
                                                          l.illegal.start.params.model = l.illegal.start.params.model)
  fct.testthat.inputchecks.fails.for.start.params.subzero(method = method, l.std.args = l.std.args.withholdout,
                                                          l.illegal.start.params.model = l.illegal.start.params.model)
}


fct.testthat.inputchecks.clvfittedtransactions.nocov <- function(name.method, method, start.params.model, l.illegal.start.params.model,
                                                                 has.cor, data.cdnow){

  expect_silent(clv.data.cdnow.no.holdout   <- clvdata(data.cdnow, date.format = "ymd", time.unit = "w", estimation.split = NULL))
  expect_silent(clv.data.cdnow.with.holdout <- clvdata(data.cdnow, date.format = "ymd", time.unit = "w", estimation.split = 37))

  l.std.args.noholdout   <- list(clv.data=clv.data.cdnow.no.holdout)
  l.std.args.withholdout <- list(clv.data=clv.data.cdnow.with.holdout)

  .fct.testthat.inputchecks.clvfitted(name.method=name.method, method=method, start.params.model=start.params.model,
                                      l.std.args.noholdout=l.std.args.noholdout, l.std.args.withholdout=l.std.args.withholdout,
                                      l.illegal.start.params.model=l.illegal.start.params.model, data.cdnow=data.cdnow)


  fct.testthat.inputchecks.cannot.predict.without.spending(method = method, data.cdnow = data.cdnow, is.spending.model = FALSE)

  if(has.cor){
    context(paste0("Inputchecks - ",name.method," nocov - Parameter use.cor"))
    fct.testthat.inputchecks.usecor(method = method, l.std.args = l.std.args.noholdout)
    fct.testthat.inputchecks.usecor(method = method, l.std.args = l.std.args.withholdout)

    context(paste0("Inputchecks - ",name.method," nocov - Parameter start.param.cor"))
    fct.testthat.inputchecks.startparamcor(method = method, l.std.args = l.std.args.noholdout,   correct.param = 0.5)
    fct.testthat.inputchecks.startparamcor(method = method, l.std.args = l.std.args.withholdout, correct.param = 0.5)
  }else{
    test_that("Fails for use.cor", {
      expect_error(do.call(what = method,
                           args = list(clv.data=clv.data.cdnow.with.holdout, use.cor = TRUE)),
                   regexp = "ignored because they are not needed")
    })
  }
}


fct.testthat.inputchecks.clvfittedspending.nocov <- function(name.method, method, start.params.model, l.illegal.start.params.model,
                                                             data.cdnow){

  expect_silent(clv.data.cdnow.no.holdout   <- clvdata(data.cdnow, date.format = "ymd", time.unit = "w", estimation.split = NULL))
  expect_silent(clv.data.cdnow.with.holdout <- clvdata(data.cdnow, date.format = "ymd", time.unit = "w", estimation.split = 37))

  l.std.args.noholdout   <- list(clv.data=clv.data.cdnow.no.holdout)
  l.std.args.withholdout <- list(clv.data=clv.data.cdnow.with.holdout)

  .fct.testthat.inputchecks.clvfitted(name.method=name.method, method=method, start.params.model=start.params.model,
                                      l.std.args.noholdout=l.std.args.noholdout, l.std.args.withholdout=l.std.args.withholdout,
                                      l.illegal.start.params.model=l.illegal.start.params.model, data.cdnow=data.cdnow)


  context(paste0("Inputchecks - ",name.method," nocov - Parameter remove.first.transaction"))
  fct.testthat.inputchecks.remove.first.transaction(method = method, l.std.args = l.std.args.noholdout)
  fct.testthat.inputchecks.remove.first.transaction(method = method, l.std.args = l.std.args.withholdout)

  context(paste0("Inputchecks - ",name.method," nocov - Parameter clvdata/newdata always have spending"))
  fct.testthat.inputchecks.cannot.fit.without.spending(method = method, data.cdnow = data.cdnow)
  fct.testthat.inputchecks.cannot.predict.without.spending(method = method, data.cdnow = data.cdnow, is.spending.model = TRUE)
}



