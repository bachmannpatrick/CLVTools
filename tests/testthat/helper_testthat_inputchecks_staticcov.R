fct.testthat.inputchecks.startparamcov <- function(method, l.std.args, name.param){
  test_that("Fails if not numeric", {
    expect_error(do.call(method, modifyList(l.std.args, setNames(list(c(Gender="0.5")), name.param))),
                 regexp = "numeric vector")
    expect_error(do.call(method, modifyList(l.std.args, setNames(list(data.frame(Gender=0.5)), name.param))),
                 regexp = "numeric vector")
    expect_error(do.call(method, modifyList(l.std.args, setNames(list(list(Gender=0.5)), name.param))),
                 regexp = "numeric vector")
  })
  test_that("Fails if any NA", {
    expect_error(do.call(method, modifyList(l.std.args, setNames(list(c(Gender=NA_real_, Gender=0.5)), name.param))),
                 regexp = "no NA")
  })
  test_that("Fails if any not a covariate", {
    expect_error(do.call(method, modifyList(l.std.args, setNames(list(c(gender=0.5)), name.param))),
                 regexp = "Please provide")
  })

#   test_that("Fails if a covariate missing", {
#     Channel is missing -
#     expect_error(do.call(method, modifyList(l.std.args, setNames(list(c(Gender = 0.5)), name.param))),
#                  regexp = "Please provide")
#   })

  test_that("Fails if not named", {
    expect_error(do.call(method, modifyList(l.std.args, setNames(list(c(0.5)), name.param))),
                 regexp = "Please provide")
  })
  test_that("Fails if one entry not named", {
    expect_error(do.call(method, modifyList(l.std.args, setNames(list(c(Gender=0.5, 0.5)), name.param))),
                 regexp = "Please provide")
  })
  test_that("Fails if duplicates", {
    expect_error(do.call(method, modifyList(l.std.args, setNames(list(c(gender=0.5, Gender=0.5)), name.param))),
                 regexp = "Please provide exactly")
    expect_error(do.call(method, modifyList(l.std.args, setNames(list(c(Gender=0.5, Gender=0.5)), name.param))),
                 regexp = "Please provide")
    expect_error(do.call(method, modifyList(l.std.args, setNames(list(c(Gender=0.5, Gender=NA_real_)), name.param))),
                 regexp = "Please provide")
  })
}

fct.testthat.inputchecks.namescovconstr <- function(method, l.std.args){

  test_that("Fails if not a character vector", {
    expect_error(do.call(method, modifyList(l.std.args, list(names.cov.constr=1))),
                 regexp = "character vector")
    expect_error(do.call(method, modifyList(l.std.args, list(names.cov.constr=list("Gender")))),
                 regexp = "character vector")
    expect_error(do.call(method, modifyList(l.std.args, list(names.cov.constr=data.frame("Gender")))),
                 regexp = "character vector")
  })

  test_that("Fails if any NA", {
    expect_error(do.call(method, modifyList(l.std.args, list(names.cov.constr=c(NA_character_)))),
                 regexp = "no NA")
    expect_error(do.call(method, modifyList(l.std.args, list(names.cov.constr=c("Gender", NA_character_)))),
                 regexp = "no NA")
  })

  test_that("Fails constraint names not in covariates", {
    expect_error(do.call(method, modifyList(l.std.args, list(names.cov.constr=c("Gender1")))),
                 regexp = "could not be found")
    expect_error(do.call(method, modifyList(l.std.args, list(names.cov.constr=c("Gender", "Gender1")))),
                 regexp = "could not be found")
  })

  test_that("Fails if duplicates", {
    expect_error(do.call(method, modifyList(l.std.args, list(names.cov.constr=c("Gender", "Gender")))),
                 regexp = "exactly once")
  })

  test_that("Fails if want to give start params for constraint params as free covariates", {
    expect_error(do.call(method, modifyList(l.std.args, list(names.cov.constr=c("Gender"),
                                                                start.params.life = c(Gender=1)))),
                 regexp = "for the constraint parameters in")
    expect_error(do.call(method, modifyList(l.std.args, list(names.cov.constr=c("Gender"),
                                                                start.params.trans = c(Gender=1)))),
                 regexp = "for the constraint parameters in")

    expect_error(do.call(method, modifyList(l.std.args, list(names.cov.constr=c("Gender"),
                                                                start.params.trans = c(Gender=1),
                                                                start.params.life  = c(Gender=1)))),
                 regexp = "for the constraint parameters in")
  })
}

fct.testthat.inputchecks.startparamconstr <- function(method, l.std.args){
  l.std.args <- modifyList(l.std.args, list(names.cov.constr = "Gender"))

  test_that("Fails if any NA", {
    expect_error(do.call(method, modifyList(l.std.args, list(start.params.constr=c(Gender=NA_real_)))),
                 regexp = "no NA")
  })

  test_that("Fails if not a numeric vector", {
    expect_error(do.call(method, modifyList(l.std.args, list(start.params.constr=c(Gender="0.5")))),
                 regexp = "numeric vector")
    expect_error(do.call(method, modifyList(l.std.args, list(start.params.constr=list(Gender=0.5)))),
                 regexp = "numeric vector")
    expect_error(do.call(method, modifyList(l.std.args, list(start.params.constr=data.frame(Gender=0.5)))),
                 regexp = "numeric vector")
  })

  test_that("Fails if unnnamed", {
    expect_error(do.call(method, modifyList(l.std.args, list(start.params.constr=c(0.5)))),
                 regexp = "named vector")
    expect_error(do.call(method, modifyList(l.std.args, list(start.params.constr=c(Gender=0.5, 0.2)))),
                 regexp = "names for every parameter")
  })

  test_that("Fails for params not constraint ", { #constraint names not in covariates", {
    expect_error(do.call(method, modifyList(l.std.args, list(names.cov.constr = NULL,
                                                                start.params.constr=c(Gender=0.5)))),
                 regexp = "should only be provided")
  })
}

fct.testthat.inputchecks.reglambdas <- function(method, l.std.args){

  test_that("Fails if is not a numeric vector", {
    expect_error(do.call(method, modifyList(l.std.args, list(reg.lambdas=list(list(trans=10, life=10))))),
                 regexp = "numeric vector")
    expect_error(do.call(method, modifyList(l.std.args, list(reg.lambdas=data.frame(trans=10, life=10)))),
                 regexp = "numeric vector")
    expect_error(do.call(method, modifyList(l.std.args, list(reg.lambdas=c(trans="10", life="10")))),
                 regexp = "numeric vector")
    expect_error(do.call(method, modifyList(l.std.args, list(reg.lambdas=factor(c(trans="10", life="10"))))),
                 regexp = "numeric vector")
  })

  test_that("Fails if any is NA", {
    expect_error(do.call(method, modifyList(l.std.args, list(reg.lambdas=c(trans=10, life=NA_real_)))),
                 regexp = "NA")
    expect_error(do.call(method, modifyList(l.std.args, list(reg.lambdas=c(life=10, trans=NA_real_)))),
                 regexp = "NA")
    expect_error(do.call(method, modifyList(l.std.args, list(reg.lambdas=c(life=10, trans=10, NA_real_)))),
                 regexp = "NA")
    expect_error(do.call(method, modifyList(l.std.args, list(reg.lambdas=NA_real_))),
                 regexp = "NA")
  })

  test_that("Fails for negative regularization lambdas", {
    expect_error(do.call(method, modifyList(l.std.args, list(reg.lambdas=c(trans=10,  life=-10)))),
                 regexp = "positive")
    expect_error(do.call(method, modifyList(l.std.args, list(reg.lambdas=c(trans=-10, life=10)))),
                 regexp = "positive")
    expect_error(do.call(method, modifyList(l.std.args, list(reg.lambdas=c(trans=-10, life=-10)))),
                 regexp = "positive")
  })

  test_that("Fails for wrongly named regularization lambdas", {
    expect_error(do.call(method, modifyList(l.std.args, list(reg.lambdas=c(trans=10,  live=10)))),
                 regexp = "be named")
    expect_error(do.call(method, modifyList(l.std.args, list(reg.lambdas=c(trans=10,  Life=10)))),
                 regexp = "be named")
    expect_error(do.call(method, modifyList(l.std.args, list(reg.lambdas=c(frans=10,  life=10)))),
                 regexp = "be named")
    expect_error(do.call(method, modifyList(l.std.args, list(reg.lambdas=c(Trans=10,  life=10)))),
                 regexp = "be named")
    expect_error(do.call(method, modifyList(l.std.args, list(reg.lambdas=c(Trans=10,  Life=10)))),
                 regexp = "be named")
    expect_error(do.call(method, modifyList(l.std.args, list(reg.lambdas=c(trans=10,  lifetime=10)))),
                 regexp = "be named")
  })

  test_that("Fails if only one given", {
    expect_error(do.call(method, modifyList(l.std.args, list(reg.lambdas=c(trans=10)))),
                 regexp = "need to be given")
    expect_error(do.call(method, modifyList(l.std.args, list(reg.lambdas=c(life=10)))),
                 regexp = "need to be given")
  })

  test_that("Fails if too many given", {
    expect_error(do.call(method, modifyList(l.std.args, list(reg.lambdas=c(trans=10, life=10, abc=10)))),
                 regexp = "need to be given")
    expect_error(do.call(method, modifyList(l.std.args, list(reg.lambdas=c(life=10, life=10, tra=12)))),
                 regexp = "need to be given")
  })

  test_that("Fails if duplicates given", {
    expect_error(do.call(method, modifyList(l.std.args, list(reg.lambdas=c(life=10, life=10)))),
                 regexp = "need to be named")
    expect_error(do.call(method, modifyList(l.std.args, list(reg.lambdas=c(trans=10, trans=10)))),
                 regexp = "need to be named")

    expect_error(do.call(method, modifyList(l.std.args, list(reg.lambdas=c(life=10, life=10, trans=10)))),
                 regexp = "need to be given")
    expect_error(do.call(method, modifyList(l.std.args, list(reg.lambdas=c(life=10, trans=10, trans=10)))),
                 regexp = "need to be given")
  })
}


fct.testthat.inputchecks.staticcov... <- function(method, l.std.args){

  test_that("Stop if unnecessary inputs given in ellipsis", {
    skip_on_cran()
    # stuff that is for nothing
    expect_error(do.call(method, modifyList(l.std.args, list(abc="Gender"))),
                 regexp = "not needed")
    expect_error(do.call(method, modifyList(l.std.args, list(abc=124))),
                 regexp = "not needed")
    expect_error(do.call(method, modifyList(l.std.args, list(dyn.cov =124))),
                 regexp = "not needed")
  })
}




fct.testthat.inputchecks.staticcov <- function(name.method, method, start.params.model, has.cor, data.apparelTrans, data.apparelStaticCov){

  expect_silent(clv.data.apparel.no.holdout   <- clvdata(data.apparelTrans, date.format = "ymd", time.unit = "w"))
  expect_silent(clv.data.apparel.with.holdout <- clvdata(data.apparelTrans, date.format = "ymd", time.unit = "w"))

  expect_silent(clv.data.apparel.no.holdout <- SetStaticCovariates(clv.data = clv.data.apparel.no.holdout,
                                                                   data.cov.life = data.apparelStaticCov, names.cov.life = "Gender",
                                                                   data.cov.trans = data.apparelStaticCov, names.cov.trans = "Gender"))
  expect_silent(clv.data.apparel.with.holdout <- SetStaticCovariates(clv.data = clv.data.apparel.with.holdout,
                                                                     data.cov.life = data.apparelStaticCov, names.cov.life = "Gender",
                                                                     data.cov.trans = data.apparelStaticCov, names.cov.trans = "Gender"))
  l.std.args.noholdout   <- list(clv.data=clv.data.apparel.no.holdout)
  l.std.args.withholdout <- list(clv.data=clv.data.apparel.with.holdout)


  context(paste0("Inputchecks - ", name.method," staticcov - Parameter start.params.model"))
  fct.testthat.inputchecks.startparamsmodel(method = method,
                                            l.std.args = l.std.args.noholdout,
                                            correct.params = start.params.model,
                                            names.params = names(start.params.model))
  fct.testthat.inputchecks.startparamsmodel(method = method,
                                            l.std.args = l.std.args.withholdout,
                                            correct.params = start.params.model,
                                            names.params = names(start.params.model))

  context(paste0("Inputchecks - ", name.method," staticcov - Parameter optimx.args"))
  fct.testthat.inputchecks.optimxargs(method, l.std.args.noholdout)
  fct.testthat.inputchecks.optimxargs(method, l.std.args.withholdout)

  context(paste0("Inputchecks - ",name.method," staticcov - Parameter ..."))
  fct.testthat.inputchecks.staticcov...(method = method, l.std.args = l.std.args.noholdout)
  fct.testthat.inputchecks.staticcov...(method = method, l.std.args = l.std.args.withholdout)

  if(has.cor){
    context(paste0("Inputchecks - ",name.method," staticcov - Parameter use.cor"))
    fct.testthat.inputchecks.usecor(method = method, l.std.args = l.std.args.noholdout)
    fct.testthat.inputchecks.usecor(method = method, l.std.args = l.std.args.withholdout)

    context(paste0("Inputchecks - ",name.method," staticcov - Parameter start.param.cor"))
    fct.testthat.inputchecks.startparamcor(method = method, l.std.args = l.std.args.noholdout,   correct.param = 0.5)
    fct.testthat.inputchecks.startparamcor(method = method, l.std.args = l.std.args.withholdout, correct.param = 0.5)
  }else{
    test_that("Fails for use.cor", {
      expect_error(do.call(what = method,
                           args = list(clv.data=clv.data.apparel.with.holdout, use.cor = TRUE)),
                   regexp = "ignored because they are not needed")
    })
  }



  # Covariate specific inputs ---------------------------------------------------------------

  context(paste0("Inputchecks - ",name.method," staticcov - Parameter start.param.life"))
  fct.testthat.inputchecks.startparamcov(method = method, l.std.args = l.std.args.noholdout, name.param = "start.params.life")
  fct.testthat.inputchecks.startparamcov(method = method, l.std.args = l.std.args.withholdout, name.param = "start.params.life")

  context(paste0("Inputchecks - ",name.method," staticcov - Parameter start.param.trans"))
  fct.testthat.inputchecks.startparamcov(method = method, l.std.args = l.std.args.noholdout, name.param = "start.params.trans")
  fct.testthat.inputchecks.startparamcov(method = method, l.std.args = l.std.args.withholdout, name.param = "start.params.trans")

  context(paste0("Inputchecks - ",name.method," staticcov - Parameter names.cov.constr"))
  fct.testthat.inputchecks.namescovconstr(method = method, l.std.args = l.std.args.noholdout)
  fct.testthat.inputchecks.namescovconstr(method = method, l.std.args = l.std.args.withholdout)

  context(paste0("Inputchecks - ",name.method," staticcov - Parameter start.params.constr"))
  fct.testthat.inputchecks.startparamconstr(method = method, l.std.args = l.std.args.noholdout)
  fct.testthat.inputchecks.startparamconstr(method = method, l.std.args = l.std.args.withholdout)

  context(paste0("Inputchecks - ",name.method," staticcov - Parameter reg.lambdas"))
  fct.testthat.inputchecks.reglambdas(method = method, l.std.args = l.std.args.noholdout)
  fct.testthat.inputchecks.reglambdas(method = method, l.std.args = l.std.args.withholdout)

}
