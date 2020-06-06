.fct.helper.inputchecks.startparamcov <- function(fct.model, l.std.args, name.param){
  test_that("Fails if not numeric", {
    expect_error(do.call(fct.model, modifyList(l.std.args, setNames(list(c(Gender="0.5")), name.param))),
                 regexp = "numeric vector")
    expect_error(do.call(fct.model, modifyList(l.std.args, setNames(list(data.frame(Gender=0.5)), name.param))),
                 regexp = "numeric vector")
    expect_error(do.call(fct.model, modifyList(l.std.args, setNames(list(list(Gender=0.5)), name.param))),
                 regexp = "numeric vector")
  })
  test_that("Fails if any NA", {
    expect_error(do.call(fct.model, modifyList(l.std.args, setNames(list(c(Gender=NA_real_, Gender=0.5)), name.param))),
                 regexp = "no NA")
  })
  test_that("Fails if any not a covariate", {
    expect_error(do.call(fct.model, modifyList(l.std.args, setNames(list(c(gender=0.5)), name.param))),
                 regexp = "Please provide")
  })

  # test_that("Fails if a covariate missing", {
  #   # ** TODO: more than one covariate needed
  # })

  test_that("Fails if not named", {
    expect_error(do.call(fct.model, modifyList(l.std.args, setNames(list(c(0.5)), name.param))),
                 regexp = "Please provide")
  })
  test_that("Fails if one entry not named", {
    expect_error(do.call(fct.model, modifyList(l.std.args, setNames(list(c(Gender=0.5, 0.5)), name.param))),
                 regexp = "Please provide")
  })
  test_that("Fails if duplicates", {
    expect_error(do.call(fct.model, modifyList(l.std.args, setNames(list(c(gender=0.5, Gender=0.5)), name.param))),
                 regexp = "Please provide exactly")
    expect_error(do.call(fct.model, modifyList(l.std.args, setNames(list(c(Gender=0.5, Gender=0.5)), name.param))),
                 regexp = "Please provide")
    expect_error(do.call(fct.model, modifyList(l.std.args, setNames(list(c(Gender=0.5, Gender=NA_real_)), name.param))),
                 regexp = "Please provide")
  })
}

.fct.helper.inputchecks.namescovconstr <- function(fct.model, l.std.args){

  test_that("Fails if not a character vector", {
    expect_error(do.call(fct.model, modifyList(l.std.args, list(names.cov.constr=1))),
                 regexp = "character vector")
    expect_error(do.call(fct.model, modifyList(l.std.args, list(names.cov.constr=list("Gender")))),
                 regexp = "character vector")
    expect_error(do.call(fct.model, modifyList(l.std.args, list(names.cov.constr=data.frame("Gender")))),
                 regexp = "character vector")
  })

  test_that("Fails if any NA", {
    expect_error(do.call(fct.model, modifyList(l.std.args, list(names.cov.constr=c(NA_character_)))),
                 regexp = "no NA")
    expect_error(do.call(fct.model, modifyList(l.std.args, list(names.cov.constr=c("Gender", NA_character_)))),
                 regexp = "no NA")
  })

  test_that("Fails constraint names not in covariates", {
    expect_error(do.call(fct.model, modifyList(l.std.args, list(names.cov.constr=c("Gender1")))),
                 regexp = "could not be found")
    expect_error(do.call(fct.model, modifyList(l.std.args, list(names.cov.constr=c("Gender", "Gender1")))),
                 regexp = "could not be found")
  })

  test_that("Fails if duplicates", {
    expect_error(do.call(fct.model, modifyList(l.std.args, list(names.cov.constr=c("Gender", "Gender")))),
                 regexp = "exactly once")
  })

  test_that("Fails if want to give start params for constraint params as free covariates", {
    expect_error(do.call(fct.model, modifyList(l.std.args, list(names.cov.constr=c("Gender"),
                                                                start.params.life = c(Gender=1)))),
                 regexp = "for the constraint parameters in")
    expect_error(do.call(fct.model, modifyList(l.std.args, list(names.cov.constr=c("Gender"),
                                                                start.params.trans = c(Gender=1)))),
                 regexp = "for the constraint parameters in")

    expect_error(do.call(fct.model, modifyList(l.std.args, list(names.cov.constr=c("Gender"),
                                                                start.params.trans = c(Gender=1),
                                                                start.params.life  = c(Gender=1)))),
                 regexp = "for the constraint parameters in")
  })
}

.fct.helper.inputchecks.startparamconstr <- function(fct.model, l.std.args){
  l.std.args <- modifyList(l.std.args, list(names.cov.constr = "Gender"))

  test_that("Fails if any NA", {
    expect_error(do.call(fct.model, modifyList(l.std.args, list(start.params.constr=c(Gender=NA_real_)))),
                 regexp = "no NA")
  })

  test_that("Fails if not a numeric vector", {
    expect_error(do.call(fct.model, modifyList(l.std.args, list(start.params.constr=c(Gender="0.5")))),
                 regexp = "numeric vector")
    expect_error(do.call(fct.model, modifyList(l.std.args, list(start.params.constr=list(Gender=0.5)))),
                 regexp = "numeric vector")
    expect_error(do.call(fct.model, modifyList(l.std.args, list(start.params.constr=data.frame(Gender=0.5)))),
                 regexp = "numeric vector")
  })

  test_that("Fails if unnnamed", {
    expect_error(do.call(fct.model, modifyList(l.std.args, list(start.params.constr=c(0.5)))),
                 regexp = "named vector")
    expect_error(do.call(fct.model, modifyList(l.std.args, list(start.params.constr=c(Gender=0.5, 0.2)))),
                 regexp = "names for every parameter")
  })

  test_that("Fails for params not constraint ", { #constraint names not in covariates", {
    expect_error(do.call(fct.model, modifyList(l.std.args, list(names.cov.constr = NULL,
                                                                start.params.constr=c(Gender=0.5)))),
                 regexp = "should only be provided")
  })
}

.fct.helper.inputchecks.reglambdas <- function(fct.model, l.std.args){

  test_that("Fails if is not a numeric vector", {
    expect_error(do.call(fct.model, modifyList(l.std.args, list(reg.lambdas=list(list(trans=10, life=10))))),
                 regexp = "numeric vector")
    expect_error(do.call(fct.model, modifyList(l.std.args, list(reg.lambdas=data.frame(trans=10, life=10)))),
                 regexp = "numeric vector")
    expect_error(do.call(fct.model, modifyList(l.std.args, list(reg.lambdas=c(trans="10", life="10")))),
                 regexp = "numeric vector")
    expect_error(do.call(fct.model, modifyList(l.std.args, list(reg.lambdas=factor(c(trans="10", life="10"))))),
                 regexp = "numeric vector")
  })

  test_that("Fails if any is NA", {
    expect_error(do.call(fct.model, modifyList(l.std.args, list(reg.lambdas=c(trans=10, life=NA_real_)))),
                 regexp = "NA")
    expect_error(do.call(fct.model, modifyList(l.std.args, list(reg.lambdas=c(life=10, trans=NA_real_)))),
                 regexp = "NA")
    expect_error(do.call(fct.model, modifyList(l.std.args, list(reg.lambdas=c(life=10, trans=10, NA_real_)))),
                 regexp = "NA")
    expect_error(do.call(fct.model, modifyList(l.std.args, list(reg.lambdas=NA_real_))),
                 regexp = "NA")
  })

  test_that("Fails for negative regularization lambdas", {
    expect_error(do.call(fct.model, modifyList(l.std.args, list(reg.lambdas=c(trans=10,  life=-10)))),
                 regexp = "positive")
    expect_error(do.call(fct.model, modifyList(l.std.args, list(reg.lambdas=c(trans=-10, life=10)))),
                 regexp = "positive")
    expect_error(do.call(fct.model, modifyList(l.std.args, list(reg.lambdas=c(trans=-10, life=-10)))),
                 regexp = "positive")
  })

  test_that("Fails for wrongly named regularization lambdas", {
    expect_error(do.call(fct.model, modifyList(l.std.args, list(reg.lambdas=c(trans=10,  live=10)))),
                 regexp = "be named")
    expect_error(do.call(fct.model, modifyList(l.std.args, list(reg.lambdas=c(trans=10,  Life=10)))),
                 regexp = "be named")
    expect_error(do.call(fct.model, modifyList(l.std.args, list(reg.lambdas=c(frans=10,  life=10)))),
                 regexp = "be named")
    expect_error(do.call(fct.model, modifyList(l.std.args, list(reg.lambdas=c(Trans=10,  life=10)))),
                 regexp = "be named")
    expect_error(do.call(fct.model, modifyList(l.std.args, list(reg.lambdas=c(Trans=10,  Life=10)))),
                 regexp = "be named")
    expect_error(do.call(fct.model, modifyList(l.std.args, list(reg.lambdas=c(trans=10,  lifetime=10)))),
                 regexp = "be named")
  })

  test_that("Fails if only one given", {
    expect_error(do.call(fct.model, modifyList(l.std.args, list(reg.lambdas=c(trans=10)))),
                 regexp = "need to be given")
    expect_error(do.call(fct.model, modifyList(l.std.args, list(reg.lambdas=c(life=10)))),
                 regexp = "need to be given")
  })

  test_that("Fails if too many given", {
    expect_error(do.call(fct.model, modifyList(l.std.args, list(reg.lambdas=c(trans=10, life=10, abc=10)))),
                 regexp = "need to be given")
    expect_error(do.call(fct.model, modifyList(l.std.args, list(reg.lambdas=c(life=10, life=10, tra=12)))),
                 regexp = "need to be given")
  })

  test_that("Fails if duplicates given", {
    expect_error(do.call(fct.model, modifyList(l.std.args, list(reg.lambdas=c(life=10, life=10)))),
                 regexp = "need to be named")
    expect_error(do.call(fct.model, modifyList(l.std.args, list(reg.lambdas=c(trans=10, trans=10)))),
                 regexp = "need to be named")

    expect_error(do.call(fct.model, modifyList(l.std.args, list(reg.lambdas=c(life=10, life=10, trans=10)))),
                 regexp = "need to be given")
    expect_error(do.call(fct.model, modifyList(l.std.args, list(reg.lambdas=c(life=10, trans=10, trans=10)))),
                 regexp = "need to be given")
  })
}


.fct.helper.inputchecks.staticcov... <- function(fct.model, l.std.args){

  test_that("Stop if unnecessary inputs given in ellipsis", {
    # stuff that is for nothing
    expect_error(do.call(fct.model, modifyList(l.std.args, list(abc="Gender"))),
                   regexp = "not needed")
    expect_error(do.call(fct.model, modifyList(l.std.args, list(abc=124))),
                   regexp = "not needed")
    expect_error(do.call(fct.model, modifyList(l.std.args, list(dyn.cov =124))),
                   regexp = "not needed")
  })
}


fct.helper.inputchecks.check.all.static.cov.model <- function(fct.model, l.std.args, name.model, correct.params, param.names){

  context(paste0("Inputchecks - ", name.model," - Parameter start.params.model"))
  .fct.helper.inputchecks.startparamsmodel(fct.model = fct.model, l.std.args = l.std.args,
                                           correct.params = correct.params,
                                           names.params = param.names)

  context(paste0("Inputchecks - ", name.model," - Parameter use.cor"))
  .fct.helper.inputchecks.usecor(fct.model = fct.model, l.std.args = l.std.args,
                                 correct.param = TRUE)

  context(paste0("Inputchecks - ", name.model," - Parameter start.param.cor"))
  .fct.helper.inputchecks.startparamcor(fct.model, l.std.args, correct.param = 0.5)

  context(paste0("Inputchecks - ", name.model," - Parameter optimx.args"))
  .fct.helper.inputchecks.optimxargs(fct.model, l.std.args)

  context(paste0("Inputchecks - ",name.model," - Parameter start.param.life"))
  .fct.helper.inputchecks.startparamcov(fct.model = fct.model, l.std.args = l.std.args, name.param = "start.params.life")

  context(paste0("Inputchecks - ",name.model," - Parameter start.param.trans"))
  .fct.helper.inputchecks.startparamcov(fct.model = fct.model, l.std.args = l.std.args, name.param = "start.params.trans")

  context(paste0("Inputchecks - ",name.model," - Parameter names.cov.constr"))
  .fct.helper.inputchecks.namescovconstr(fct.model = fct.model, l.std.args = l.std.args)

  context(paste0("Inputchecks - ",name.model," - Parameter start.params.constr"))
  .fct.helper.inputchecks.startparamconstr(fct.model = fct.model, l.std.args = l.std.args)

  context(paste0("Inputchecks - ",name.model," - Parameter reg.lambdas"))
  .fct.helper.inputchecks.reglambdas(fct.model = fct.model, l.std.args = l.std.args)

  context(paste0("Inputchecks - ",name.model," - Parameter ..."))
  .fct.helper.inputchecks.staticcov...(fct.model = fct.model, l.std.args = l.std.args)
}
