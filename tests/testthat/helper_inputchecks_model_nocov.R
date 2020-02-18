# start.params.model ------------------------------------------------------------------------------------
.fct.helper.inputchecks.startparamsmodel <- function(fct.model, l.std.args, correct.params, names.params){

  test_that("Fails if not vector", {
    expect_error(do.call(fct.model, modifyList(l.std.args,
                                               list(start.params.model = as.list(correct.params)))),
                 regexp = "numeric vector")
    expect_error(do.call(fct.model, modifyList(l.std.args,
                                               list(start.params.model = as.data.frame(correct.params)))),
                 regexp = "numeric vector")
  })

  test_that("Fails if not numeric", {
    expect_error(do.call(fct.model, modifyList(l.std.args,list(start.params.model =
                                                                 setNames(as.character(correct.params), names.params)))),
                 regexp = "numeric vector")

    expect_error(do.call(fct.model, modifyList(l.std.args,list(start.params.model =
                                                                 setNames(as.factor(as.character(correct.params)), names.params)))),
                 regexp = "numeric vector")
  })

  test_that("Fails if not named", {
    expect_error(do.call(fct.model, modifyList(l.std.args,list(start.params.model = unname(correct.params)))),
                 regexp = "Please provide a named")
  })
  test_that("Fails if named wrongly", {
    expect_error(do.call(fct.model, modifyList(l.std.args,list(start.params.model =
                                                                 setNames(correct.params, rep("a", length(names.params)))))),
                 regexp = "Please provide the model")
  })
  test_that("Fails if single name missing", {
    expect_error(do.call(fct.model, modifyList(l.std.args,list(start.params.model =
                                                                 setNames(correct.params, c("",names.params[-1]))))),
                 regexp = "Please provide the model")
  })
  test_that("Fails if duplicate params", {
    expect_error(do.call(fct.model, modifyList(l.std.args,list(start.params.model =
                                                                 setNames(c(1, correct.params), c(names.params[1],names.params))))),
                 regexp = "Please provide exactly")
  })

  test_that("Fails if unneded param", {
    expect_error(do.call(fct.model, modifyList(l.std.args,list(start.params.model =
                                                                 setNames(c(1, correct.params), c("abc",names.params))))),
                 regexp = "Please provide exactly")
  })

  test_that("Fails if any param NA", {
    expect_error(do.call(fct.model, modifyList(l.std.args,list(start.params.model =
                                                                 setNames(c(NA_real_, correct.params[-1]),
                                                                          names.params)))),
                 regexp = "no NAs ")
  })
}

# use.cor ------------------------------------------------------------------------------------
.fct.helper.inputchecks.usecor <- function(fct.model, l.std.args, correct.param){
  test_that("Fails if not logical", {

    expect_error(do.call(fct.model, modifyList(l.std.args,
                                               list(use.cor = NULL), keep.null = TRUE)),
                 regexp = "type logical")
    expect_error(do.call(fct.model, modifyList(l.std.args,
                                               list(use.cor = as.list(correct.param)))),
                 regexp = "type logical")
    expect_error(do.call(fct.model, modifyList(l.std.args,
                                               list(use.cor = as.character(correct.param)))),
                 regexp = "type logical")
    expect_error(do.call(fct.model, modifyList(l.std.args,
                                               list(use.cor = as.factor(correct.param)))),
                 regexp = "type logical")
  })

  test_that("Fails if more than 1", {
    expect_error(do.call(fct.model, modifyList(l.std.args,
                                               list(use.cor = rep(correct.param,2)))),
                 regexp = "single element")
  })

  test_that("Fails if is NA", {
    expect_error(do.call(fct.model, modifyList(l.std.args,
                                               list(use.cor = NA))),
                 regexp = "NA")
  })
}

# start.param.cor ------------------------------------------------------------------------------------
.fct.helper.inputchecks.startparamcor <- function(fct.model, l.std.args, correct.param){

  l.std.args <- modifyList(l.std.args, list(use.cor=TRUE))

  test_that("Fails if not numeric vector", {
    expect_error(do.call(fct.model, modifyList(l.std.args,
                                               list(start.param.cor = as.list(correct.param)))),
                 regexp = "numeric")
    expect_error(do.call(fct.model, modifyList(l.std.args,
                                               list(start.param.cor = as.data.frame(correct.param)))),
                 regexp = "numeric")

    expect_error(do.call(fct.model, modifyList(l.std.args,
                                               list(start.param.cor = as.character(correct.param)))),
                 regexp = "numeric")

    expect_error(do.call(fct.model, modifyList(l.std.args,
                                               list(start.param.cor = as.factor(correct.param)))),
                 regexp = "numeric")

    expect_error(do.call(fct.model, modifyList(l.std.args,
                                               list(start.param.cor = TRUE))),
                 regexp = "numeric")
  })

  test_that("Fails if more than 1", {
    expect_error(do.call(fct.model, modifyList(l.std.args,
                                               list(start.param.cor = rep(correct.param,2)))),
                 regexp = "single")
  })

  test_that("Fails if is NA", {
    expect_error(do.call(fct.model, modifyList(l.std.args,
                                               list(start.param.cor = NA_real_))),
                 regexp = "NA")
  })

  test_that("Fails if is out of [-1, 1]", {
    expect_error(do.call(fct.model, modifyList(l.std.args,
                                               list(start.param.cor = 42))),
                 regexp = "interval")
    expect_error(do.call(fct.model, modifyList(l.std.args,
                                               list(start.param.cor = -42))),
                 regexp = "interval")
  })

  test_that("Warning if given but use.cor = FALSE", {
    expect_error(do.call(fct.model, modifyList(l.std.args,
                                               list(start.param.cor = correct.param,
                                                    use.cor=FALSE))),
                 regexp = "no correlation")
  })
}

# optimx.args ------------------------------------------------------------------------------------------
.fct.helper.inputchecks.optimxargs <- function(fct.model, l.std.args){

  test_that("Fails if not list", {
    expect_error(do.call(fct.model, modifyList(l.std.args, list(optimx.args=data.frame(kkt=TRUE)))),
                 regexp = "list")
    expect_error(do.call(fct.model, modifyList(l.std.args, list(optimx.args=c(kkt=TRUE)))),
                 regexp = "list")
  })
  test_that("Fails if NULL/NA", {
    expect_error(do.call(fct.model, modifyList(l.std.args, list(optimx.args=NULL), keep.null = TRUE)),
                 regexp = "NULL")
    expect_error(do.call(fct.model, modifyList(l.std.args, list(optimx.args=NA))),
                 regexp = "list")
  })

  test_that("Fails if unnamed elements", {
    expect_error(do.call(fct.model, modifyList(l.std.args, list(optimx.args=list(TRUE)))),
                 regexp = "named")
    expect_error(do.call(fct.model, modifyList(l.std.args, list(optimx.args=list("Nelder-Mead")))),
                 regexp = "named")
    expect_error(do.call(fct.model, modifyList(l.std.args, list(optimx.args=list(method="Nelder-Mead", TRUE)))),
                 regexp = "names for every element")
  })
  test_that("Fails if top level names which are not in optimx", {
    expect_error(do.call(fct.model, modifyList(l.std.args, list(optimx.args=list(methods="Nelder-Mead")))),
                 regexp = "valid input to optimx()")
    expect_error(do.call(fct.model, modifyList(l.std.args, list(optimx.args=list(method="Nelder-Mead", kkk=TRUE)))),
                 regexp = "valid input to optimx()")
  })
}

.fct.helper.inputchecks.nocov... <- function(fct.model, l.std.args){

  test_that("Warning if unnecessary inputs given in ellipsis", {
    # stuff that is for covariate models only

    expect_warning(do.call(fct.model, modifyList(l.std.args, list(names.cov.trans="Gender"))),
                   regexp = "ignored")
    expect_warning(do.call(fct.model, modifyList(l.std.args, list(names.cov.life="Gender"))),
                   regexp = "ignored")

    expect_warning(do.call(fct.model, modifyList(l.std.args, list(start.params.life=c(Gender=0.5)))),
                   regexp = "ignored")
    expect_warning(do.call(fct.model, modifyList(l.std.args, list(start.params.trans=c(Gender=0.5)))),
                   regexp = "ignored")

    expect_warning(do.call(fct.model, modifyList(l.std.args, list(names.cov.constr="Gender"))),
                   regexp = "ignored")
    expect_warning(do.call(fct.model, modifyList(l.std.args, list(start.params.constr=c(Gender=0.5)))),
                   regexp = "ignored")

    expect_warning(do.call(fct.model, modifyList(l.std.args, list(reg.lambdas=c(trans=10, life=10)))),
                   regexp = "ignored")
  })

  test_that("Warning if anything else in ellipsis", {
    expect_warning(do.call(fct.model, modifyList(l.std.args, list(abc=10))),
                   regexp = "ignored")
  })
}



# Check all params ----------------------------------------------------
fct.helper.inputchecks.check.all.no.cov.model <- function(fct.model, l.std.args, name.model){

  context(paste0("Checkinputs - ", name.model," - Parameter start.params.model"))
  .fct.helper.inputchecks.startparamsmodel(fct.model = fct.model, l.std.args = l.std.args,
                                          correct.params = c(alpha=1, beta=1, r=1, s=1),
                                          names.params = c("alpha", "beta", "r", "s"))


  context(paste0("Checkinputs - ", name.model," - Parameter use.cor"))
  .fct.helper.inputchecks.usecor(fct.model = fct.model, l.std.args = l.std.args,
                                correct.param = TRUE)

  context(paste0("Checkinputs - ", name.model," - Parameter start.param.cor"))
  .fct.helper.inputchecks.startparamcor(fct.model, l.std.args, correct.param = 0.5)

  context(paste0("Checkinputs - ", name.model," - Parameter optimx.args"))
  .fct.helper.inputchecks.optimxargs(fct.model, l.std.args)

  context(paste0("Checkinputs - ", name.model," - Parameter ..."))
  .fct.helper.inputchecks.nocov...(fct.model = fct.model, l.std.args = l.std.args)
}

