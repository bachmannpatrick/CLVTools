#use nocov
fct.testthat.runability.staticcov.custom.model.covariate.start.params <- function(method, clv.data.holdout, clv.data.no.holdout, start.params.model){
  test_that("Works with custom model and covariate start parameters", {
    skip_on_cran()
    l.args.holdout <- list(clv.data = clv.data.holdout,    start.params.model = start.params.model,
                           start.params.life = c(Gender = 1, Channel=0.4), start.params.trans = c(Gender=1, Channel=2), verbose = FALSE)
    l.args.no.holdout <- list(clv.data = clv.data.no.holdout, start.params.model = start.params.model,
                              start.params.life = c(Channel=0.4, Gender = 1), start.params.trans = c(Channel=2, Gender=1), verbose = FALSE)

    expect_silent(do.call(what = method, args = l.args.holdout))
    expect_silent(do.call(what = method, args = l.args.no.holdout))
  })
}

#use nocov
fct.testthat.runability.staticcov.all.optimization.methods <- function(method, clv.data.holdout, expected.message){
  test_that("Works for all optimx optimization methods", {
    skip_on_cran()
    skip_on_covr()
    skip_on_ci()
    l.args <- list(clv.data=clv.data.holdout, optimx.args = list(control=list(all.methods=TRUE)), verbose=FALSE)
    expect_warning(do.call(what = method, args = l.args),
                   regexp = expected.message, all=TRUE)
  })
}

# use nocov
fct.testthat.runability.staticcov.multiple.optimization.methods <- function(method, clv.data.holdout, clv.newdata.nohold, clv.newdata.withhold, param.names){
  test_that("Works fully with multiple optimization methods", {
    skip_on_cran()
    l.args <- list(clv.data=clv.data.holdout, optimx.args = list(method = c("BFGS", "L-BFGS-B", "Nelder-Mead")), verbose = FALSE)

    expect_silent(p.hold <- do.call(what = method, args = l.args))
    fct.helper.fitted.all.s3(clv.fitted = p.hold,  full.names = c(param.names, p.hold@names.prefixed.params.free.life, p.hold@names.prefixed.params.free.trans),
                             clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold)
  })
}

fct.testthat.runability.staticcov.reduce.relevant.covariates.estimation <- function(method, clv.data.holdout){
  test_that("Reduces to relevant covariates only for estimation", {
    skip_on_cran()
    l.args.trans <- list(clv.data = clv.data.holdout, names.cov.trans = "Gender",verbose=FALSE)
    l.args.life <- list(clv.data = clv.data.holdout, names.cov.life = "Gender",verbose=FALSE)

    # Transaction: Fit with Gender covariate only
    expect_silent(e.model.1.less <-do.call(what = method, args = l.args.trans)) # only keep Gender
    expect_false("Channel" %in% names(coef(e.model.1.less)))
    expect_true("Channel" %in% colnames(e.model.1.less@clv.data@data.cov.life))
    expect_false("Channel" %in% colnames(e.model.1.less@clv.data@data.cov.trans))

    # Lifetime: Same
    expect_silent(e.model.1.less <-do.call(what = method, args = l.args.life)) # only keep Gender
    expect_false("Channel" %in% names(coef(e.model.1.less)))
    expect_false("Channel" %in% colnames(e.model.1.less@clv.data@data.cov.life))
    expect_true("Channel" %in% colnames(e.model.1.less@clv.data@data.cov.trans))
  })
}




fct.testthat.runability.staticcov.works.with.2.constraints <- function(method, clv.data.holdout, clv.data.no.holdout, clv.newdata.nohold, clv.newdata.withhold, param.names, DERT.not.implemented = FALSE){
  test_that("Works with 2 constraints", {
    skip_on_cran()

    l.args.holdout <- list(clv.data = clv.data.holdout, names.cov.constr = c("Gender", "Channel"),verbose=FALSE)
    l.args.no.holdout <- list(clv.data = clv.data.no.holdout,   names.cov.constr = c("Gender", "Channel"),verbose=FALSE)

    expect_silent(p.hold <- do.call(what = method, args = l.args.holdout))
    expect_silent(p.hold <- do.call(what = method, args = l.args.no.holdout))

    fct.helper.fitted.all.s3(p.hold,    full.names = c(param.names, "constr.Gender", "constr.Channel"),
                           clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold, DERT.not.implemented = DERT.not.implemented)
    fct.helper.fitted.all.s3(p.hold,    full.names = c(param.names, "constr.Gender", "constr.Channel"),
                           clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold, DERT.not.implemented = DERT.not.implemented)
  })
}


fct.testthat.runability.staticcov.works.with.1.constraint.1.free <- function(method, clv.data.holdout, clv.data.no.holdout, clv.newdata.nohold, clv.newdata.withhold, param.names, DERT.not.implemented = FALSE){
  test_that("Works with 1 constraint, 1 free", {
    skip_on_cran()
    l.args.holdout.1 <- list(clv.data = clv.data.holdout, names.cov.constr = "Gender",verbose=FALSE)
    l.args.no.holdout.1 <- list(clv.data = clv.data.no.holdout, names.cov.constr = "Gender",verbose=FALSE)

    # Without start param
    expect_silent(p.hold    <- do.call(what = method, args = l.args.holdout.1))
    expect_silent(p.no.hold <- do.call(what = method, args = l.args.no.holdout.1))
    fct.helper.fitted.all.s3(p.hold,    full.names = c(param.names, "life.Channel", "trans.Channel", "constr.Gender"),
                             clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold, DERT.not.implemented = DERT.not.implemented)
    fct.helper.fitted.all.s3(p.no.hold, full.names = c(param.names, "life.Channel", "trans.Channel", "constr.Gender"),
                             clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold, DERT.not.implemented = DERT.not.implemented)

    l.args.holdout.2 <- list(clv.data = clv.data.holdout,    names.cov.constr = "Gender", start.params.constr = c(Gender=1),verbose=FALSE)
    l.args.no.holdout.2 <- list(clv.data = clv.data.no.holdout, names.cov.constr = "Gender", start.params.constr = c(Gender=1),verbose=FALSE)
    # With start param
    expect_silent(do.call(what = method, args = l.args.holdout.2))
    expect_silent(do.call(what = method, args = l.args.no.holdout.2))
  })
}

fct.testthat.runability.staticcov.works.with.regularization <- function(method, clv.data.holdout, clv.data.no.holdout, clv.newdata.nohold, clv.newdata.withhold, param.names, DERT.not.implemented = FALSE){
   test_that("Works with regularization", {
     skip_on_cran()
     l.args.holdout <- list(clv.data = clv.data.holdout, reg.lambdas = c(trans=10, life=10),verbose=FALSE)
     l.args.no.holdout <- list(clv.data = clv.data.no.holdout, reg.lambdas = c(trans=10, life=10),verbose=FALSE)

     expect_silent(p.hold    <- do.call(what = method, args = l.args.holdout))
     expect_silent(p.no.hold <- do.call(what = method, args = l.args.no.holdout))

     fct.helper.fitted.all.s3(p.hold,    full.names = c(param.names, p.hold@names.prefixed.params.free.life, p.hold@names.prefixed.params.free.trans),
                              clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold, DERT.not.implemented = DERT.not.implemented)
     fct.helper.fitted.all.s3(p.no.hold, full.names = c(param.names, p.no.hold@names.prefixed.params.free.life, p.no.hold@names.prefixed.params.free.trans),
                              clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold, DERT.not.implemented = DERT.not.implemented)
   })
 }

fct.testthat.runability.staticcov.works.with.0.lambdas <- function(method, clv.data.holdout, clv.data.no.holdout, clv.newdata.nohold, clv.newdata.withhold, param.names, DERT.not.implemented = FALSE){
  test_that("Works with 0 regularization lambdas", {
    skip_on_cran()
    skip_on_covr()
    skip_on_ci()

    l.args.holdout <- list(clv.data = clv.data.holdout,   reg.lambdas = c(trans=0, life=0),verbose=FALSE)
    l.args.no.holdout <- list(clv.data = clv.data.no.holdout,reg.lambdas = c(trans=0, life=0),verbose=FALSE)

    expect_silent(p.hold    <- do.call(what = method, args = l.args.holdout))
    expect_silent(p.no.hold <- do.call(what = method, args = l.args.no.holdout))
    fct.helper.fitted.all.s3(p.hold,    full.names = c(param.names, p.hold@names.prefixed.params.free.life, p.hold@names.prefixed.params.free.trans),
                             clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold, DERT.not.implemented = DERT.not.implemented)
    fct.helper.fitted.all.s3(p.no.hold, full.names = c(param.names, p.no.hold@names.prefixed.params.free.life, p.no.hold@names.prefixed.params.free.trans),
                             clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold, DERT.not.implemented = DERT.not.implemented)
  })
}

fct.testthat.runability.staticcov.works.with.combined.interlayers.without.cor <- function(method, clv.data.holdout, clv.data.no.holdout, clv.newdata.nohold, clv.newdata.withhold, param.names, DERT.not.implemented = FALSE){
  test_that("Works with combined interlayers (without correlation)", {
    # Try all combinations of interlayers
    skip_on_cran()

    l.args.holdout <- list(clv.data = clv.data.holdout, names.cov.constr = c("Gender", "Channel"), reg.lambdas = c(trans=10, life=10),verbose=FALSE)
    l.args.no.holdout <- list(clv.data = clv.data.no.holdout, names.cov.constr = c("Gender", "Channel"), reg.lambdas = c(trans=10, life=10),verbose=FALSE)

    # Regularization + Constraints
    expect_silent(p.hold    <- do.call(what = method, args = l.args.holdout))
    expect_silent(p.no.hold <- do.call(what = method, args = l.args.no.holdout))

    fct.helper.fitted.all.s3(p.hold,    full.names = c(param.names, p.hold@names.prefixed.params.constr),
                             clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold, DERT.not.implemented = DERT.not.implemented)
    fct.helper.fitted.all.s3(p.no.hold, full.names = c(param.names, p.no.hold@names.prefixed.params.constr),
                             clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold, DERT.not.implemented = DERT.not.implemented)
  })
}

fct.testthat.runability.staticcov.works.with.combined.interlayers.with.cor <- function(method, clv.data.holdout, clv.data.no.holdout, clv.newdata.nohold, clv.newdata.withhold, param.names){
  test_that("Works with combined interlayers (with correlation)", {
    # Try all combinations of interlayers
    skip_on_cran()
    skip_on_covr()
    skip_on_ci()

    l.args.holdout.1 = list(clv.data = clv.data.holdout, use.cor = TRUE, names.cov.constr = c("Gender", "Channel"),verbose=FALSE)
    l.args.no.holdout.1 = list(clv.data = clv.data.no.holdout, use.cor = TRUE, names.cov.constr = c("Gender", "Channel"), verbose=FALSE)

    # Constraints + Correlation
    expect_silent(p.hold <- do.call(what = method, args = l.args.holdout.1))
    expect_silent(p.no.hold <- do.call(what = method, args = l.args.no.holdout.1))

    fct.helper.fitted.all.s3(p.hold,    full.names = c(param.names,  p.hold@name.correlation.cor, p.hold@names.prefixed.params.constr),
                             clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold)
    fct.helper.fitted.all.s3(p.no.hold, full.names = c(param.names, p.no.hold@name.correlation.cor, p.no.hold@names.prefixed.params.constr),
                             clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold)

    l.args.holdout.2 <- list(clv.data = clv.data.holdout, use.cor = TRUE, reg.lambdas = c(trans=10, life=10),verbose=FALSE)
    l.args.no.holdout.2 <- list(clv.data = clv.data.no.holdout, use.cor = TRUE, reg.lambdas = c(trans=10, life=10),verbose=FALSE)

    # Regularization + Correlation
    expect_silent(p.hold    <- do.call(what = method, args = l.args.holdout.2))
    expect_silent(p.no.hold <- do.call(what = method, args = l.args.no.holdout.2))

    fct.helper.fitted.all.s3(p.hold,    full.names = c(param.names,  p.hold@name.correlation.cor, p.hold@names.prefixed.params.free.life, p.hold@names.prefixed.params.free.trans),
                             clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold)
    fct.helper.fitted.all.s3(p.no.hold, full.names = c(param.names, p.no.hold@name.correlation.cor, p.no.hold@names.prefixed.params.free.life, p.no.hold@names.prefixed.params.free.trans),
                             clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold)

    l.args.holdout.3 <- list(clv.data = clv.data.holdout, use.cor = TRUE, names.cov.constr = c("Gender", "Channel"),reg.lambdas = c(trans=10, life=10),verbose=FALSE)
    l.args.no.holdout.3 <- list(clv.data = clv.data.no.holdout, use.cor = TRUE, names.cov.constr = c("Gender", "Channel"),reg.lambdas = c(trans=10, life=10),verbose=FALSE)

    # Regularization + Correlation + Constraints
    expect_silent(p.hold    <- do.call(what = method, args = l.args.holdout.3))
    expect_silent(p.no.hold <- do.call(what = method, args = l.args.no.holdout.3))

    fct.helper.fitted.all.s3(p.hold,    full.names = c(param.names, p.hold@name.correlation.cor, p.hold@names.prefixed.params.constr),
                             clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold)
    fct.helper.fitted.all.s3(p.no.hold, full.names = c(param.names, p.no.hold@name.correlation.cor, p.no.hold@names.prefixed.params.constr),
                             clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold)
  })
}