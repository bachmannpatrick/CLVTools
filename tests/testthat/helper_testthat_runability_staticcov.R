#use nocov
fct.testthat.runability.staticcov.custom.model.covariate.start.params <- function(method, clv.data.holdout, clv.data.no.holdout, start.params.model){
  test_that("Works with custom model and covariate start parameters", {
    skip_on_cran()
    skip_on_ci()
    skip_on_covr()
    l.args.holdout <- list(clv.data = clv.data.holdout,    start.params.model = start.params.model,
                           start.params.life = c(Gender = 1, Channel=0.4), start.params.trans = c(Gender=1, Channel=2), verbose = FALSE)
    l.args.no.holdout <- list(clv.data = clv.data.no.holdout, start.params.model = start.params.model,
                              start.params.life = c(Channel=0.4, Gender = 1), start.params.trans = c(Channel=2, Gender=1), verbose = FALSE)

    expect_silent(do.call(what = method, args = l.args.holdout))
    expect_silent(do.call(what = method, args = l.args.no.holdout))
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




fct.testthat.runability.staticcov.works.with.2.constraints <- function(method, clv.data.holdout, clv.data.no.holdout, clv.newdata.nohold, clv.newdata.withhold,
                                                                       param.names.model, DERT.not.implemented){
  test_that("Works with 2 constraints", {
    skip_on_cran()
    skip_on_ci()
    skip_on_covr()

    l.args.holdout <- list(clv.data = clv.data.holdout, names.cov.constr = c("Gender", "Channel"),verbose=FALSE)
    l.args.no.holdout <- list(clv.data = clv.data.no.holdout,   names.cov.constr = c("Gender", "Channel"),verbose=FALSE)

    expect_silent(p.hold <- do.call(what = method, args = l.args.holdout))
    expect_silent(p.hold <- do.call(what = method, args = l.args.no.holdout))

    fct.helper.fitted.all.s3(p.hold,    full.names = c(param.names.model, "constr.Gender", "constr.Channel"),
                             clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold, DERT.not.implemented = DERT.not.implemented)
    fct.helper.fitted.all.s3(p.hold,    full.names = c(param.names.model, "constr.Gender", "constr.Channel"),
                             clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold, DERT.not.implemented = DERT.not.implemented)
  })
}


fct.testthat.runability.staticcov.works.with.1.constraint.1.free <- function(method, clv.data.holdout, clv.data.no.holdout, clv.newdata.nohold, clv.newdata.withhold,
                                                                             param.names.model, DERT.not.implemented){
  test_that("Works with 1 constraint, 1 free", {
    skip_on_cran()
    l.args.holdout.1 <- list(clv.data = clv.data.holdout, names.cov.constr = "Gender",verbose=FALSE)
    l.args.no.holdout.1 <- list(clv.data = clv.data.no.holdout, names.cov.constr = "Gender",verbose=FALSE)

    # Without start param
    expect_silent(p.hold    <- do.call(what = method, args = l.args.holdout.1))
    expect_silent(p.no.hold <- do.call(what = method, args = l.args.no.holdout.1))
    fct.helper.fitted.all.s3(p.hold,    full.names = c(param.names.model, "life.Channel", "trans.Channel", "constr.Gender"),
                             clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold, DERT.not.implemented = DERT.not.implemented)
    fct.helper.fitted.all.s3(p.no.hold, full.names = c(param.names.model, "life.Channel", "trans.Channel", "constr.Gender"),
                             clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold, DERT.not.implemented = DERT.not.implemented)

    l.args.holdout.2 <- list(clv.data = clv.data.holdout,    names.cov.constr = "Gender", start.params.constr = c(Gender=1),verbose=FALSE)
    l.args.no.holdout.2 <- list(clv.data = clv.data.no.holdout, names.cov.constr = "Gender", start.params.constr = c(Gender=1),verbose=FALSE)
    # With start param
    expect_silent(do.call(what = method, args = l.args.holdout.2))
    expect_silent(do.call(what = method, args = l.args.no.holdout.2))
  })
}

fct.testthat.runability.staticcov.works.with.regularization <- function(method, clv.data.holdout, clv.data.no.holdout, clv.newdata.nohold, clv.newdata.withhold,
                                                                        param.names.model, DERT.not.implemented){
  test_that("Works with regularization", {
    skip_on_cran()
    l.args.holdout <- list(clv.data = clv.data.holdout, reg.lambdas = c(trans=10, life=10),verbose=FALSE)
    l.args.no.holdout <- list(clv.data = clv.data.no.holdout, reg.lambdas = c(trans=10, life=10),verbose=FALSE)

    expect_silent(p.hold    <- do.call(what = method, args = l.args.holdout))
    expect_silent(p.no.hold <- do.call(what = method, args = l.args.no.holdout))

    fct.helper.fitted.all.s3(p.hold,    full.names = c(param.names.model, p.hold@names.prefixed.params.free.life, p.hold@names.prefixed.params.free.trans),
                             clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold, DERT.not.implemented = DERT.not.implemented)
    fct.helper.fitted.all.s3(p.no.hold, full.names = c(param.names.model, p.no.hold@names.prefixed.params.free.life, p.no.hold@names.prefixed.params.free.trans),
                             clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold, DERT.not.implemented = DERT.not.implemented)
  })
}

fct.testthat.runability.staticcov.works.with.0.lambdas <- function(method, clv.data.holdout, clv.data.no.holdout, clv.newdata.nohold, clv.newdata.withhold,
                                                                   param.names.model, DERT.not.implemented){
  test_that("Works with 0 regularization lambdas", {
    skip_on_cran()
    skip_on_ci()
    skip_on_covr()

    l.args.holdout <- list(clv.data = clv.data.holdout,   reg.lambdas = c(trans=0, life=0),verbose=FALSE)
    l.args.no.holdout <- list(clv.data = clv.data.no.holdout,reg.lambdas = c(trans=0, life=0),verbose=FALSE)

    expect_silent(p.hold    <- do.call(what = method, args = l.args.holdout))
    expect_silent(p.no.hold <- do.call(what = method, args = l.args.no.holdout))
    fct.helper.fitted.all.s3(p.hold,    full.names = c(param.names.model, p.hold@names.prefixed.params.free.life, p.hold@names.prefixed.params.free.trans),
                             clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold, DERT.not.implemented = DERT.not.implemented)
    fct.helper.fitted.all.s3(p.no.hold, full.names = c(param.names.model, p.no.hold@names.prefixed.params.free.life, p.no.hold@names.prefixed.params.free.trans),
                             clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold, DERT.not.implemented = DERT.not.implemented)
  })
}

fct.testthat.runability.staticcov.works.with.combined.interlayers.without.cor <- function(method, clv.data.holdout, clv.data.no.holdout, clv.newdata.nohold, clv.newdata.withhold,
                                                                                          model.param.names, DERT.not.implemented){
  test_that("Works with combined interlayers (without correlation)", {
    # Try all combinations of interlayers
    skip_on_cran()
    skip_on_ci()
    skip_on_covr()

    l.args.holdout <- list(clv.data = clv.data.holdout, names.cov.constr = c("Gender", "Channel"), reg.lambdas = c(trans=10, life=10),verbose=FALSE)
    l.args.no.holdout <- list(clv.data = clv.data.no.holdout, names.cov.constr = c("Gender", "Channel"), reg.lambdas = c(trans=10, life=10),verbose=FALSE)

    # Regularization + Constraints
    expect_silent(p.hold    <- do.call(what = method, args = l.args.holdout))
    expect_silent(p.no.hold <- do.call(what = method, args = l.args.no.holdout))

    fct.helper.fitted.all.s3(p.hold,    full.names = c(model.param.names, p.hold@names.prefixed.params.constr),
                             clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold,
                             DERT.not.implemented = DERT.not.implemented)
    fct.helper.fitted.all.s3(p.no.hold, full.names = c(model.param.names, p.no.hold@names.prefixed.params.constr),
                             clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold,
                             DERT.not.implemented = DERT.not.implemented)
  })
}

fct.testthat.runability.staticcov.works.with.combined.interlayers.with.cor <- function(method, clv.data.holdout, clv.data.no.holdout, clv.newdata.nohold, clv.newdata.withhold,
                                                                                       DERT.not.implemented, model.param.names){
  test_that("Works with combined interlayers (with correlation)", {
    # Try all combinations of interlayers
    skip_on_cran()
    skip_on_ci()
    skip_on_covr()

    l.args.holdout.1 = list(clv.data = clv.data.holdout, use.cor = TRUE, names.cov.constr = c("Gender", "Channel"),verbose=FALSE)
    l.args.no.holdout.1 = list(clv.data = clv.data.no.holdout, use.cor = TRUE, names.cov.constr = c("Gender", "Channel"), verbose=FALSE)

    # Constraints + Correlation
    expect_silent(p.hold <- do.call(what = method, args = l.args.holdout.1))
    expect_silent(p.no.hold <- do.call(what = method, args = l.args.no.holdout.1))

    fct.helper.fitted.all.s3(p.hold,    full.names = c(model.param.names,  p.hold@clv.model@name.correlation.cor, p.hold@names.prefixed.params.constr),
                             clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold,
                             DERT.not.implemented = DERT.not.implemented)
    fct.helper.fitted.all.s3(p.no.hold, full.names = c(model.param.names, p.no.hold@clv.model@name.correlation.cor, p.no.hold@names.prefixed.params.constr),
                             clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold,
                             DERT.not.implemented = DERT.not.implemented)

    l.args.holdout.2 <- list(clv.data = clv.data.holdout, use.cor = TRUE, reg.lambdas = c(trans=10, life=10),verbose=FALSE)
    l.args.no.holdout.2 <- list(clv.data = clv.data.no.holdout, use.cor = TRUE, reg.lambdas = c(trans=10, life=10),verbose=FALSE)

    # Regularization + Correlation
    expect_silent(p.hold    <- do.call(what = method, args = l.args.holdout.2))
    expect_silent(p.no.hold <- do.call(what = method, args = l.args.no.holdout.2))

    fct.helper.fitted.all.s3(p.hold,    full.names = c(model.param.names,  p.hold@clv.model@name.correlation.cor, p.hold@names.prefixed.params.free.life, p.hold@names.prefixed.params.free.trans),
                             clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold, DERT.not.implemented = DERT.not.implemented)
    fct.helper.fitted.all.s3(p.no.hold, full.names = c(model.param.names, p.no.hold@clv.model@name.correlation.cor, p.no.hold@names.prefixed.params.free.life, p.no.hold@names.prefixed.params.free.trans),
                             clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold, DERT.not.implemented = DERT.not.implemented)

    l.args.holdout.3 <- list(clv.data = clv.data.holdout, use.cor = TRUE, names.cov.constr = c("Gender", "Channel"),reg.lambdas = c(trans=10, life=10),verbose=FALSE)
    l.args.no.holdout.3 <- list(clv.data = clv.data.no.holdout, use.cor = TRUE, names.cov.constr = c("Gender", "Channel"),reg.lambdas = c(trans=10, life=10),verbose=FALSE)

    # Regularization + Correlation + Constraints
    expect_silent(p.hold    <- do.call(what = method, args = l.args.holdout.3))
    expect_silent(p.no.hold <- do.call(what = method, args = l.args.no.holdout.3))

    fct.helper.fitted.all.s3(p.hold,    full.names = c(model.param.names, p.hold@clv.model@name.correlation.cor, p.hold@names.prefixed.params.constr),
                             clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold,
                             DERT.not.implemented = DERT.not.implemented)
    fct.helper.fitted.all.s3(p.no.hold, full.names = c(model.param.names, p.no.hold@clv.model@name.correlation.cor, p.no.hold@names.prefixed.params.constr),
                             clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold,
                             DERT.not.implemented = DERT.not.implemented)
  })
}



fct.testthat.runability.staticcov <- function(name.model, method, start.params.model, has.DERT, has.cor,
                                              data.apparelTrans, data.apparelStaticCov,
                                              failed.optimization.methods.expected.message){
  context(paste0("Runability - ",name.model," staticcov - Basic runability"))

  # Data objects: normal data
  expect_silent(clv.data.apparel.no.holdout  <- clvdata(data.transactions = data.apparelTrans, date.format = "ymd", time.unit = "W"))
  expect_silent(clv.data.apparelwith.holdout <- clvdata(data.transactions = data.apparelTrans, date.format = "ymd", time.unit = "W",
                                                        estimation.split = 40))
  expect_silent(clv.data.cov.holdout    <- SetStaticCovariates(clv.data.apparelwith.holdout,
                                                               data.cov.life = data.apparelStaticCov, data.cov.trans = data.apparelStaticCov,
                                                               names.cov.life = c("Gender", "Channel"), names.cov.trans = c("Gender", "Channel")))
  expect_silent(clv.data.cov.no.holdout <- SetStaticCovariates(clv.data.apparel.no.holdout,
                                                               data.cov.life = data.apparelStaticCov, data.cov.trans = data.apparelStaticCov,
                                                               names.cov.life = c("Gender", "Channel"), names.cov.trans = c("Gender", "Channel")))


  # Data objects: newdata
  # to test plot/predict
  #   Create with new fake data and generally other names
  set.seed(0xcaffe) # hipster seed
  expect_silent(dt.newdata.trans <- data.table::rbindlist(lapply(paste0(LETTERS,1:100,sep=""), function(cid){
    data.table::data.table(cust.id = cid,
                           trans.date = seq.Date(from = as.Date(data.apparelTrans[, min(Date)]), to = as.Date(data.apparelTrans[, max(Date)]),
                                                 length.out = sample.int(n=5, size = 1, replace=FALSE)))
  })))
  expect_silent(dt.newdata.trans[, trans.date := format(trans.date, "%Y:%d:%m")])
  # Generate fake cov data
  expect_silent(dt.newdata.covs <- data.table::rbindlist(lapply(paste0(LETTERS,1:100,sep=""), function(cid){
    data.table::data.table(cid, Gender = sample(0:1, size = 1), Channel =  sample(0:1, size = 1))
  })))

  expect_silent(clv.newdata.nohold <- clvdata(data.transactions = dt.newdata.trans, date.format = "ydm", time.unit = "w",
                                              estimation.split = NULL, name.id = "cust.id", name.date = "trans.date",
                                              name.price = NULL))
  expect_silent(clv.newdata.withhold <- clvdata(data.transactions = dt.newdata.trans, date.format = "ydm", time.unit = "w",
                                                estimation.split = 40, name.id = "cust.id", name.date = "trans.date",
                                                name.price = NULL))
  expect_silent(clv.newdata.nohold <- SetStaticCovariates(clv.newdata.nohold,
                                                          data.cov.life = dt.newdata.covs, data.cov.trans = dt.newdata.covs,
                                                          names.cov.life = c("Gender", "Channel"), names.cov.trans = c("Gender", "Channel"),
                                                          name.id = "cid"))
  expect_silent(clv.newdata.withhold <- SetStaticCovariates(clv.newdata.withhold,
                                                            data.cov.life = dt.newdata.covs, data.cov.trans = dt.newdata.covs,
                                                            names.cov.life =  c("Gender", "Channel"), names.cov.trans =  c("Gender", "Channel"),
                                                            name.id = "cid"))


  names.params.all.free <- c(names(start.params.model), "life.Gender", "life.Channel", "trans.Gender", "trans.Channel")

  # Common tests ------------------------------------------------------------------------------------------------------------
  fct.testthat.runability.common.out.of.the.box.no.hold(method = method, clv.data.noholdout = clv.data.cov.no.holdout,
                                                        clv.newdata.withhold = clv.newdata.withhold, clv.newdata.nohold = clv.newdata.nohold,
                                                        full.param.names = names.params.all.free, DERT.not.implemented = !has.DERT)
  fct.testthat.runability.common.out.of.the.box.with.hold(method = method, clv.data.withholdout = clv.data.cov.holdout,
                                                          clv.newdata.withhold = clv.newdata.withhold, clv.newdata.nohold = clv.newdata.nohold,
                                                          full.param.names = names.params.all.free, DERT.not.implemented = !has.DERT)

  fct.testthat.runability.common.custom.model.start.params(method = method, start.params.model = start.params.model,
                                                           clv.data.noholdout = clv.data.cov.no.holdout, clv.data.withholdout = clv.data.cov.holdout)

  # fct.testthat.runability.common.all.optimization.methods(method = method, clv.data.noholdout = clv.data.cov.no.holdout,
  #                                                         expected.message = failed.optimization.methods.expected.message)
  #
  # fct.testthat.runability.common.multiple.optimization.methods(method = method, param.names = names.params.all.free,
  #                                                              DERT.not.implemented = !has.DERT,
  #                                                              clv.data.noholdout = clv.data.cov.no.holdout,
  #                                                              clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold)


  # Static cov tests ------------------------------------------------------------------------------------------------------------
  fct.testthat.runability.staticcov.custom.model.covariate.start.params(method = method, start.params.model = start.params.model,
                                                                        clv.data.holdout = clv.data.cov.holdout,
                                                                        clv.data.no.holdout = clv.data.cov.no.holdout)
  fct.testthat.runability.staticcov.reduce.relevant.covariates.estimation(method = method, clv.data.holdout = clv.data.cov.holdout)


  if(has.cor){
    context(paste0("Runability - ",name.model," static cov - w/ Correlation"))

    fct.testthat.runability.common.works.with.cor(method = method,
                                                  DERT.not.implemented = !has.DERT,
                                                  clv.data.holdout = clv.data.cov.holdout,
                                                  clv.newdata.nohold = clv.newdata.nohold,
                                                  clv.newdata.withhold = clv.newdata.withhold,
                                                  names.params.model = names(start.params.model))

    fct.testthat.runability.common.works.with.cor.start.params(method = method,
                                                               DERT.not.implemented = !has.DERT,
                                                               clv.data.holdout = clv.data.cov.holdout,
                                                               clv.newdata.nohold = clv.newdata.nohold,
                                                               clv.newdata.withhold = clv.newdata.withhold,
                                                               names.params.model = names(start.params.model))
  }

  context(paste0("Runability - ",name.model," static cov - w/ Constraint"))
  fct.testthat.runability.staticcov.works.with.2.constraints(method = method,
                                                             param.names.model = names(start.params.model),
                                                             DERT.not.implemented = !has.DERT,
                                                             clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold,
                                                             clv.data.holdout = clv.data.cov.holdout, clv.data.no.holdout = clv.data.cov.no.holdout)

  fct.testthat.runability.staticcov.works.with.1.constraint.1.free(method = method,
                                                                   param.names.model = names(start.params.model),
                                                                   DERT.not.implemented = !has.DERT,
                                                                   clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold,
                                                                   clv.data.holdout = clv.data.cov.holdout, clv.data.no.holdout = clv.data.cov.no.holdout)

  context(paste0("Runability - ",name.model," static cov - w/ Regularization"))
  fct.testthat.runability.staticcov.works.with.regularization(method = method,
                                                              param.names.model = names(start.params.model),
                                                              DERT.not.implemented = !has.DERT,
                                                              clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold,
                                                              clv.data.holdout = clv.data.cov.holdout, clv.data.no.holdout = clv.data.cov.no.holdout)

  fct.testthat.runability.staticcov.works.with.0.lambdas(method = method,
                                                         param.names.model = names(start.params.model),
                                                         DERT.not.implemented = !has.DERT,
                                                         clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold,
                                                         clv.data.holdout = clv.data.cov.holdout, clv.data.no.holdout = clv.data.cov.no.holdout)

  context(paste0("Runability - ",name.model," static cov - w/ Combinations"))
  if(has.cor){
    fct.testthat.runability.staticcov.works.with.combined.interlayers.with.cor(method = method,
                                                                               model.param.names = names(start.params.model),
                                                                               DERT.not.implemented = !has.DERT,
                                                                               clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold,
                                                                               clv.data.holdout = clv.data.cov.holdout, clv.data.no.holdout = clv.data.cov.no.holdout)
  }else{
    fct.testthat.runability.staticcov.works.with.combined.interlayers.without.cor(method = method,
                                                                                  model.param.names = names(start.params.model),
                                                                                  DERT.not.implemented = !has.DERT,
                                                                                  clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold,
                                                                                  clv.data.holdout = clv.data.cov.holdout, clv.data.no.holdout = clv.data.cov.no.holdout)
  }
}

