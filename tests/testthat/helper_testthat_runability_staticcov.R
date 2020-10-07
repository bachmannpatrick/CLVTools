#use nocov
fct.testthat.runability.staticcov.custom.model.covariate.start.params <- function(method, clv.data, start.params.model){
  test_that("Works with custom model and covariate start parameters", {
    skip_on_cran()
    skip_on_ci()
    skip_on_covr()
    l.args <- list(clv.data = clv.data,    start.params.model = start.params.model,
                   start.params.life = c(Gender = 1, Channel=0.4), start.params.trans = c(Gender=1, Channel=2), verbose = FALSE)

    expect_silent(do.call(what = method, args = l.args))
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

    fct.helper.clvfittedtransactions.all.s3(p.hold,    full.names = c(param.names.model, "constr.Gender", "constr.Channel"),
                                            clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold, DERT.not.implemented = DERT.not.implemented)
    fct.helper.clvfittedtransactions.all.s3(p.hold,    full.names = c(param.names.model, "constr.Gender", "constr.Channel"),
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
    fct.helper.clvfittedtransactions.all.s3(p.hold,    full.names = c(param.names.model, "life.Channel", "trans.Channel", "constr.Gender"),
                                            clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold, DERT.not.implemented = DERT.not.implemented)
    fct.helper.clvfittedtransactions.all.s3(p.no.hold, full.names = c(param.names.model, "life.Channel", "trans.Channel", "constr.Gender"),
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

    fct.helper.clvfittedtransactions.all.s3(p.hold,    full.names = c(param.names.model, p.hold@names.prefixed.params.free.life, p.hold@names.prefixed.params.free.trans),
                                            clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold, DERT.not.implemented = DERT.not.implemented)
    fct.helper.clvfittedtransactions.all.s3(p.no.hold, full.names = c(param.names.model, p.no.hold@names.prefixed.params.free.life, p.no.hold@names.prefixed.params.free.trans),
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
    fct.helper.clvfittedtransactions.all.s3(p.hold,    full.names = c(param.names.model, p.hold@names.prefixed.params.free.life, p.hold@names.prefixed.params.free.trans),
                                            clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold, DERT.not.implemented = DERT.not.implemented)
    fct.helper.clvfittedtransactions.all.s3(p.no.hold, full.names = c(param.names.model, p.no.hold@names.prefixed.params.free.life, p.no.hold@names.prefixed.params.free.trans),
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

    fct.helper.clvfittedtransactions.all.s3(p.hold,    full.names = c(model.param.names, p.hold@names.prefixed.params.constr),
                                            clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold,
                                            DERT.not.implemented = DERT.not.implemented)
    fct.helper.clvfittedtransactions.all.s3(p.no.hold, full.names = c(model.param.names, p.no.hold@names.prefixed.params.constr),
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

    fct.helper.clvfittedtransactions.all.s3(p.hold,    full.names = c(model.param.names,  p.hold@clv.model@name.correlation.cor, p.hold@names.prefixed.params.constr),
                                            clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold,
                                            DERT.not.implemented = DERT.not.implemented)
    fct.helper.clvfittedtransactions.all.s3(p.no.hold, full.names = c(model.param.names, p.no.hold@clv.model@name.correlation.cor, p.no.hold@names.prefixed.params.constr),
                                            clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold,
                                            DERT.not.implemented = DERT.not.implemented)

    l.args.holdout.2 <- list(clv.data = clv.data.holdout, use.cor = TRUE, reg.lambdas = c(trans=10, life=10),verbose=FALSE)
    l.args.no.holdout.2 <- list(clv.data = clv.data.no.holdout, use.cor = TRUE, reg.lambdas = c(trans=10, life=10),verbose=FALSE)

    # Regularization + Correlation
    expect_silent(p.hold    <- do.call(what = method, args = l.args.holdout.2))
    expect_silent(p.no.hold <- do.call(what = method, args = l.args.no.holdout.2))

    fct.helper.clvfittedtransactions.all.s3(p.hold,    full.names = c(model.param.names,  p.hold@clv.model@name.correlation.cor, p.hold@names.prefixed.params.free.life, p.hold@names.prefixed.params.free.trans),
                                            clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold, DERT.not.implemented = DERT.not.implemented)
    fct.helper.clvfittedtransactions.all.s3(p.no.hold, full.names = c(model.param.names, p.no.hold@clv.model@name.correlation.cor, p.no.hold@names.prefixed.params.free.life, p.no.hold@names.prefixed.params.free.trans),
                                            clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold, DERT.not.implemented = DERT.not.implemented)

    l.args.holdout.3 <- list(clv.data = clv.data.holdout, use.cor = TRUE, names.cov.constr = c("Gender", "Channel"),reg.lambdas = c(trans=10, life=10),verbose=FALSE)
    l.args.no.holdout.3 <- list(clv.data = clv.data.no.holdout, use.cor = TRUE, names.cov.constr = c("Gender", "Channel"),reg.lambdas = c(trans=10, life=10),verbose=FALSE)

    # Regularization + Correlation + Constraints
    expect_silent(p.hold    <- do.call(what = method, args = l.args.holdout.3))
    expect_silent(p.no.hold <- do.call(what = method, args = l.args.no.holdout.3))

    fct.helper.clvfittedtransactions.all.s3(p.hold,    full.names = c(model.param.names, p.hold@clv.model@name.correlation.cor, p.hold@names.prefixed.params.constr),
                                            clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold,
                                            DERT.not.implemented = DERT.not.implemented)
    fct.helper.clvfittedtransactions.all.s3(p.no.hold, full.names = c(model.param.names, p.no.hold@clv.model@name.correlation.cor, p.no.hold@names.prefixed.params.constr),
                                            clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold,
                                            DERT.not.implemented = DERT.not.implemented)
  })
}


fct.testthat.runability.staticcov.works.with.illegal.cov.names <- function(method, data.apparelTrans, data.apparelStaticCov,
                                                                           clv.data.holdout, clv.data.no.holdout,
                                                                           DERT.not.implemented, names.params.model){

  test_that("Works with static covs that have syntactically illegal names", {
    skip_on_cran()
    # skip_on_ci()
    fct.run.with.renamed.cov <- function(new.names){
      apparelStaticCov.named <- data.table::copy(data.apparelStaticCov)
      data.table::setnames(apparelStaticCov.named, old = c("Gender", "Channel"), new=new.names)
      clv.data.named <- fct.helper.create.clvdata.apparel.staticcov(data.apparelTrans = data.apparelTrans,
                                                                    data.apparelStaticCov = apparelStaticCov.named,
                                                                    estimation.split = 40,
                                                                    names.cov.life = new.names, names.cov.trans = new.names)
      expect_silent(fitted <- do.call(what = method, args = list(clv.data=clv.data.named, verbose = FALSE)))

      # Newdata is created here because of different names
      clv.newdata.nohold <- fct.helper.create.fake.newdata.staticcov(data.trans = data.apparelTrans, estimation.split = NULL,
                                                                     names.cov = new.names)
      clv.newdata.withhold <- fct.helper.create.fake.newdata.staticcov(data.trans = data.apparelTrans, estimation.split = 40,
                                                                       names.cov = new.names)

      fct.helper.clvfittedtransactions.all.s3(clv.fitted = fitted,  full.names = c(names.params.model,
                                                                                   paste0("life.",make.names(new.names)),
                                                                                   paste0("trans.",make.names(new.names))),
                                              clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold,
                                              DERT.not.implemented = DERT.not.implemented)
    }

    # Numbers
    fct.run.with.renamed.cov(new.names = c("84", "99"))
    # With spaces
    fct.run.with.renamed.cov(new.names = c("Gen der", " Channel"))
  })
}


fct.helper.create.fake.newdata.staticcov <- function(data.trans, estimation.split, names.cov){

  # Create with new fake data and generally other names
  #   Generate fake cov data
  dt.newdata.trans <- fct.helper.create.fake.transdata(data = data.trans)

  expect_silent(dt.newdata.covs <- data.table::rbindlist(lapply(dt.newdata.trans[, unique(cust.id)], function(cid){
    dt <- data.table::data.table(cid)
    for(n in names.cov){
      dt[, (n) := sample(0:1, size = 1)]
    }
    return(dt)
  })))
  expect_silent(clv.newdata <- clvdata(data.transactions = dt.newdata.trans, date.format = "ydm", time.unit = "w",
                                       estimation.split = estimation.split, name.id = "cust.id", name.date = "trans.date",
                                       name.price = "Price"))
  expect_silent(clv.newdata <- SetStaticCovariates(clv.newdata,
                                                   data.cov.life = dt.newdata.covs, data.cov.trans = dt.newdata.covs,
                                                   names.cov.life = names.cov, names.cov.trans = names.cov,
                                                   name.id = "cid"))
  return(clv.newdata)
}

fct.helper.create.clvdata.apparel.staticcov <- function(data.apparelTrans, data.apparelStaticCov, estimation.split,
                                                        names.cov.life = c("Gender", "Channel"), names.cov.trans = c("Gender", "Channel")){

  expect_silent(clv.data.apparel <- clvdata(data.transactions = data.apparelTrans, date.format = "ymd", time.unit = "W",
                                            estimation.split = estimation.split))

  expect_silent(clv.data.apparel    <- SetStaticCovariates(clv.data.apparel,
                                                           data.cov.life = data.apparelStaticCov, data.cov.trans = data.apparelStaticCov,
                                                           names.cov.life = names.cov.life, names.cov.trans = names.cov.trans))

  return(clv.data.apparel)
}



fct.testthat.runability.staticcov <- function(name.model, method, start.params.model, has.DERT, has.cor,
                                              data.apparelTrans, data.apparelStaticCov,
                                              failed.optimization.methods.expected.message){
  context(paste0("Runability - ",name.model," staticcov - Basic runability"))

  # Data objects: normal data
  clv.data.cov.no.holdout <- fct.helper.create.clvdata.apparel.staticcov(data.apparelTrans = data.apparelTrans, data.apparelStaticCov = data.apparelStaticCov,
                                                                         estimation.split = NULL)
  clv.data.cov.holdout   <- fct.helper.create.clvdata.apparel.staticcov(data.apparelTrans = data.apparelTrans, data.apparelStaticCov = data.apparelStaticCov,
                                                                        estimation.split = 40)

  clv.newdata.nohold   <- fct.helper.create.fake.newdata.staticcov(data.trans = data.apparelTrans, names.cov = c("Gender", "Channel"),
                                                                   estimation.split = NULL)
  clv.newdata.withhold <- fct.helper.create.fake.newdata.staticcov(data.trans = data.apparelTrans, names.cov = c("Gender", "Channel"),
                                                                   estimation.split = 40)

  names.params.all.free <- c(names(start.params.model), "life.Gender", "life.Channel", "trans.Gender", "trans.Channel")
  l.args.test.all.s3 <- list(full.names = names.params.all.free, clv.newdata.nohold = clv.newdata.nohold,
                             clv.newdata.withhold = clv.newdata.withhold, DERT.not.implemented = !has.DERT)

  # Common tests ------------------------------------------------------------------------------------------------------------
  fct.testthat.runability.clvfitted.out.of.the.box.no.hold(method = method, clv.data.noholdout = clv.data.cov.no.holdout,
                                                           l.args.test.all.s3 = l.args.test.all.s3, fct.test.all.s3=fct.helper.clvfittedtransactions.all.s3)
  fct.testthat.runability.clvfitted.out.of.the.box.with.hold(method = method, clv.data.withholdout = clv.data.cov.holdout,
                                                             l.args.test.all.s3 = l.args.test.all.s3, fct.test.all.s3=fct.helper.clvfittedtransactions.all.s3)

  fct.testthat.runability.clvfitted.custom.model.start.params(method = method, start.params.model = start.params.model, clv.data = clv.data.cov.no.holdout)
  fct.testthat.runability.clvfitted.custom.model.start.params(method = method, start.params.model = start.params.model, clv.data = clv.data.cov.holdout)

  # fct.testthat.runability.clvfitted.all.optimization.methods(method = method, clv.data = clv.data.cov.no.holdout,
  #                                                         expected.message = failed.optimization.methods.expected.message)

  fct.testthat.runability.clvfitted.multiple.optimization.methods(method = method, clv.data=clv.data.cov.no.holdout,
                                                                  l.args.test.all.s3 = l.args.test.all.s3, fct.test.all.s3=fct.helper.clvfittedtransactions.all.s3)



  # Static cov tests ------------------------------------------------------------------------------------------------------------
  fct.testthat.runability.staticcov.custom.model.covariate.start.params(method = method, clv.data = clv.data.cov.holdout,
                                                                        start.params.model = start.params.model)
  fct.testthat.runability.staticcov.custom.model.covariate.start.params(method = method, clv.data = clv.data.cov.no.holdout,
                                                                        start.params.model = start.params.model)

  fct.testthat.runability.staticcov.reduce.relevant.covariates.estimation(method = method, clv.data.holdout = clv.data.cov.holdout)

  fct.testthat.runability.staticcov.works.with.illegal.cov.names(method = method, data.apparelTrans = data.apparelTrans, data.apparelStaticCov = data.apparelStaticCov,
                                                                 DERT.not.implemented = !has.DERT,
                                                                 clv.data.holdout = clv.data.cov.holdout,
                                                                 names.params.model = names(start.params.model))


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

