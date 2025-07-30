skip_on_cran()

# for bootstrapping, seed is important. Otherwise, may yield unfortunate samples
# for which the estimation fails
set.seed(124)



# This tests many things at once:
# - That clv.bootstrapped.apply can be applied on many different models (nocov, static cov, dyncov)
# - That the model fitted on bootstrapped data works correctly and can be used (all S3 methods)

fct.testthat.clv.boostrapped.apply.downstream.methods.work.on.boots <- function(clv.fitted, newdata.nohold, newdata.withhold){
  test_that("Downstream methods work on all transaction models fitted on bootstrapped data", {
    clv.bootstrapped.apply(clv.fitted, num.boots=1, fn.boot.apply=function(boots.fitted){

      fct.helper.clvfittedtransactions.all.s3(
        clv.fitted=boots.fitted,
        full.names=names(coef(boots.fitted)),
        clv.newdata.nohold=newdata.nohold,
        clv.newdata.withhold=newdata.withhold
      )
      return(NULL)
    })
  })
}


fct.testthat.clv.bootstrapped.apply.ellipsis.works <- function(clv.fitted){

  test_that("Can pass additional args to the optimization through ellipsis arg", {

    # use an optimization method not used anywhere else
    # the optimx.args used during the original optimization are kept (overwritten with `modifyList`)

    expect_warning(l.boots <- clv.bootstrapped.apply(
      clv.fitted,
      num.boots=1,
      fn.boot.apply=function(o){return(o)},
      optimx.args=list(method='CG', itnmax=5, hessian=FALSE, control=list(kkt=FALSE))
    ), regexp = 'Hessian')

    expect_true(rownames(l.boots[[1]]@optimx.estimation.output) == 'CG')
  })
}



# clv.data: bootstrapped clv.data ----------------------------------------------
# all sorts of plotting etc work

test_that("All clv.data methods work on bootstrapped clv.data", {
  clv.data.dyn <- fct.helper.create.clvdata.apparel.dyncov(estimation.split=104)

  expect_silent(clv.boots <- clv.data.create.bootstrapping.data(
    clv.data.dyn,
    ids = sample(x = unique(clv.data.dyn@data.transactions$Id), size = 50)
  ))

  expect_silent(nobs(clv.boots))
  expect_output(print(clv.boots))
  expect_silent(summary(clv.boots))

  expect_silent(subset(clv.boots, Price>0))
  expect_silent(as.data.table(clv.boots))

  expect_silent(plot(clv.boots, which="tracking", verbose=FALSE))
  expect_silent(plot(clv.boots, which="frequency", verbose=FALSE))
  expect_silent(plot(clv.boots, which="spending", verbose=FALSE))
  expect_silent(plot(clv.boots, which="interpurchasetime", verbose=FALSE))
  expect_silent(plot(clv.boots, which="timings", verbose=FALSE))

  # bootstrap from bootstrapped data
  expect_silent(clv.data.create.bootstrapping.data(
    clv.boots,
    ids = unique(clv.boots@data.transactions$Id)
  ))
})


# No cov models ---------------------------------------------------------------

# Optimx args:
#  - use Nelder-Mead which is less prone to lead failed estimation on bootstrapped data
#  - restrict max iterations to reduce runtime as NelderMead takes longer to converge
optimx.args.NM <- list(method='Nelder-Mead', itnmax=100)

clv.apparel.nocov.holdout <- fct.helper.create.clvdata.apparel.nocov()
clv.apparel.nocov.no.holdout <- fct.helper.create.clvdata.apparel.nocov(estimation.split=NULL)

for(clv.fitted in list(
  fit.apparel.nocov(model=pnbd, estimation.split=104, optimx.args=optimx.args.NM),
  fit.apparel.nocov(model=pnbd, estimation.split=NULL, optimx.args=optimx.args.NM),

  fit.apparel.nocov(model=bgnbd, estimation.split=104, optimx.args=optimx.args.NM),
  fit.apparel.nocov(model=bgnbd, estimation.split=NULL, optimx.args=optimx.args.NM),

  fit.apparel.nocov(model=ggomnbd, estimation.split=104, optimx.args=optimx.args.NM),
  fit.apparel.nocov(model=ggomnbd, estimation.split=NULL, optimx.args=optimx.args.NM)
  )){

  # . clv.bootstrapped.apply ----------------------------------------------------
  fct.testthat.clv.boostrapped.apply.downstream.methods.work.on.boots(
    clv.fitted = clv.fitted,
    newdata.nohold = clv.apparel.nocov.no.holdout,
    newdata.withhold = clv.apparel.nocov.holdout
  )

  fct.testthat.clv.bootstrapped.apply.ellipsis.works(
    clv.fitted = clv.fitted
  )


  # . predict -----------------------------------------------------------------
  test_that("predict bootstrapping works for no cov models", {
    expect_warning(predict(clv.fitted, uncertainty='boots', num.boots=2, prediction.end=5, predict.spending=TRUE, verbose=FALSE), regexp = "It is recommended")
  })
}


# Static cov models -----------------------------------------------------------
clv.apparel.static.holdout <- fct.helper.create.clvdata.apparel.staticcov(estimation.split=104)
clv.apparel.static.no.holdout <- fct.helper.create.clvdata.apparel.staticcov(estimation.split=NULL)


for(clv.fitted in list(
  fit.apparel.static(model=pnbd, estimation.split=NULL, optimx.args=optimx.args.NM),
  fit.apparel.static(model=pnbd, estimation.split=104, optimx.args=optimx.args.NM),

  fit.apparel.static(model=bgnbd, estimation.split=NULL, optimx.args=optimx.args.NM),
  fit.apparel.static(model=bgnbd, estimation.split=104, optimx.args=optimx.args.NM),

  fit.apparel.static(model=ggomnbd, estimation.split=NULL, optimx.args=optimx.args.NM),
  fit.apparel.static(model=ggomnbd, estimation.split=104, optimx.args=optimx.args.NM)
  )){

  # . clv.bootstrapped.apply ----------------------------------------------------
  fct.testthat.clv.boostrapped.apply.downstream.methods.work.on.boots(
    clv.fitted = clv.fitted,
    newdata.nohold = clv.apparel.static.no.holdout,
    newdata.withhold = clv.apparel.static.holdout
  )

  fct.testthat.clv.bootstrapped.apply.ellipsis.works(
    clv.fitted = clv.fitted
  )

  # . predict -----------------------------------------------------------------
  test_that("predict bootstrapping works for static cov models", {
    expect_warning(predict(clv.fitted, uncertainty='boots', num.boots=2, prediction.end=5, verbose=FALSE), regexp = "It is recommended")
  })
}

# Dync cov models -------------------------------------------------------------

clv.apparel.dyn.holdout <- fct.helper.create.clvdata.apparel.dyncov()
clv.apparel.dyn.no.holdout <- fct.helper.create.clvdata.apparel.dyncov(estimation.split=NULL)

for(clv.fitted in list(
  fit.apparel.dyncov.quick(estimation.split=NULL, hessian=FALSE),
  fit.apparel.dyncov.quick(estimation.split=104, hessian=FALSE)
)){

  # . clv.bootstrapped.apply ----------------------------------------------------

  # Expect warning because again fitted without hessian
  expect_warning(clv.bootstrapped.apply(clv.fitted, num.boots=1, fn.boot.apply=function(boots.fitted){


    # Basic S3
    expect_silent(coef(boots.fitted))
    expect_silent(logLik(boots.fitted))
    expect_silent(nobs(boots.fitted))
    expect_output(print(boots.fitted))
    # doesnt work because fitted w/o hessian
    expect_warning(summary(boots.fitted), regexp = "could not be calculated")

    if(fct.helper.has.pmf(boots.fitted)){
      expect_silent(pmf(boots.fitted, x = 0:2))
    }

    # Predict

    # Can only predict if has holdout because dyncov needs longer cov data
    if(clv.data.has.holdout(boots.fitted@clv.data)){
      expect_silent(predict(boots.fitted, prediction.end=5, predict.spending=TRUE, verbose=FALSE))
    }

    # predicting with newcustomer as newdata
    newc.dyn <- newcustomer.dynamic(
      num.periods=3,
      data.cov.life=fct.helper.default.newcustomer.covdata.dyncov(),
      data.cov.trans=fct.helper.default.newcustomer.covdata.dyncov(),
      first.transaction='2000-01-03'
    )
    predict(boots.fitted, newdata=newc.dyn)

    # Plot
    # keep plot length short. Leads to warning when not plotting beyond holdout period
    # dyncov cannot predict w/o holdout because requires more covs
    if(clv.data.has.holdout(boots.fitted@clv.data)){
      expect_warning(plot(boots.fitted, which="tracking", prediction.end=5, verbose=FALSE), regexp = "Not plotting full holdout period")
    }

    if(fct.helper.has.pmf(boots.fitted)){
      expect_silent(plot(boots.fitted, which="pmf", trans.bins=0:2, verbose=FALSE))
    }

    return(NULL)
  }), regexp = "Hessian could not")


  fct.testthat.clv.bootstrapped.apply.ellipsis.works(
    clv.fitted = clv.fitted
  )

  # . predict -----------------------------------------------------------------

  if(clv.data.has.holdout(clv.fitted@clv.data)){
    test_that("predict bootstrapping works for dyn cov models", {
      # need as many expect_warning as num.boots to catch all that are thrown per boots run
      expect_warning(
        expect_warning(
          predict(clv.fitted, uncertainty='boots', prediction.end=5, num.boots=1, verbose=FALSE),
          regexp = 'recommended'),
        regexp = 'Hessian')
    })
  }
}

# Spending models ------------------------------------------------------------
for(clv.fitted in list(
  gg(clv.apparel.nocov.holdout, remove.first.transaction = TRUE, verbose=FALSE),
  gg(clv.apparel.nocov.holdout, remove.first.transaction = FALSE, verbose=FALSE),

  gg(clv.apparel.nocov.no.holdout, remove.first.transaction = TRUE, verbose=FALSE),
  gg(clv.apparel.nocov.no.holdout, remove.first.transaction = FALSE, verbose=FALSE)
)){

  # . clv.bootstrapped.apply ---------------------------------------------------
  clv.bootstrapped.apply(clv.fitted, num.boots=1, fn.boot.apply=function(boots.fitted){

    # Basic S3
    expect_silent(coef(boots.fitted))
    expect_silent(logLik(boots.fitted))
    expect_silent(nobs(boots.fitted))
    expect_output(print(boots.fitted))
    expect_silent(summary(boots.fitted))

    expect_silent(predict(boots.fitted))

    return(NULL)
  })

  fct.testthat.clv.bootstrapped.apply.ellipsis.works(
    clv.fitted = clv.fitted
  )


  # . predict -----------------------------------------------------------------
  expect_warning(predict(clv.fitted, uncertainty='boots', num.boots=2, verbose=FALSE), regexp = "It is recommended")
}


# predict(uncertainty=boots) works on all model specifications -----------------------------
# This also includes testing `clv.bootstrapped.apply` because it is used under the hood
# - fit with correlation
# - constrained params
# - regularization
# - combinations

test_that("predict(uncertainty=boots) works on all model specifications", {
  fn.predict.boots <- function(clv.fitted){
    expect_warning(predict(clv.fitted, uncertainty='boots', num.boots=2, predict.spending=TRUE, verbose=FALSE), regexp = 'recommended to run')
  }

  p.cor <- fit.apparel.nocov(use.cor=TRUE, verbose=FALSE, optimx.args=optimx.args.NM)
  fn.predict.boots(p.cor)

  bg.reg <- fit.apparel.static(model=bgnbd, reg.weights = c(trans=10, life=20), verbose=FALSE, optimx.args=optimx.args.NM)
  fn.predict.boots(bg.reg)

  ggom.constr <- fit.apparel.static(model=ggomnbd, names.cov.constr = "Channel", verbose=FALSE, optimx.args=optimx.args.NM)
  fn.predict.boots(ggom.constr)

  p.combo <- fit.apparel.static(
    model=pnbd,
    use.cor=TRUE,
    names.cov.constr = "Channel",
    reg.weights = c(trans=20, life=30),
    verbose=FALSE,
    optimx.args=optimx.args.NM)
  fn.predict.boots(p.combo)


  # Throws Hessian error in each bootstrap run
  expect_warning(p.dyn.combo <- fit.apparel.dyncov(
    model=pnbd,
    use.cor=TRUE,
    names.cov.constr = "Channel",
    reg.weights = c(trans=5, life=5),
    verbose=FALSE,
    optimx.args = fct.helper.dyncov.get.optimxargs.quickfit(hessian=FALSE)
    ))
  # need to catch every warning about hessian in boots with separate expect_warning
  expect_warning(expect_warning(fn.predict.boots(p.dyn.combo), regexp = "Hessian"), regexp = "Hessian")

})



# predict(uncertainty=boots) works with various inputs ------------------------------------

test_that("predict(uncertainty=boots) works with predict.spending, newdata, prediction.end", {

  p.cdnow <- fit.cdnow(optimx.args = optimx.args.NM)

  fn.predict.boots <- function(predict.spending=TRUE, newdata=NULL, prediction.end=NULL){
    expect_warning(dt.pred <- predict(
      p.cdnow,
      verbose=FALSE,
      uncertainty='boots',
      num.boots=2,
      newdata=newdata,
      prediction.end=prediction.end,
      predict.spending=predict.spending
    ), regexp = "recommended to run")
    return(dt.pred)
  }

  # predict.spending
  fn.predict.boots(predict.spending = TRUE)
  fn.predict.boots(predict.spending = FALSE)
  fn.predict.boots(predict.spending = gg)
  fn.predict.boots(predict.spending = fit.cdnow(model = gg))

  # newdata
  clv.apparel.nocov <- fct.helper.create.clvdata.apparel.nocov()
  dt.pred <- fn.predict.boots(newdata=clv.apparel.nocov)
  # really did predict for the apparel dataset and not the cdnow
  expect_true(dt.pred[, .N] == nobs(clv.apparel.nocov))

  # prediction.end
  clv.cdnow.noholdout <- fct.helper.create.clvdata.cdnow(estimation.split = NULL)

  # with holdout, no prediction.end is required
  fn.predict.boots(prediction.end=NULL)
  # with holdout, can also with prediction.end
  fn.predict.boots(prediction.end=10)

  # without holdout, prediction.end is required
  expect_error(
    predict(p.cdnow, uncertainty='boots', newdata=clv.cdnow.noholdout),
    regexp = "Cannot predict without prediction.end"
  )
  # without holdout, works if prediction.end is given
  fn.predict.boots(newdata=clv.cdnow.noholdout, prediction.end=10)

})
