skip_on_cran()
data("cdnow")
data("apparelTrans")
data("apparelStaticCov")
data("apparelDynCov")



optimx.args.fast <- list(method='Nelder-Mead', itnmax=25, hessian=FALSE)

# create with holdout
clv.cdnow <- fct.helper.create.clvdata.cdnow()

# create with different covs for both processes
clv.apparel.cov <- fct.helper.create.clvdata.apparel.staticcov(
  names.cov.life = c("Gender"),
  names.cov.trans = c("Gender", "Channel")
)
clv.apparel.dyn <- fct.helper.create.clvdata.apparel.dyncov(
  names.cov.life = c("Gender"),
  names.cov.trans = c("Gender", "Channel")
)



# clv.data.create.bootstrapping.data -------------------------------------------------------------------------------------------

test_that("Sampling all customers (nocov, static, dyn) results in same clv.data object", {
  # nocov
  expect_silent(clv.sampled.nocov <- clv.data.create.bootstrapping.data(clv.cdnow, ids=cdnow[, unique(Id)]))
  expect_silent(clv.sampled.static <- clv.data.create.bootstrapping.data(clv.apparel.cov, ids=apparelTrans[, unique(Id)]))
  expect_silent(clv.sampled.dyn <- clv.data.create.bootstrapping.data(clv.apparel.dyn, ids=apparelTrans[, unique(Id)]))

  clv.sampled.nocov@call <- clv.cdnow@call
  clv.sampled.static@call <- clv.apparel.cov@call
  clv.sampled.dyn@call <- clv.apparel.dyn@call

  expect_equal(clv.sampled.nocov, clv.cdnow)
  expect_equal(clv.sampled.static, clv.apparel.cov)
  expect_equal(clv.sampled.dyn, clv.apparel.dyn)
})

test_that("Bootstrapping clv.time keep same information, incl holdout period", {
  skip_on_cran()

  # zero-repeater on 1997-01-06
  expect_silent(clv.sampled <- clv.data.create.bootstrapping.data(clv.cdnow, ids=c("102")))
  expect_true(clv.sampled@clv.time@timepoint.estimation.start == "1997-01-01")
  expect_equal(clv.sampled@clv.time, clv.cdnow@clv.time)
})


test_that("Sampling keeps holdout (repeat) transactions", {
  skip_on_cran()

  # customers which make holdout transactions
  expect_silent(clv.sampled <- clv.data.create.bootstrapping.data(clv.cdnow, ids=c("1000", "990")))

  dt.original.holdout <- clv.data.get.transactions.in.holdout.period(clv.cdnow)
  dt.sampled.holdout <- clv.data.get.transactions.in.holdout.period(clv.sampled)
  expect_equal(dt.sampled.holdout, dt.original.holdout[c("1000", "990"), on="Id"])
})

test_that("Passing non-existent Ids does not create them in the transcation data", {
  skip_on_cran()

  expect_warning(
    clv.sampled <- clv.data.create.bootstrapping.data(clv.cdnow, ids=c("1", "2", "abc")),
    regexp = "Not all given")

  # abc may not appear in ids
  expect_setequal(clv.sampled@data.transactions$Id, c("1", "2"))
  expect_false(anyNA(clv.sampled@data.transactions))
})

test_that("Sampling yields same cbs value again for sampled customers, incl for duplicate ids", {
  # bootstrapping data has to be created, such that cbs values remain the same, also if
  # customers' transactions would result in different estimation.end (such as for customers 1 & 2)
  skip_on_cran()


  dt.cbs.orig <- pnbd_cbs(clv.cdnow)

  # without replacement
  expect_equal(dt.cbs.orig[Id %in% c("1", "2")],
               pnbd_cbs(clv.data.create.bootstrapping.data(clv.data=clv.cdnow, ids=c("1", "2"))))

  # with replacement
  dt.cbs.orig.replaced <- dt.cbs.orig[c("1", "2", "2", "2")]
  dt.cbs.orig.replaced[c(3, 4), Id := c("2_BOOTSTRAP_ID_2", "2_BOOTSTRAP_ID_3")]
  # use == because expect_equal also checks property sorted
  expect_true(all(
    dt.cbs.orig.replaced ==
      pnbd_cbs(clv.data.create.bootstrapping.data(clv.data=clv.cdnow, ids=c("1", "2", "2", "2")))))


  # sampling all ids
  expect_equal(dt.cbs.orig,
               pnbd_cbs(clv.data.create.bootstrapping.data(clv.data=clv.cdnow, ids=dt.cbs.orig$Id)))
})

test_that("Sampling with replacement creates duplicate entries with new ids", {
  skip_on_cran()

  expect_silent(clv.sampled <- clv.data.create.bootstrapping.data(clv.cdnow, ids=c("1", "1", "2", "2")))

  expect_setequal(clv.sampled@data.transactions$Id, c("1", "1_BOOTSTRAP_ID_2", "2", "2_BOOTSTRAP_ID_2"))
  expect_equal(clv.sampled@data.transactions[, .N, keyby="Id"],
               data.table(Id=c("1", "1_BOOTSTRAP_ID_2", "2", "2_BOOTSTRAP_ID_2"), N=c(4, 4, 2, 2), key = "Id"))
})



test_that("Sampling with and without replacement selects static covariates of the same ids", {
  skip_on_cran()

  # This test is essential because this is never verified in the data when it the object is created

  # without replacement
  sampled.ids <- c("1", "10")
  expect_silent(clv.sampled <- clv.data.create.bootstrapping.data(clv.apparel.cov, ids=sampled.ids))
  # same ids in cov data
  expect_setequal(clv.sampled@data.cov.life$Id, sampled.ids)
  expect_setequal(clv.sampled@data.cov.trans$Id, sampled.ids)
  expect_setequal(clv.sampled@data.transactions$Id, clv.sampled@data.cov.life$Id)
  expect_true(all(clv.sampled@data.cov.trans == clv.apparel.cov@data.cov.trans[Id %in% sampled.ids,]))
  expect_true(all(clv.sampled@data.cov.life == clv.apparel.cov@data.cov.life[Id %in% sampled.ids,]))

  # with replacement
  expect_silent(clv.sampled <- clv.data.create.bootstrapping.data(clv.apparel.cov, ids=c("1", "1", "10", "10")))
  expect_setequal(clv.sampled@data.cov.life$Id, c("1", "1_BOOTSTRAP_ID_2", "10", "10_BOOTSTRAP_ID_2"))
  expect_setequal(clv.sampled@data.cov.trans$Id, c("1", "1_BOOTSTRAP_ID_2", "10", "10_BOOTSTRAP_ID_2"))
  expect_setequal(clv.sampled@data.transactions$Id, clv.sampled@data.cov.life$Id)
  expect_setequal(colnames(clv.sampled@data.cov.trans), c("Id", "Gender", "Channel"))
  expect_setequal(colnames(clv.sampled@data.cov.life), c("Id", "Gender"))
})


test_that("Static covariates are sorted same as cbs", {
  # this went undetected for some time

  all.ids <- unique(clv.apparel.cov@data.transactions[, Id])
  expect_silent(
    clv.sampled <- clv.data.create.bootstrapping.data(
      clv.apparel.cov,
      ids=c(all.ids, all.ids) # duplicate ids to also have `1_BOOTS...`
  ))

  dt.cbs <- pnbd_cbs(clv.sampled)

  expect_true(all(dt.cbs$Id == clv.sampled@data.cov.life$Id))
  expect_true(all(dt.cbs$Id == clv.sampled@data.cov.trans$Id))
})


test_that("Sampling selects correct dynamic covariates", {
  skip_on_cran()

  # This test is essential because this is never verified in the data when the object is created

  dyn.cols <- c("Id", "Cov.Date", "tp.cov.lower", "tp.cov.upper")

  # without replacement
  sampled.ids <- c("1", "10")
  expect_silent(clv.sampled <- clv.data.create.bootstrapping.data(clv.apparel.dyn, ids=sampled.ids))
  # same ids
  expect_setequal(clv.sampled@data.cov.life$Id, sampled.ids)
  expect_setequal(clv.sampled@data.cov.trans$Id, sampled.ids)

  # content of covariate data exactly tge same as original
  expect_true(all(clv.sampled@data.cov.trans[, c("Gender", "Channel")] == clv.apparel.dyn@data.cov.trans[Id %in% sampled.ids, c("Gender", "Channel")]))
  expect_true(all(clv.sampled@data.cov.trans[, c("Gender")] == clv.apparel.dyn@data.cov.trans[Id %in% sampled.ids, c("Gender")]))

  # transaction data
  expect_setequal(clv.sampled@data.transactions$Id, sampled.ids)

  # with replacement
  expect_silent(clv.sampled <- clv.data.create.bootstrapping.data(clv.apparel.dyn, ids=c("1", "1", "10", "10")))# ids in transaction data
  # ids in cov data
  expect_setequal(clv.sampled@data.cov.life$Id, c("1", "1_BOOTSTRAP_ID_2", "10", "10_BOOTSTRAP_ID_2"))
  expect_setequal(clv.sampled@data.cov.trans$Id, c("1", "1_BOOTSTRAP_ID_2", "10", "10_BOOTSTRAP_ID_2"))
  # ids in transaction same as in cov
  expect_setequal(clv.sampled@data.cov.life$Id, clv.sampled@data.transactions$Id)
  expect_setequal(clv.sampled@data.cov.trans$Id, clv.sampled@data.transactions$Id)
  # columns in cov data correct
  expect_setequal(colnames(clv.sampled@data.cov.trans), c(dyn.cols, "Gender", "Channel"))
  expect_setequal(colnames(clv.sampled@data.cov.life), c(dyn.cols, "Gender"))
  # leaves full length of covariates. Same dates for each id, incl duplicated ids
  full.length.dates <- clv.apparel.dyn@data.cov.life[, unique(Cov.Date)]
  expect_true(all(clv.sampled@data.cov.life[, list(same=setequal(Cov.Date, full.length.dates)), by="Id"][, all(same)]))
  expect_true(all(clv.sampled@data.cov.trans[, list(same=setequal(Cov.Date, full.length.dates)), by="Id"][, all(same)]))
})


# model.specification.args -------------------------------------------------------------------------------------------

test_that("Correct model specification args for nocov models", {
  # start.params.model, optimx.args, verbose
  # use.cor, start.param.cor

  # No args
  p.cdnow <- pnbd(clv.cdnow)

  expect_mapequal(
    p.cdnow@model.specification.args,
    list(
      start.params.model = c(),
      optimx.args = list(),
      verbose=TRUE,
      use.cor=FALSE,
      start.param.cor = c()
    )
  )


  # With args
  # double expect_warning() because there are two exceptions but only one is
  # caught while the other will bubble up/out
  expect_warning(expect_warning(p.cdnow <- pnbd(
    clv.cdnow,
    start.params.model = c(r=0.5, alpha=15, s=0.5, beta=10),
    use.cor=TRUE,
    start.param.cor = c(cor=0.2),
    optimx.args = optimx.args.fast,
    verbose=FALSE), regexp = "Hessian"), regexp = "NA coefficients")

  expect_mapequal(
    p.cdnow@model.specification.args,
    list(
      start.params.model = c(r=0.5, alpha=15, s=0.5, beta=10),
      optimx.args = optimx.args.fast,
      verbose=FALSE,
      use.cor=TRUE,
      start.param.cor = c(cor=0.2)
    )
  )
})


test_that("Correct interface args for static covariate models", {
  # need to test correlation because not part of standard controlflow but
  # specific to pnbd static

  # No options
  p.apparel.static <- pnbd(clv.apparel.cov)

  expect_mapequal(
    p.apparel.static@model.specification.args,
    list(
      start.params.model = c(),
      optimx.args = list(),
      verbose = TRUE,
      names.cov.life = c(),
      names.cov.trans = c(),
      start.params.life = c(),
      start.params.trans = c(),
      names.cov.constr = c(),
      start.params.constr = c(),
      reg.weights = c(),
      use.cor = FALSE,
      start.param.cor = c())
  )


  # With options
  expect_warning(expect_warning(p.apparel.static <- pnbd(
    clv.apparel.cov,
    start.params.model = c(r=0.5, alpha=15, s=0.5, beta=10),
    use.cor=TRUE,
    start.param.cor = c(cor=0.1),
    optimx.args = optimx.args.fast,
    verbose=FALSE,
    names.cov.life="Gender",
    names.cov.trans=c("Gender", "Channel"),
    start.params.life = c(),
    start.params.trans = c(Channel=-0.345),
    names.cov.constr = "Gender",
    start.params.constr=c(Gender=0.23),
    reg.weights=c(life=8, trans=10)
    ), regexp = "Hessian"), regexp = "NA coefficients")

  expect_mapequal(
    p.apparel.static@model.specification.args,
    list(
      start.params.model = c(r=0.5, alpha=15, s=0.5, beta=10),
      optimx.args = optimx.args.fast,
      verbose = FALSE,
      names.cov.life="Gender",
      names.cov.trans=c("Gender", "Channel"),
      start.params.life = c(),
      start.params.trans = c(Channel=-0.345),
      names.cov.constr =  "Gender",
      start.params.constr=c(Gender=0.23),
      reg.weights = c(life=8, trans=10),
      use.cor = TRUE,
      start.param.cor = c(cor=0.1))
  )

})


test_that("Correct interface args for dynamic covariate models", {
  # only additional thing to test vs static cov is correlation

  # 3 warnings: c("Hessian", "NA coefficients", "cannot predict and plot")
  suppressWarnings(p.dyn <- pnbd(
    clv.apparel.dyn,
    optimx.args = optimx.args.fast,
    verbose = FALSE,
    start.params.model = c(r=0.5, alpha=15, s=0.5, beta=10),
    use.cor = TRUE,
    start.param.cor = c(cor=0.2),
    names.cov.life="Gender",
    names.cov.trans=c("Gender", "Channel"),
    start.params.life = c(),
    start.params.trans = c(Channel=-0.345),
    names.cov.constr = "Gender",
    start.params.constr=c(Gender=0.23),
    reg.weights=c(life=8, trans=10)
  ))

  expect_mapequal(
    p.dyn@model.specification.args,
    list(
      optimx.args = optimx.args.fast,
      verbose = FALSE,
      start.params.model = c(r=0.5, alpha=15, s=0.5, beta=10),
      use.cor = TRUE,
      start.param.cor = c(cor=0.2),
      names.cov.life="Gender",
      names.cov.trans=c("Gender", "Channel"),
      start.params.life = c(),
      start.params.trans = c(Channel=-0.345),
      names.cov.constr =  "Gender",
      start.params.constr=c(Gender=0.23),
      reg.weights = c(life=8, trans=10))
  )


})

test_that("Correct args for spending models", {

  # no args
  g.cdnow <- gg(clv.cdnow)

  expect_mapequal(
    g.cdnow@model.specification.args,
    list(
      start.params.model=NULL,
      optimx.args = list(),
      verbose = TRUE,
      remove.first.transaction=TRUE
    ))

  # with args
  g.cdnow <- gg(
    clv.cdnow,
    start.params.model = c(p=1.23, q=2.34, gamma=3.45),
    remove.first.transaction = FALSE,
    optimx.args = optimx.args.fast,
    verbose = FALSE
    )

  expect_mapequal(
    g.cdnow@model.specification.args,
    list(
      start.params.model=c(p=1.23, q=2.34, gamma=3.45),
      optimx.args = optimx.args.fast,
      verbose = FALSE,
      remove.first.transaction=FALSE
    ))

})


# clv.bootstrapped.apply ---------------------------------------------------------

# with holdout
p.cdnow <- fit.cdnow(cdnow, model=pnbd, optimx.args = optimx.args.fast)
bg.cdnow <- fit.cdnow(cdnow, model=bgnbd, optimx.args = optimx.args.fast)
bg.apparel.static <- fit.apparel.static(model = bgnbd, optimx.args = optimx.args.fast)
p.apparel.dyn <- fit.apparel.dyncov.quick(hessian=FALSE)
gg.cdnow <- fit.cdnow(cdnow, model=gg, optimx.args = optimx.args.fast)

test_that("Sampling all customers leads to same model estimate (nocov, static cov, dyncov, spending)", {

  for(clv.fitted in list(bg.cdnow, bg.apparel.static, p.apparel.dyn, gg.cdnow)){

    fn.sample.all <- function(){
      return(clv.bootstrapped.apply(clv.fitted, num.boots = 1, fn.boot.apply = coef, fn.sample = function(ids){return(ids)})[[1]])
    }

    if(is(clv.fitted, "clv.pnbd.dynamic.cov")){
      # re-uses optimx.args which does not calculate hessian
      expect_warning(boots.coef <- fn.sample.all(), regexp = 'Hessian')
    }else{
      expect_silent(boots.coef <- fn.sample.all())
    }

    expect_equal(coef(clv.fitted), boots.coef)
  }
})


test_that("Predict to same prediction end if given num periods even if sampled transactions would not define same estimation end", {
  dt.pred.boot <- clv.bootstrapped.apply(
    bg.cdnow,
    num.boots = 1,
    fn.boot.apply = function(clv.fitted){
      return(predict(clv.fitted, prediction.end=99, predict.spending=FALSE, verbose=FALSE))
      },
    # No Id which has the last transaction on the last date (1998-06-30)
    fn.sample = function(ids){return(as.character(1:10))})[[1]]


  # cannot simply compare content of predictions because not all customers are present
  expect_equal(
    unique(predict(bg.cdnow, prediction.end=99, predict.spending=FALSE, verbose=FALSE)[, c("period.first", "period.last", "period.length")]),
    unique(dt.pred.boot[, c("period.first", "period.last", "period.length")]))

})


# predict -----------------------------------------------------------------------------

test_that("Bootstrapped predictions have correct format", {
  set.seed(42)

  # Remains silent (no progress bar) with verbose=FALSE
  expect_warning(
    dt.pred.boots.1 <- predict(
      p.cdnow, # requires pnbd because dert not available for bgnbd
      verbose=FALSE,
      uncertainty="boots",
      num.boots=1,
      predict.spending=TRUE,
      level=0.7
    ),
    regexp = "1000 or more"
  )

  # No Id in prediction contains _BOOTSTRAP_ID_ anymore
  expect_length(dt.pred.boots.1[, grep(pattern = "BOOTSTRAP_ID", x = Id)], n = 0)

  # correct CI labels: Names based on level and for all predicted metrics
  ci.cols <- c('DERT', 'CET', "PAlive", "predicted.CLV", "predicted.mean.spending")
  ci.cols <- unlist(lapply(ci.cols, function(x){paste0(x, ".CI.", c(15, 85))}))
  expect_contains(colnames(dt.pred.boots.1), ci.cols)

  # Customers which are not sampled are still present in the predictions
  # There is no guarantee that not all customers are sampled but its very, very
  # unlikely that all customers are sampled (fixed with set.seed)
  expect_setequal(dt.pred.boots.1$Id, bg.cdnow@cbs$Id)
  # some are not sampled and have NAs in the CI columns (also relevant to make
  # sure some were indeed not sampled and then not lost)
  expect_true(anyNA(dt.pred.boots.1[, "CET.CI.15"]))


})

test_that("Bootstrapped predictions lower CI < upper CI", {
  set.seed(42)

  expect_warning(
    dt.pred.boots.5 <- predict(
      p.cdnow, # requires pnbd because dert not available for bgnbd
      verbose=FALSE,
      uncertainty="boots",
      num.boots=5,
      predict.spending=TRUE,
      level=0.7
    ),
    regexp = "1000 or more"
  )

  # only such where there are predictions
  dt.pred.boots.5 <- dt.pred.boots.5[rowSums(is.na(dt.pred.boots.5), na.rm = FALSE) == 0]

  # exclude ids which were sampled only once
  dt.pred.boots.5 <- dt.pred.boots.5[CET.CI.15 != CET.CI.85]

  # Cannot check PAlive because for zero repeaters (x=0) there are no repeat
  # transactions to sample and their PAlive will always be the same
  ci.cols <- c('DERT', 'CET', "predicted.CLV", "predicted.mean.spending")
  for(col in ci.cols){
    expect_true(all(
      dt.pred.boots.5[[paste0(col, ".CI.15")]] < dt.pred.boots.5[[paste0(col, ".CI.85")]]
    ))
  }
})

