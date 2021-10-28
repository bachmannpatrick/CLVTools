fct.helper.dyncov.quickfit <- function(data.apparelTrans, data.apparelDynCov){
  # Create dyncov model, quickly
  expect_silent(clv.apparel <- clvdata(data.transactions = data.apparelTrans, date.format = "ymd",
                                       time.unit = "w",estimation.split = 38))
  expect_message(clv.apparel.dyn  <- SetDynamicCovariates(clv.apparel,name.id = "Id",name.date = "Cov.Date",
                                                          data.cov.life  = data.apparelDynCov,names.cov.life = c("Marketing", "Gender", "Channel"),
                                                          data.cov.trans = data.apparelDynCov, names.cov.trans = c("Marketing", "Gender", "Channel")),
                 regexp = "cut off")

  expect_warning(p.dyncov <- pnbd(clv.apparel.dyn, start.params.model = c(r=1, alpha=3, s=1, beta=3),
                                  optimx.args = list(method="Nelder-Mead", # NelderMead verifies nothing = faster
                                                     hessian=FALSE, # no hessian
                                                     control=list(kkt=FALSE, # kkt takes forever
                                                                  reltol = 1000)),
                                  verbose = FALSE),
                 regexp = "Hessian")
  return(p.dyncov)
}


fct.helper.create.clvdata.apparel.dyncov <- function(data.apparelTrans,  data.apparelDynCov, estimation.end){
  expect_silent(clv.dyn <- clvdata(data.apparelTrans, date.format = "ymd", time.unit = "w", estimation.split = estimation.end))
  expect_message(clv.dyn <- SetDynamicCovariates(clv.dyn, data.cov.life = data.apparelDynCov, data.cov.trans = data.apparelDynCov,
                                                 names.cov.life = c("Marketing", "Gender", "Channel"), names.cov.trans = c("Marketing", "Gender", "Channel"),
                                                 name.date = "Cov.Date"), regexp = "cut")
  return(clv.dyn)
}

fct.helper.dyncov.load.fitted <- function(){
  # Created using fct.helper.dyncov.quickfit
  return(readRDS(file = "fitted_dyncov.rds"))
}

fct.helper.dyncov.fit.LL.once <- function(clv.data, params){

  obj <- clv.pnbd.dynamic.cov(cl = str2lang("fakecall"), clv.data=clv.data)

  l.walks <- pnbd_dyncov_makewalks(clv.data = clv.data)
  obj@data.walks.life  <- l.walks[["data.walks.life"]]
  obj@data.walks.trans <- l.walks[["data.walks.trans"]]

  obj <- clv.controlflow.estimate.put.inputs(clv.fitted=obj, verbose=FALSE, reg.lambdas=NULL, names.cov.constr=NULL,
                                             names.cov.life  = clv.data@names.cov.data.life,
                                             names.cov.trans = clv.data@names.cov.data.trans)
  return(pnbd_dyncov_LL(params = params,
                        clv.fitted = obj,
                        return.all.intermediate.results = TRUE))
}

fct.testthat.correctness.dyncov.expectation <- function(data.apparelTrans, data.apparelDynCov){
  skip_on_cran()

  clv.dyncov <- fct.helper.dyncov.load.fitted()

  # For customer 1041, set all dyncov data to 0
  data.apparelDynCov <- copy(data.apparelDynCov)
  data.apparelDynCov[Id == "1041", Marketing := 0]
  data.apparelDynCov[Id == "1041", Gender    := 0]
  data.apparelDynCov[Id == "1041", Channel   := 0]

  clv.dyncov@clv.data@data.cov.life  <- copy(data.apparelDynCov)
  clv.dyncov@clv.data@data.cov.trans <- copy(data.apparelDynCov)
  # clv.dyncov <- fct.helper.dyncov.quickfit(data.apparelTrans = data.apparelTrans, data.apparelDynCov = data.apparelDynCov)


  # Same params for life and trans to check Bbar_i = Dbar_i
  clv.dyncov@prediction.params.life  <- c(Marketing = 1.23, Gender = 0.678, Channel = 2.34)
  clv.dyncov@prediction.params.trans <- c(Marketing = 1.23, Gender = 0.678, Channel = 2.34)

  expect_silent(dt.expectation.seq <- clv.time.expectation.periods(clv.time = clv.dyncov@clv.data@clv.time,
                                                                   user.tp.end = NULL))
  expect_silent(dt.expectation <- CLVTools:::pnbd_dyncov_expectation(clv.fitted = clv.dyncov,
                                                                     dt.expectation.seq = dt.expectation.seq,
                                                                     verbose = FALSE,
                                                                     only.return.input.to.expectation = TRUE))

  test_that("d_omega = d1", {
    expect_true(dt.expectation[, all(d_omega == d1)])
  })

  test_that("If cov data = 0, Ai and Ci = 0", {
    # For customer 1041 with all cov data = 0, all Ai and Ci have to be exp(0)=1
    expect_true(dt.expectation[Id == "1041", all(Ai == 1)])
    expect_true(dt.expectation[Id == "1041", all(Ci == 1)])
  })

  test_that("If cov data = 0, Dbar_i = Bbar_i = 0", {
    # use all.equal to have tolerance because not exactly 0
    expect_true(dt.expectation[Id == "1041", isTRUE(all.equal(rep(0, .N),Bbar_i))])
    expect_true(dt.expectation[Id == "1041", isTRUE(all.equal(rep(0, .N),Dbar_i))])
  })

  test_that("For all i=1, Bbar_i = 0 and Dbar_i = 0", {
    # use all.equal to have tolerance because not exactly 0
    expect_true(dt.expectation[i == 1, isTRUE(all.equal(rep(0, .N),Bbar_i))])
    expect_true(dt.expectation[i == 1, isTRUE(all.equal(rep(0, .N),Dbar_i))])
  })

  test_that("For the same covariate data, Bbar_i=Dbar_i", {
    # All customers have the same covariate in the lifetime and transaction process
    #   and params are the same. Therefore, everywhere Bbar_i=Dbar_i
    expect_true(isTRUE(all.equal(dt.expectation[, .(Xbar_i = Bbar_i)],
                                 dt.expectation[, .(Xbar_i = Dbar_i)])))
  })

  test_that("i is integer and the same max for all customers", {
    expect_true(dt.expectation[, is.integer(i)])
    expect_true(dt.expectation[, .(max_i = max(i)), by="Id"][, uniqueN(max_i)  == 1])
  })

  test_that("All customers start and end on same Date", {
    expect_true(dt.expectation[, .(min_cov = min(Cov.Date)), by="Id"][, uniqueN(min_cov) == 1])
    expect_true(dt.expectation[, .(max_cov = max(Cov.Date)), by="Id"][, uniqueN(max_cov) == 1])
    expect_true(dt.expectation[, .(num_cov = .N),            by="Id"][, uniqueN(num_cov) == 1])
  })

  # All params = 0 to check Ai=Ci=1 and
  clv.dyncov@prediction.params.life  <- c(Marketing = 0, Gender = 0, Channel = 0)
  clv.dyncov@prediction.params.trans <- c(Marketing = 0, Gender = 0, Channel = 0)

  expect_silent(dt.expectation <- CLVTools:::pnbd_dyncov_expectation(clv.fitted = clv.dyncov,
                                                                     dt.expectation.seq = dt.expectation.seq,
                                                                     verbose = FALSE,
                                                                     only.return.input.to.expectation = TRUE))


  test_that("For all cov params = 0, all Ai = Ci = 1 and all Bbar_i = Dbar_i = 0", {
    expect_true(dt.expectation[, isTRUE(all.equal(rep(1, .N),Ai))])
    expect_true(dt.expectation[, isTRUE(all.equal(rep(1, .N),Ci))])
    expect_true(dt.expectation[, isTRUE(all.equal(rep(0, .N),Bbar_i))])
    expect_true(dt.expectation[, isTRUE(all.equal(rep(0, .N),Dbar_i))])
  })


}


fct.testthat.correctness.dyncov.LL <- function(data.apparelTrans, data.apparelDynCov){

  fct.verify.LL.intermediate.results <- function(LL.out, A, C){

    expect_true(LL.out[, isTRUE(all.equal(Akprod, A^x))])
    expect_true(LL.out[, isTRUE(all.equal(Bksum,  A*T.cal))])
    # barBi = -A*t.x, barDi=0 -> individual i not in data, but checked as part of a*T (barBi) and DkT (barDi)

    # a1T, b1T
    # aT* (paper) = a1T (paper, when k_T=1) = aT
    # bT* (paper)=  b1T (paper, whnn k_T=1) = bT.
    expect_true(LL.out[, isTRUE(all.equal(aT,  A*T.cal))])
    expect_true(LL.out[, isTRUE(all.equal(bT,  C*T.cal))])

    # *** TODO: DYNCOV LL wrong for bkT
    # expect_true(LL.out[, isTRUE(all.equal(bkT, C*T.cal))])
    expect_true(LL.out[, isTRUE(all.equal(DkT, C*T.cal))])
  }

  test_that("Dyncov LL yields correct intemdiate results",{
    skip_on_cran()

    clv.dyncov <- fct.helper.dyncov.load.fitted()
    params.model <- c(log.r=1, log.alpha=0, log.s=1.23, log.beta = 2.344)

    # Gamma=0 ------------------------------------------------------------------------------------------------
    LL.out.gamma.0 <- pnbd_dyncov_LL(params = c(params.model,
                                                life.Channel  = 0, life.Gender  = 0, life.Marketing  = 0,
                                                trans.Channel = 0, trans.Gender = 0, trans.Marketing = 0),
                                     clv.fitted = clv.dyncov,
                                     return.all.intermediate.results = TRUE)

    fct.verify.LL.intermediate.results(LL.out = LL.out.gamma.0, A = exp(0), C = exp(0))
    # Same LL values as nocov
    expect_equal(LL.out.gamma.0$LL, drop(pnbd_nocov_LL_ind(vLogparams = params.model,
                                                           vX = clv.dyncov@cbs$x, vT_x = clv.dyncov@cbs$t.x,
                                                           vT_cal = clv.dyncov@cbs$T.cal)))

    # Dyncov Data is static ----------------------------------------------------------------------------------
    apparelDynCov.static <- copy(data.apparelDynCov)
    apparelDynCov.static[, Gender    := sample(0:2, size = 1), by="Id"]
    apparelDynCov.static[, Channel   := sample(0:2, size = 1), by="Id"]
    apparelDynCov.static[, Marketing := sample(0:2, size = 1), by="Id"]
    clv.data.dyn <- copy(clv.dyncov@clv.data)
    clv.data.dyn@data.cov.life  <- copy(apparelDynCov.static)
    clv.data.dyn@data.cov.trans <- copy(apparelDynCov.static)
    l.walks <- pnbd_dyncov_makewalks(clv.data = clv.data.dyn)
    clv.dyncov@data.walks.life  <- copy(l.walks$data.walks.life)
    clv.dyncov@data.walks.trans <- copy(l.walks$data.walks.trans)

    LL.out.static.cov <- pnbd_dyncov_LL(params = c(params.model,
                                                   life.Channel  = 0.123, life.Gender  = 0.678, life.Marketing  = 1.234,
                                                   trans.Channel = 0.111, trans.Gender = 2.222, trans.Marketing = 1.756),
                                        clv.fitted = clv.dyncov,
                                        return.all.intermediate.results = TRUE)

    dt.A <- clv.data.dyn@data.cov.trans[, .(A=head(exp(0.111*Channel+2.222*Gender+1.756*Marketing), 1)), keyby="Id"]
    dt.C <- clv.data.dyn@data.cov.life[,  .(C=head(exp(0.123*Channel+0.678*Gender+1.234*Marketing), 1)), keyby="Id"]

    fct.verify.LL.intermediate.results(LL.out = LL.out.static.cov,
                                       A = dt.A$A,
                                       C = dt.C$C)
    # Same LL values as staticcov
    m.cov <- data.matrix(apparelDynCov.static[, head(.SD, 1), keyby="Id"][, c("Channel", "Gender", "Marketing")])
    expect_equal(LL.out.static.cov$LL, drop(pnbd_staticcov_LL_ind(vParams =  c(params.model,
                                                                               life.Channel  = 0.123, life.Gender  = 0.678, life.Marketing  = 1.234,
                                                                               trans.Channel = 0.111, trans.Gender = 2.222, trans.Marketing = 1.756),
                                                                  vX = clv.dyncov@cbs$x, vT_x = clv.dyncov@cbs$t.x, vT_cal = clv.dyncov@cbs$T.cal,
                                                                  mCov_life = m.cov, mCov_trans = m.cov)))
  })

  test_that("Dyncov LL same if there is holdout and no holdout <==> if there are more covariates than required",{
    skip_on_cran()

    # data until 2005-12-31
    clv.short <- fct.helper.create.clvdata.apparel.dyncov(data.apparelTrans = data.apparelTrans[Date <= "2005-12-31"],
                                                          data.apparelDynCov = data.apparelDynCov[Cov.Date <= "2005-12-31"],
                                                          estimation.end = NULL)

    # Short transaction data but full dyncov covariate data
    clv.full.cov <- fct.helper.create.clvdata.apparel.dyncov(data.apparelTrans = data.apparelTrans[Date <= "2005-12-31"],
                                                             data.apparelDynCov = data.apparelDynCov,
                                                             estimation.end = NULL)
    # Full data but estimation period only same as short
    clv.holdout <- fct.helper.create.clvdata.apparel.dyncov(data.apparelTrans = data.apparelTrans,
                                                            data.apparelDynCov = data.apparelDynCov,
                                                            estimation.end = "2005-12-31")

    params.dyncov <- c(log.r=1, log.alpha=0, log.s=1.23, log.beta = 2.344,
                       life.Channel  = 0.123, life.Gender  = 0.234, life.Marketing  = 0.345,
                       trans.Channel = 0.456, trans.Gender = 0.567, trans.Marketing = 0.678)

    # **Add back once walk creation is fixed
    # expect_equal(fct.helper.dyncov.fit.LL.once(clv.data = clv.short, params = params.dyncov),
    #              fct.helper.dyncov.fit.LL.once(clv.data = clv.full.cov, params = params.dyncov))

    expect_equal(fct.helper.dyncov.fit.LL.once(clv.data = clv.full.cov, params = params.dyncov),
                 fct.helper.dyncov.fit.LL.once(clv.data = clv.holdout,  params = params.dyncov))
  })
}

fct.testthat.correctness.dyncov.CET <- function(data.apparelTrans, data.apparelDynCov){
  skip_on_cran()

  # For constant covariates (ie static)
  data.apparelDynCov <- copy(data.apparelDynCov)
  # Set static cov by Id
  data.apparelDynCov[, Marketing := sample(x = c(0, 1), size = 1), by="Id"]
  data.apparelDynCov[, Gender    := sample(x = c(0, 1), size = 1), by="Id"]
  data.apparelDynCov[, Channel   := sample(x = c(0, 1), size = 1), by="Id"]

  clv.dyncov <- fct.helper.dyncov.load.fitted()
  clv.dyncov@clv.data@data.cov.life  <- copy(data.apparelDynCov)
  clv.dyncov@clv.data@data.cov.trans <- copy(data.apparelDynCov)
  clv.dyncov@prediction.params.life  <- c(Marketing = 1.23, Gender = 0.678, Channel = 2.34)
  clv.dyncov@prediction.params.trans <- c(Marketing = 0.999, Gender = 0.111, Channel = 2.222)


  dt.prediction.time.table <- clv.time.get.prediction.table(clv.time = clv.dyncov@clv.data@clv.time,
                                                            user.prediction.end = NULL)
  dt.CET <- pnbd_dyncov_CET(clv.fitted = clv.dyncov, predict.number.of.periods = dt.prediction.time.table[1, period.length],
                            prediction.end.date = dt.prediction.time.table[1, period.last],
                            only.return.input.to.CET = TRUE)

  test_that("For static cov, Ai=static, Ci=static", {
    expect_true(dt.CET[, .(num_ai = uniqueN(Ai)), by = "Id"][, all(num_ai == 1)])
    expect_true(dt.CET[, .(num_ci = uniqueN(Ci)), by = "Id"][, all(num_ci == 1)])
  })

  test_that("For static cov, Dbar_i = 0", {
    expect_true(dt.CET[, isTRUE(all.equal(Dbar_i, rep(0, .N)))])
  })

  test_that("For static cov, Bbar_i=-T*A", {
    expect_true(dt.CET[, isTRUE(all.equal(Bbar_i, -T.cal*Ai)), by="Id"][, all(V1 == TRUE)])
  })

  test_that("CET = 0 for prediction period = 0", {
    clv.dyncov@prediction.params.model["s"] <- 1.5 # s=1 fails mathematically
    dt.CET.0 <- pnbd_dyncov_CET(clv.fitted = clv.dyncov,
                                predict.number.of.periods = 0,
                                prediction.end.date = clv.dyncov@clv.data@clv.time@timepoint.holdout.start,
                                only.return.input.to.CET = FALSE)
    expect_true(dt.CET.0[, all(CET == 0)])
  })

}

fct.testthat.correctness.dyncov <- function(data.apparelTrans, data.apparelDynCov){

  context("Correctness - PNBD dyncov - Expectation")
  fct.testthat.correctness.dyncov.expectation(data.apparelTrans = data.apparelTrans, data.apparelDynCov = data.apparelDynCov)

  context("Correctness - PNBD dyncov - CET")
  fct.testthat.correctness.dyncov.CET(data.apparelTrans = data.apparelTrans, data.apparelDynCov = data.apparelDynCov)

  context("Correctness - PNBD dyncov - LL")
  fct.testthat.correctness.dyncov.LL(data.apparelTrans = data.apparelTrans, data.apparelDynCov = data.apparelDynCov)
}
