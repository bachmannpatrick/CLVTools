fct.helper.dyncov.quickfit.apparel.data <- function(data.apparelTrans, data.apparelDynCov, hessian=FALSE){
  clv.apparel.dyn <- fct.helper.create.clvdata.apparel.dyncov(data.apparelTrans=data.apparelTrans,  data.apparelDynCov=data.apparelDynCov, estimation.end=38)
  fitted.dyncov <- fct.helper.dyncov.quickfit(clv.apparel.dyn, hessian=hessian)

  # Cheat and set a fake hessian as it was not estimated during optimization for speed reasons
  #   from the same quickfit with hessian=T
  # fitted.dyncov@optimx.hessian <- structure(c(297.816319061493, -167.25149771185, -31.1871793488259, 20.6861676370988, -1.58639809655043, -12.8221347285802, -4.94089249955333, 55.8052753176486, 123.954642184586, 81.2903073168579,
  #                                             -167.25149771185, 95.3476714370161, 12.9167310849014, -10.1137198331798, 0.424460467632814, 6.37531286735515, 2.44102023788079, -33.6105840548744, -79.6231571791094, -49.7391927152517,
  #                                             -31.1871793488259, 12.9167310849014, 379.471103854916, -153.375279198312, 53.9449467436583, 115.32150287651, 77.5101682093185,  -6.3319394400015, -8.32942983004706, -3.62426045915698,
  #                                             20.6861676370988, -10.1137198331798, -153.375279198312, 35.3637997261069, -8.48000837221749, -25.6663662320161, -14.8103647360409, 2.41903914201104, 6.37531270788257, 2.4410199256767,
  #                                             -1.58639809655043, 0.424460467632814, 53.9449467436583, -8.48000837221749, 37.1492244614716, 5.86660037871061, 3.63233840338311, -2.5812278842046, -0.205974188766537, 0.0130428435022102,
  #                                             -12.8221347285802, 6.37531286735515, 115.32150287651, -25.6663662320161, 5.86660037871061,  25.666362989592, 10.9813500456409, -1.29314556417944, -6.37531003596571, -1.27313681231238,
  #                                             -4.94089249955333, 2.44102023788079, 77.5101682093185, -14.8103647360409, 3.63233840338311, 10.9813500456409, 14.8103619343572, -0.619168654976496, -1.27313616513791, -2.44101785877623,
  #                                             55.8052753176486, -33.6105840548744, -6.3319394400015, 2.41903914201104, -2.5812278842046, -1.29314556417944, -0.619168654976496, 299.362652279345, 29.2516552513793, 17.3123174944356,
  #                                             123.954642184586, -79.6231571791094, -8.32942983004706,  6.37531270788257, -0.205974188766537, -6.37531003596571, -1.27313616513791, 29.2516552513793, 79.6231520568093, 42.6967589924783,
  #                                             81.2903073168579, -49.7391927152517, -3.62426045915698, 2.4410199256767, 0.0130428435022102, -1.27313681231238, -2.44101785877623, 17.3123174944356, 42.6967589924783, 49.7391916698474),
  #                                           .Dim = c(10L, 10L),
  #                                           .Dimnames = list(c("log.r", "log.alpha", "log.s", "log.beta", "life.Marketing", "life.Gender", "life.Channel", "trans.Marketing", "trans.Gender", "trans.Channel"),
  #                                                            c("log.r",  "log.alpha", "log.s", "log.beta", "life.Marketing", "life.Gender", "life.Channel", "trans.Marketing", "trans.Gender", "trans.Channel")))

  return(fitted.dyncov)
}

fct.helper.dyncov.quickfit <- function(clv.data.dyn, hessian){

  l.args <- list(clv.data=clv.data.dyn,
                 start.params.model = c(r=1, alpha=3, s=1, beta=3),
                 optimx.args = list(method="Nelder-Mead", # Nelder-Mead verifies nothing = faster
                                    itnmax=2,
                                    hessian=hessian,
                                    control=list(kkt=FALSE, # kkt will always demand hessian
                                                 reltol = 1000)),
                 verbose = FALSE)

  if(hessian){
    expect_silent(p.dyncov <- do.call(pnbd, l.args))
  }else{
    expect_warning(p.dyncov <- do.call(pnbd, l.args), regexp = "Hessian")
  }
  return(p.dyncov)
}


fct.helper.dyncov.get.optimxargs.quickfit <- function(){
  return(list(method="Nelder-Mead", # NelderMead verifies nothing = faster
              itnmax = 2,
              hessian=FALSE, # no hessian
              control=list(kkt = FALSE, # kkt takes forever because requires hessian
                           reltol = 1000))) # anything counts as converged
}


fct.helper.create.clvdata.apparel.dyncov <- function(data.apparelTrans,  data.apparelDynCov, estimation.split){
  expect_silent(clv.dyn <- clvdata(data.apparelTrans, date.format = "ymd", time.unit = "w", estimation.split = estimation.split))
  expect_message(clv.dyn <- SetDynamicCovariates(clv.dyn, data.cov.life = data.apparelDynCov, data.cov.trans = data.apparelDynCov,
                                                 names.cov.life = c("Marketing", "Gender", "Channel"), names.cov.trans = c("Marketing", "Gender", "Channel"),
                                                 name.date = "Cov.Date"), regexp = "cut")
  return(clv.dyn)
}

fct.helper.dyncov.create.longer.dyncov.data <- function(num.additional, data.apparelDynCov){

  # Add additional <num.additional> weeks of fake cov data for all Ids
  dt.additional.cov <- expand.grid(Id = unique(apparelDynCov$Id),
                                   Cov.Date = seq(from=apparelDynCov[, max(Cov.Date)]+lubridate::weeks(1),
                                                  length.out = num.additional, by = "week"), stringsAsFactors = FALSE)
  setDT(dt.additional.cov)
  dt.additional.cov[, Marketing := rep(c(0,1,2,3),.N/4)]
  dt.additional.cov[, Gender    := rep(c(0,1),.N/2)]
  dt.additional.cov[, Channel   := rep(c(0,1),.N/2)]

  return(rbindlist(list(data.apparelDynCov, dt.additional.cov), use.names = TRUE))
}

fct.helper.dyncov.LLdata.from.clvdata <- function(clv.data, params){
  return(pnbd_dyncov_getLLdata(clv.fitted=fct.helper.dyncov.quickfit(clv.data, hessian=FALSE), params=params))
}

fct.testthat.correctness.dyncov.expectation <- function(data.apparelTrans, data.apparelDynCov){
  skip_on_cran()

  # For customer 1041, set all dyncov data to 0
  data.apparelDynCov <- copy(data.apparelDynCov)
  data.apparelDynCov[Id == "1041", Marketing := 0]
  data.apparelDynCov[Id == "1041", Gender    := 0]
  data.apparelDynCov[Id == "1041", Channel   := 0]

  p.dyn <- fct.helper.dyncov.quickfit.apparel.data(data.apparelTrans = data.apparelTrans, data.apparelDynCov = data.apparelDynCov)


  # Same params for life and trans to check Bbar_i = Dbar_i
  p.dyn@prediction.params.life  <- c(Marketing = 1.23, Gender = 0.678, Channel = 2.34)
  p.dyn@prediction.params.trans <- c(Marketing = 1.23, Gender = 0.678, Channel = 2.34)

  expect_silent(dt.expectation.seq <- clv.time.expectation.periods(clv.time = p.dyn@clv.data@clv.time,
                                                                   user.tp.end = NULL))
  expect_silent(dt.expectation <- CLVTools:::pnbd_dyncov_expectation(clv.fitted = p.dyn,
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
  p.dyn@prediction.params.life  <- c(Marketing = 0, Gender = 0, Channel = 0)
  p.dyn@prediction.params.trans <- c(Marketing = 0, Gender = 0, Channel = 0)

  expect_silent(dt.expectation <- CLVTools:::pnbd_dyncov_expectation(clv.fitted = p.dyn,
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

  fct.verify.LL.intermediate.results <- function(dt.LLdata, dt.A, dt.C, dt.cbs){
    dt.LLdata[dt.cbs, x     := x,      on="Id"]
    dt.LLdata[dt.cbs, T.cal := T.cal,  on="Id"]
    dt.LLdata[dt.A, A:=i.A, on="Id"]
    dt.LLdata[dt.C, C:=i.C, on="Id"]

    expect_true(dt.LLdata[, isTRUE(all.equal(Akprod, A^x))])
    expect_true(dt.LLdata[, isTRUE(all.equal(Bksum,  A*T.cal))])
    # barBi = -A*t.x, barDi=0 -> individual i not in data, but checked as part of a*T (barBi) and DkT (barDi)

    # a1T, b1T
    # aT* (paper) = a1T (paper, when k_T=1) = aT
    # bT* (paper)=  b1T (paper, whnn k_T=1) = bT.
    #   subset for is.na() because is not calculated where aux_walk.n_elem == 1
    expect_true(dt.LLdata[!is.na(aT), isTRUE(all.equal(aT,  A*T.cal))])
    expect_true(dt.LLdata[!is.na(bT), isTRUE(all.equal(bT,  C*T.cal))])

    # DkT = CkT * c.T_cal + DT
    # bkT = DT + CkT * (c.t_x + dT + n_walks - 2.0);
    # DkT is correct =>  DT and CkT are correct -> only dT could be wrong
    expect_true(dt.LLdata[!is.na(bkT), isTRUE(all.equal(bkT, C*T.cal))]) # *** TODO: DYNCOV LL wrong for bkT. likely dT
    expect_true(dt.LLdata[, isTRUE(all.equal(DkT, C*T.cal))])
  }

  test_that("Dyncov LL yields correct intemdiate results",{
    skip_on_cran()

    p.dyn <- fct.helper.dyncov.quickfit.apparel.data(data.apparelTrans = apparelTrans, data.apparelDynCov = apparelDynCov)
    params.model <- c(log.r=-1, log.alpha=0, log.s=1.23, log.beta = 2.344)

    # Gamma=0 ------------------------------------------------------------------------------------------------
    dt.LLdata.gamma.0 <- pnbd_dyncov_getLLdata(p.dyn, params = c(params.model,
                                                                 life.Marketing  = 0, life.Gender  = 0, life.Channel  = 0,
                                                                 trans.Marketing = 0, trans.Gender = 0, trans.Channel = 0))

    # dyncov intermediate results are correct
    dt.A <- data.table(Id=dt.LLdata.gamma.0$Id, A=exp(0))
    dt.C <- data.table(Id=dt.LLdata.gamma.0$Id, C=exp(0))
    fct.verify.LL.intermediate.results(dt.LLdata = dt.LLdata.gamma.0, dt.A = dt.A, dt.C = dt.C, dt.cbs=p.dyn@cbs)

    # # vs nocov: same LL values
    expect_equal(dt.LLdata.gamma.0$LL, drop(pnbd_nocov_LL_ind(vLogparams = params.model,
                                                              vX = p.dyn@cbs$x, vT_x = p.dyn@cbs$t.x,
                                                              vT_cal = p.dyn@cbs$T.cal)))


    # # Dyncov data is static ----------------------------------------------------------------------------------
    apparelDynCov.static <- copy(data.apparelDynCov)
    apparelDynCov.static[, Gender    := sample(0:2, size = 1), by="Id"]
    apparelDynCov.static[, Channel   := sample(0:2, size = 1), by="Id"]
    apparelDynCov.static[, Marketing := sample(0:2, size = 1), by="Id"]

    p.dyn.static <- fct.helper.dyncov.quickfit.apparel.data(data.apparelTrans = data.apparelTrans, data.apparelDynCov = apparelDynCov.static)
    params.static.cov <- c(params.model,
                           life.Marketing  = 0.123, life.Gender  = 0.678, life.Channel = 1.234,
                           trans.Marketing = 0.111, trans.Gender = 2.222, trans.Channel= 1.756)
    dt.LLdata.static.cov <- pnbd_dyncov_getLLdata(p.dyn.static, params=params.static.cov)

    dt.A <- p.dyn.static@data.walks.trans.aux[, .(A=head(exp(0.111*Marketing+2.222*Gender+1.756*Channel), 1)), keyby="Id"]
    dt.C <- p.dyn.static@data.walks.life.aux[,  .(C=head(exp(0.123*Marketing+0.678*Gender+1.234*Channel), 1)), keyby="Id"]
    fct.verify.LL.intermediate.results(dt.LLdata = dt.LLdata.static.cov, dt.A = dt.A, dt.C = dt.C, dt.cbs=p.dyn@cbs)

    # Same LL values as staticcov
    m.cov <- data.matrix(apparelDynCov.static[, head(.SD, 1), keyby="Id"][, c("Marketing", "Gender", "Channel")])
    expect_equal(dt.LLdata.static.cov$LL, drop(pnbd_staticcov_LL_ind(vParams = params.static.cov,
                                                                  vX = p.dyn.static@cbs$x, vT_x = p.dyn.static@cbs$t.x, vT_cal = p.dyn.static@cbs$T.cal,
                                                                  mCov_life = m.cov, mCov_trans = m.cov)))
  })

  test_that("Dyncov LL same if there is holdout and no holdout <==> if there are more covariates than required",{
    skip_on_cran()

    # data until 2005-12-31
    clv.short <- fct.helper.create.clvdata.apparel.dyncov(data.apparelTrans = data.apparelTrans[Date <= "2005-12-31"],
                                                          data.apparelDynCov = data.apparelDynCov[Cov.Date <= "2005-12-31"],
                                                          estimation.split = NULL)

    # Short transaction data but full dyncov covariate data
    clv.full.cov <- fct.helper.create.clvdata.apparel.dyncov(data.apparelTrans = data.apparelTrans[Date <= "2005-12-31"],
                                                             data.apparelDynCov = data.apparelDynCov,
                                                             estimation.split = NULL)
    # Full data but estimation period only same as short
    clv.holdout <- fct.helper.create.clvdata.apparel.dyncov(data.apparelTrans = data.apparelTrans,
                                                            data.apparelDynCov = data.apparelDynCov,
                                                            estimation.split = "2005-12-31")

    params.dyncov <- c(log.r=1, log.alpha=0, log.s=1.23, log.beta = 2.344,
                       life.Marketing  = 0.123, life.Gender  = 0.234,  life.Channel= 0.345,
                       trans.Marketing = 0.456, trans.Gender = 0.567,  trans.Channel= 0.678)

    expect_equal(fct.helper.dyncov.LLdata.from.clvdata(clv.data = clv.short,    params = params.dyncov),
                 fct.helper.dyncov.LLdata.from.clvdata(clv.data = clv.full.cov, params = params.dyncov))

    expect_equal(fct.helper.dyncov.LLdata.from.clvdata(clv.data = clv.full.cov, params = params.dyncov),
                 fct.helper.dyncov.LLdata.from.clvdata(clv.data = clv.holdout,  params = params.dyncov))
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

  p.dyn <- fct.helper.dyncov.quickfit.apparel.data(data.apparelTrans = data.apparelTrans, data.apparelDynCov = data.apparelDynCov)
  p.dyn@clv.data@data.cov.life  <- copy(data.apparelDynCov)
  p.dyn@clv.data@data.cov.trans <- copy(data.apparelDynCov)
  p.dyn@prediction.params.life  <- c(Marketing = 1.23, Gender = 0.678, Channel = 2.34)
  p.dyn@prediction.params.trans <- c(Marketing = 0.999, Gender = 0.111, Channel = 2.222)


  dt.prediction.time.table <- clv.time.get.prediction.table(clv.time = p.dyn@clv.data@clv.time,
                                                            user.prediction.end = NULL)
  dt.CET <- pnbd_dyncov_CET(clv.fitted = p.dyn, predict.number.of.periods = dt.prediction.time.table[1, period.length],
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
    p.dyn@prediction.params.model["s"] <- 1.5 # s=1 fails mathematically
    dt.CET.0 <- pnbd_dyncov_CET(clv.fitted = p.dyn,
                                predict.number.of.periods = 0,
                                prediction.end.date = p.dyn@clv.data@clv.time@timepoint.holdout.start,
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
