# Methods to create clv.data objects and do model fitting with defaults.
# To be used in many test files.
#
# Similar to what "fixtures" are in pytest but do not name file "fixtures"
# as they have a different meaning in testthat.
# Instead name "arrange", as in where we prepare everything for the tests.


.load.data.locally <- function(d) {
  e <- new.env()
  data(list = d, envir = e)
  return(e[[d]])
}

fct.helper.load.cdnow <- function(){.load.data.locally("cdnow")}
fct.helper.load.apparelTrans <- function(){.load.data.locally("apparelTrans")}
fct.helper.load.apparelStaticCov <- function(){.load.data.locally("apparelStaticCov")}
fct.helper.load.apparelDynCov <- function(){.load.data.locally("apparelDynCov")}



fct.helper.create.clvdata.cdnow <- function(data.cdnow = NULL, estimation.split = 37) {
  if (is.null(data.cdnow)) {
    data.cdnow <- fct.helper.load.cdnow()
  }
  clv.cdnow <- clvdata(
    data.transactions = data.cdnow,
    date.format = "ymd",
    time.unit = "w",
    estimation.split = estimation.split
  )
  return(clv.cdnow)
}

fct.helper.create.clvdata.apparel.nocov <- function(
    data.apparelTrans = NULL,
    estimation.split = 104) {

  if (is.null(data.apparelTrans)) {
    data.apparelTrans <- fct.helper.load.apparelTrans()
  }

  return(clvdata(
    data.transactions = data.apparelTrans,
    date.format = "ymd",
    time.unit = "W",
    estimation.split = estimation.split
    ))
}

fct.helper.create.clvdata.apparel.staticcov <- function(
    data.apparelTrans = NULL,
    data.apparelStaticCov = NULL,
    estimation.split = 104,
    names.cov.life = c("Gender", "Channel"),
    names.cov.trans = c("Gender", "Channel")) {
  if (is.null(data.apparelTrans)) {
    data.apparelTrans <- fct.helper.load.apparelTrans()
  }
  if (is.null(data.apparelStaticCov)) {
    data.apparelStaticCov <- fct.helper.load.apparelStaticCov()
  }

  return(SetStaticCovariates(
    clvdata(
      data.transactions = data.apparelTrans, date.format = "ymd", time.unit = "W",
      estimation.split = estimation.split
    ),
    data.cov.life = data.apparelStaticCov,
    data.cov.trans = data.apparelStaticCov,
    names.cov.life = names.cov.life,
    names.cov.trans = names.cov.trans
  ))
}

fct.helper.create.clvdata.apparel.dyncov <- function(
    data.apparelTrans = NULL,
    data.apparelDynCov = NULL,
    estimation.split = 104,
    names.cov.life = c("High.Season", "Gender", "Channel"),
    names.cov.trans = c("High.Season", "Gender", "Channel")) {

  if (is.null(data.apparelTrans)) {
    data.apparelTrans <- fct.helper.load.apparelTrans()
  }
  if (is.null(data.apparelDynCov)) {
    data.apparelDynCov <- fct.helper.load.apparelDynCov()
  }

  expect_silent(clv.dyn <- clvdata(
    data = data.apparelTrans,
    date.format = "ymd",
    time.unit = "w",
    estimation.split = estimation.split
  ))

  suppressMessages(clv.dyn <- SetDynamicCovariates(
    clv.dyn,
    data.cov.life = data.apparelDynCov,
    data.cov.trans = data.apparelDynCov,
    names.cov.life = names.cov.life,
    names.cov.trans = names.cov.trans,
    name.date = "Cov.Date"
  ))

  return(clv.dyn)
}


fit.cdnow <- function(
    data.cdnow = NULL,
    estimation.split = 37,
    model = pnbd,
    start.params.model = c(),
    verbose = FALSE,
    optimx.args = list()) {
  clv.cdnow <- fct.helper.create.clvdata.cdnow(
    data.cdnow = data.cdnow, estimation.split = estimation.split
  )

  return(do.call(
    what = model,
    args = list(
      clv.data = clv.cdnow,
      start.params.model = start.params.model,
      optimx.args = optimx.args,
      verbose = verbose
    )
  ))
}



fit.apparel.nocov <- function(
    data.apparelTrans = NULL,
    estimation.split = 104,
    model = pnbd,
    verbose=FALSE,
    # start.params.model = c(),
    # verbose = FALSE,
    # optimx.args = list()
    ...
    ) {

  clv.data.apparel <- fct.helper.create.clvdata.apparel.nocov(
    data.apparelTrans = data.apparelTrans,
    estimation.split = estimation.split
  )

  return(do.call(
    what = model,
    args = list(
      clv.data = clv.data.apparel,
      verbose=verbose,
      ...
    )
  ))
}

fit.apparel.static <- function(
    data.apparelTrans = NULL,
    data.apparelStaticCov = NULL,
    estimation.split = 104,
    names.cov.life = c("Gender", "Channel"),
    names.cov.trans = c("Gender", "Channel"),
    model = pnbd,
    verbose=FALSE,
    # start.params.model = c(),
    # verbose = FALSE,
    # optimx.args = list(),
    ...
    ) {
  clv.data.apparel.cov <- fct.helper.create.clvdata.apparel.staticcov(
    data.apparelTrans = data.apparelTrans,
    data.apparelStaticCov = data.apparelStaticCov,
    estimation.split = estimation.split,
    names.cov.life = names.cov.life,
    names.cov.trans = names.cov.trans
  )

  return(do.call(
    what = model,
    args = list(
      clv.data = clv.data.apparel.cov,
      verbose=verbose,
      ...
    )
  ))
}


fit.apparel.dyncov <- function(
    data.apparelTrans = NULL,
    data.apparelDynCov = NULL,
    estimation.split = 104,
    names.cov.life = c("High.Season", "Gender", "Channel"),
    names.cov.trans = c("High.Season", "Gender", "Channel"),
    model = pnbd,
    verbose=FALSE,
    ...
) {
  clv.data.apparel.dyncov <- fct.helper.create.clvdata.apparel.dyncov(
    data.apparelTrans = data.apparelTrans,
    data.apparelDynCov = data.apparelDynCov,
    estimation.split = estimation.split,
    names.cov.life = names.cov.life,
    names.cov.trans = names.cov.trans
  )

  return(do.call(
    what = model,
    args = list(
      clv.data = clv.data.apparel.dyncov,
      verbose=verbose,
      ...
    )
  ))
}


fit.apparel.dyncov <- function(
    data.apparelTrans = NULL,
    data.apparelDynCov = NULL,
    estimation.split = 40,
    names.cov.life = c("High.Season", "Gender", "Channel"),
    names.cov.trans = c("High.Season", "Gender", "Channel"),
    model = pnbd,
    verbose=FALSE,
    ...
) {
  clv.data.apparel.dyncov <- fct.helper.create.clvdata.apparel.dyncov(
    data.apparelTrans = data.apparelTrans,
    data.apparelDynCov = data.apparelDynCov,
    estimation.split = estimation.split,
    names.cov.life = names.cov.life,
    names.cov.trans = names.cov.trans
  )

  return(do.call(
    what = model,
    args = list(
      clv.data = clv.data.apparel.dyncov,
      verbose=verbose,
      ...
    )
  ))
}


fct.helper.dyncov.get.optimxargs.quickfit <- function(hessian){
  optimx.args <- list(
    method="Nelder-Mead", # NelderMead verifies nothing = faster
    itnmax = 10,
    hessian=hessian,
    control=list(
      reltol = 1000, # anything counts as converged
      kkt=hessian # also disable kkt if `hessian==FALSE` because it requires the hessian and overrules 'hessian' param
    )
  )
  return(optimx.args)
}


fct.helper.dyncov.quickfit <- function(clv.data.dyn, hessian){
  l.quickfit.args <- fct.helper.dyncov.get.optimxargs.quickfit(hessian=hessian)

  l.args <- list(
    clv.data=clv.data.dyn,
    # start params from std model fitted before apparel dyncov
    # other start params may yield estimated params which are unsuitable for prediction/plot (NAs & Inf) which removes dates during plotting
    start.params.model = c(r= 0.7422, alpha= 2.1222, s= 0.9991, beta=92.0867),
    optimx.args = l.quickfit.args,
    verbose = FALSE)

  if(hessian){
    expect_silent(p.dyncov <- do.call(pnbd, l.args))
  }else{
    expect_warning(p.dyncov <- do.call(pnbd, l.args), regexp = "Hessian")
  }
  return(p.dyncov)
}

fct.helper.dyncov.quickfit.apparel.data <- function(data.apparelTrans=NULL, data.apparelDynCov=NULL, hessian=FALSE,
                                                    estimation.split=104,
                                                    names.cov.life = c("High.Season", "Gender", "Channel"),
                                                    names.cov.trans = c("High.Season", "Gender", "Channel")){
  clv.apparel.dyn <- fct.helper.create.clvdata.apparel.dyncov(
    data.apparelTrans=data.apparelTrans,
    data.apparelDynCov=data.apparelDynCov,
    estimation.split = estimation.split,
    names.cov.life = names.cov.life,
    names.cov.trans = names.cov.trans)

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
  #                                           .Dimnames = list(c("log.r", "log.alpha", "log.s", "log.beta", "life.High.Season", "life.Gender", "life.Channel", "trans.High.Season", "trans.Gender", "trans.Channel"),
  #                                                            c("log.r",  "log.alpha", "log.s", "log.beta", "life.High.Season", "life.Gender", "life.Channel", "trans.High.Season", "trans.Gender", "trans.Channel")))

  return(fitted.dyncov)
}

# New method until `fct.helper.dyncov.quickfit.apparel.data` is replaced everywhere
fit.apparel.dyncov.quick <- fct.helper.dyncov.quickfit.apparel.data


fct.helper.default.newcustomer.covdata.static <- function(){
  # create default cov data for newcustomer.static
  # with same covs (columns) as created by default in
  # fct.helper.create.clvdata.apparel.staticcov

  return(data.frame(
    Gender=1,
    Channel=2
  ))
}


fct.helper.default.newcustomer.covdata.dyncov <- function(){
  # create default cov data for newcustomer.dynamic
  # with same covs (columns) as created by default in
  # fct.helper.create.clvdata.apparel.staticcov
  cov.dates <- seq.Date(as.Date("2000-01-02"), length.out = 10, by = "7 day")
  return(data.frame(
    Cov.Date=cov.dates,
    Gender=rep_len(0, length(cov.dates)),
    Channel=rep_len(c(-0.678, 0, 2, 1.23, -1.23, -2), length(cov.dates)),
    High.Season=rep_len(c(4, 0, 7, 2, 9, 0), length(cov.dates))))
}
