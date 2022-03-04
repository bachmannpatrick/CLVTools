context("Correctness - PNBD dynamiccov - walk creation")

test_that("Math for d_x is correct", {
  # From accompanying Excel, tab "Walk d"
  clv.week <- clv.time.weeks("ymd")

  # assuming Sunday (day 7) being the start of the week
  # Monday = 2007-01-01
  expect_equal(pnbd_dyncov_walk_d(clv.time=clv.week, tp.trans = ymd("2007-01-01")), 6/7)
  expect_equal(pnbd_dyncov_walk_d(clv.time=clv.week, tp.trans = ymd("2007-01-02")), 5/7)
  expect_equal(pnbd_dyncov_walk_d(clv.time=clv.week, tp.trans = ymd("2007-01-03")), 4/7)
  expect_equal(pnbd_dyncov_walk_d(clv.time=clv.week, tp.trans = ymd("2007-01-04")), 3/7)
  expect_equal(pnbd_dyncov_walk_d(clv.time=clv.week, tp.trans = ymd("2007-01-05")), 2/7)
  expect_equal(pnbd_dyncov_walk_d(clv.time=clv.week, tp.trans = ymd("2007-01-06")), 1/7)
  # Sunday, on boundary
  expect_equal(pnbd_dyncov_walk_d(clv.time=clv.week, tp.trans = ymd("2007-01-07")), 0)
})

test_that("d1 is correct in walk", {
})



test_that("d_omega is correct in walk", {
})


test_that("Aux walk splitting method correct", {
  clv.time <- clv.time.weeks("ymd")
  # Start of week: Monday = 2007-01-01
  dt.cov <- data.table(Id="1",
                       tp.cov.lower = ymd(c("2007-01-01", "2007-01-08", "2007-01-15")),
                       tp.cov.upper = ymd(c("2007-01-07", "2007-01-14", "2007-01-21")),
                       cov.data = c(0.123, 0.678, 2.345))

  fct.get.walk <- function(tp.last.trans, tp.estimation.end){
    dt.tp.first.last <- data.table(tp.last.trans = ymd(tp.last.trans), Id="1")
    clv.time@timepoint.estimation.end <- ymd(tp.estimation.end)
    return(pnbd_dyncov_createwalks_auxwalk(dt.cov, dt.tp.first.last, names.cov="cov.data", clv.time = clv.time))
  }

  dt.walk <- fct.get.walk(tp.last.trans = "2007-01-05", tp.estimation.end = "2007-01-07")
  expect_true(dt.walk[, .N] == 1)
  expect_true(dt.walk[, cov.data] == 0.123)

  dt.walk <- fct.get.walk(tp.last.trans = "2007-01-05", tp.estimation.end = "2007-01-08")
  expect_true(dt.walk[, .N] == 2)
  expect_true(all(dt.walk[, cov.data] == c(0.123, 0.678)))
})

test_that("Aux walk splitting method correct", {
  # Same input as aux splitting method, but run over full walk methods (pnbd_dyncov_createwalks_auxwalk)
})


# VS old implementation

params <- drop(coef(p.dyncov.g0@optimx.estimation.output))
dt.LL.original <- LegacyCLVTools:::pnbd_dyncov_LL(params=params, clv.fitted=p.dyncov.g0, return.all.intermediate.results=TRUE)
dt.LL.cpp <- CLVTools:::pnbd_dyncov_getLLdata(clv.fitted = p.dyn, params = params)
all.equal(p.dyn@clv.data@clv.time, p.dyncov.g0@clv.data@clv.time)



dt.LL.original[ abs(LL - LL.cpp)>0.1, log.F3-log.F3.cpp]
dt.LL.cpp[, sum(LL)]
dt.LL.original[, sum(LL)]

# SetDynamicCovariates(as(p.dyncov.g0@clv.data, "clv.data"),
#                      data.cov.life = p.dyncov.g0@clv.data@data.cov.life,
#                      data.cov.trans = p.dyncov.g0@clv.data@data.cov.trans,
#                      names.cov.life = c("Marketing", "Gender", "Channel"),
#                      names.cov.trans = c("Marketing", "Gender", "Channel"),
#                      name.date = "Cov.Date")

# VS nocov

# based on  itself
# params["log.r"] <- 0.789 # spice it up
# params["log.s"] <- 1.234
vec.LL.nocov <- CLVTools:::pnbd_nocov_LL_ind(vLogparams = params[c("log.r", "log.alpha", "log.s", "log.beta")],
                                             vX = p.dyn@cbs$x, vT_x = p.dyn@cbs$t.x, vT_cal = p.dyn@cbs$T.cal)
params.g0 <- params
params.g0[setdiff(names(params.g0), c("log.r", "log.alpha", "log.s", "log.beta"))] <- 0
dt.LL.nocov <- CLVTools:::pnbd_dyncov_getLLdata(clv.fitted = p.dyn, params = params.g0)
dt.LL.nocov[, sum(LL)]
sum(vec.LL.nocov)
all.equal(sum(vec.LL.nocov), dt.LL.nocov[, sum(LL)])

# based on pnbd
p.dyn.nocov <- pnbd(as(p.dyn@clv.data, "clv.data"))
vec.LL.nocov <- CLVTools:::pnbd_nocov_LL_ind(vLogparams = params[c("log.r", "log.alpha", "log.s", "log.beta")],
                                             vX = p.dyn.nocov@cbs$x, vT_x = p.dyn.nocov@cbs$t.x, vT_cal = p.dyn.nocov@cbs$T.cal)


# based on Patrick's data
epnbd.env <- new.env()
load("~/Desktop/CLVTools/dyncov_rcpp_verify/EPNBD-Example.Rdata", envir = epnbd.env)
epnbd.env$clvd.mydata.dyncov
p.patrick <- pnbd(CLVTools::SetDynamicCovariates(epnbd.env$clvd.mydata,
                     data.cov.life = epnbd.env$clvd.mydata.dyncov@data.cov.life,
                     data.cov.trans = epnbd.env$clvd.mydata.dyncov@data.cov.trans,
                     names.cov.life = c("high.season", "direct.marketing", "low.season", "gender"),
                     names.cov.trans = c("high.season", "direct.marketing", "low.season", "gender"),
                     name.date = "Cov.Date"), optimx.args = list(itnmax=2, hessian=F,
                                                                 control=list(kkt=F, trace=6)))

params <- drop(coef(p.patrick@optimx.estimation.output))
vec.LL.nocov <- CLVTools:::pnbd_nocov_LL_ind(vLogparams = params[c("log.r", "log.alpha", "log.s", "log.beta")],
                                             vX = p.patrick@cbs$x, vT_x = p.patrick@cbs$t.x, vT_cal = p.patrick@cbs$T.cal)
params.g0 <- params
params.g0[setdiff(names(params.g0), c("log.r", "log.alpha", "log.s", "log.beta"))] <- 0
dt.LL.nocov <- CLVTools:::pnbd_dyncov_getLLdata(clv.fitted = p.patrick, params = params.g0)
dt.LL.nocov[, sum(LL)]
sum(vec.LL.nocov)
all.equal(sum(vec.LL.nocov), dt.LL.nocov[, sum(LL)])





setnames(dt.LLdata.cpp, c("log_F0", "log_F1", "log_F3"), c("log.F0", "log.F1", "log.F3"))
for(var in setdiff(colnames(dt.LLdata.cpp), "Id")){
  dt.LLdata.orig[dt.LLdata.cpp, paste0(var, ".cpp") := get(paste0("i.", var)), on = "Id"]
}

for(n in setdiff(names(dt.LLdata.cpp), "Id")){
  print(n)
  cpp.no.na <- dt.LLdata.cpp[[n]]
  orig.no.na <- dt.LLdata.orig[[n]]
  idx.na <- which(is.na(cpp.no.na))
  if(length(idx.na)){
    cpp.no.na <- cpp.no.na[-idx.na]
    orig.no.na <- orig.no.na[-idx.na]
  }

  print(all.equal(orig.no.na, cpp.no.na, check.attributes = F))
}






