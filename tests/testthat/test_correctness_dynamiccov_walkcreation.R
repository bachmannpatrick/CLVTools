context("Correctness - PNBD dynamiccov - walk creation")

data("apparelTrans")
data("apparelDynCov")

test_that("Math for d_x is correct (Excel, tab Walk d)", {
  # From accompanying Excel, tab "Walk d"
  clv.week <- clv.time.weeks("ymd")

  # assuming Sunday (day 7) being the start of the week
  # Monday = 2007-01-01
  expect_equal(pnbd_dyncov_walk_d(clv.time=clv.week, tp.relevant.transaction = ymd("2007-01-01")), 6/7)
  expect_equal(pnbd_dyncov_walk_d(clv.time=clv.week, tp.relevant.transaction = ymd("2007-01-02")), 5/7)
  expect_equal(pnbd_dyncov_walk_d(clv.time=clv.week, tp.relevant.transaction = ymd("2007-01-03")), 4/7)
  expect_equal(pnbd_dyncov_walk_d(clv.time=clv.week, tp.relevant.transaction = ymd("2007-01-04")), 3/7)
  expect_equal(pnbd_dyncov_walk_d(clv.time=clv.week, tp.relevant.transaction = ymd("2007-01-05")), 2/7)
  expect_equal(pnbd_dyncov_walk_d(clv.time=clv.week, tp.relevant.transaction = ymd("2007-01-06")), 1/7)
  # Sunday, on boundary
  expect_equal(pnbd_dyncov_walk_d(clv.time=clv.week, tp.relevant.transaction = ymd("2007-01-07")), 0)
})

test_that("d_x changes correctly on boundary", {
  # year
  # 2006-12-31 < 1 and >0 (just before boundary)
  # 2007-01-02: first after boundary, >0 and <1
  # 2006-01-01: on boundary -> change -> 1
  # 2007-01-01: on boundary -> 1
  # 2008-01-01: on boundary -> 1 (also test for leap year)
  clv.year <- clv.time.years("ymd")
  expect_true(pnbd_dyncov_walk_d(clv.time=clv.year, tp.relevant.transaction = ymd("2005-12-31")) > 0)
  expect_true(pnbd_dyncov_walk_d(clv.time=clv.year, tp.relevant.transaction = ymd("2005-12-31")) < 1)

  expect_true(pnbd_dyncov_walk_d(clv.time=clv.year, tp.relevant.transaction = ymd("2007-01-02")) < 1)
  expect_true(pnbd_dyncov_walk_d(clv.time=clv.year, tp.relevant.transaction = ymd("2007-01-02")) > 0)

  expect_true(pnbd_dyncov_walk_d(clv.time=clv.year, tp.relevant.transaction = ymd("2006-01-01")) == 1)
  expect_true(pnbd_dyncov_walk_d(clv.time=clv.year, tp.relevant.transaction = ymd("2007-01-01")) == 1)
  expect_true(pnbd_dyncov_walk_d(clv.time=clv.year, tp.relevant.transaction = ymd("2008-01-01")) == 1)


  # week
  #   2007-01-01: monday, first day after boundary
  #   2007-01-07: sunday, on boundary -> change -> 1
  clv.week <- clv.time.weeks("ymd")
  expect_equal(pnbd_dyncov_walk_d(clv.time=clv.week, tp.relevant.transaction = ymd("2007-01-01")), 6/7)
  expect_equal(pnbd_dyncov_walk_d(clv.time=clv.week, tp.relevant.transaction = ymd("2007-01-07")), 1)

  # day
  # ** TODO: changes always??, always == 1??
  # clv.day <- clv.time.days("ymd")
  # pnbd_dyncov_walk_d(clv.time=clv.day, tp.relevant.transaction = ymd("2007-01-01"))

  # hour
  #   00:00:01: < 1 and > 0 (first after boundary)
  #   00:00:59: < 1
  #   00:01:00: < 1
  #   00:59:00: < 1
  #   00:59:59: < 1 and > 0 (just before boundary)
  #   00:00:00: == 1
  #   00:01:00: == 1
  clv.hour <- clv.time.hours("ymd hms")
  expect_true(pnbd_dyncov_walk_d(clv.time=clv.hour, tp.relevant.transaction = ymd_hms("2007-01-01 00:00:01")) < 1)
  expect_true(pnbd_dyncov_walk_d(clv.time=clv.hour, tp.relevant.transaction = ymd_hms("2007-01-01 00:00:01")) > 0)
  expect_true(pnbd_dyncov_walk_d(clv.time=clv.hour, tp.relevant.transaction = ymd_hms("2007-01-01 00:00:59")) < 1)
  expect_true(pnbd_dyncov_walk_d(clv.time=clv.hour, tp.relevant.transaction = ymd_hms("2007-01-01 00:01:00")) < 1)
  expect_true(pnbd_dyncov_walk_d(clv.time=clv.hour, tp.relevant.transaction = ymd_hms("2007-01-01 00:59:00")) < 1)
  expect_true(pnbd_dyncov_walk_d(clv.time=clv.hour, tp.relevant.transaction = ymd_hms("2007-01-01 00:59:59")) > 0)
  expect_true(pnbd_dyncov_walk_d(clv.time=clv.hour, tp.relevant.transaction = ymd_hms("2007-01-01 00:59:59")) < 1)
  # on boundary jumps
  expect_true(pnbd_dyncov_walk_d(clv.time=clv.hour, tp.relevant.transaction = ymd_hms("2007-01-01 00:00:00")) == 1)
  expect_true(pnbd_dyncov_walk_d(clv.time=clv.hour, tp.relevant.transaction = ymd_hms("2007-01-01 01:00:00")) == 1)

})

test_that("d1 is actually correct in walk", {

})

test_that("d_omega is actually correct in cbs", {
  apparelTrans.spaced <- copy(apparelTrans)
  # Fix data: space customers' first transaction out over whole first week
  ids <- apparelTrans.spaced[, unique(Id)]
  apparelTrans.spaced[, first.trans := Date == min(Date), by="Id"]
  tp.first.trans <- apparelTrans.spaced[, min(Date)]
  for(i in 1:7){
    apparelTrans.spaced[Id == ids[i] & first.trans == TRUE, Date := tp.first.trans+days(i-1)]
  }
  clv.dyn.spaced <- fct.helper.create.clvdata.apparel.dyncov(apparelTrans.spaced, apparelDynCov, estimation.end = 38)
  dt.cbs <- pnbd_dyncov_cbs(clv.dyn.spaced)

  # We assume Sunday (7) to be start
  dt.cbs[, weekday := lubridate::wday(date.first.actual.trans, label=TRUE, abbr=TRUE)]
  dt.cbs[weekday == "Mon", unique(d_omega) == 6/7]
  dt.cbs[weekday == "Tue", unique(d_omega) == 5/7]
  dt.cbs[weekday == "Wed", unique(d_omega) == 4/7]
  dt.cbs[weekday == "Thu", unique(d_omega) == 3/7]
  dt.cbs[weekday == "Fri", unique(d_omega) == 2/7]
  dt.cbs[weekday == "Sat", unique(d_omega) == 1/7]
  dt.cbs[weekday == "Sun", unique(d_omega) == 1]
})

test_that("tjk is correctly calculated (as in excel)", {

})

test_that("tjk is correctly calculated for aux trans with t.x=Tcal", {
  tp.T <- ymd("2006-01-01")
  # ensure at least one with t.x=T
  apparelTrans.zeroaux <- rbindlist(list(apparelTrans, data.table(Id="1", Date=tp.T, Price=12.34)))
  clv.dyn.zeroaux <- fct.helper.create.clvdata.apparel.dyncov(apparelTrans.zeroaux, apparelDynCov, estimation.end = tp.T)
  l.walks <- pnbd_dyncov_createwalks(clv.dyn.zeroaux)

  ids.tx.eq.T <- apparelTrans.zeroaux[Date == tp.T, Id]
  expect_true(l.walks$data.walks.trans.aux[Id %in% ids.tx.eq.T, all(tjk == 0)])
  # Coincidence: T is also on the boundary!
  expect_true(l.walks$data.walks.trans.aux[Id %in% ids.tx.eq.T, all(d1 == 1)])
})


# ***TODO: Is this from excel ?? ***
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

test_that("Aux walk splitting method correct (in resulting walk table)", {
  # Same input as aux splitting method, but run over full walk methods (pnbd_dyncov_createwalks_auxwalk)
})


test_that("Does not loose aux walks if there are covariates only for the calibration period (see #134)", {
  clv.short <- fct.helper.create.clvdata.apparel.dyncov(data.apparelTrans=apparelTrans[Date <= "2005-12-31"],
                                                        data.apparelDynCov=apparelDynCov[Cov.Date <= "2005-12-31"],
                                                        estimation.end=NULL)
  l.walks <- CLVTools:::pnbd_dyncov_createwalks(clv.short)
  expect_true(l.walks$data.walks.life.aux[, uniqueN(Id)] == 250)
  expect_true(l.walks$data.walks.trans.aux[, uniqueN(Id)] == 250)
})

