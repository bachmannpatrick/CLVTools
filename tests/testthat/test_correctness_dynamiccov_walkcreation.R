skip_on_cran()

data("apparelTrans")
data("apparelDynCov")

test_that("interval created with eps same as if used shift()", {
  skip_on_cran()
  clv.time <- clv.time.weeks("ymd")
  dt.interval <- pnbd_dyncov_covariate_add_interval_bounds(dt.cov=copy(apparelDynCov), clv.time=clv.time)

  # same as if shifted + short manual
  # no NAs in tp columns
  dt.shift <- copy(apparelDynCov)
  dt.shift[, tp.cov.lower := Cov.Date]
  dt.shift[, tp.cov.upper := shift(tp.cov.lower, n=-1L), by="Id"]
  dt.shift[, tp.cov.upper := tp.cov.upper - clv.time.epsilon(clv.time)]
  dt.shift[is.na(tp.cov.upper), tp.cov.upper := tp.cov.lower + clv.time.number.timeunits.to.timeperiod(clv.time, user.number.periods=1L) - clv.time.epsilon(clv.time)]

  expect_false(anyNA(dt.interval))
  expect_false(anyNA(dt.shift))
  expect_true(all(dt.interval == dt.shift))
})


test_that("Math for d_x is correct (Excel, tab Walk d)", {
  skip_on_cran()

  # From accompanying Excel, tab "Walk d"
  clv.week <- clv.time.weeks("ymd")

  # assuming Sunday (day 7) being the start of the week
  # Monday = 2007-01-01
  expect_equal(pnbd_dyncov_walk_d(clv.time=clv.week, tp.relevant.transaction = lubridate::ymd("2007-01-01")), 6/7)
  expect_equal(pnbd_dyncov_walk_d(clv.time=clv.week, tp.relevant.transaction = lubridate::ymd("2007-01-02")), 5/7)
  expect_equal(pnbd_dyncov_walk_d(clv.time=clv.week, tp.relevant.transaction = lubridate::ymd("2007-01-03")), 4/7)
  expect_equal(pnbd_dyncov_walk_d(clv.time=clv.week, tp.relevant.transaction = lubridate::ymd("2007-01-04")), 3/7)
  expect_equal(pnbd_dyncov_walk_d(clv.time=clv.week, tp.relevant.transaction = lubridate::ymd("2007-01-05")), 2/7)
  expect_equal(pnbd_dyncov_walk_d(clv.time=clv.week, tp.relevant.transaction = lubridate::ymd("2007-01-06")), 1/7)
  # Sunday, on boundary
  expect_equal(pnbd_dyncov_walk_d(clv.time=clv.week, tp.relevant.transaction = lubridate::ymd("2007-01-07")), 0)
})

test_that("d_x changes correctly on boundary", {
  skip_on_cran()
  # year
  # 2006-12-31 < 1 and >0 (just before boundary)
  # 2007-01-02: first after boundary, >0 and <1
  # 2006-01-01: on boundary -> change -> 1
  # 2007-01-01: on boundary -> 1
  # 2008-01-01: on boundary -> 1 (also test for leap year)
  clv.year <- clv.time.years("ymd")
  expect_true(pnbd_dyncov_walk_d(clv.time=clv.year, tp.relevant.transaction = lubridate::ymd("2005-12-31")) > 0)
  expect_true(pnbd_dyncov_walk_d(clv.time=clv.year, tp.relevant.transaction = lubridate::ymd("2005-12-31")) < 1)

  expect_true(pnbd_dyncov_walk_d(clv.time=clv.year, tp.relevant.transaction = lubridate::ymd("2007-01-02")) < 1)
  expect_true(pnbd_dyncov_walk_d(clv.time=clv.year, tp.relevant.transaction = lubridate::ymd("2007-01-02")) > 0)

  expect_true(pnbd_dyncov_walk_d(clv.time=clv.year, tp.relevant.transaction = lubridate::ymd("2006-01-01")) == 1)
  expect_true(pnbd_dyncov_walk_d(clv.time=clv.year, tp.relevant.transaction = lubridate::ymd("2007-01-01")) == 1)
  expect_true(pnbd_dyncov_walk_d(clv.time=clv.year, tp.relevant.transaction = lubridate::ymd("2008-01-01")) == 1)


  # week
  #   2007-01-01: monday, first day after boundary
  #   2007-01-07: sunday, on boundary -> change -> 1
  clv.week <- clv.time.weeks("ymd")
  expect_equal(pnbd_dyncov_walk_d(clv.time=clv.week, tp.relevant.transaction = lubridate::ymd("2007-01-01")), 6/7)
  expect_equal(pnbd_dyncov_walk_d(clv.time=clv.week, tp.relevant.transaction = lubridate::ymd("2007-01-07")), 1)

  # day
  # ** TODO: changes always??, always == 1??
  # clv.day <- clv.time.days("ymd")
  # pnbd_dyncov_walk_d(clv.time=clv.day, tp.relevant.transaction = lubridate::ymd("2007-01-01"))

  # hour
  #   00:00:01: < 1 and > 0 (first after boundary)
  #   00:00:59: < 1
  #   00:01:00: < 1
  #   00:59:00: < 1
  #   00:59:59: < 1 and > 0 (just before boundary)
  #   00:00:00: == 1
  #   00:01:00: == 1
  clv.hour <- clv.time.hours("ymd hms")
  expect_true(pnbd_dyncov_walk_d(clv.time=clv.hour, tp.relevant.transaction = lubridate::ymd_hms("2007-01-01 00:00:01")) < 1)
  expect_true(pnbd_dyncov_walk_d(clv.time=clv.hour, tp.relevant.transaction = lubridate::ymd_hms("2007-01-01 00:00:01")) > 0)
  expect_true(pnbd_dyncov_walk_d(clv.time=clv.hour, tp.relevant.transaction = lubridate::ymd_hms("2007-01-01 00:00:59")) < 1)
  expect_true(pnbd_dyncov_walk_d(clv.time=clv.hour, tp.relevant.transaction = lubridate::ymd_hms("2007-01-01 00:01:00")) < 1)
  expect_true(pnbd_dyncov_walk_d(clv.time=clv.hour, tp.relevant.transaction = lubridate::ymd_hms("2007-01-01 00:59:00")) < 1)
  expect_true(pnbd_dyncov_walk_d(clv.time=clv.hour, tp.relevant.transaction = lubridate::ymd_hms("2007-01-01 00:59:59")) > 0)
  expect_true(pnbd_dyncov_walk_d(clv.time=clv.hour, tp.relevant.transaction = lubridate::ymd_hms("2007-01-01 00:59:59")) < 1)
  # on boundary jumps
  expect_true(pnbd_dyncov_walk_d(clv.time=clv.hour, tp.relevant.transaction = lubridate::ymd_hms("2007-01-01 00:00:00")) == 1)
  expect_true(pnbd_dyncov_walk_d(clv.time=clv.hour, tp.relevant.transaction = lubridate::ymd_hms("2007-01-01 01:00:00")) == 1)

})

test_that("d1 is actually correct in walk", {
  skip_on_cran()

})

test_that("d_omega is actually correct in cbs", {
  skip_on_cran()
  apparelTrans.spaced <- copy(apparelTrans)
  # Fix data: space customers' first transaction out over whole first week
  ids <- apparelTrans.spaced[, unique(Id)]
  apparelTrans.spaced[, first.trans := Date == min(Date), by="Id"]
  tp.first.trans <- apparelTrans.spaced[, min(Date)]
  for(i in 1:7){
    apparelTrans.spaced[Id == ids[i] & first.trans == TRUE, Date := tp.first.trans+days(i-1)]
  }
  clv.dyn.spaced <- fct.helper.create.clvdata.apparel.dyncov(apparelTrans.spaced, apparelDynCov, estimation.split = 38)
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
  skip_on_cran()
  tp.T <- lubridate::ymd("2006-01-01")
  # ensure at least one with t.x=T
  apparelTrans.zeroaux <- rbindlist(list(apparelTrans, data.table(Id="1", Date=tp.T, Price=12.34)))
  clv.dyn.zeroaux <- fct.helper.create.clvdata.apparel.dyncov(apparelTrans.zeroaux, apparelDynCov, estimation.split = tp.T)
  l.walks <- pnbd_dyncov_createwalks(clv.dyn.zeroaux)

  ids.tx.eq.T <- apparelTrans.zeroaux[Date == tp.T, Id]
  expect_true(l.walks$data.walks.trans.aux[Id %in% ids.tx.eq.T, all(tjk == 0)])
  # Coincidence: T is also on the boundary!
  expect_true(l.walks$data.walks.trans.aux[Id %in% ids.tx.eq.T, all(d1 == 1)])
})


# ***TODO: Is this from excel ?? ***
test_that("Aux walk splitting method correct", {
  skip_on_cran()
  clv.time <- clv.time.weeks("ymd")
  # Start of week: Monday = 2007-01-01
  dt.cov <- data.table(Id="1",
                       tp.cov.lower = lubridate::ymd(c("2007-01-01", "2007-01-08", "2007-01-15")),
                       tp.cov.upper = lubridate::ymd(c("2007-01-07", "2007-01-14", "2007-01-21")),
                       cov.data = c(0.123, 0.678, 2.345))

  fct.get.walk <- function(tp.last.trans, tp.estimation.end){
    dt.tp.first.last <- data.table(tp.last.trans = lubridate::ymd(tp.last.trans), Id="1")
    clv.time@timepoint.estimation.end <- lubridate::ymd(tp.estimation.end)
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
  skip_on_cran()
  # Same input as aux splitting method, but run over full walk methods (pnbd_dyncov_createwalks_auxwalk)
})


test_that("Does not loose aux walks if there are covariates only for the calibration period (see #134)", {
  skip_on_cran()

  clv.short <- fct.helper.create.clvdata.apparel.dyncov(data.apparelTrans=apparelTrans[Date <= "2005-12-31"],
                                                        data.apparelDynCov=apparelDynCov[Cov.Date <= "2005-12-31"],
                                                        estimation.split=NULL)
  l.walks <- CLVTools:::pnbd_dyncov_createwalks(clv.short)
  expect_true(l.walks$data.walks.life.aux[, uniqueN(Id)] == 250)
  expect_true(l.walks$data.walks.trans.aux[, uniqueN(Id)] == 250)
})


test_that("real trans walk correct as in excel", {
  skip_on_cran()

  # dt.trans <- data.table(Id = c(1, 1, 2),
  #                        Date = lubridate::ymd(c("2000 01 01", "2000 01 02", "2000 01 10")),
  #                        Price=0)
  # dt.cov <- data.table(Id = 1, Date = lubridate::ymd("2000 01 01"), cov=1)
  # SetDynamicCovariates(clvdata(dt.trans, "ymd", "w", estimation.split = NULL), data.cov.life = dt.cov, data.cov.trans = dt.cov, names.cov.life = "cov", names.cov.trans = "cov")
  #
  # data.table(Id = c(1),
  #            Date = lubridate::ymd(c("2000 01 01"))
  #            Price=0)
  # d <- new('clv.data.dynamic.covariates')
  # d@data.cov.trans <- data.table()

  # real trans walk gives walk and not lost if transactions only 1 eps apart

  # real trans walk same number as num repeat transactions


})



test_that("real life walk + aux life walk give original covariate data",{
  skip_on_cran()

  # use with estimation split because real+aux only exist in estimation period
  clv.dyn <- fct.helper.create.clvdata.apparel.dyncov(data.apparelTrans=apparelTrans,
                                                      data.apparelDynCov=apparelDynCov,
                                                      estimation.split="2005 06 30")

  l.walks <- pnbd_dyncov_createwalks(clv.dyn)

  fct.real.plus.aux <- function(id){
    dt.plus <- rbind(l.walks$data.walks.life.real[Id == id],
                     l.walks$data.walks.life.aux[Id == id])

    # no gap in cov dates (all 7 days apart)
    expect_setequal(dt.plus[, as.numeric(diff(tp.cov.lower), units="days")], 7)
    expect_setequal(dt.plus[, as.numeric(diff(tp.cov.upper), units="days")], 7)

    # covs same as used to create
    dt.original <- apparelDynCov[Id == id &
        Cov.Date >= apparelTrans[Id == id, floor_date(min(Date), unit = "week")] &
          Cov.Date <= lubridate::ymd("20050630")]
    expect_true(all(dt.plus[, .(Id, Cov.Date = tp.cov.lower, Marketing, Gender, Channel)] == dt.original))
  }

  # many are zerorep and do not have real trans walk
  fct.real.plus.aux("10")
  fct.real.plus.aux("100")
  fct.real.plus.aux("1000")
})

test_that("repeat buyers: life walk and first trans walk start on the same timepoint",{
  skip_on_cran()

  clv.dyn <- fct.helper.create.clvdata.apparel.dyncov(data.apparelTrans=apparelTrans,
                                                      data.apparelDynCov=apparelDynCov,
                                                      estimation.split="2005 06 30")

  l.walks <- pnbd_dyncov_createwalks(clv.dyn)
  expect_true(l.walks$data.walks.life.real[Id == "10", min(tp.cov.lower)] == l.walks$data.walks.trans.real[Id == "10", min(tp.cov.lower)])
  expect_true(l.walks$data.walks.life.real[Id == "100", min(tp.cov.lower)] == l.walks$data.walks.trans.real[Id == "100", min(tp.cov.lower)])
  expect_true(l.walks$data.walks.life.real[Id == "1000", min(tp.cov.lower)] == l.walks$data.walks.trans.real[Id == "1000", min(tp.cov.lower)])
})


