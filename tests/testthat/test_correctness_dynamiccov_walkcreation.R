context("Correctness - PNBD dynamiccov - walk creation")

data("apparelTrans")
data("apparelDynCov")

test_that("Math for d_x is correct", {
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

test_that("d1 is correct in walk", {
})

test_that("d_omega is correct in cbs", {
})

test_that("tjk is correctly calculated (as in excel)", {
})

test_that("tjk is correctly calculated for aux trans with t.x=Tcal", {
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

test_that("Aux walk splitting method correct", {
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

