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

  oldval <- getOption("lubridate.week.start", 7)
  options(lubridate.week.start=7)

  # Monday = 2007-01-01
  expect_equal(pnbd_dyncov_walk_d(clv.time=clv.week, tp.relevant.transaction = lubridate::ymd("2007-01-01")), 6/7)
  expect_equal(pnbd_dyncov_walk_d(clv.time=clv.week, tp.relevant.transaction = lubridate::ymd("2007-01-02")), 5/7)
  expect_equal(pnbd_dyncov_walk_d(clv.time=clv.week, tp.relevant.transaction = lubridate::ymd("2007-01-03")), 4/7)
  expect_equal(pnbd_dyncov_walk_d(clv.time=clv.week, tp.relevant.transaction = lubridate::ymd("2007-01-04")), 3/7)
  expect_equal(pnbd_dyncov_walk_d(clv.time=clv.week, tp.relevant.transaction = lubridate::ymd("2007-01-05")), 2/7)
  expect_equal(pnbd_dyncov_walk_d(clv.time=clv.week, tp.relevant.transaction = lubridate::ymd("2007-01-06")), 1/7)
  # Sunday, on boundary
  expect_equal(pnbd_dyncov_walk_d(clv.time=clv.week, tp.relevant.transaction = lubridate::ymd("2007-01-07")), 1)

  options(lubridate.week.start=oldval)
})

test_that("d_x changes correctly on (lower) boundary", {
  skip_on_cran()

  fct.expect.d.between.0.1 <- function(clv, d){
    d <- clv.time.convert.user.input.to.timepoint(clv, user.timepoint = d)
    expect_true(pnbd_dyncov_walk_d(clv.time=clv, tp.relevant.transaction = d) > 0)
    expect_true(pnbd_dyncov_walk_d(clv.time=clv, tp.relevant.transaction = d) < 1)
  }

  fct.expect.d.equal.1 <- function(clv, d){
    d <- clv.time.convert.user.input.to.timepoint(clv, user.timepoint = d)
    expect_true(pnbd_dyncov_walk_d(clv.time=clv, tp.relevant.transaction = d) == 1)
  }

  # year
  # 2006-12-31 < 1 and >0 (just before (lower) boundary)
  # 2007-01-02: first after boundary, >0 and <1
  # 2006-01-01: on (lower) boundary -> change -> 1
  # 2007-01-01: on (lower) boundary -> 1
  # 2008-01-01: on (lower) boundary -> 1 (also test for leap year)
  clv.year <- clv.time.years("ymd")
  fct.expect.d.between.0.1(clv.year, d="2005-12-31")
  fct.expect.d.between.0.1(clv.year, d="2007-01-02")

  fct.expect.d.equal.1(clv.year, d="2006-01-01")
  fct.expect.d.equal.1(clv.year, d="2007-01-01")
  fct.expect.d.equal.1(clv.year, d="2008-01-01")


  # week
  #   2007-01-01: monday, first day after (lower) boundary
  #   2007-01-07: sunday, on (lower) boundary -> change -> 1
  clv.week <- clv.time.weeks("ymd")
  expect_equal(pnbd_dyncov_walk_d(clv.time=clv.week, tp.relevant.transaction = lubridate::ymd("2007-01-01")), 6/7)
  fct.expect.d.equal.1(clv.week, d="2007-01-07")

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
  clv.hour <- clv.time.hours("ymd HMS")
  fct.expect.d.between.0.1(clv.hour, d="2007-01-01 00:00:01")
  fct.expect.d.between.0.1(clv.hour, d="2007-01-01 00:00:59")
  fct.expect.d.between.0.1(clv.hour, d="2007-01-01 00:01:00")
  fct.expect.d.between.0.1(clv.hour, d="2007-01-01 00:59:00")
  fct.expect.d.between.0.1(clv.hour, d="2007-01-01 00:59:59")
  fct.expect.d.between.0.1(clv.hour, d="2007-01-01 00:59:59")
  fct.expect.d.between.0.1(clv.hour, d="2007-01-01 23:59:59")

  # on lower boundary
  fct.expect.d.equal.1(clv.hour, d="2007-01-01 00:00:00")
  fct.expect.d.equal.1(clv.hour, d="2007-01-01 01:00:00")
  fct.expect.d.equal.1(clv.hour, d="2007-01-01 23:00:00")
  fct.expect.d.equal.1(clv.hour, d="2007-12-31 23:00:00")

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

test_that("tjk is correctly calculated", {
  skip_on_cran()

  # Fake transactions for customer 1
  # - dates: 2005-01-03, 2005-01-04, 2005-01-05, 2005-01-07, 2005-01-18
  # - Id: Replace existing transactions of "1" because also requires covariates
  #
  # 2005-01-03 -> 2005-01-04: tjk=1/7
  # 2005-01-04 -> 2005-01-05: tjk=1/7
  # 2005-01-05 -> 2005-01-07: tjk=2/7
  # 2005-01-07 -> 2005-01-18: tjk=11/7
  apparelTrans.tjk <- rbindlist(list(apparelTrans[Id != "1"],
                                     data.table(Id="1", Date=lubridate::ymd(c("2005-01-03", "2005-01-04", "2005-01-05", "2005-01-07", "2005-01-18")), Price=12.34)))
  expect_silent(l.walks <- pnbd_dyncov_createwalks(fct.helper.create.clvdata.apparel.dyncov(apparelTrans.tjk, apparelDynCov, estimation.split = NULL)))

  expect_equal(unique(l.walks$data.walks.trans.real[Id == "1", c("tp.this.trans", "tjk")])[order(tp.this.trans)],
               data.table(tp.this.trans=lubridate::ymd(c("2005-01-04", "2005-01-05", "2005-01-07", "2005-01-18")),
                          tjk=c(1/7, 1/7, 2/7, 11/7)))
})

test_that("tjk is correctly calculated for aux trans with t.x=Tcal", {
  skip_on_cran()
  tp.T <- lubridate::ymd("2006-01-01")
  # ensure at least one with t.x=T
  apparelTrans.zeroaux <- rbindlist(list(apparelTrans, data.table(Id="1", Date=tp.T, Price=12.34)))
  clv.dyn.zeroaux <- fct.helper.create.clvdata.apparel.dyncov(apparelTrans.zeroaux, apparelDynCov, estimation.split = tp.T)
  expect_silent(l.walks <- pnbd_dyncov_createwalks(clv.dyn.zeroaux))

  ids.tx.eq.T <- apparelTrans.zeroaux[Date == tp.T, Id]
  expect_true(l.walks$data.walks.trans.aux[Id %in% ids.tx.eq.T, all(tjk == 0)])
  # Coincidence: T is also on the boundary!
  expect_true(l.walks$data.walks.trans.aux[Id %in% ids.tx.eq.T, all(d1 == 1)])
})


test_that("Aux walk splitting method correct", {
  # this is not from test case excel

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

test_that("Aux walk is 2 periods if T is on week start and alive at T-1 one day before (and has no real life walk)", {
  # T on week start, alive is T-1 -> aux walk is 2 periods, but no real life walk (run test with T on all 7 days)

  for(i in seq(7)){
    data.trans <- copy(apparelTrans)
    data.cov <- copy(apparelDynCov)

    oldval <- getOption("lubridate.week.start")
    options(lubridate.week.start=(7+i-1) %% 7+1)
    # print(getOption("lubridate.week.start"))

    # make cov dates to be on week start
    data.cov[, Cov.Date := Cov.Date + i]

    # split is on start of week
    date.estimation.split <- lubridate::ymd("2005-06-26") + i

    # Id==1 is zero-repeater
    data.trans[Id==1, Date := date.estimation.split-1]
    clv <- fct.helper.create.clvdata.apparel.dyncov(data.apparelTrans=data.trans,
                                                    data.apparelDynCov=data.cov,
                                                    estimation.split=date.estimation.split)
    expect_silent(l.walks <- pnbd_dyncov_createwalks(clv))

    # has 2 aux walks
    expect_true(l.walks$data.walks.life.aux[Id==1, .N] == 2)
    # no real walk
    expect_true(l.walks$data.walks.life.real[Id==1, .N] == 0)

    options(lubridate.week.start=oldval)
  }
})

test_that("Aux walks not lost if there are covariates only for the calibration period (see #134)", {
  skip_on_cran()

  clv.short <- fct.helper.create.clvdata.apparel.dyncov(data.apparelTrans=apparelTrans[Date <= "2005-12-31"],
                                                        data.apparelDynCov=apparelDynCov[Cov.Date <= "2005-12-31"],
                                                        estimation.split=NULL)
  expect_silent(l.walks <- pnbd_dyncov_createwalks(clv.short))
  expect_true(l.walks$data.walks.life.aux[, uniqueN(Id)] == 250)
  expect_true(l.walks$data.walks.trans.aux[, uniqueN(Id)] == 250)
})


test_that("Real Trans Walk correct", {
  skip_on_cran()

  # Fake transactions for customer "1"
  # - Id: Replace existing transactions of "1" because also requires covariates
  # - on and off the cov boundary
  #   - cov boundaries: [2005-01-02, 2005-01-08][2005-01-09, 2005-01-15][2005-01-16, 2005-01-22][2005-01-23, 2005-01-29][2005-01-30, 2005-02-05]
  #   - cov values:              0.123                   0.234                   0.345                    0.456                   0.567
  #   - transaction dates: 2005-01-02, 2005-01-07, 2005-01-10, 2005-01-15, 2005-01-16, 2005-01-30
  #
  # Real Trans Walk: Covariates up to a repeat transaction
  # Resulting real trans walks
  # 2005-01-07: [0.123]
  # 2005-01-10: [0.123], [0.234]
  # 2005-01-15: [0.234]
  # 2005-01-16: [0.234], [0.345]
  # 2005-01-30: [0.345], [0.456], [0.567]
  #
  apparelTrans.realwalk <- rbindlist(list(apparelTrans[Id != "1"],
                                     data.table(Id="1", Date=lubridate::ymd(c("2005-01-02", "2005-01-07", "2005-01-10", "2005-01-15", "2005-01-16", "2005-01-30")), Price=12.34)))
  dates.cov.realwalk <- lubridate::ymd(c("2005-01-02", "2005-01-09", "2005-01-16", "2005-01-23", "2005-01-30"))
  values.cov.realwalk <- c(0.123, 0.234, 0.345, 0.456, 0.567)
  cov.realwalk <- rbindlist(list(apparelDynCov[!(Id == "1" & Cov.Date %in% dates.cov.realwalk)],
                                 data.table(Id="1", Cov.Date=dates.cov.realwalk, Marketing=c(0.123, 0.234, 0.345, 0.456, 0.567), Gender=0, Channel=0)))
  expect_silent(l.walks <- pnbd_dyncov_createwalks(fct.helper.create.clvdata.apparel.dyncov(apparelTrans.realwalk, cov.realwalk, estimation.split = NULL)))

  expect_equal(
    l.walks$data.walks.trans.real[Id==1][order(tp.this.trans), list(tp.this.trans=as.character(tp.this.trans), Marketing)],
    rbindlist(list(
      data.table(tp.this.trans="2005-01-07", Marketing=0.123),
      data.table(tp.this.trans="2005-01-10", Marketing=c(0.123, 0.234)),
      data.table(tp.this.trans="2005-01-15", Marketing=0.234),
      data.table(tp.this.trans="2005-01-16", Marketing=c(0.234, 0.345)),
      data.table(tp.this.trans="2005-01-30", Marketing=c(0.345, 0.456, 0.567)))))
})

test_that("All walks have basic correctness, estimation.split at every day of week", {
  skip_on_cran()
  # From walk creation asserts function

  # Test with estimation.split at every week day
  for(i in seq(7)){

    clv <- fct.helper.create.clvdata.apparel.dyncov(data.apparelTrans=apparelTrans,
                                                    data.apparelDynCov=apparelDynCov,
                                                    # move split by i days
                                                    estimation.split=lubridate::ymd("2005-06-30") + i)
    expect_silent(l.walks <- pnbd_dyncov_createwalks(clv))
    dt.cbs <- pnbd_dyncov_cbs(clv)

    fct.walk.basic.correctness <- function(dt.walks){

      # no NA# coreelements
      expect_true(setequal(key(dt.walks), c("Id", "walk_id", "tp.this.trans", "tp.cov.lower")))
      expect_true(dt.walks[, !is.unsorted(abs_pos,   strictly = TRUE)])
      expect_true(dt.walks[, !is.unsorted(walk_id,   strictly = FALSE)])
      expect_true(dt.walks[, !is.unsorted(walk_from, strictly = FALSE)])
      expect_true(dt.walks[, !is.unsorted(walk_to,   strictly = FALSE)])

      expect_true(!anyNA(dt.walks))
      # walks are backwards looking from second transaction: transaction must be after all of walks' lower covariate periods
      # all transactions must be after all of walks' lower covariate periods
    }

    fct.walk.basic.correctness(l.walks$data.walks.life.aux)
    fct.walk.basic.correctness(l.walks$data.walks.life.real)
    fct.walk.basic.correctness(l.walks$data.walks.trans.aux)
    fct.walk.basic.correctness(l.walks$data.walks.trans.real)

    expect_true(l.walks$data.walks.trans.real[tp.this.trans < tp.cov.lower, .N] == 0)

    # All transactions on week start are longer than 1 walk (because also include cov of period before)
    l.walks$data.walks.trans.real[wday(tp.this.trans) == getOption("lubridate.week.start", 1), walk_id]

    # number of real trans walks == num repeat transactions

    # all aux walks:
    #   every Id only once
    #   every Id in walks
    #   same length per customer
    expect_true(l.walks$data.walks.life.aux[, list(num_walks = uniqueN(walk_id)), by="Id"][, all(num_walks==1)])
    expect_true(l.walks$data.walks.life.aux[, uniqueN(Id)] == apparelTrans[, uniqueN(Id)])
    expect_true(setequal(l.walks$data.walks.life.aux[, unique(Id)], apparelTrans[, unique(Id)]))

    expect_true(l.walks$data.walks.trans.aux[, list(num_walks = uniqueN(walk_id)), by="Id"][, all(num_walks==1)])
    expect_true(l.walks$data.walks.trans.aux[, uniqueN(Id)] == apparelTrans[, uniqueN(Id)])
    expect_true(setequal(l.walks$data.walks.trans.aux[, unique(Id)], apparelTrans[, unique(Id)]))

    expect_true(identical(l.walks$data.walks.life.aux[, .N, keyby="Id"],
                          l.walks$data.walks.trans.aux[, .N, keyby="Id"]))


    # lifetime aux walk:
    #   no date overlap with real lifetime walks
    dt.tmp <- l.walks$data.walks.life.aux[, list(first_cov_aux = min(tp.cov.lower)), keyby="Id"]
    dt.tmp[l.walks$data.walks.life.real[, list(first_cov_real = min(tp.cov.lower)) , keyby="Id"], first_cov_real := i.first_cov_real, on="Id"]
    # some first_cov_real are NA because have no real walk
    expect_true(dt.tmp[first_cov_aux < first_cov_real, .N] == 0)

    # lifetime real walk:
    #   exactly 1 walk per customer
    #   all ids except where aux walk reaches to the first transactions (coming alive)
    #   (number of customers = num customers in trans real walks where .N>1)
    #   n real walk + n aux walk >= ceiling(Tcal)
    expect_true(l.walks$data.walks.life.real[, list(num_walks=uniqueN(walk_id)), keyby="Id"][, all(num_walks == 1)])
    # dt.tmp[clv.fitted@clv.data@data.transactions[, list(last_trans = max(Date)), by="Id"], last_trans := i.last_trans, on="Id"]
    dt.tmp[dt.cbs, first_trans := i.date.first.actual.trans, on="Id"]
    expect_true(setequal(l.walks$data.walks.life.real[, unique(Id)],
                         dt.tmp[first_cov_aux > first_trans, Id]))

    # trans real walks:
    #   every Id with x>0 is in ...
    #            ... with x-1 walks
    expect_true(setequal(l.walks$data.walks.trans.real[, unique(Id)], dt.cbs[x>0, Id]))
    expect_true(identical(l.walks$data.walks.trans.real[, list(num_walks=as.double(uniqueN(walk_id))), keyby="Id"],
                          dt.cbs[x>0, list(num_walks=as.double(x)), keyby="Id"]))

    # trans walks
    #   tjk >= 0 (== 0 when t.x=T)
    expect_true(l.walks$data.walks.trans.aux[tjk < 0, .N] == 0)
    expect_true(l.walks$data.walks.trans.real[tjk < 0, .N] == 0)

    # d1 and d_omega measures are in (0,1]
    expect_true(dt.cbs[, all(d_omega > 0 & d_omega <= 1)])
    expect_true(l.walks$data.walks.trans.aux[, all(d1 > 0 & d1 <= 1)])
    expect_true(l.walks$data.walks.trans.real[, all(d1 > 0 & d1 <= 1)])
  }

})

test_that("Real Trans Walk do no lose walk if transactions only 1 eps apart", {
  skip_on_cran()
  # real trans walk gives walk and not lost if transactions only 1 eps apart
  #   Customer 1 is zero repeater. Add 8 transactions only 1 day apart for 1 week
  clv.dyn <- fct.helper.create.clvdata.apparel.dyncov(
    data.apparelTrans=rbind(apparelTrans,
                            data.table(Id=1, Price = 10,
                                       Date = seq.Date(from=apparelTrans[Id==1, min(Date)]+1,
                                                       length.out = 8, by="1 day"))),
    data.apparelDynCov=apparelDynCov,
    estimation.split=NULL)

  expect_silent(l.walks <- pnbd_dyncov_createwalks(clv.dyn))

  # every repeat-transaction is in walks (not lost)
  #   unique(tp.this.trans) because has a walk longer than 1 period (when tp.this.trans on week start)
  expect_equal(l.walks$data.walks.trans.real[Id == 1, sort(unique(tp.this.trans))],
               sort(clv.dyn@data.transactions[Id==1, Date][-1]))

  # number of walks == num repeat transactions
  #   cannot check .N==8 because some walks may be longer than 1 period if second transaction on week start
  expect_true(l.walks$data.walks.trans.real[Id == 1, uniqueN(walk_id)] == 8)
})


test_that("Real Trans Walk: None if there are no repeat transactions", {
  skip_on_cran()

  # Real trans walks only exist for repeat transactions
  #   if there are no repeat transactions, there should be no real trans walks
  #
  # Data: Remove repeat transactions of each customer from apparelTrans

  # only keep first transaction of every customer
  # Because in apparelTrans all customers have their transaction on the first date,
  # no clvdata object can be created as it requires at least 1 period.
  # Therefore move dates forward by up to 30days
  apparelTrans.norepeat <- apparelTrans[order(Date), head(.SD, n=1), by="Id"]
  apparelTrans.norepeat[, Date := Date + .I %% 30]
  expect_silent(l.walks <- pnbd_dyncov_createwalks(fct.helper.create.clvdata.apparel.dyncov(apparelTrans.norepeat, apparelDynCov, estimation.split = NULL)))

  expect_true(nrow(l.walks$data.walks.trans.real) == 0)
  expect_true(ncol(l.walks$data.walks.trans.real) == 14)
  expect_true(all(colnames(l.walks$data.walks.trans.real) == colnames(l.walks$data.walks.trans.aux)))

})


test_that("real life walk + aux life walk give original covariate data",{
  skip_on_cran()

  # use with estimation split because real+aux only exist in estimation period
  clv.dyn <- fct.helper.create.clvdata.apparel.dyncov(data.apparelTrans=apparelTrans,
                                                      data.apparelDynCov=apparelDynCov,
                                                      estimation.split="2005-06-30")

  expect_silent(l.walks <- pnbd_dyncov_createwalks(clv.dyn))

  fct.real.plus.aux <- function(id){
    dt.plus <- rbind(l.walks$data.walks.life.real[Id == id],
                     l.walks$data.walks.life.aux[Id == id])

    # no gap in cov dates (all 7 days apart)
    expect_setequal(dt.plus[, as.numeric(diff(tp.cov.lower), units="days")], 7)
    expect_setequal(dt.plus[, as.numeric(diff(tp.cov.upper), units="days")], 7)

    # covs same as used to create
    dt.original <- apparelDynCov[Id == id &
        Cov.Date >= apparelTrans[Id == id, floor_date(min(Date), unit = "week")] &
          Cov.Date <= lubridate::ymd("2005-06-30")]
    expect_true(all(dt.plus[, .(Id, Cov.Date = tp.cov.lower, Marketing, Gender, Channel)] == dt.original))
  }

  # many are zerorep and do not have real trans walk
  # fct.real.plus.aux("10")
  # fct.real.plus.aux("100")
  # fct.real.plus.aux("1000")

  # all repeat buyers (zeroreps do not have real trans walk)
  for(id in apparelTrans[, .N, by="Id"][N>1][1:25, Id]){
    fct.real.plus.aux(id)
  }

})

test_that("repeat buyers: life walk and first trans walk start on the same timepoint",{
  skip_on_cran()

  clv.dyn <- fct.helper.create.clvdata.apparel.dyncov(data.apparelTrans=apparelTrans,
                                                      data.apparelDynCov=apparelDynCov,
                                                      estimation.split="2005 06 30")

  expect_silent(l.walks <- pnbd_dyncov_createwalks(clv.dyn))
  expect_true(l.walks$data.walks.life.real[Id == "10", min(tp.cov.lower)] == l.walks$data.walks.trans.real[Id == "10", min(tp.cov.lower)])
  expect_true(l.walks$data.walks.life.real[Id == "100", min(tp.cov.lower)] == l.walks$data.walks.trans.real[Id == "100", min(tp.cov.lower)])
  expect_true(l.walks$data.walks.life.real[Id == "1000", min(tp.cov.lower)] == l.walks$data.walks.trans.real[Id == "1000", min(tp.cov.lower)])
})


