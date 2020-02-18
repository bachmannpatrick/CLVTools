# Load data ---------------------------------------------------------------------------------------
data("apparelTrans")
data("apparelDynCov")

# skip_on_cran()

# set Date as will otherwise complain in all the setdyncovs
expect_silent(apparelDynCov[, Cov.Date := as.Date(Cov.Date)])
# cutoff last as will result in "cutoff" message and not silent anymore
apparelDynCov <- apparelDynCov[Cov.Date < "2016-10-09" ]

# Covariate dummies ---------------------------------------------------------------------------------------
context("Correctness - SetDynamicCovariates - Covariate dummies")

expect_message(clv.data.apparel.nohold   <- clvdata(apparelTrans, date.format = "ymd", time.unit = "w"), regexp = "ignored")
expect_message(clv.data.apparel.withhold <- clvdata(apparelTrans, date.format = "ymd", time.unit = "w",
                                                    estimation.split = 39), regexp = "ignored")

l.std.args <- alist(data.cov.life  = apparelDynCov,  names.cov.life = c("DM", "High.Season", "Gender"),
                    data.cov.trans = apparelDynCov,  names.cov.trans = c("DM", "High.Season", "Gender"),
                    name.date = "Cov.Date")

test_that("Factor and char covariates result in same dummies",{


  apparelDynCov.char <- data.table::copy(apparelDynCov)
  apparelDynCov.char[, Gender := as.character(Gender)]
  apparelDynCov.char[, DM     := as.character(DM)]
  apparelDynCov.char[High.Season == 1, High.Season.char := "Y"]
  apparelDynCov.char[High.Season == 0, High.Season.char := "N"]
  apparelDynCov.char[, High.Season := High.Season.char]
  apparelDynCov.char[, High.Season.char := NULL]

  apparelDynCov.factor <- data.table::copy(apparelDynCov.char)
  apparelDynCov.factor[, Gender := as.factor(Gender)]
  apparelDynCov.factor[, DM     := as.factor(DM)]
  apparelDynCov.factor[, High.Season := as.factor(High.Season)]

  fct.char.vs.factor <- function(clv.data){
    l.data <- modifyList(l.std.args, alist(clv.data = clv.data))
    expect_silent(dyn.char.life  <- do.call(SetDynamicCovariates, modifyList(l.data, alist(data.cov.life=apparelDynCov.char))))
    expect_silent(dyn.char.trans <- do.call(SetDynamicCovariates, modifyList(l.data, alist(data.cov.trans=apparelDynCov.char))))
    expect_silent(dyn.char.both  <- do.call(SetDynamicCovariates, modifyList(l.data, alist(data.cov.life=apparelDynCov.char,
                                                                                               data.cov.trans=apparelDynCov.char))))

    expect_silent(dyn.factor.life  <- do.call(SetDynamicCovariates, modifyList(l.data, alist(data.cov.life=apparelDynCov.factor))))
    expect_silent(dyn.factor.trans <- do.call(SetDynamicCovariates, modifyList(l.data, alist(data.cov.trans=apparelDynCov.factor))))
    expect_silent(dyn.factor.both  <- do.call(SetDynamicCovariates, modifyList(l.data, alist(data.cov.life=apparelDynCov.factor,
                                                                                             data.cov.trans=apparelDynCov.factor))))

    expect_equal(dyn.char.life,  dyn.factor.life)
    expect_equal(dyn.char.trans, dyn.factor.trans)
    expect_equal(dyn.char.both,  dyn.factor.both)
  }

  fct.char.vs.factor(clv.data = clv.data.apparel.nohold)
  fct.char.vs.factor(clv.data = clv.data.apparel.withhold)

})


test_that("Cuts to correct range if more cov data before estimation start than needed ",{

  # longer lower end
  apparelDynCov.longer.lower <-
    data.table::rbindlist(list(apparelDynCov,
                               data.table::data.table(Id=1, Cov.Date = seq(from=apparelDynCov[, min(Cov.Date)]-lubridate::weeks(1),
                                                   by="-1 weeks",length.out = 10),
                              DM=1, High.Season=0, Gender=1)), use.names = TRUE)


  fct.longer.lower.cov <- function(clv.data, dt.cov){
    l.data <- modifyList(l.std.args, alist(clv.data = clv.data))

    expect_message(dyn.longer.life  <- do.call(SetDynamicCovariates, modifyList(l.data, alist(data.cov.life=dt.cov))),
                   regexp = "ifetime covariate data before")
    expect_message(dyn.longer.trans <- do.call(SetDynamicCovariates, modifyList(l.data, alist(data.cov.trans=dt.cov))),
                   regexp = "ransaction covariate data before")
    expect_message(dyn.longer.both  <- do.call(SetDynamicCovariates, modifyList(l.data, alist(data.cov.life=dt.cov,
                                                                                           data.cov.trans=dt.cov))),
                   regexp = "covariate data before")

    # verify data is cut for this Id like everybody' elses
    expect_true(dyn.longer.life@data.cov.life[Id==1, min(Cov.Date)] == dyn.longer.life@data.cov.life[Id != 1, min(Cov.Date)])
    expect_true(dyn.longer.life@data.cov.life[Id==1, max(Cov.Date)] == dyn.longer.life@data.cov.life[Id != 1, max(Cov.Date)])

    expect_true(dyn.longer.trans@data.cov.life[Id==1, min(Cov.Date)] == dyn.longer.trans@data.cov.life[Id != 1, min(Cov.Date)])
    expect_true(dyn.longer.trans@data.cov.life[Id==1, max(Cov.Date)] == dyn.longer.trans@data.cov.life[Id != 1, max(Cov.Date)])

    expect_true(dyn.longer.both@data.cov.life[Id==1, min(Cov.Date)] == dyn.longer.both@data.cov.life[Id != 1, min(Cov.Date)])
    expect_true(dyn.longer.both@data.cov.life[Id==1, max(Cov.Date)] == dyn.longer.both@data.cov.life[Id != 1, max(Cov.Date)])
  }

  # Apply
  fct.longer.lower.cov(clv.data.apparel.nohold, apparelDynCov.longer.lower)
  fct.longer.lower.cov(clv.data.apparel.withhold, apparelDynCov.longer.lower)

})

test_that("Single cov data longer than other data requires all data to be this long", {
  # longer upper end
  apparelDynCov.longer.upper  <-
    data.table::rbindlist(list(apparelDynCov,
                               data.table::data.table(Id=1, Cov.Date = seq(from=apparelDynCov[, max(Cov.Date)]+lubridate::weeks(1),
                                                   length.out = 100, by = "week"),
                              DM=1, High.Season=0, Gender=1)), use.names = TRUE)
  fct.longer.upper.cov <- function(clv.data, dt.cov){
    expect_error(dyn.longer.life  <- do.call(SetDynamicCovariates, modifyList(l.data, alist(data.cov.life=dt.cov))),
                 regexp = "covariate data need to have the same number of Dates")
    expect_error(dyn.longer.trans <- do.call(SetDynamicCovariates, modifyList(l.data, alist(data.cov.trans=dt.cov))),
                 regexp = "covariate data need to have the same number of Dates")
    expect_error(dyn.longer.both  <- do.call(SetDynamicCovariates, modifyList(l.data, alist(data.cov.life=dt.cov,
                                                                                            data.cov.trans=dt.cov))),
                 regexp = "covariate data need to have the same number of Dates")
  }

  l.data <- modifyList(l.std.args, alist(clv.data = clv.data))

  # Apply
  fct.longer.upper.cov(clv.data.apparel.nohold, apparelDynCov.longer.upper)
  fct.longer.upper.cov(clv.data.apparel.withhold, apparelDynCov.longer.upper)
})

test_that("Creates correct number of dummies - 2 categories", {

  apparelDynCov.2cat <- data.table::copy(apparelDynCov)
  apparelDynCov.2cat[, Gender := rep(c("F", "M"), nrow(apparelDynCov.2cat)/2)]

  # Life
  expect_silent(dyn.cov <- SetDynamicCovariates(clv.data = clv.data.apparel.withhold,
                                                data.cov.life  = apparelDynCov.2cat, names.cov.life = "Gender",
                                                data.cov.trans = apparelDynCov,      names.cov.trans = "Gender",
                                                name.date = "Cov.Date"))

  expect_true(ncol(dyn.cov@data.cov.life) == 3)
  expect_true(all(colnames(dyn.cov@data.cov.life) %in% c("Id", "Cov.Date", "GenderM")))
  expect_true(all(dyn.cov@names.cov.data.life %in% c("GenderM")))
  expect_true(dyn.cov@data.cov.life[, all(sapply(.SD, is.numeric)), .SDcols = c("GenderM")])

  expect_silent(dyn.cov <- SetDynamicCovariates(clv.data = clv.data.apparel.withhold,
                                                data.cov.life  = apparelDynCov,      names.cov.life = "Gender",
                                                data.cov.trans = apparelDynCov.2cat, names.cov.trans = "Gender",
                                                name.date = "Cov.Date"))
  expect_true(ncol(dyn.cov@data.cov.trans) == 3)
  expect_true(all(colnames(dyn.cov@data.cov.trans) %in% c("Id", "Cov.Date", "GenderM")))
  expect_true(all(dyn.cov@names.cov.data.trans %in% c("GenderM")))
  expect_true(dyn.cov@data.cov.trans[, all(sapply(.SD, is.numeric)), .SDcols = c("GenderM")])
})


test_that("Creates correct number of dummies - 3 categories",{


  apparelDynCov.3cat <- data.table::copy(apparelDynCov)
  apparelDynCov.3cat[, Gender := c(rep(c("F", "M", "X"), nrow(apparelDynCov.3cat)/3))]

  # Life
  expect_silent(dyn.cov <- SetDynamicCovariates(clv.data = clv.data.apparel.withhold,
                                                data.cov.life  = apparelDynCov.3cat, names.cov.life = "Gender",
                                                data.cov.trans = apparelDynCov, names.cov.trans = "Gender",
                                                name.date = "Cov.Date"))

  expect_true(ncol(dyn.cov@data.cov.life) == 4)
  expect_true(all(colnames(dyn.cov@data.cov.life) %in% c("Id", "Cov.Date", "GenderM", "GenderX")))
  expect_true(all(dyn.cov@names.cov.data.life %in% c("GenderM", "GenderX")))
  expect_true(dyn.cov@data.cov.life[, all(sapply(.SD, is.numeric)), .SDcols = c("GenderM", "GenderX")])

  # Trans
  expect_silent(dyn.cov <- SetDynamicCovariates(clv.data = clv.data.apparel.withhold,
                                                data.cov.life  = apparelDynCov, names.cov.life = "Gender",
                                                data.cov.trans = apparelDynCov.3cat, names.cov.trans = "Gender",
                                                name.date = "Cov.Date"))

  expect_true(ncol(dyn.cov@data.cov.trans) == 4)
  expect_true(all(colnames(dyn.cov@data.cov.trans) %in% c("Id", "Cov.Date", "GenderM", "GenderX")))
  expect_true(all(dyn.cov@names.cov.data.trans %in% c("GenderM", "GenderX")))
  expect_true(dyn.cov@data.cov.trans[, all(sapply(.SD, is.numeric)), .SDcols = c("GenderM", "GenderX")])
})


# Covariate datatypes ---------------------------------------------------------------------------
context("Correctness - SetDynamicCovariates - Covariate datatypes")

test_that("Converts categories to dummies - no numeric", {

  apparelDynCov.dummy <- data.table::copy(apparelDynCov)
  apparelDynCov.dummy[, Gender.char := as.character(Gender)]
  # Life
  expect_silent(dyn.cov <- SetDynamicCovariates(clv.data = clv.data.apparel.withhold,
                                                data.cov.life  = apparelDynCov.dummy, names.cov.life = "Gender.char",
                                                data.cov.trans = apparelDynCov, names.cov.trans = "Gender",
                                                name.date = "Cov.Date"))

  expect_true(ncol(dyn.cov@data.cov.life) == 3)
  # expect_true(nrow(dyn.cov@data.cov.life) == nrow(apparelDynCov.dummy))
  expect_true(all(colnames(dyn.cov@data.cov.life) %in% c("Id", "Cov.Date", "Gender.char1")))
  expect_true(dyn.cov@data.cov.life[, all(sapply(.SD, is.numeric)), .SDcols = "Gender.char1"])

  # Trans
  expect_silent(dyn.cov <- SetDynamicCovariates(clv.data = clv.data.apparel.withhold,
                                                data.cov.life  = apparelDynCov, names.cov.life = "Gender",
                                                data.cov.trans = apparelDynCov.dummy, names.cov.trans = "Gender.char",
                                                name.date = "Cov.Date"))

  expect_true(ncol(dyn.cov@data.cov.trans) == 3)
  # expect_true(nrow(dyn.cov@data.cov.trans) == nrow(apparelDynCov.dummy))
  expect_true(all(colnames(dyn.cov@data.cov.trans) %in% c("Id", "Cov.Date", "Gender.char1")))
  expect_true(dyn.cov@data.cov.trans[, all(sapply(.SD, is.numeric)), .SDcols = "Gender.char1"])
})


test_that("Converts categories to dummies - with numeric", {

  apparelDynCov.mixed <- data.table::copy(apparelDynCov)
  apparelDynCov.mixed[, Gender.char := as.character(Gender)]

  # Life
  expect_silent(dyn.cov <- SetDynamicCovariates(clv.data = clv.data.apparel.withhold,
                                                data.cov.life  = apparelDynCov.mixed, names.cov.life =c("Gender","Gender.char"),
                                                data.cov.trans = apparelDynCov, names.cov.trans = "Gender",
                                                name.date = "Cov.Date"))

  expect_true(ncol(dyn.cov@data.cov.life) == 4)
  # expect_true(nrow(dyn.cov@data.cov.life) == nrow(apparelDynCov.mixed))
  expect_true(all(colnames(dyn.cov@data.cov.life) %in% c("Id","Cov.Date", "Gender","Gender.char1")))
  expect_true(dyn.cov@data.cov.life[, all(sapply(.SD, is.numeric)), .SDcols = c("Gender","Gender.char1")])

  # Trans
  expect_silent(dyn.cov <- SetDynamicCovariates(clv.data = clv.data.apparel.withhold,
                                                data.cov.life  = apparelDynCov, names.cov.life = "Gender",
                                                data.cov.trans = apparelDynCov.mixed, names.cov.trans = c("Gender","Gender.char"),
                                                name.date = "Cov.Date"))

  expect_true(ncol(dyn.cov@data.cov.trans) == 4)
  # expect_true(nrow(dyn.cov@data.cov.trans) == nrow(apparelDynCov.mixed))
  expect_true(all(colnames(dyn.cov@data.cov.trans) %in% c("Id","Cov.Date", "Gender","Gender.char1")))
  expect_true(dyn.cov@data.cov.trans[, all(sapply(.SD, is.numeric)), .SDcols = c("Gender","Gender.char1")])
})


# test_that("Keeps numeric as numeric - no categories", {})
# test_that("Keeps numeric as numeric - with categories", {})


# Copied ----------------------------------------------------------------------------------------

test_that("Cov data was properly copied", {

  expect_silent(dyn.cov <- SetDynamicCovariates(clv.data = clv.data.apparel.withhold,
                                                data.cov.life  = apparelDynCov, names.cov.life = c("DM", "High.Season", "Gender"),
                                                data.cov.trans = apparelDynCov, names.cov.trans = c("DM", "High.Season", "Gender"),
                                                name.date = "Cov.Date"))
  expect_false(isTRUE(all.equal(data.table::address(dyn.cov@data.cov.life),
                                data.table::address(apparelDynCov))))
  expect_false(isTRUE(all.equal(data.table::address(dyn.cov@data.cov.trans),
                                data.table::address(apparelDynCov))))
})
