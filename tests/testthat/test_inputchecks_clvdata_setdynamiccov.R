# Load data ---------------------------------------------------------------------------------------
data("apparelTrans")
data("apparelDynCov")
data("apparelStaticCov")

# cutoff first as will result in "cutoff" message and not silent anymore
apparelDynCov <- apparelDynCov[Cov.Date > "2005-01-01" ]

context("Inputchecks - SetDynamicCovariates - Parameter clv.data")

expect_silent(clv.data.apparel <- clvdata(apparelTrans, date.format = "ymd", time.unit = "w"))


# Parameter clv.data ---------------------------------------------------------------------------------------
test_that("Fails for missing/NA/NULL", {
  expect_error(SetDynamicCovariates(clv.data = ,
                                   data.cov.life  = apparelDynCov, names.cov.life = c("Marketing", "Gender", c("Marketing", "Gender", "Channel")),
                                   data.cov.trans = apparelDynCov, names.cov.trans = c("Marketing", "Gender", c("Marketing", "Gender", "Channel"))))
  expect_error(SetDynamicCovariates(clv.data = NULL,
                                   data.cov.life  = apparelDynCov, names.cov.life = c("Marketing", "Gender", c("Marketing", "Gender", "Channel")),
                                   data.cov.trans = apparelDynCov, names.cov.trans = c("Marketing", "Gender", c("Marketing", "Gender", "Channel"))))
  expect_error(SetDynamicCovariates(clv.data = NA_real_,
                                   data.cov.life  = apparelDynCov, names.cov.life = c("Marketing", "Gender", c("Marketing", "Gender", "Channel")),
                                   data.cov.trans = apparelDynCov, names.cov.trans = c("Marketing", "Gender", c("Marketing", "Gender", "Channel")),
                                   name.date = "Cov.Date"))
})


test_that("Fails if not clv.data input", {
  # dataframe / transactions
  expect_error(SetDynamicCovariates(clv.data = apparelTrans,
                                   data.cov.life  = apparelDynCov, names.cov.life = c("Marketing", "Gender", "Channel"),
                                   data.cov.trans = apparelDynCov, names.cov.trans = c("Marketing", "Gender", "Channel"),
                                   name.date = "Cov.Date"))
  expect_error(SetDynamicCovariates(clv.data = list(apparelTrans),
                                   data.cov.life  = apparelDynCov, names.cov.life = c("Marketing", "Gender", "Channel"),
                                   data.cov.trans = apparelDynCov, names.cov.trans = c("Marketing", "Gender", "Channel"),
                                   name.date = "Cov.Date"))
})

test_that("Fails if already has covariates", {
  expect_silent(clv.data.apparel.staticcov <-
                  SetStaticCovariates(clv.data = clv.data.apparel,
                                      data.cov.life  = apparelStaticCov, names.cov.life = c("Gender"),
                                      data.cov.trans = apparelStaticCov, names.cov.trans = c("Gender")))
  expect_error(SetDynamicCovariates(clv.data = clv.data.apparel.staticcov,
                                   data.cov.life  = apparelStaticCov, names.cov.life = c("Marketing", "Gender", "Channel"),
                                   data.cov.trans = apparelStaticCov, names.cov.trans = c("Marketing", "Gender", "Channel"),
                                   name.date = "Cov.Date"),
               regexp = "Cannot set")

  expect_silent(clv.data.apparel.dyncov <-
                  SetDynamicCovariates(clv.data = clv.data.apparel,
                                      data.cov.life  = apparelDynCov, names.cov.life = c("Marketing", "Gender"),
                                      data.cov.trans = apparelDynCov, names.cov.trans = c("Marketing", "Gender"),
                                      name.date = "Cov.Date"))
  expect_error(SetDynamicCovariates(clv.data = clv.data.apparel.dyncov,
                                    data.cov.life  = apparelDynCov, names.cov.life = c("Marketing", "Gender"),
                                    data.cov.trans = apparelDynCov, names.cov.trans = c("Marketing", "Gender"),
                                    name.date = "Cov.Date"),
               regexp = "Cannot set")
})


# Parameter data.cov.life ---------------------------------------------------------------------------------------
context("Inputchecks - SetDynamicCovariates - Parameter data.cov.life")

# ** TODO: id type wrong?

test_that("Fails if missing/NULL/NA", {
  expect_error(SetDynamicCovariates(clv.data = clv.data.apparel,
                                   data.cov.life  = , names.cov.life = c("Marketing", "Gender", "Channel"),
                                   data.cov.trans = apparelDynCov, names.cov.trans = c("Marketing", "Gender", "Channel"),
                                   name.date = "Cov.Date"),
               regexp = "missing")
  expect_error(SetDynamicCovariates(clv.data = clv.data.apparel,
                                   data.cov.life = NULL, names.cov.life = c("Marketing", "Gender", "Channel"),
                                   data.cov.trans = apparelDynCov, names.cov.trans = c("Marketing", "Gender", "Channel"),
                                   name.date = "Cov.Date"),
               regexp = "type data.frame or data.table")
  expect_error(SetDynamicCovariates(clv.data = clv.data.apparel,
                                   data.cov.life = NA, names.cov.life = c("Marketing", "Gender", "Channel"),
                                   data.cov.trans = apparelDynCov, names.cov.trans = c("Marketing", "Gender", "Channel"),
                                   name.date = "Cov.Date"),
               regexp = "type data.frame or data.table")
})

test_that("Fails if not dataframe/datetable", {
  expect_error(SetDynamicCovariates(clv.data = clv.data.apparel,
                                   data.cov.life  = as.list(apparelDynCov), names.cov.life = c("Marketing", "Gender", "Channel"),
                                   data.cov.trans = apparelDynCov, names.cov.trans = c("Marketing", "Gender", "Channel"),
                                   name.date = "Cov.Date"),
               regexp = "type data.frame or data.table")
})

test_that("Fails if empty", {
  expect_error(SetDynamicCovariates(clv.data = clv.data.apparel,
                                   data.cov.life  = data.frame(), names.cov.life = c("Marketing", "Gender", "Channel"),
                                   data.cov.trans = apparelDynCov, names.cov.trans = c("Marketing", "Gender", "Channel"),
                                   name.date = "Cov.Date"),
               regexp = "empty")
  expect_error(SetDynamicCovariates(clv.data = clv.data.apparel,
                                   data.cov.life  = data.frame(Id=character(), Gender=character()), names.cov.life = c("Marketing", "Gender", "Channel"),
                                   data.cov.trans = apparelDynCov, names.cov.trans = c("Marketing", "Gender", "Channel"),
                                   name.date = "Cov.Date"),
               regexp = "empty")
})

test_that("Fails if covariate data is to short for all customers",{
  apparelDynCov.tooshort <- data.table::copy(apparelDynCov)
  apparelDynCov.tooshort <- apparelDynCov.tooshort[Cov.Date < "2005-02-01"]
                                                     # clv.data.apparel@clv.time@timepoint.estimation.end - lubridate::dweeks(15)]

  expect_error(SetDynamicCovariates(clv.data = clv.data.apparel,
                                    data.cov.life  = apparelDynCov.tooshort, names.cov.life = c("Marketing", "Gender", "Channel"),
                                    data.cov.trans = apparelDynCov, names.cov.trans = c("Marketing", "Gender", "Channel"),
                                    name.date = "Cov.Date"),
               regexp = "covariate data exactly from")
})

test_that("Fails if does not have covariates for all customers", {
  apparelDynCov.1missing <- data.table::copy(apparelDynCov)
  first.customer <- apparelDynCov.1missing[1,]$Id
  # delete all by this customer
  apparelDynCov.1missing <- apparelDynCov.1missing[Id != first.customer]
  expect_error(SetDynamicCovariates(clv.data = clv.data.apparel,
                                    data.cov.life  = apparelDynCov.1missing, names.cov.life = c("Marketing", "Gender", "Channel"),
                                    data.cov.trans = apparelDynCov, names.cov.trans = c("Marketing", "Gender", "Channel"),
                                    name.date = "Cov.Date"),
               regexp = "Every Id")
})

test_that("Fails if does not have covariates for all customers and dates", {
  apparelDynCov.1missing <- data.table::copy(apparelDynCov)
  apparelDynCov.1missing <- apparelDynCov.1missing[-1000]
  expect_error(SetDynamicCovariates(clv.data = clv.data.apparel,
                                   data.cov.life  = apparelDynCov.1missing, names.cov.life = c("Marketing", "Gender", "Channel"),
                                   data.cov.trans = apparelDynCov,          names.cov.trans = c("Marketing", "Gender", "Channel"),
                                   name.date = "Cov.Date"),
               regexp = "need to have the same number of Dates")
})

test_that("Fails if duplicate covariate (Id/Date)", {

  apparelDynCov.duplicate.same <-
    data.table::rbindlist(list(apparelDynCov, apparelDynCov[1000]))

  apparelDynCov.duplicate.diff <-
    data.table::rbindlist(list(apparelDynCov,
                   # Different cov value
                   apparelDynCov[1000,.(Id,Cov.Date, Marketing=1,Gender=0, Channel=0)]))

  expect_error(SetDynamicCovariates(clv.data = clv.data.apparel,
                                    data.cov.life  = apparelDynCov.duplicate.same, names.cov.life = c("Marketing", "Gender", "Channel"),
                                    data.cov.trans = apparelDynCov, names.cov.trans = c("Marketing", "Gender", "Channel"),
                                    name.date = "Cov.Date"),
               regexp = "need to have the same number")
  expect_error(SetDynamicCovariates(clv.data = clv.data.apparel,
                                    data.cov.life  = apparelDynCov.duplicate.diff, names.cov.life = c("Marketing", "Gender", "Channel"),
                                    data.cov.trans = apparelDynCov, names.cov.trans = c("Marketing", "Gender", "Channel"),
                                    name.date = "Cov.Date"),
               regexp = "need to have the same number")
})


test_that("Fails if any NA in cov data - Id", {
  apparelDemo.1na <- data.table::copy(apparelDynCov)
  apparelDemo.1na[1000, Id := NA]
  expect_error(SetDynamicCovariates(clv.data = clv.data.apparel,
                                   data.cov.life  = apparelDemo.1na, names.cov.life = c("Marketing", "Gender", "Channel"),
                                   data.cov.trans = apparelDynCov, names.cov.trans = c("Marketing", "Gender", "Channel"),
                                   name.date = "Cov.Date"),
               regexp = "any NA")
})

test_that("Fails if any NA in cov data - Date", {
  apparelDemo.1na <- data.table::copy(apparelDynCov)
  apparelDemo.1na[1000, Cov.Date := NA_real_]
  expect_error(SetDynamicCovariates(clv.data = clv.data.apparel,
                                    data.cov.life  = apparelDemo.1na, names.cov.life = c("Marketing", "Gender", "Channel"),
                                    data.cov.trans = apparelDynCov, names.cov.trans = c("Marketing", "Gender", "Channel"),
                                    name.date = "Cov.Date"),
               regexp = "any NA")
})

test_that("Fails if any NA in cov data - Cov", {
  apparelDemo.1na <- data.table::copy(apparelDynCov)
  apparelDemo.1na[1000, Channel := NA_real_]
  expect_error(SetDynamicCovariates(clv.data = clv.data.apparel,
                                    data.cov.life  = apparelDemo.1na, names.cov.life = c("Marketing", "Gender", "Channel"),
                                    data.cov.trans = apparelDynCov, names.cov.trans = c("Marketing", "Gender", "Channel"),
                                    name.date = "Cov.Date"),
               regexp = "any NA")

  apparelDemo.1na <- data.table::copy(apparelDynCov)
  apparelDemo.1na[1000, Gender := NA_real_]
  expect_error(SetDynamicCovariates(clv.data = clv.data.apparel,
                                    data.cov.life  = apparelDemo.1na, names.cov.life = c("Marketing", "Gender", "Channel"),
                                    data.cov.trans = apparelDynCov, names.cov.trans = c("Marketing", "Gender", "Channel"),
                                    name.date = "Cov.Date"),
               regexp = "any NA")


  apparelDemo.1na <- data.table::copy(apparelDynCov)
  apparelDemo.1na[1000, Marketing := NA_real_]
  expect_error(SetDynamicCovariates(clv.data = clv.data.apparel,
                                    data.cov.life  = apparelDemo.1na, names.cov.life = c("Marketing", "Gender", "Channel"),
                                    data.cov.trans = apparelDynCov, names.cov.trans = c("Marketing", "Gender", "Channel"),
                                    name.date = "Cov.Date"),
               regexp = "any NA")
})



test_that("Fails for variable with single category", {
  apparelDynCov.1cat <- data.table::copy(apparelDynCov)
  apparelDynCov.1cat[, Gender := as.character(Gender)]
  apparelDynCov.1cat[, Gender := "F/M"]

  # "variable with a single category cannot be used as covariates."
  expect_error(SetDynamicCovariates(clv.data = clv.data.apparel,
                                    data.cov.life  = apparelDynCov.1cat,  names.cov.life  = c("Marketing", "Gender", "Channel"),
                                    data.cov.trans = apparelDynCov,       names.cov.trans = c("Marketing", "Gender", "Channel"),
                                    name.date = "Cov.Date"),
               regexp = "variables with only a single category")
})


# Parameter data.cov.trans ---------------------------------------------------------------------------------------
context("Inputchecks - SetDynamicCovariates - Parameter data.cov.trans")

test_that("Fails if missing/NULL/NA", {
  expect_error(SetDynamicCovariates(clv.data = clv.data.apparel,
                                    data.cov.life  = apparelDynCov, names.cov.life = c("Marketing", "Gender", "Channel"),
                                    data.cov.trans = , names.cov.trans = c("Marketing", "Gender", "Channel"),
                                    name.date = "Cov.Date"),
               regexp = "missing")
  expect_error(SetDynamicCovariates(clv.data = clv.data.apparel,
                                    data.cov.life = apparelDynCov, names.cov.life = c("Marketing", "Gender", "Channel"),
                                    data.cov.trans = NULL, names.cov.trans = c("Marketing", "Gender", "Channel"),
                                    name.date = "Cov.Date"),
               regexp = "type data.frame or data.table")
  expect_error(SetDynamicCovariates(clv.data = clv.data.apparel,
                                    data.cov.life = apparelDynCov, names.cov.life = c("Marketing", "Gender", "Channel"),
                                    data.cov.trans = NA, names.cov.trans = c("Marketing", "Gender", "Channel"),
                                    name.date = "Cov.Date"),
               regexp = "type data.frame or data.table")
})

test_that("Fails if not dataframe/datetable", {
  expect_error(SetDynamicCovariates(clv.data = clv.data.apparel,
                                    data.cov.life  = apparelDynCov, names.cov.life = c("Marketing", "Gender", "Channel"),
                                    data.cov.trans = as.list(apparelDynCov), names.cov.trans = cc("Marketing", "Gender", "Channel"),
                                    name.date = "Cov.Date"),
               regexp = "type data.frame or data.table")
})

test_that("Fails if empty", {
  expect_error(SetDynamicCovariates(clv.data = clv.data.apparel,
                                    data.cov.life  = apparelDynCov, names.cov.life = c("Marketing", "Gender", "Channel"),
                                    data.cov.trans = data.frame(), names.cov.trans = c("Marketing", "Gender", "Channel"),
                                    name.date = "Cov.Date"),
               regexp = "empty")
  expect_error(SetDynamicCovariates(clv.data = clv.data.apparel,
                                    data.cov.life  = apparelDynCov,
                                    names.cov.life = c("Marketing", "Gender", "Channel"),
                                    data.cov.trans = data.frame(Id=character(), Gender=character()),
                                    names.cov.trans = c("Marketing", "Gender", "Channel"),
                                    name.date = "Cov.Date"),
               regexp = "empty")
})


test_that("Fails if covariate data is to short for all customers",{
  apparelDynCov.tooshort <- data.table::copy(apparelDynCov)
  apparelDynCov.tooshort <- apparelDynCov.tooshort[Cov.Date < "2005-02-01"]
  # clv.data.apparel@clv.time@timepoint.estimation.end - lubridate::dweeks(15)]

  expect_error(SetDynamicCovariates(clv.data = clv.data.apparel,
                                    data.cov.life  = apparelDynCov, names.cov.life = c("Marketing", "Gender", "Channel"),
                                    data.cov.trans = apparelDynCov.tooshort, names.cov.trans = c("Marketing", "Gender", "Channel"),
                                    name.date = "Cov.Date"),
               regexp = "covariate data exactly from")
})


test_that("Fails if does not have covariates for all customers", {
  apparelDemo.1missing <- data.table::copy(apparelDynCov)
  first.customer <- apparelDemo.1missing[1,]$Id
  # delete all by this customer
  apparelDemo.1missing <- apparelDemo.1missing[Id != first.customer]
  expect_error(SetDynamicCovariates(clv.data = clv.data.apparel,
                                    data.cov.life  = apparelDynCov, names.cov.life = c("Marketing", "Gender", "Channel"),
                                    data.cov.trans = apparelDemo.1missing, names.cov.trans = c("Marketing", "Gender", "Channel"),
                                    name.date = "Cov.Date"),
               regexp = "Every Id")
})

test_that("Fails if does not have covariates for all customers and dates", {
  apparelDemo.1missing <- data.table::copy(apparelDynCov)
  apparelDemo.1missing <- apparelDemo.1missing[-1000]
  expect_error(SetDynamicCovariates(clv.data = clv.data.apparel,
                                    data.cov.life  = apparelDynCov, names.cov.life = c("Marketing", "Gender", "Channel"),
                                    data.cov.trans = apparelDemo.1missing, names.cov.trans = c("Marketing", "Gender", "Channel"),
                                    name.date = "Cov.Date"),
               regexp = "need to have the same number of Dates")
})

test_that("Fails if duplicate covariate (Id/Date)", {

  apparelDynCov.duplicate.same <-
    data.table::rbindlist(list(apparelDynCov, apparelDynCov[1000]))

  apparelDynCov.duplicate.diff <-
    data.table::rbindlist(list(apparelDynCov,
                   # Different cov value
                   apparelDynCov[1000,.(Id,Cov.Date, Marketing=1,Gender=0, Channel=0)]))

  expect_error(SetDynamicCovariates(clv.data = clv.data.apparel,
                                    data.cov.life  = apparelDynCov, names.cov.life = c("Marketing", "Gender", "Channel"),
                                    data.cov.trans = apparelDynCov.duplicate.same, names.cov.trans = c("Marketing", "Gender", "Channel"),
                                    name.date = "Cov.Date"),
               regexp = "need to have the same number")
  expect_error(SetDynamicCovariates(clv.data = clv.data.apparel,
                                    data.cov.life  = apparelDynCov, names.cov.life = c("Marketing", "Gender", "Channel"),
                                    data.cov.trans = apparelDynCov.duplicate.diff, names.cov.trans = c("Marketing", "Gender", "Channel"),
                                    name.date = "Cov.Date"),
               regexp = "need to have the same number")
})


test_that("Fails if any NA in cov data - Id", {
  apparelDemo.1na <- data.table::copy(apparelDynCov)
  apparelDemo.1na[1000, Id := NA]
  expect_error(SetDynamicCovariates(clv.data = clv.data.apparel,
                                    data.cov.life  = apparelDynCov, names.cov.life = c("Marketing", "Gender", "Channel"),
                                    data.cov.trans = apparelDemo.1na, names.cov.trans = c("Marketing", "Gender", "Channel"),
                                    name.date = "Cov.Date"),
               regexp = "any NA")
})

test_that("Fails if any NA in cov data - Date", {
  apparelDemo.1na <- data.table::copy(apparelDynCov)
  apparelDemo.1na[1000, Cov.Date := NA_real_]
  expect_error(SetDynamicCovariates(clv.data = clv.data.apparel,
                                    data.cov.life  = apparelDynCov, names.cov.life = c("Marketing", "Gender", "Channel"),
                                    data.cov.trans = apparelDemo.1na, names.cov.trans = c("Marketing", "Gender", "Channel"),
                                    name.date = "Cov.Date"),
               regexp = "any NA")
})

test_that("Fails if any NA in cov data - Cov", {
  apparelDemo.1na <- data.table::copy(apparelDynCov)
  apparelDemo.1na[1000, Channel := NA_real_]
  expect_error(SetDynamicCovariates(clv.data = clv.data.apparel,
                                    data.cov.life  = apparelDynCov, names.cov.life = c("Marketing", "Gender", "Channel"),
                                    data.cov.trans = apparelDemo.1na, names.cov.trans = c("Marketing", "Gender", "Channel"),
                                    name.date = "Cov.Date"),
               regexp = "any NA")

  apparelDemo.1na <- data.table::copy(apparelDynCov)
  apparelDemo.1na[1000, Gender := NA_real_]
  expect_error(SetDynamicCovariates(clv.data = clv.data.apparel,
                                    data.cov.life  = apparelDynCov, names.cov.life = c("Marketing", "Gender", "Channel"),
                                    data.cov.trans = apparelDemo.1na, names.cov.trans = c("Marketing", "Gender", "Channel"),
                                    name.date = "Cov.Date"),
               regexp = "any NA")


  apparelDemo.1na <- data.table::copy(apparelDynCov)
  apparelDemo.1na[1000, Marketing := NA_real_]
  expect_error(SetDynamicCovariates(clv.data = clv.data.apparel,
                                    data.cov.life  = apparelDynCov, names.cov.life = c("Marketing", "Gender", "Channel"),
                                    data.cov.trans = apparelDemo.1na, names.cov.trans = c("Marketing", "Gender", "Channel"),
                                    name.date = "Cov.Date"),
               regexp = "any NA")
})


test_that("Fails for variable with single category", {
  apparelDynCov.1cat <- data.table::copy(apparelDynCov)
  apparelDynCov.1cat[, Gender := as.character(Gender)]
  apparelDynCov.1cat[, Gender := "F/M"]

  # "variable with a single category cannot be used as covariates."
  expect_error(SetDynamicCovariates(clv.data = clv.data.apparel,
                                    data.cov.life  = apparelDynCov,     names.cov.life  = c("Marketing", "Gender", "Channel"),
                                    data.cov.trans = apparelDynCov.1cat,names.cov.trans = c("Marketing", "Gender", "Channel"),
                                    name.date = "Cov.Date"),
               regexp = "variables with only a single category")
})

# Parameter name.cov.life ---------------------------------------------------------------------------------------
context("Inputchecks - SetDynamicCovariates - Parameter name.cov.life")
test_that("Fails if missing/NULL/NA/empty",{
  expect_error(SetDynamicCovariates(clv.data = clv.data.apparel,
                                   data.cov.life = apparelDynCov, names.cov.life = ,
                                   data.cov.trans = apparelDynCov, names.cov.trans = c("Marketing", "Gender", "Channel"),
                                   name.date = "Cov.Date"),
               regexp = "missing")
  expect_error(SetDynamicCovariates(clv.data = clv.data.apparel,
                                   data.cov.life = apparelDynCov, names.cov.life = NULL ,
                                   data.cov.trans = apparelDynCov, names.cov.trans = c("Marketing", "Gender", "Channel"),
                                   name.date = "Cov.Date"),
               regexp = "may not be NULL")
  expect_error(SetDynamicCovariates(clv.data = clv.data.apparel,
                                   data.cov.life = apparelDynCov, names.cov.life = NA_character_ ,
                                   data.cov.trans = apparelDynCov, names.cov.trans = c("Marketing", "Gender", "Channel"),
                                   name.date = "Cov.Date"),
               regexp = "any NA")
  expect_error(SetDynamicCovariates(clv.data = clv.data.apparel,
                                   data.cov.life = apparelDynCov, names.cov.life = "" ,
                                   data.cov.trans = apparelDynCov, names.cov.trans = c("Marketing", "Gender", "Channel"),
                                   name.date = "Cov.Date"),
               regexp = "could not be found")
})

test_that("Fails if not character vector",{
  # ** Fails properly
  expect_error(SetDynamicCovariates(clv.data = clv.data.apparel,
                                   data.cov.life = apparelDynCov, names.cov.life = list(c("Marketing", "Gender", "Channel")),
                                   data.cov.trans = apparelDynCov, names.cov.trans = c("Marketing", "Gender", "Channel"),
                                   name.date = "Cov.Date"),
               regexp = "character")
  expect_error(SetDynamicCovariates(clv.data = clv.data.apparel,
                                   data.cov.life = apparelDynCov, names.cov.life = data.frame(c("Marketing", "Gender", "Channel")),
                                   data.cov.trans = apparelDynCov, names.cov.trans = c("Marketing", "Gender", "Channel"),
                                   name.date = "Cov.Date"),
               regexp = "character")
  expect_error(SetDynamicCovariates(clv.data = clv.data.apparel,
                                   data.cov.life = apparelDynCov, names.cov.life = 2,
                                   data.cov.trans = apparelDynCov, names.cov.trans = c("Marketing", "Gender", "Channel"),
                                   name.date = "Cov.Date"),
               regexp = "character")
})

test_that("Fails if contains NA", {
  expect_error(SetDynamicCovariates(clv.data = clv.data.apparel,
                                   data.cov.life = apparelDynCov, names.cov.life = NA_character_,
                                   data.cov.trans = apparelDynCov, names.cov.trans = c("Marketing", "Gender", "Channel"),
                                   name.date = "Cov.Date"),
               regexp = "any NA")
  expect_error(SetDynamicCovariates(clv.data = clv.data.apparel,
                                   data.cov.life = apparelDynCov, names.cov.life = c("Marketing", "Gender", "Channel", NA_character_),
                                   data.cov.trans = apparelDynCov, names.cov.trans = c("Marketing", "Gender", "Channel"),
                                   name.date = "Cov.Date"),
               regexp = "any NA")
})

test_that("Fails if names not in data", {
  # ** TODO: Check and fail properly
  expect_error(SetDynamicCovariates(clv.data = clv.data.apparel,
                                   data.cov.life = apparelDynCov, names.cov.life = c("Marketing", "Gender", "ChannelBender"),
                                   data.cov.trans = apparelDynCov, names.cov.trans = c("Marketing", "Gender", "Channel"),
                                   name.date = "Cov.Date"),
               regexp = "could not be found")
  expect_error(SetDynamicCovariates(clv.data = clv.data.apparel,
                                   data.cov.life = apparelDynCov, names.cov.life = c("Marketing", "Gender", "Channel"),
                                   data.cov.trans = apparelDynCov, names.cov.trans = c("Marketing", "Gender", "ChannelBender"),
                                   name.date = "Cov.Date"),
               regexp = "could not be found")
})

test_that("Fails if has duplicate names", {
  # ** TODO: Fail properly
  expect_error(SetDynamicCovariates(clv.data = clv.data.apparel,
                                   data.cov.life = apparelDynCov, names.cov.life = c("Marketing", "Gender", "Channel", "Channel"),
                                   data.cov.trans = apparelDynCov, names.cov.trans = c("Marketing", "Gender", "Channel"),
                                   name.date = "Cov.Date"),
               regexp = "duplicates")

  expect_error(SetDynamicCovariates(clv.data = clv.data.apparel,
                                    data.cov.life = apparelDynCov, names.cov.life = c("Gender", "Gender"),
                                    data.cov.trans = apparelDynCov, names.cov.trans = c("Marketing", "Gender", "Channel"),
                                    name.date = "Cov.Date"),
               regexp = "duplicates")
})


# Parameter name.cov.trans ---------------------------------------------------------------------------------------
context("Inputchecks - SetDynamicCovariates - Parameter name.cov.trans")
test_that("Fails if missing/NULL/NA/empty",{
  expect_error(SetDynamicCovariates(clv.data = clv.data.apparel,
                                    data.cov.life = apparelDynCov, names.cov.life = c("Marketing", "Gender", "Channel"),
                                    data.cov.trans = apparelDynCov, names.cov.trans = ,
                                    name.date = "Cov.Date"),
               regexp = "missing")
  expect_error(SetDynamicCovariates(clv.data = clv.data.apparel,
                                    data.cov.life = apparelDynCov, names.cov.life = c("Marketing", "Gender", "Channel") ,
                                    data.cov.trans = apparelDynCov, names.cov.trans = NULL,
                                    name.date = "Cov.Date"),
               regexp = "may not be NULL")
  expect_error(SetDynamicCovariates(clv.data = clv.data.apparel,
                                    data.cov.life = apparelDynCov, names.cov.life = c("Marketing", "Gender", "Channel") ,
                                    data.cov.trans = apparelDynCov, names.cov.trans = NA_character_,
                                    name.date = "Cov.Date"),
               regexp = "any NA")
  expect_error(SetDynamicCovariates(clv.data = clv.data.apparel,
                                    data.cov.life = apparelDynCov, names.cov.life = c("Marketing", "Gender", "Channel") ,
                                    data.cov.trans = apparelDynCov, names.cov.trans = "",
                                    name.date = "Cov.Date"),
               regexp = "could not be found")
})

test_that("Fails if not character vector",{
  # ** Fails properly
  expect_error(SetDynamicCovariates(clv.data = clv.data.apparel,
                                    data.cov.life = apparelDynCov, names.cov.life = c("Marketing", "Gender", "Channel"),
                                    data.cov.trans = apparelDynCov, names.cov.trans = list("Marketing", "Gender", "Channel"),
                                    name.date = "Cov.Date"),
               regexp = "character")
  expect_error(SetDynamicCovariates(clv.data = clv.data.apparel,
                                    data.cov.life = apparelDynCov, names.cov.life = c("Marketing", "Gender", "Channel"),
                                    data.cov.trans = apparelDynCov, names.cov.trans = data.frame(c("Marketing", "Gender", "Channel")),
                                    name.date = "Cov.Date"),
               regexp = "character")
  expect_error(SetDynamicCovariates(clv.data = clv.data.apparel,
                                    data.cov.life = apparelDynCov, names.cov.life = c("Marketing", "Gender", "Channel"),
                                    data.cov.trans = apparelDynCov, names.cov.trans = 2,
                                    name.date = "Cov.Date"),
               regexp = "character")
})

test_that("Fails if contains NA", {
  expect_error(SetDynamicCovariates(clv.data = clv.data.apparel,
                                    data.cov.life = apparelDynCov, names.cov.life = c("Marketing", "Gender", "Channel"),
                                    data.cov.trans = apparelDynCov, names.cov.trans = NA_character_,
                                    name.date = "Cov.Date"),
               regexp = "any NA")
  expect_error(SetDynamicCovariates(clv.data = clv.data.apparel,
                                    data.cov.life = apparelDynCov, names.cov.life = c("Marketing", "Gender", "Channel"),
                                    data.cov.trans = apparelDynCov, names.cov.trans = c("Marketing", "Gender", "Channel", NA_character_),
                                    name.date = "Cov.Date"),
               regexp = "any NA")
})

test_that("Fails if names not in data", {
  # ** TODO: Check and fail properly
  expect_error(SetDynamicCovariates(clv.data = clv.data.apparel,
                                    data.cov.life = apparelDynCov, names.cov.life = c("Marketing", "Gender", "Channel"),
                                    data.cov.trans = apparelDynCov, names.cov.trans = c("Marketing", "Gender", "GenderBender"),
                                    name.date = "Cov.Date"),
               regexp = "could not be found")
  expect_error(SetDynamicCovariates(clv.data = clv.data.apparel,
                                    data.cov.life = apparelDynCov, names.cov.life = c("Marketing", "Gend", "Channel"),
                                    data.cov.trans = apparelDynCov, names.cov.trans = c("Marketing", "Gend", "Channel"),
                                    name.date = "Cov.Date"),
               regexp = "could not be found")
})

test_that("Fails if has duplicate names", {
  # ** TODO: Fail properly
  expect_error(SetDynamicCovariates(clv.data = clv.data.apparel,
                                    data.cov.life = apparelDynCov, names.cov.life = c("Marketing", "Gender", "Channel"),
                                    data.cov.trans = apparelDynCov, names.cov.trans = c("Marketing", "Gender", "Channel", "Channel"),
                                    name.date = "Cov.Date"),
               regexp = "duplicates")

  expect_error(SetDynamicCovariates(clv.data = clv.data.apparel,
                                    data.cov.life = apparelDynCov, names.cov.life = c("Marketing", "Gender", "Channel"),
                                    data.cov.trans = apparelDynCov, names.cov.trans = c("Gender", "Gender"),
                                    name.date = "Cov.Date"),
               regexp = "duplicates")
})


# Parameter name.id ---------------------------------------------------------------------------------------
# **TODO: proper inputchecks and fails
l.std.args <- list(clv.data = clv.data.apparel,
                   data.cov.life = apparelDynCov, names.cov.life = c("Marketing", "Gender", "Channel"),
                   data.cov.trans = apparelDynCov, names.cov.trans = c("Marketing", "Gender", "Channel"),
                   name.date = "Cov.Date", name.id = "Id")

test_that("Fails if NA/NULL", {
  expect_error(do.call(SetDynamicCovariates, modifyList(l.std.args, list(name.id=NULL), keep.null = TRUE)),
               regexp = "NULL")
  expect_error(do.call(SetDynamicCovariates, modifyList(l.std.args, list(name.id=NA_character_))),
               regexp = "any NA")
  expect_error(do.call(SetDynamicCovariates, modifyList(l.std.args, list(name.id=character(0)))),
               regexp = "single element")
})

test_that("Fails if not character", {
  expect_error(do.call(SetDynamicCovariates, modifyList(l.std.args, list(name.id=list("Id")), keep.null = TRUE)),
               regexp = "character")
  expect_error(do.call(SetDynamicCovariates, modifyList(l.std.args, list(name.id=data.frame("Id")), keep.null = TRUE)),
               regexp = "character")
  expect_error(do.call(SetDynamicCovariates, modifyList(l.std.args, list(name.id=1), keep.null = TRUE)),
               regexp = "character")
})

test_that("Fails if multiple", {
  expect_error(do.call(SetDynamicCovariates, modifyList(l.std.args, list(name.id=c("Id", "Id")), keep.null = TRUE)),
               regexp = "single element")
  expect_error(do.call(SetDynamicCovariates, modifyList(l.std.args, list(name.id=c("Id", "Id")), keep.null = TRUE)),
               regexp = "single element")
  expect_error(do.call(SetDynamicCovariates, modifyList(l.std.args, list(name.id=c("Id", "Date")), keep.null = TRUE)),
               regexp = "single element")
})

test_that("Fails if not in transaction data", {
  expect_error(do.call(SetDynamicCovariates, modifyList(l.std.args, list(name.id="id"), keep.null = TRUE)),
               regexp = "not be found in the data")

  expect_error(do.call(SetDynamicCovariates, modifyList(l.std.args, list(name.id="ID"), keep.null = TRUE)),
               regexp = "not be found in the data")

  expect_error(do.call(SetDynamicCovariates, modifyList(l.std.args, list(name.id="di"), keep.null = TRUE)),
               regexp = "not be found in the data")

  expect_error(do.call(SetDynamicCovariates, modifyList(l.std.args, list(name.id="customer"), keep.null = TRUE)),
               regexp = "not be found in the data")
})

test_that("Has default argument Id",{
  default.arg <- eval(formals(SetDynamicCovariates)[["name.id"]])
  expect_is(default.arg, "character")
  expect_true(default.arg == "Id")
})


# Parameter name.date ---------------------------------------------------------------------------------------
test_that("Fails if NA/NULL", {
  expect_error(do.call(SetDynamicCovariates, modifyList(l.std.args, list(name.date=NULL), keep.null = TRUE)),
               regexp = "NULL")
  expect_error(do.call(SetDynamicCovariates, modifyList(l.std.args, list(name.date=NA_character_))),
               regexp = "any NA")
  expect_error(do.call(SetDynamicCovariates, modifyList(l.std.args, list(name.date=character(0)))),
               regexp = "single element")
})

test_that("Fails if not character", {
  expect_error(do.call(SetDynamicCovariates, modifyList(l.std.args, list(name.date=list("Id")), keep.null = TRUE)),
               regexp = "character")
  expect_error(do.call(SetDynamicCovariates, modifyList(l.std.args, list(name.date=data.frame("Id")), keep.null = TRUE)),
               regexp = "character")
  expect_error(do.call(SetDynamicCovariates, modifyList(l.std.args, list(name.date=1), keep.null = TRUE)),
               regexp = "character")
})

test_that("Fails if multiple", {
  expect_error(do.call(SetDynamicCovariates, modifyList(l.std.args, list(name.date=c("Id", "Id")), keep.null = TRUE)),
               regexp = "single element")
  expect_error(do.call(SetDynamicCovariates, modifyList(l.std.args, list(name.date=c("Id", "Id")), keep.null = TRUE)),
               regexp = "single element")
  expect_error(do.call(SetDynamicCovariates, modifyList(l.std.args, list(name.date=c("Id", "Date")), keep.null = TRUE)),
               regexp = "single element")
})

test_that("Fails if not in transaction data", {
  expect_error(do.call(SetDynamicCovariates, modifyList(l.std.args, list(name.date="id"), keep.null = TRUE)),
               regexp = "not be found in the data")

  expect_error(do.call(SetDynamicCovariates, modifyList(l.std.args, list(name.date="ID"), keep.null = TRUE)),
               regexp = "not be found in the data")

  expect_error(do.call(SetDynamicCovariates, modifyList(l.std.args, list(name.date="di"), keep.null = TRUE)),
               regexp = "not be found in the data")

  expect_error(do.call(SetDynamicCovariates, modifyList(l.std.args, list(name.date="customer"), keep.null = TRUE)),
               regexp = "not be found in the data")
})

test_that("Has default argument Id",{
  default.arg <- eval(formals(SetDynamicCovariates)[["name.date"]])
  expect_is(default.arg, "character")
  expect_true(default.arg == "Date")
})


