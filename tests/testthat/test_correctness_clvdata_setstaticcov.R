# Load data ---------------------------------------------------------------------------------------
data("apparelTrans")
data("apparelStaticCov")

# Covariate dummies ---------------------------------------------------------------------------------------
context("Correctness - SetStaticCovariates - Covariate dummies")

expect_silent(clv.data.apparel.nohold   <- clvdata(apparelTrans, date.format = "ymd", time.unit = "w"))
expect_silent(clv.data.apparel.withhold <- clvdata(apparelTrans, date.format = "ymd", time.unit = "w",
                                                   estimation.split = 39))


test_that("Factor and char covariates result in same dummies - no holdout",{
  apparelStaticCov.char <- data.table::copy(apparelStaticCov)
  apparelStaticCov.char[, Gender := as.character(Gender)]

  expect_silent(static.char.life <- SetStaticCovariates(clv.data = clv.data.apparel.nohold,
                                                        data.cov.life  = apparelStaticCov.char, names.cov.life = "Gender",
                                                        data.cov.trans = apparelStaticCov, names.cov.trans = "Gender"))

  expect_silent(static.char.trans <- SetStaticCovariates(clv.data = clv.data.apparel.nohold,
                                                        data.cov.life  = apparelStaticCov, names.cov.life = "Gender",
                                                        data.cov.trans = apparelStaticCov.char, names.cov.trans = "Gender"))

  expect_silent(static.char.both <- SetStaticCovariates(clv.data = clv.data.apparel.nohold,
                                    data.cov.life  = apparelStaticCov.char, names.cov.life = "Gender",
                                    data.cov.trans = apparelStaticCov.char, names.cov.trans = "Gender"))



  apparelStaticCov.factor <- data.table::copy(apparelStaticCov)
  apparelStaticCov.factor[, Gender := as.factor(as.character(Gender))]

  expect_silent(static.factor.life <- SetStaticCovariates(clv.data = clv.data.apparel.nohold,
                                                          data.cov.life  = apparelStaticCov.factor, names.cov.life = "Gender",
                                                          data.cov.trans = apparelStaticCov, names.cov.trans = "Gender"))

  expect_silent(static.factor.trans <- SetStaticCovariates(clv.data = clv.data.apparel.nohold,
                                                          data.cov.life  = apparelStaticCov, names.cov.life = "Gender",
                                                          data.cov.trans = apparelStaticCov.factor, names.cov.trans = "Gender"))

  expect_silent(static.factor.both <- SetStaticCovariates(clv.data = clv.data.apparel.nohold,
                                                   data.cov.life  = apparelStaticCov.factor, names.cov.life = "Gender",
                                                   data.cov.trans = apparelStaticCov.factor, names.cov.trans = "Gender"))


  expect_equal(static.char.life, static.factor.life)
  expect_equal(static.char.trans, static.factor.trans)
  expect_equal(static.char.both, static.factor.both)
})



test_that("Factor and char covariates result in same dummies - with holdout",{
  skip_on_cran()

  apparelStaticCov.char <- data.table::copy(apparelStaticCov)
  apparelStaticCov.char[, Gender := as.character(Gender)]

  expect_silent(static.char.life <- SetStaticCovariates(clv.data = clv.data.apparel.withhold,
                                                        data.cov.life  = apparelStaticCov.char, names.cov.life = "Gender",
                                                        data.cov.trans = apparelStaticCov, names.cov.trans = "Gender"))

  expect_silent(static.char.trans <- SetStaticCovariates(clv.data = clv.data.apparel.withhold,
                                                         data.cov.life  = apparelStaticCov, names.cov.life = "Gender",
                                                         data.cov.trans = apparelStaticCov.char, names.cov.trans = "Gender"))

  expect_silent(static.char.both <- SetStaticCovariates(clv.data = clv.data.apparel.withhold,
                                                        data.cov.life  = apparelStaticCov.char, names.cov.life = "Gender",
                                                        data.cov.trans = apparelStaticCov.char, names.cov.trans = "Gender"))



  apparelStaticCov.factor <- data.table::copy(apparelStaticCov)
  apparelStaticCov.factor[, Gender := as.factor(as.character(Gender))]

  expect_silent(static.factor.life <- SetStaticCovariates(clv.data = clv.data.apparel.withhold,
                                                          data.cov.life  = apparelStaticCov.factor, names.cov.life = "Gender",
                                                          data.cov.trans = apparelStaticCov, names.cov.trans = "Gender"))

  expect_silent(static.factor.trans <- SetStaticCovariates(clv.data = clv.data.apparel.withhold,
                                                           data.cov.life  = apparelStaticCov, names.cov.life = "Gender",
                                                           data.cov.trans = apparelStaticCov.factor, names.cov.trans = "Gender"))

  expect_silent(static.factor.both <- SetStaticCovariates(clv.data = clv.data.apparel.withhold,
                                                          data.cov.life  = apparelStaticCov.factor, names.cov.life = "Gender",
                                                          data.cov.trans = apparelStaticCov.factor, names.cov.trans = "Gender"))

  expect_equal(static.char.life, static.factor.life)
  expect_equal(static.char.trans, static.factor.trans)
  expect_equal(static.char.both, static.factor.both)
})


test_that("Creates correct number of dummies - 2 categories", {
  skip_on_cran()

  apparelStaticCov.2cat <- data.table::copy(apparelStaticCov)
  apparelStaticCov.2cat[, Gender := c(rep(c("F", "M"), nrow(apparelStaticCov.2cat)/2))]

  expect_silent(static.cov <- SetStaticCovariates(clv.data = clv.data.apparel.withhold,
                                                  data.cov.life  = apparelStaticCov.2cat, names.cov.life = "Gender",
                                                  data.cov.trans = apparelStaticCov,      names.cov.trans = "Gender"))

  expect_true(ncol(static.cov@data.cov.life) == 2)
  expect_true(all(colnames(static.cov@data.cov.life) %in% c("Id", "GenderM")))
  expect_true(static.cov@data.cov.life[, all(sapply(.SD, is.numeric)), .SDcols = c("GenderM")])
})

test_that("Creates correct number of dummies - 3 categories",{
  skip_on_cran()
  apparelStaticCov.3cat <- data.table::copy(apparelStaticCov)
  apparelStaticCov.3cat[, Gender := c("F", rep(c("F", "M", "X"), nrow(apparelStaticCov.3cat)/3))]
  expect_silent(static.cov <- SetStaticCovariates(clv.data = clv.data.apparel.withhold,
                                                  data.cov.life  = apparelStaticCov.3cat, names.cov.life = "Gender",
                                                  data.cov.trans = apparelStaticCov, names.cov.trans = "Gender"))

  expect_true(ncol(static.cov@data.cov.life) == 3)
  expect_true(all(colnames(static.cov@data.cov.life) %in% c("Id", "GenderM", "GenderX")))
  expect_true(static.cov@data.cov.life[, all(sapply(.SD, is.numeric)), .SDcols = c("GenderM", "GenderX")])
})



# Covariate datatypes ---------------------------------------------------------------------------
context("Correctness - SetStaticCovariates - Covariate datatypes")

test_that("Converts categories to dummies - no numeric", {
  skip_on_cran()
  apparelStaticCov.dummy <- data.table::copy(apparelStaticCov)
  apparelStaticCov.dummy[, Gender.char := as.character(Gender)]
  expect_silent(static.cov <- SetStaticCovariates(clv.data = clv.data.apparel.withhold,
                                                  data.cov.life  = apparelStaticCov.dummy, names.cov.life = "Gender.char",
                                                  data.cov.trans = apparelStaticCov, names.cov.trans = "Gender"))

  expect_true(ncol(static.cov@data.cov.life) == 2)
  expect_true(nrow(static.cov@data.cov.life) == nrow(apparelStaticCov.dummy))
  expect_true(all(colnames(static.cov@data.cov.life) %in% c("Id", "Gender.char1")))
  expect_true(static.cov@data.cov.life[, all(sapply(.SD, is.numeric)), .SDcols = "Gender.char1"])
})

test_that("Converts categories to dummies - with numeric", {
  skip_on_cran()
  apparelStaticCov.mixed <- data.table::copy(apparelStaticCov)
  apparelStaticCov.mixed[, Gender.char := as.character(Gender)]
  expect_silent(static.cov <- SetStaticCovariates(clv.data = clv.data.apparel.withhold,
                                                  data.cov.life  = apparelStaticCov.mixed, names.cov.life = c("Gender","Gender.char"),
                                                  data.cov.trans = apparelStaticCov, names.cov.trans = "Gender"))

  expect_true(ncol(static.cov@data.cov.life) == 3)
  expect_true(nrow(static.cov@data.cov.life) == nrow(apparelStaticCov.mixed))
  expect_true(all(colnames(static.cov@data.cov.life) %in% c("Id", "Gender","Gender.char1")))
  expect_true(static.cov@data.cov.life[, all(sapply(.SD, is.numeric)), .SDcols = c("Gender","Gender.char1")])
})

test_that("Keeps numeric as numeric - no categories", {
  skip_on_cran()
  expect_silent(static.cov <- SetStaticCovariates(clv.data = clv.data.apparel.withhold,
                                                  data.cov.life  = apparelStaticCov, names.cov.life = "Gender",
                                                  data.cov.trans = apparelStaticCov, names.cov.trans = "Gender"))

  expect_true(ncol(static.cov@data.cov.life) == 2)
  expect_true(nrow(static.cov@data.cov.life) == nrow(apparelStaticCov))
  expect_true(all(colnames(static.cov@data.cov.life) %in% c("Id", "Gender")))
  expect_true(static.cov@data.cov.life[, all(sapply(.SD, is.numeric)), .SDcols = "Gender"])
})


test_that("Keeps numeric as numeric - with categories", {
  skip_on_cran()
  apparelStaticCov.mixed <- data.table::copy(apparelStaticCov)
  apparelStaticCov.mixed[, Gender.char := as.character(Gender)]
  expect_silent(static.cov <- SetStaticCovariates(clv.data = clv.data.apparel.withhold,
                                                  data.cov.life  = apparelStaticCov.mixed, names.cov.life = c("Gender", "Gender.char"),
                                                  data.cov.trans = apparelStaticCov, names.cov.trans = "Gender"))

  expect_true(ncol(static.cov@data.cov.life) == 3)
  expect_true(nrow(static.cov@data.cov.life) == nrow(apparelStaticCov))
  expect_true(all(colnames(static.cov@data.cov.life) %in% c("Id", "Gender", "Gender.char1")))
  expect_true(static.cov@data.cov.life[, all(sapply(.SD, is.numeric)), .SDcols = c("Gender", "Gender.char1")])
})


# Copied ---------------------------------------------------------------------------
test_that("Cov data was properly copied", {
  skip_on_cran()
  expect_silent(static.cov <- SetStaticCovariates(clv.data = clv.data.apparel.withhold,
                                                        data.cov.life  = apparelStaticCov, names.cov.life = "Gender",
                                                        data.cov.trans = apparelStaticCov, names.cov.trans = "Gender"))
  expect_false(isTRUE(all.equal(data.table::address(static.cov@data.cov.life),
                                data.table::address(apparelStaticCov))))
  expect_false(isTRUE(all.equal(data.table::address(static.cov@data.cov.trans),
                                data.table::address(apparelStaticCov))))
})



# clvdata works with Id as factor
