# Load data ---------------------------------------------------------------------------------------
data("apparelTrans")
data("apparelDemographics")

# Covariate dummies ---------------------------------------------------------------------------------------
context("Correctness - SetStaticCovariates - Covariate dummies")

expect_message(clv.data.apparel.nohold   <- clvdata(apparelTrans, date.format = "ymd", time.unit = "w"), regexp = "ignored")
expect_message(clv.data.apparel.withhold <- clvdata(apparelTrans, date.format = "ymd", time.unit = "w",
                                                   estimation.split = 39), regexp = "ignored")


test_that("Factor and char covariates result in same dummies - no holdout",{
  apparelDemographics.char <- data.table::copy(apparelDemographics)
  apparelDemographics.char[, Gender := as.character(Gender)]

  expect_silent(static.char.life <- SetStaticCovariates(clv.data = clv.data.apparel.nohold,
                                                        data.cov.life  = apparelDemographics.char, names.cov.life = "Gender",
                                                        data.cov.trans = apparelDemographics, names.cov.trans = "Gender"))

  expect_silent(static.char.trans <- SetStaticCovariates(clv.data = clv.data.apparel.nohold,
                                                        data.cov.life  = apparelDemographics, names.cov.life = "Gender",
                                                        data.cov.trans = apparelDemographics.char, names.cov.trans = "Gender"))

  expect_silent(static.char.both <- SetStaticCovariates(clv.data = clv.data.apparel.nohold,
                                    data.cov.life  = apparelDemographics.char, names.cov.life = "Gender",
                                    data.cov.trans = apparelDemographics.char, names.cov.trans = "Gender"))



  apparelDemographics.factor <- data.table::copy(apparelDemographics)
  apparelDemographics.factor[, Gender := as.factor(as.character(Gender))]

  expect_silent(static.factor.life <- SetStaticCovariates(clv.data = clv.data.apparel.nohold,
                                                          data.cov.life  = apparelDemographics.factor, names.cov.life = "Gender",
                                                          data.cov.trans = apparelDemographics, names.cov.trans = "Gender"))

  expect_silent(static.factor.trans <- SetStaticCovariates(clv.data = clv.data.apparel.nohold,
                                                          data.cov.life  = apparelDemographics, names.cov.life = "Gender",
                                                          data.cov.trans = apparelDemographics.factor, names.cov.trans = "Gender"))

  expect_silent(static.factor.both <- SetStaticCovariates(clv.data = clv.data.apparel.nohold,
                                                   data.cov.life  = apparelDemographics.factor, names.cov.life = "Gender",
                                                   data.cov.trans = apparelDemographics.factor, names.cov.trans = "Gender"))


  expect_equal(static.char.life, static.factor.life)
  expect_equal(static.char.trans, static.factor.trans)
  expect_equal(static.char.both, static.factor.both)
})



test_that("Factor and char covariates result in same dummies - with holdout",{
  apparelDemographics.char <- data.table::copy(apparelDemographics)
  apparelDemographics.char[, Gender := as.character(Gender)]

  expect_silent(static.char.life <- SetStaticCovariates(clv.data = clv.data.apparel.withhold,
                                                        data.cov.life  = apparelDemographics.char, names.cov.life = "Gender",
                                                        data.cov.trans = apparelDemographics, names.cov.trans = "Gender"))

  expect_silent(static.char.trans <- SetStaticCovariates(clv.data = clv.data.apparel.withhold,
                                                         data.cov.life  = apparelDemographics, names.cov.life = "Gender",
                                                         data.cov.trans = apparelDemographics.char, names.cov.trans = "Gender"))

  expect_silent(static.char.both <- SetStaticCovariates(clv.data = clv.data.apparel.withhold,
                                                        data.cov.life  = apparelDemographics.char, names.cov.life = "Gender",
                                                        data.cov.trans = apparelDemographics.char, names.cov.trans = "Gender"))



  apparelDemographics.factor <- data.table::copy(apparelDemographics)
  apparelDemographics.factor[, Gender := as.factor(as.character(Gender))]

  expect_silent(static.factor.life <- SetStaticCovariates(clv.data = clv.data.apparel.withhold,
                                                          data.cov.life  = apparelDemographics.factor, names.cov.life = "Gender",
                                                          data.cov.trans = apparelDemographics, names.cov.trans = "Gender"))

  expect_silent(static.factor.trans <- SetStaticCovariates(clv.data = clv.data.apparel.withhold,
                                                           data.cov.life  = apparelDemographics, names.cov.life = "Gender",
                                                           data.cov.trans = apparelDemographics.factor, names.cov.trans = "Gender"))

  expect_silent(static.factor.both <- SetStaticCovariates(clv.data = clv.data.apparel.withhold,
                                                          data.cov.life  = apparelDemographics.factor, names.cov.life = "Gender",
                                                          data.cov.trans = apparelDemographics.factor, names.cov.trans = "Gender"))

  expect_equal(static.char.life, static.factor.life)
  expect_equal(static.char.trans, static.factor.trans)
  expect_equal(static.char.both, static.factor.both)
})


test_that("Creates correct number of dummies - 2 categories", {
  apparelDemographics.2cat <- data.table::copy(apparelDemographics)
  apparelDemographics.2cat[, Gender := c("F",rep(c("F", "M"), nrow(apparelDemographics.2cat)/2))]

  expect_silent(static.cov <- SetStaticCovariates(clv.data = clv.data.apparel.withhold,
                                                  data.cov.life  = apparelDemographics.2cat, names.cov.life = "Gender",
                                                  data.cov.trans = apparelDemographics,      names.cov.trans = "Gender"))

  expect_true(ncol(static.cov@data.cov.life) == 2)
  expect_true(all(colnames(static.cov@data.cov.life) %in% c("Id", "GenderM")))
  expect_true(static.cov@data.cov.life[, all(sapply(.SD, is.numeric)), .SDcols = c("GenderM")])
})

test_that("Creates correct number of dummies - 3 categories",{
  apparelDemographics.3cat <- data.table::copy(apparelDemographics)
  apparelDemographics.3cat[, Gender := c("F", "M", rep(c("F", "M", "X"), nrow(apparelDemographics.3cat)/3))]
  expect_silent(static.cov <- SetStaticCovariates(clv.data = clv.data.apparel.withhold,
                                                  data.cov.life  = apparelDemographics.3cat, names.cov.life = "Gender",
                                                  data.cov.trans = apparelDemographics, names.cov.trans = "Gender"))

  expect_true(ncol(static.cov@data.cov.life) == 3)
  expect_true(all(colnames(static.cov@data.cov.life) %in% c("Id", "GenderM", "GenderX")))
  expect_true(static.cov@data.cov.life[, all(sapply(.SD, is.numeric)), .SDcols = c("GenderM", "GenderX")])
})



# Covariate datatypes ---------------------------------------------------------------------------
context("Correctness - SetStaticCovariates - Covariate datatypes")

test_that("Converts categories to dummies - no numeric", {
  skip_on_cran()
  apparelDemographics.dummy <- data.table::copy(apparelDemographics)
  apparelDemographics.dummy[, Gender.char := as.character(Gender)]
  expect_silent(static.cov <- SetStaticCovariates(clv.data = clv.data.apparel.withhold,
                                                  data.cov.life  = apparelDemographics.dummy, names.cov.life = "Gender.char",
                                                  data.cov.trans = apparelDemographics, names.cov.trans = "Gender"))

  expect_true(ncol(static.cov@data.cov.life) == 2)
  expect_true(nrow(static.cov@data.cov.life) == nrow(apparelDemographics.dummy))
  expect_true(all(colnames(static.cov@data.cov.life) %in% c("Id", "Gender.char1")))
  expect_true(static.cov@data.cov.life[, all(sapply(.SD, is.numeric)), .SDcols = "Gender.char1"])
})

test_that("Converts categories to dummies - with numeric", {
  skip_on_cran()
  apparelDemographics.mixed <- data.table::copy(apparelDemographics)
  apparelDemographics.mixed[, Gender.char := as.character(Gender)]
  expect_silent(static.cov <- SetStaticCovariates(clv.data = clv.data.apparel.withhold,
                                                  data.cov.life  = apparelDemographics.mixed, names.cov.life = c("Gender","Gender.char"),
                                                  data.cov.trans = apparelDemographics, names.cov.trans = "Gender"))

  expect_true(ncol(static.cov@data.cov.life) == 3)
  expect_true(nrow(static.cov@data.cov.life) == nrow(apparelDemographics.mixed))
  expect_true(all(colnames(static.cov@data.cov.life) %in% c("Id", "Gender","Gender.char1")))
  expect_true(static.cov@data.cov.life[, all(sapply(.SD, is.numeric)), .SDcols = c("Gender","Gender.char1")])
})

test_that("Keeps numeric as numeric - no categories", {
  skip_on_cran()
  expect_silent(static.cov <- SetStaticCovariates(clv.data = clv.data.apparel.withhold,
                                                  data.cov.life  = apparelDemographics, names.cov.life = "Gender",
                                                  data.cov.trans = apparelDemographics, names.cov.trans = "Gender"))

  expect_true(ncol(static.cov@data.cov.life) == 2)
  expect_true(nrow(static.cov@data.cov.life) == nrow(apparelDemographics))
  expect_true(all(colnames(static.cov@data.cov.life) %in% c("Id", "Gender")))
  expect_true(static.cov@data.cov.life[, all(sapply(.SD, is.numeric)), .SDcols = "Gender"])
})


test_that("Keeps numeric as numeric - with categories", {
  skip_on_cran()
  apparelDemographics.mixed <- data.table::copy(apparelDemographics)
  apparelDemographics.mixed[, Gender.char := as.character(Gender)]
  expect_silent(static.cov <- SetStaticCovariates(clv.data = clv.data.apparel.withhold,
                                                  data.cov.life  = apparelDemographics.mixed, names.cov.life = c("Gender", "Gender.char"),
                                                  data.cov.trans = apparelDemographics, names.cov.trans = "Gender"))

  expect_true(ncol(static.cov@data.cov.life) == 3)
  expect_true(nrow(static.cov@data.cov.life) == nrow(apparelDemographics))
  expect_true(all(colnames(static.cov@data.cov.life) %in% c("Id", "Gender", "Gender.char1")))
  expect_true(static.cov@data.cov.life[, all(sapply(.SD, is.numeric)), .SDcols = c("Gender", "Gender.char1")])
})


# Copied ---------------------------------------------------------------------------
test_that("Cov data was properly copied", {
  expect_silent(static.cov <- SetStaticCovariates(clv.data = clv.data.apparel.withhold,
                                                        data.cov.life  = apparelDemographics, names.cov.life = "Gender",
                                                        data.cov.trans = apparelDemographics, names.cov.trans = "Gender"))
  expect_false(isTRUE(all.equal(data.table::address(static.cov@data.cov.life),
                                data.table::address(apparelDemographics))))
  expect_false(isTRUE(all.equal(data.table::address(static.cov@data.cov.trans),
                                data.table::address(apparelDemographics))))
})



# clvdata works with Id as factor
