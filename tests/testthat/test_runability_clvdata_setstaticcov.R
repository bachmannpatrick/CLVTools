# Load data ---------------------------------------------------------------------------------------
data("apparelTrans")
data("apparelDemographics")



# Parameter clv.data ---------------------------------------------------------------------------------------
context("Runability - SetStaticCovariates - Data inputs")
expect_message(clv.data.apparel.nohold   <- clvdata(apparelTrans, date.format = "ymd", time.unit = "w"), regexp = "ignored")
expect_message(clv.data.apparel.withhold <- clvdata(apparelTrans, date.format = "ymd", time.unit = "w",
                                                   estimation.split = 39), regexp = "ignored")


test_that("Works with and withouth holdout period", {
  skip_on_cran()
  expect_silent(SetStaticCovariates(clv.data = clv.data.apparel.nohold,
                                    data.cov.life  = apparelDemographics, names.cov.life = "Gender",
                                    data.cov.trans = apparelDemographics, names.cov.trans = "Gender"))

  expect_silent(SetStaticCovariates(clv.data = clv.data.apparel.withhold,
                                    data.cov.life  = apparelDemographics, names.cov.life = "Gender",
                                    data.cov.trans = apparelDemographics, names.cov.trans = "Gender"))
})

test_that("Works with data.table and data.frame", {
  expect_silent(SetStaticCovariates(clv.data = clv.data.apparel.nohold,
                                    data.cov.life  = as.data.frame(apparelDemographics), names.cov.life = "Gender",
                                    data.cov.trans = apparelDemographics, names.cov.trans = "Gender"))

  expect_silent(SetStaticCovariates(clv.data = clv.data.apparel.nohold,
                                    data.cov.life  = apparelDemographics, names.cov.life = "Gender",
                                    data.cov.trans = as.data.frame(apparelDemographics), names.cov.trans = "Gender"))


  expect_silent(SetStaticCovariates(clv.data = clv.data.apparel.withhold,
                                    data.cov.life  = as.data.frame(apparelDemographics), names.cov.life = "Gender",
                                    data.cov.trans = apparelDemographics, names.cov.trans = "Gender"))

  expect_silent(SetStaticCovariates(clv.data = clv.data.apparel.withhold,
                                    data.cov.life  = apparelDemographics, names.cov.life = "Gender",
                                    data.cov.trans = as.data.frame(apparelDemographics), names.cov.trans = "Gender"))

})

test_that("Works with char covariates", {
  apparelDemographics.char <- data.table::copy(apparelDemographics)
  apparelDemographics.char[, Gender := as.character(Gender)]

  expect_silent(SetStaticCovariates(clv.data = clv.data.apparel.nohold,
                                    data.cov.life  = apparelDemographics.char, names.cov.life = "Gender",
                                    data.cov.trans = apparelDemographics, names.cov.trans = "Gender"))
  expect_silent(SetStaticCovariates(clv.data = clv.data.apparel.nohold,
                                    data.cov.life  = apparelDemographics, names.cov.life = "Gender",
                                    data.cov.trans = apparelDemographics.char, names.cov.trans = "Gender"))
})

test_that("Works with factor covariates", {
  apparelDemographics.factor <- data.table::copy(apparelDemographics)
  apparelDemographics.factor[, Gender := as.factor(as.character(Gender))]

  expect_silent(SetStaticCovariates(clv.data = clv.data.apparel.nohold,
                                    data.cov.life  = apparelDemographics.factor, names.cov.life = "Gender",
                                    data.cov.trans = apparelDemographics, names.cov.trans = "Gender"))
  expect_silent(SetStaticCovariates(clv.data = clv.data.apparel.nohold,
                                    data.cov.life  = apparelDemographics, names.cov.life = "Gender",
                                    data.cov.trans = apparelDemographics.factor, names.cov.trans = "Gender"))
})

test_that("Works with numeric covariates", {
  apparelDemographics.numeric <- data.table::copy(apparelDemographics)
  apparelDemographics.numeric[, Gender := as.numeric(Gender)]

  expect_silent(SetStaticCovariates(clv.data = clv.data.apparel.nohold,
                                    data.cov.life  = apparelDemographics.numeric, names.cov.life = "Gender",
                                    data.cov.trans = apparelDemographics, names.cov.trans = "Gender"))
  expect_silent(SetStaticCovariates(clv.data = clv.data.apparel.nohold,
                                    data.cov.life  = apparelDemographics, names.cov.life = "Gender",
                                    data.cov.trans = apparelDemographics.numeric, names.cov.trans = "Gender"))
})


test_that("Works with non standard Id name", {
  apparelDemographics.id <- data.table::copy(apparelDemographics)
  apparelDemographics.id[, Idasfasflj := Id]

  expect_silent(SetStaticCovariates(clv.data = clv.data.apparel.nohold,
                                    data.cov.life  = apparelDemographics.id, names.cov.life = "Gender",
                                    data.cov.trans = apparelDemographics.id, names.cov.trans = "Gender",
                                    name.id = "Idasfasflj"))
  expect_silent(SetStaticCovariates(clv.data = clv.data.apparel.nohold,
                                    data.cov.life  = apparelDemographics.id, names.cov.life = "Gender",
                                    data.cov.trans = apparelDemographics.id, names.cov.trans = "Gender",
                                    name.id="Idasfasflj"))
})


test_that("Works with Ids as factor, numeric, character", {

  apparelDemographics.char <- data.table::copy(apparelDemographics)
  apparelDemographics.char[, Id := as.character(Id)]
  expect_silent(res.char.1 <- SetStaticCovariates(clv.data = clv.data.apparel.nohold,
                                    data.cov.life  = apparelDemographics.char, names.cov.life = "Gender",
                                    data.cov.trans = apparelDemographics, names.cov.trans = "Gender"))
  expect_silent(res.char.2 <- SetStaticCovariates(clv.data = clv.data.apparel.nohold,
                                    data.cov.life  = apparelDemographics, names.cov.life = "Gender",
                                    data.cov.trans = apparelDemographics.char, names.cov.trans = "Gender"))

  apparelDemographics.factor <- data.table::copy(apparelDemographics)
  apparelDemographics.factor[, Id := as.factor(as.character(Id))]
  expect_silent(res.factor.1 <- SetStaticCovariates(clv.data = clv.data.apparel.nohold,
                                    data.cov.life  = apparelDemographics.factor, names.cov.life = "Gender",
                                    data.cov.trans = apparelDemographics, names.cov.trans = "Gender"))
  expect_silent(res.factor.2 <- SetStaticCovariates(clv.data = clv.data.apparel.nohold,
                                    data.cov.life  = apparelDemographics, names.cov.life = "Gender",
                                    data.cov.trans = apparelDemographics.factor, names.cov.trans = "Gender"))

  apparelDemographics.numeric <- data.table::copy(apparelDemographics)
  apparelDemographics.numeric[, Id := as.numeric(Id)]
  expect_silent(res.numeric.1 <- SetStaticCovariates(clv.data = clv.data.apparel.nohold,
                                    data.cov.life  = apparelDemographics.numeric, names.cov.life = "Gender",
                                    data.cov.trans = apparelDemographics, names.cov.trans = "Gender"))
  expect_silent(res.numeric.2 <- SetStaticCovariates(clv.data = clv.data.apparel.nohold,
                                    data.cov.life  = apparelDemographics, names.cov.life = "Gender",
                                    data.cov.trans = apparelDemographics.numeric, names.cov.trans = "Gender"))

  expect_equal(res.char.1, res.factor.1)
  expect_equal(res.factor.1, res.numeric.1)

  expect_equal(res.char.2, res.factor.2)
  expect_equal(res.factor.2, res.numeric.2)
})


# works (with warning) if there are more covariates than customers - also fitting usw
