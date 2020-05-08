# Load data ---------------------------------------------------------------------------------------
data("apparelTrans")
data("apparelStaticCov")



# Parameter clv.data ---------------------------------------------------------------------------------------
context("Runability - SetStaticCovariates - Data inputs")
expect_silent(clv.data.apparel.nohold   <- clvdata(apparelTrans, date.format = "ymd", time.unit = "w"))
expect_silent(clv.data.apparel.withhold <- clvdata(apparelTrans, date.format = "ymd", time.unit = "w",
                                                   estimation.split = 39))


test_that("Works with and withouth holdout period", {
  expect_silent(SetStaticCovariates(clv.data = clv.data.apparel.nohold,
                                    data.cov.life  = apparelStaticCov, names.cov.life = "Gender",
                                    data.cov.trans = apparelStaticCov, names.cov.trans = "Gender"))

  expect_silent(SetStaticCovariates(clv.data = clv.data.apparel.withhold,
                                    data.cov.life  = apparelStaticCov, names.cov.life = "Gender",
                                    data.cov.trans = apparelStaticCov, names.cov.trans = "Gender"))
})

test_that("Works with data.table and data.frame", {
  skip_on_cran()
  expect_silent(SetStaticCovariates(clv.data = clv.data.apparel.nohold,
                                    data.cov.life  = as.data.frame(apparelStaticCov), names.cov.life = "Gender",
                                    data.cov.trans = apparelStaticCov, names.cov.trans = "Gender"))

  expect_silent(SetStaticCovariates(clv.data = clv.data.apparel.nohold,
                                    data.cov.life  = apparelStaticCov, names.cov.life = "Gender",
                                    data.cov.trans = as.data.frame(apparelStaticCov), names.cov.trans = "Gender"))


  expect_silent(SetStaticCovariates(clv.data = clv.data.apparel.withhold,
                                    data.cov.life  = as.data.frame(apparelStaticCov), names.cov.life = "Gender",
                                    data.cov.trans = apparelStaticCov, names.cov.trans = "Gender"))

  expect_silent(SetStaticCovariates(clv.data = clv.data.apparel.withhold,
                                    data.cov.life  = apparelStaticCov, names.cov.life = "Gender",
                                    data.cov.trans = as.data.frame(apparelStaticCov), names.cov.trans = "Gender"))

})

test_that("Works with char covariates", {
  skip_on_cran()
  apparelStaticCov.char <- data.table::copy(apparelStaticCov)
  apparelStaticCov.char[, Gender := as.character(Gender)]

  expect_silent(SetStaticCovariates(clv.data = clv.data.apparel.nohold,
                                    data.cov.life  = apparelStaticCov.char, names.cov.life = "Gender",
                                    data.cov.trans = apparelStaticCov, names.cov.trans = "Gender"))
  expect_silent(SetStaticCovariates(clv.data = clv.data.apparel.nohold,
                                    data.cov.life  = apparelStaticCov, names.cov.life = "Gender",
                                    data.cov.trans = apparelStaticCov.char, names.cov.trans = "Gender"))
})

test_that("Works with factor covariates", {
  skip_on_cran()
  apparelStaticCov.factor <- data.table::copy(apparelStaticCov)
  apparelStaticCov.factor[, Gender := as.factor(as.character(Gender))]

  expect_silent(SetStaticCovariates(clv.data = clv.data.apparel.nohold,
                                    data.cov.life  = apparelStaticCov.factor, names.cov.life = "Gender",
                                    data.cov.trans = apparelStaticCov, names.cov.trans = "Gender"))
  expect_silent(SetStaticCovariates(clv.data = clv.data.apparel.nohold,
                                    data.cov.life  = apparelStaticCov, names.cov.life = "Gender",
                                    data.cov.trans = apparelStaticCov.factor, names.cov.trans = "Gender"))
})

test_that("Works with numeric covariates", {
  skip_on_cran()
  apparelStaticCov.numeric <- data.table::copy(apparelStaticCov)
  apparelStaticCov.numeric[, Gender := as.numeric(Gender)]

  expect_silent(SetStaticCovariates(clv.data = clv.data.apparel.nohold,
                                    data.cov.life  = apparelStaticCov.numeric, names.cov.life = "Gender",
                                    data.cov.trans = apparelStaticCov, names.cov.trans = "Gender"))
  expect_silent(SetStaticCovariates(clv.data = clv.data.apparel.nohold,
                                    data.cov.life  = apparelStaticCov, names.cov.life = "Gender",
                                    data.cov.trans = apparelStaticCov.numeric, names.cov.trans = "Gender"))
})


test_that("Works with non standard Id name", {
  skip_on_cran()

  apparelStaticCov.id <- data.table::copy(apparelStaticCov)
  apparelStaticCov.id[, Idasfasflj := Id]

  expect_silent(SetStaticCovariates(clv.data = clv.data.apparel.nohold,
                                    data.cov.life  = apparelStaticCov.id, names.cov.life = "Gender",
                                    data.cov.trans = apparelStaticCov.id, names.cov.trans = "Gender",
                                    name.id = "Idasfasflj"))
  expect_silent(SetStaticCovariates(clv.data = clv.data.apparel.nohold,
                                    data.cov.life  = apparelStaticCov.id, names.cov.life = "Gender",
                                    data.cov.trans = apparelStaticCov.id, names.cov.trans = "Gender",
                                    name.id="Idasfasflj"))
})


test_that("Works with Ids as factor, numeric, character", {
  skip_on_cran()

  apparelStaticCov.char <- data.table::copy(apparelStaticCov)
  apparelStaticCov.char[, Id := as.character(Id)]
  expect_silent(res.char.1 <- SetStaticCovariates(clv.data = clv.data.apparel.nohold,
                                    data.cov.life  = apparelStaticCov.char, names.cov.life = "Gender",
                                    data.cov.trans = apparelStaticCov, names.cov.trans = "Gender"))
  expect_silent(res.char.2 <- SetStaticCovariates(clv.data = clv.data.apparel.nohold,
                                    data.cov.life  = apparelStaticCov, names.cov.life = "Gender",
                                    data.cov.trans = apparelStaticCov.char, names.cov.trans = "Gender"))

  apparelStaticCov.factor <- data.table::copy(apparelStaticCov)
  apparelStaticCov.factor[, Id := as.factor(as.character(Id))]
  expect_silent(res.factor.1 <- SetStaticCovariates(clv.data = clv.data.apparel.nohold,
                                    data.cov.life  = apparelStaticCov.factor, names.cov.life = "Gender",
                                    data.cov.trans = apparelStaticCov, names.cov.trans = "Gender"))
  expect_silent(res.factor.2 <- SetStaticCovariates(clv.data = clv.data.apparel.nohold,
                                    data.cov.life  = apparelStaticCov, names.cov.life = "Gender",
                                    data.cov.trans = apparelStaticCov.factor, names.cov.trans = "Gender"))

  apparelStaticCov.numeric <- data.table::copy(apparelStaticCov)
  apparelStaticCov.numeric[, Id := as.numeric(Id)]
  expect_silent(res.numeric.1 <- SetStaticCovariates(clv.data = clv.data.apparel.nohold,
                                    data.cov.life  = apparelStaticCov.numeric, names.cov.life = "Gender",
                                    data.cov.trans = apparelStaticCov, names.cov.trans = "Gender"))
  expect_silent(res.numeric.2 <- SetStaticCovariates(clv.data = clv.data.apparel.nohold,
                                    data.cov.life  = apparelStaticCov, names.cov.life = "Gender",
                                    data.cov.trans = apparelStaticCov.numeric, names.cov.trans = "Gender"))

  expect_equal(res.char.1, res.factor.1)
  expect_equal(res.factor.1, res.numeric.1)

  expect_equal(res.char.2, res.factor.2)
  expect_equal(res.factor.2, res.numeric.2)
})


# works (with warning) if there are more covariates than customers - also fitting usw
