
skip_on_cran()

# Load data ---------------------------------------------------------------------------------------
data("apparelTrans")
data("apparelDynCov")
apparelDynCov <- apparelDynCov[Cov.Date > "2005-01-01" ] #otherwise "cutoff" message

# Parameter clv.data ---------------------------------------------------------------------------------------
expect_silent(clv.data.apparel.nohold   <- clvdata(apparelTrans, date.format = "ymd", time.unit = "w"))
expect_silent(clv.data.apparel.withhold <- clvdata(apparelTrans, date.format = "ymd", time.unit = "w",
                                                    estimation.split = 39))

l.std.args <- alist(data.cov.life  = apparelDynCov,  names.cov.life = c("High.Season", "Gender", "Channel"),
                    data.cov.trans = apparelDynCov,  names.cov.trans = c("High.Season", "Gender", "Channel"),
                    name.date = "Cov.Date")

test_that("Works with and withouth holdout period", {
  expect_silent(do.call(SetDynamicCovariates, modifyList(l.std.args, alist(clv.data = clv.data.apparel.nohold))))
  expect_silent(do.call(SetDynamicCovariates, modifyList(l.std.args, alist(clv.data = clv.data.apparel.withhold))))
})

# Covariate length longer than holdout.end ---------------------------------------------------------------------------------------

test_that("Works with cov data longer than estimation.end/holdout.end", {
  skip_on_cran()

  apparelDynCov.long <- fct.helper.dyncov.create.longer.dyncov.data(num.additional=100, data.apparelDynCov=apparelDynCov)

  # No holdout
  #   Only works if for both processes the CovData is longer than minimum
  #   Fails in inputchecks accidentially implemented. Wrong place but doesnt hurt and also done in inputchecks (for single customer)
  expect_error(do.call(SetDynamicCovariates, modifyList(l.std.args, alist(clv.data = clv.data.apparel.nohold,
                                                                           data.cov.life = apparelDynCov.long))),
               regexp = "need to have the same number of Dates")
  expect_error(do.call(SetDynamicCovariates, modifyList(l.std.args, alist(clv.data = clv.data.apparel.nohold,
                                                                           data.cov.trans = apparelDynCov.long))),
               regexp = "need to have the same number of Dates")

  expect_silent(do.call(SetDynamicCovariates, modifyList(l.std.args, alist(clv.data = clv.data.apparel.nohold,
                                                                           data.cov.life = apparelDynCov.long,
                                                                           data.cov.trans = apparelDynCov.long))))

  # With holdout
  expect_error(do.call(SetDynamicCovariates, modifyList(l.std.args, alist(clv.data = clv.data.apparel.withhold,
                                                                           data.cov.life = apparelDynCov.long))),
               regexp = "need to have the same number of Dates")
  expect_error(do.call(SetDynamicCovariates, modifyList(l.std.args, alist(clv.data = clv.data.apparel.withhold,
                                                                           data.cov.trans = apparelDynCov.long))),
               regexp = "need to have the same number of Dates")
  expect_silent(do.call(SetDynamicCovariates, modifyList(l.std.args, alist(clv.data = clv.data.apparel.withhold,
                                                                           data.cov.life = apparelDynCov.long,
                                                                           data.cov.trans = apparelDynCov.long))))
})

