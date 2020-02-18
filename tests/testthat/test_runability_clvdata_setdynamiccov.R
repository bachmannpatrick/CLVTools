# Load data ---------------------------------------------------------------------------------------
data("apparelTrans")
data("apparelDynCov")


# Parameter clv.data ---------------------------------------------------------------------------------------
context("Runability - SetDynamicCovariates - Data inputs")
expect_message(clv.data.apparel.nohold   <- clvdata(apparelTrans, date.format = "ymd", time.unit = "w"), regexp = "ignored")
expect_message(clv.data.apparel.withhold <- clvdata(apparelTrans, date.format = "ymd", time.unit = "w",
                                                    estimation.split = 39), regexp = "ignored")

l.std.args <- alist(data.cov.life  = apparelDynCov,  names.cov.life = c("DM", "High.Season", "Gender"),
                    data.cov.trans = apparelDynCov,  names.cov.trans = c("DM", "High.Season", "Gender"),
                    name.date = "Cov.Date")

test_that("Works with and withouth holdout period", {
  skip_on_cran()
  expect_message(do.call(SetDynamicCovariates, modifyList(l.std.args, alist(clv.data = clv.data.apparel.nohold))), regexp = "ignored")
  expect_message(do.call(SetDynamicCovariates, modifyList(l.std.args, alist(clv.data = clv.data.apparel.withhold))), regexp = "ignored")
})

# Covariate length longer than holdout.end ---------------------------------------------------------------------------------------
context("Runability - SetDynamicCovariates - Covariate length")

test_that("Works with cov data longer than estimation.end/holdout.end", {
  skip_on_cran()

  # Add additional 100w of fake cov data for all Ids
  dt.additional.cov <- expand.grid(Id = unique(apparelDynCov$Id),
                                   Cov.Date = seq(from=apparelDynCov[, max(Cov.Date)]+lubridate::weeks(1),
                                                  length.out = 100, by = "week"))
  setDT(dt.additional.cov)
  dt.additional.cov[, DM := rep(c(0,1,2,3),.N/4)]
  dt.additional.cov[, High.Season := rep(c(rep(1, 50), rep(0, 50)),.N/100)]
  dt.additional.cov[, Gender := rep(c(0,1),.N/2)]

  apparelDynCov.long <- data.table::rbindlist(l = list(apparelDynCov, dt.additional.cov),
                                              use.names = TRUE)

  # No holdout
  #   Only works if for both processes the CovData is longer than minimum
  #   Fails in inputchecks accidentially implemented. Wrong place but doesnt hurt and also done in inputchecks (for single customer)
  expect_error(do.call(SetDynamicCovariates, modifyList(l.std.args, alist(clv.data = clv.data.apparel.nohold,
                                                                           data.cov.life = apparelDynCov.long))),
               regexp = "need to have the same number of Dates")
  expect_error(do.call(SetDynamicCovariates, modifyList(l.std.args, alist(clv.data = clv.data.apparel.nohold,
                                                                           data.cov.trans = apparelDynCov.long))),
               regexp = "need to have the same number of Dates")

  expect_message(do.call(SetDynamicCovariates, modifyList(l.std.args, alist(clv.data = clv.data.apparel.nohold,
                                                                           data.cov.life = apparelDynCov.long,
                                                                           data.cov.trans = apparelDynCov.long))),
                regexp = "ignored")

  # With holdout
  expect_error(do.call(SetDynamicCovariates, modifyList(l.std.args, alist(clv.data = clv.data.apparel.withhold,
                                                                           data.cov.life = apparelDynCov.long))),
               regexp = "need to have the same number of Dates")
  expect_error(do.call(SetDynamicCovariates, modifyList(l.std.args, alist(clv.data = clv.data.apparel.withhold,
                                                                           data.cov.trans = apparelDynCov.long))),
               regexp = "need to have the same number of Dates")
  expect_message(do.call(SetDynamicCovariates, modifyList(l.std.args, alist(clv.data = clv.data.apparel.withhold,
                                                                           data.cov.life = apparelDynCov.long,
                                                                           data.cov.trans = apparelDynCov.long))),
                 regexp = "ignored")
})

