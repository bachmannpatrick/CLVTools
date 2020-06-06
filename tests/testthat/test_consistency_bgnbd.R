# Tests that BG/NBD models are consistent among themselves
skip_on_cran()

# Consistency
# nocov vs static cov:
#   same fit with all covs = 0
#   same predict with gamma=0

context("Nocov/cov Consistency - BG/NBD - all cov data = 0")
data("apparelTrans")
expect_silent(clv.apparel <- clvdata(data.transactions = apparelTrans, date.format = "ymd", time.unit = "w",
                                     estimation.split = 38))

data("apparelStaticCov")
# Cannot set all to 0 as requires at least 2 distinct values per cov
expect_silent(apparelStaticCov.0 <- apparelStaticCov)
expect_silent(apparelStaticCov.0[, Gender := 0])
expect_silent(apparelStaticCov.0[1, Gender := 1])
expect_silent(apparelStaticCov.0[, Channel := 0])
expect_silent(apparelStaticCov.0[1, Channel := 1])
expect_silent(clv.apparel.static <- SetStaticCovariates(clv.apparel,
                                                        data.cov.life = apparelStaticCov.0, data.cov.trans = apparelStaticCov.0,
                                                        names.cov.life = c("Gender", "Channel"), names.cov.trans = c("Gender", "Channel")))


# Fit models
expect_silent(b.nocov  <- bgnbd(clv.apparel, verbose = FALSE))
expect_silent(b.static <- bgnbd(clv.apparel.static, verbose = FALSE))

fct.testthat.consistency.cov.data.0.model.params.nearly.same(m.nocov = b.nocov,
                                                             m.static = b.static,
                                                             param.names = c("r", "alpha", "a", "b"))


context("Nocov/cov Consistency - BG/NBD - cov params = 0")

# Set parameters ------------------------------------------------------------------------
# Fake the parameters to be exactly the same and 0 for covariates
#   Replace model coefs with that from nocov

# static cov
expect_silent(b.static@prediction.params.model[c("r", "alpha", "a", "b")] <-
                b.nocov@prediction.params.model[c("r", "alpha", "a", "b")])
expect_silent(b.static@prediction.params.life[c("Gender", "Channel")] <- 0)
expect_silent(b.static@prediction.params.trans[c("Gender", "Channel")] <- 0)

# Actual tests ---------------------------------------------------------------------------------

fct.testthat.consistency.cov.params.0.predict.same(m.nocov = b.nocov,
                                                   m.static = b.static,
                                                   has.DERT = FALSE,
                                                   has.dyncov = FALSE)

fct.testthat.consistency.cov.params.0.plot.same(m.nocov = b.nocov,
                                                m.static = b.static,
                                                has.dyncov = FALSE)
