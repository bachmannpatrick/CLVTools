# Load Data ------------------------------------------------------------------------------------------------------------------
data("apparelTrans")
data("apparelDynCov")

skip_on_cran()


# Basic runability ---------------------------------------------------------------------------------

fitted.dyncov <- fct.helper.dyncov.quickfit.apparel.data(data.apparelTrans = apparelTrans,
                                                         data.apparelDynCov = apparelDynCov,
                                                         hessian=TRUE)

# Standard S3 tests ---------------------------------------------------------------
# Run the standard S3 tests on the fitted model,
#   but not plot() and predict() which takes too long.
#   also plot() produces all NAs if quickfit is used
.fct.helper.clvfitted.all.s3.except.plot.and.predict(clv.fitted=fitted.dyncov,
                                                     full.names=c("r", "alpha", "s", "beta",
                                                                  "life.Marketing",  "life.Gender", "life.Channel",
                                                                  "trans.Marketing", "trans.Gender",  "trans.Channel"))



# LL.data ---------------------------------------------------------------
fct.testthat.runability.dynamiccov.LL.is.correct(clv.fitted = fitted.dyncov)

# Plot ------------------------------------------------------------------
fct.testthat.runability.dynamiccov.plot.works(clv.fitted = fitted.dyncov)

fct.testthat.runability.dynamiccov.plot.has.0.repeat.transactions.expectations(clv.fitted = fitted.dyncov)

# Predict ----------------------------------------------------------------
fct.testthat.runability.dynamiccov.predict.works(clv.fitted = fitted.dyncov)

fct.testthat.runability.dynamiccov.predict.newdata.works(clv.fitted = fitted.dyncov,
                                                         data.apparelTrans = apparelTrans,
                                                         data.apparelDynCov = apparelDynCov)



# Newdata ----------------------------------------------------------------------------------------------------------
apparelDynCov.extra <- fct.helper.dyncov.create.longer.dyncov.data(num.additional = 100,
                                                                        data.apparelDynCov = apparelDynCov)
clv.data.extra <- fct.helper.create.clvdata.apparel.dyncov(data.apparelTrans=apparelTrans,
                                                           data.apparelDynCov=apparelDynCov.extra,
                                                           estimation.end=38)

fct.testthat.runability.dynamiccov.predict.longer.with.newdata(clv.fitted = fitted.dyncov, clv.data.extra = clv.data.extra, clv.data.trans = clv.data.trans)

fct.testthat.runability.dynamiccov.plot.longer.with.newdata(clv.fitted = fitted.dyncov, clv.data.extra = clv.data.extra, clv.data.trans = clv.data.trans)

# Overlong data ------------------------------------------------------------------------------

# Cannot do without holdout because takes too long to estimate
fct.testthat.runability.dynamiccov.can.predict.plot.beyond.holdout(data.apparelTrans=apparelTrans,
                                                                   apparelDynCov.extra=apparelDynCov.extra)

