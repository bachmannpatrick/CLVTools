skip_on_cran()

data("cdnow")
data("apparelTrans")
data("apparelStaticCov")
data("apparelDynCov")

fct.testthat.runability.clvfittedspending(name.model = "Gamma-Gamma", method = gg,
                                          data.cdnow=cdnow,
                                          data.apparelTrans = apparelTrans, data.apparelStaticCov = apparelStaticCov, data.apparelDynCov=apparelDynCov,
                                          start.params.model = c(p = 1.23, q = 2.34, gamma = 0.678),
                                          failed.optimization.methods.expected.message=NULL)


