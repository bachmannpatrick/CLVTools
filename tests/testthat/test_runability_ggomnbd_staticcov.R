skip_on_cran()
data("apparelTrans")
data("apparelStaticCov")

fct.testthat.runability.staticcov(name.model = "GGompertz/NBD", method=ggomnbd,
                                  start.params.model=c(r=1.23, alpha=0.678, b = 0.222, s = 0.111, beta=2.345),
                                  has.DERT=FALSE, has.cor=FALSE,
                                  data.apparelTrans = apparelTrans, data.apparelStaticCov = apparelStaticCov,
                                  failed.optimization.methods.expected.message =
                                    "Gradient not computable after method|NA/Inf replaced by maximum positive value")

