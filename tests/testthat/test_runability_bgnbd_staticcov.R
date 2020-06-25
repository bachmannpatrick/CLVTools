skip_on_cran()
data("apparelTrans")
data("apparelStaticCov")

fct.testthat.runability.staticcov(name.model = "BG/NBD", method=bgnbd,
                                  start.params.model=c(r=1.23, alpha=0.678, a = 2.345,b = 0.222),
                                  has.DERT=FALSE, has.cor=FALSE,
                                  data.apparelTrans = apparelTrans, data.apparelStaticCov = apparelStaticCov,
                                  failed.optimization.methods.expected.message =
                                    "Gradient not computable after method|NA/Inf replaced by maximum positive value")

