skip_on_cran()
data("apparelTrans")
data("apparelStaticCov")

fct.testthat.runability.staticcov(name.model = "BG/NBD", method=bgnbd,
                                  start.params.model=c(r=1, alpha=3, a = 1,b = 3),
                                  has.cor=FALSE,
                                  data.apparelTrans = apparelTrans, data.apparelStaticCov = apparelStaticCov,
                                  failed.optimization.methods.expected.message =
                                    "Gradient not computable after method|NA/Inf replaced by maximum positive value")

