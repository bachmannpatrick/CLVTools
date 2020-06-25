skip_on_cran()
data("apparelTrans")
data("apparelStaticCov")

fct.testthat.runability.staticcov(name.model = "PNBD", method=pnbd,
                                  start.params.model=c(r=1.23, alpha=0.678, s = 0.111, beta=2.345),
                                  has.DERT=TRUE, has.cor=TRUE,
                                  data.apparelTrans = apparelTrans, data.apparelStaticCov = apparelStaticCov,
                                  failed.optimization.methods.expected.message =
                                    "Gradient not computable after method|NA/Inf replaced by maximum positive value")






