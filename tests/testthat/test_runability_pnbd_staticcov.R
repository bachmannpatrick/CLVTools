skip_on_cran()
data("apparelTrans")
data("apparelStaticCov")

fct.testthat.runability.staticcov(name.model = "PNBD", method=pnbd,
                                  start.params.model=c(r=0.5, alpha=15, s = 0.5, beta=10),
                                  has.cor=TRUE,
                                  data.apparelTrans = apparelTrans, data.apparelStaticCov = apparelStaticCov,
                                  failed.optimization.methods.expected.message =
                                    "Gradient not computable after method|NA/Inf replaced by maximum positive value")






