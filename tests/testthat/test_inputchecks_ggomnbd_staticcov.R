data("apparelTrans")
data("apparelStaticCov")

fct.testthat.inputchecks.staticcov(name.method = "GGompertz/NBD", method=bgnbd,
                                   has.cor = FALSE,
                                   start.params.model = c(r=2.345, alpha=0.678, beta =1.234, b=1.111, s=0.999),
                                   data.apparelTrans = apparelTrans, data.apparelStaticCov = apparelStaticCov)


