data("apparelTrans")
data("apparelStaticCov")

fct.testthat.inputchecks.staticcov(name.method = "BG/NBD", method=bgnbd,
                                   has.cor = FALSE,
                                   start.params.model = c(r=1, alpha=3, a=1, b=3),
                                   data.apparelTrans = apparelTrans, data.apparelStaticCov = apparelStaticCov)


