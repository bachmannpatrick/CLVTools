data("apparelTrans")
data("apparelStaticCov")

fct.testthat.inputchecks.staticcov(name.method = "PNBD", method=pnbd,
                                   has.cor = TRUE,
                                   start.params.model = c(alpha=1, beta=1, r=1, s=1),
                                   data.apparelTrans = apparelTrans, data.apparelStaticCov = apparelStaticCov)


