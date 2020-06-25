skip_on_cran()

data("cdnow")
data("apparelTrans")
data("apparelStaticCov")

fct.testthat.consistency(name.model = "BG/NBD", method = bgnbd,
                         data.apparelTrans = apparelTrans, data.apparelStaticCov=apparelStaticCov,
                         param.names = c("r", "alpha", "a", "b"),
                         fct.LL.ind.nocov = bgnbd_nocov_LL_ind, fct.LL.ind.static.cov = bgnbd_staticcov_LL_ind)

