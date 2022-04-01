skip_on_cran()

data("cdnow")
data("apparelTrans")
data("apparelStaticCov")

fct.testthat.consistency(name.model = "GGompertz/NBD", method = ggomnbd,
                         data.apparelTrans = apparelTrans, data.apparelStaticCov=apparelStaticCov,
                         fct.LL.ind.nocov = ggomnbd_nocov_LL_ind, fct.LL.ind.static.cov = ggomnbd_staticcov_LL_ind)


