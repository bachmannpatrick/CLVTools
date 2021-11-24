skip_on_cran()
data("cdnow")
data("apparelTrans")
data("apparelStaticCov")


context("Correctness - spending() - nocov")
clv.cdnow <- fct.helper.create.clvdata.cdnow(cdnow)

context("Correctness - spending() - static cov")
clv.apparel.cov <- fct.helper.create.clvdata.apparel.staticcov(apparelTrans, apparelStaticCov, estimation.split = NULL)
