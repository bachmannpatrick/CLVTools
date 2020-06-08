data("cdnow")
data("apparelTrans")
data("apparelStaticCov")


fct.testthat.correctness(name.model = "GGompertz/NBD", method=ggomnbd, data.cdnow=cdnow,
                         data.apparelTrans=apparelTrans, data.apparelStaticCov=apparelStaticCov,
                         correct.start.params.model = c(r = 0.5, alpha = 2, b = 0.5, s = 0.5, beta = 0.5),
                         correct.params.nocov.coef = c(r = 0.553, alpha = 10.578, b = 0.0002, s = 0.603, beta = 0.0026),
                         correct.LL.nocov = -9377.94)

# *** ADD comparison vs BTYD


context("Correctness - GGompertz/NBD nocov - Recover parameters")

# # ** CROSS check with paper!
fct.testthat.correctness.nocov.correct.coefs(method = ggomnbd,
                                             cdnow = cdnow,
                                             start.params.model = c(r = 0.5, alpha = 2, b = 0.5, s = 0.5, beta = 0.5),
                                             params.nocov.coef = c(r = 0.553, alpha = 10.578, b = 0.0002, s = 0.603, beta = 0.0026),
                                             LL.nocov = -9377.94)

# No reliable data available for comparison. Paper information is not sufficient
#fct.testthat.correctness.nocov.correct.se(method = ggomnbd,
#                                          cdnow = cdnow,
#                                          start.params.model = c(r = 0.5, alpha = 2, b = 0.5, s = 0.5, beta = 0.5),
#                                          params.nocov.se = c(r = 0.049, alpha = 0.949, b = 0.0000, s = 0.039, beta = 0.0004))

