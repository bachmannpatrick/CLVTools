data("cdnow")
data("apparelTrans")
data("apparelStaticCov")


# Coefs are our estimate
fct.testthat.correctness(name.model = "GGompertz/NBD", method=ggomnbd, data.cdnow=cdnow,
                         data.apparelTrans=apparelTrans, data.apparelStaticCov=apparelStaticCov,
                         correct.start.params.model = c(r = 0.5, alpha = 2, b = 0.5, s = 0.5, beta = 0.5),
                         correct.params.nocov.coef = c(r = 0.553, alpha = 10.578, b = 0.0002, s = 0.603, beta = 0.0026),
                         correct.LL.nocov = -9594.97)

# Cannot compare against BTYD

context("Correctness - GGompertz/NBD nocov - Recover parameters")

# Bemmaor and Glady (2012)
#   Table 2, p. 1018
fct.testthat.correctness.nocov.correct.coefs(method = ggomnbd,
                                             cdnow = cdnow,
                                             start.params.model = c(r = 0.5, alpha = 2, b = 0.5, s = 0.5, beta = 0.5),
                                             params.nocov.coef = c(r = 0.553, alpha = 10.578, b = 0.0002, s = 0.603, beta = 0.0026),
                                             LL.nocov = -9594.98)

# No reliable data available for comparison. Paper information is not sufficient
#   In the paper, no explanation is given on how the SEs were derived and they diverge substantially from ours for s and beta
# fct.testthat.correctness.nocov.correct.se(method = ggomnbd,
#                                           cdnow = cdnow,
#                                           start.params.model = c(r = 0.5, alpha = 2, b = 0.5, s = 0.5, beta = 0.5),
#                                           params.nocov.se = c(r = 0.049, alpha = 0.949, b = 0.0000, s = 0.039, beta = 0.0004))


