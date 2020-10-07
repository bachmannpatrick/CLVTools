data("cdnow")


l.illegal.start.params.model <- list(c(r = 0, alpha = 1, beta = 1, b = 1, s = 1),
                                     c(r = 1, alpha = 0, beta = 1, b = 1, s = 1),
                                     c(r = 1, alpha = 1, beta = 0, b = 1, s = 1),
                                     c(r = 1, alpha = 1, beta = 1, b = 0, s = 1),
                                     c(r = 1, alpha = 1, beta = 1, b = 1, s = 0),
                                     c(r = -1, alpha = 1, beta = 1, b = 1, s = 1),
                                     c(r = 1, alpha = -1, beta = 1, b = 1, s = 1),
                                     c(r = 1, alpha = 1, beta = -1, b = 1, s = 1),
                                     c(r = 1, alpha = 1, beta = 1, b = -1, s = 1),
                                     c(r = 1, alpha = 1, beta = 1, b = 1, s = -1))


fct.testthat.inputchecks.clvfittedtransactions.nocov(name.method = "GGompertz/NBD", method=ggomnbd,
                                                     start.params.model=c(r=1.234, alpha=0.678, beta=2.345, b=0.1111, s = 0.999),
                                                     l.illegal.start.params.model = l.illegal.start.params.model,
                                                     has.cor = FALSE,
                                                     data.cdnow = cdnow)




