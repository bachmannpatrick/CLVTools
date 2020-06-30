data("cdnow")

l.illegal.start.params.model <- list(c(r = 0, alpha = 1, a = 1, b = 1),
                                     c(r = 1, alpha = 0, a = 1, b = 1),
                                     c(r = 1, alpha = 1, a = 0, b = 1),
                                     c(r = 1, alpha = 1, a = 1, b = 0),
                                     c(r = -1, alpha = 1, a = 1, b = 1),
                                     c(r = 1, alpha = -1, a = 1, b = 1),
                                     c(r = 1, alpha = 1, a = -1, b = 1),
                                     c(r = 1, alpha = 1, a = 1, b = -1))


fct.testthat.inputchecks.clvfittedtransactions.nocov(name.method = "BG/NBD", method=bgnbd,
                                                     start.params.model=c(r=1.234, alpha=0.678, a=2.345, b=0.1111),
                                                     l.illegal.start.params.model = l.illegal.start.params.model,
                                                     has.cor = FALSE,
                                                     data.cdnow = cdnow)




