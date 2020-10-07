data("cdnow")

l.illegal.start.params.model <- list(c(alpha=0,  beta=1, r=1, s=1),
                                     c(alpha=-1, beta=1, r=1, s=1),
                                     c(alpha=1,  beta=1, r=0, s=1))


fct.testthat.inputchecks.clvfittedtransactions.nocov(name.method = "PNBD", method=pnbd,
                                                     start.params.model=c(r=1.234, alpha=0.678, s=2.345, beta=0.1111),
                                                     l.illegal.start.params.model = l.illegal.start.params.model,
                                                     has.cor = TRUE,
                                                     data.cdnow = cdnow)



