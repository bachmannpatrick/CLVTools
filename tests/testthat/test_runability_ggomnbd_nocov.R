data("cdnow")

fct.testthat.runability.nocov(name.model = "GGompertz/NBD", method = ggomnbd, cdnow=cdnow,
                              param.names = c("r", "alpha", "beta", "b", "s"),
                              has.DERT = FALSE, has.cor = FALSE,
                              start.params.model = c(r = 1.23, alpha = 2.34, beta = 0.999, b = 0.678, s=0.123),
                              custom.optimx.args = list(itnmax=40000),
                              failed.optimization.methods.expected.message =
                                "Gradient not computable after method|NA/Inf replaced by maximum positive")



