skip_on_cran()

data("cdnow")


fct.testthat.runability.nocov(name.model = "PNBD", method = pnbd, cdnow=cdnow,
                              has.DERT = TRUE, has.cor = TRUE,
                              start.params.model = c(r = 1.23, alpha = 2.34, s = 0.678, beta = 0.999),
                              failed.optimization.methods.expected.message =
                                "Gradient not computable after method|NA/Inf replaced by maximum positive")


