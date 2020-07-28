skip_on_cran()
data("cdnow")

fct.testthat.runability.nocov(name.model = "BG/NBD", method = bgnbd, cdnow=cdnow,
                              has.DERT = FALSE, has.cor = FALSE,
                              start.params.model = c(r = 1.23, alpha = 2.34, a = 0.999, b = 0.678),
                              failed.optimization.methods.expected.message =
                                "Gradient not computable after method|NA/Inf replaced by maximum positive")


