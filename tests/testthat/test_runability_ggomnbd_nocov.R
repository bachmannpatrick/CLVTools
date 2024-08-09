skip_on_cran()
data("cdnow")

fct.testthat.runability.nocov(name.model = "GGompertz/NBD", method = ggomnbd, cdnow=cdnow,
                              has.cor = FALSE,
                              start.params.model = c(r = 0.5, alpha = 2, b = 0.1, s=1, beta = 0.1),
                              failed.optimization.methods.expected.message =
                                "Gradient not computable after method|NA/Inf replaced by maximum positive")



