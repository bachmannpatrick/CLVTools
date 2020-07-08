data("cdnow")
context("GG")

l.illegal.start.params.model <- list(c(p = 0, q = 1, gamma = 1),
                                     c(p = 1, q = 0, gamma = 1),
                                     c(p = 0, q = 1, gamma = 0),
                                     c(p = -1, q = 1, gamma = 1),
                                     c(p = 1, q = -1, gamma = 1),
                                     c(p = 0, q = 1, gamma = -1))


fct.testthat.inputchecks.clvfittedspending.nocov(name.method = "Gamma-Gamma", method=gg,
                                                 start.params.model=c(p=1.234, q=0.678, gamma=2.345),
                                                 l.illegal.start.params.model = l.illegal.start.params.model,
                                                 data.cdnow = cdnow)


# test_that() # placeholder to have "Run Tests" button
