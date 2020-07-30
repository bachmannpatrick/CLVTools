data("cdnow")

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


test_that("Cannot fit on data with negative spending", {
  cdnow.neg <- copy(cdnow)

  # TODO: Cann fit on Prices = 0?
  # expect_silent(cdnow.neg[1000, Price := 0])
  # expect_silent(clv.neg <- clvdata(cdnow.neg, date.format = "ymd", time.unit = "w", estimation.split = 37))
  # expect_error(gg(clv.neg), regexp = "negative prices")

  expect_silent(cdnow.neg[1000, Price := -1])
  expect_silent(clv.neg <- clvdata(cdnow.neg, date.format = "ymd", time.unit = "w", estimation.split = 37))
  expect_error(gg(clv.neg), regexp = "negative prices")
})



