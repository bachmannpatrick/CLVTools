test_that("Always fails", {
  cat("alksfjasfl")
  expect_silent(warning("Warning! NOW!"))
  expect_silent(stop("FAIL! NOW!"))
})
