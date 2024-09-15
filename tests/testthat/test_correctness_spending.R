skip_on_cran()
data("cdnow")
data("apparelTrans")
data("apparelStaticCov")


clv.cdnow <- fct.helper.create.clvdata.cdnow()

test_that("Same as standard interface", {
  skip_on_cran()
  expect_silent(gg.std <- gg(clv.cdnow, verbose = FALSE))
  expect_silent(gg.sp <- spending(family=gg, data=clv.cdnow, verbose=FALSE))

  # all equal but call and runtime
  gg.std@call <- gg.sp@call
  gg.std@optimx.estimation.output[1, "xtime"] <- gg.sp@optimx.estimation.output[1, "xtime"]

  expect_true(isTRUE(all.equal(gg.std, gg.sp)))
})

test_that("remove.first.transaction yields different results", {
  skip_on_cran()
  expect_silent(gg.F <- spending(family=gg, remove.first.transaction=FALSE, data=clv.cdnow, verbose=FALSE))
  expect_silent(gg.T <- spending(family=gg, remove.first.transaction=TRUE, data=clv.cdnow, verbose=FALSE))
  expect_false(any(coef(gg.T) == coef(gg.F)))
})

