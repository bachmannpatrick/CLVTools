skip_on_cran()
data("cdnow")
data("apparelTrans")
data("apparelStaticCov")


clv.cdnow <- fct.helper.create.clvdata.cdnow(cdnow, estimation.split=37)

test_that("LHS same as std interface", {
  skip_on_cran()
  expect_silent(gg.std <- gg(clv.cdnow, verbose = FALSE))
  expect_silent(gg.sp <- spending(~gg(), data=clv.cdnow, verbose=FALSE))
  expect_silent(gg.sp.lhs <- spending(data(split=37)~gg(), data=cdnow, verbose=FALSE))
  # all equal but call and runtime
  gg.std@call <- gg.sp@call <- gg.sp.lhs@call
  gg.std@clv.data@call <- gg.sp@clv.data@call <- gg.sp.lhs@clv.data@call
  gg.std@optimx.estimation.output[1, "xtime"] <- gg.sp@optimx.estimation.output[1, "xtime"] <- gg.sp.lhs@optimx.estimation.output[1, "xtime"]
  expect_true(isTRUE(all.equal(gg.std, gg.sp)))
  expect_true(isTRUE(all.equal(gg.std, gg.sp.lhs)))
})

test_that("remove.first.trans works", {
  expect_silent(gg.F <- spending(~gg(remove=F), data=clv.cdnow, verbose=FALSE))
  expect_silent(gg.T <- spending(~gg(remove.first.transaction=TRUE), data=clv.cdnow, verbose=FALSE))
  expect_false(any(coef(gg.T) == coef(gg.F)))
})

