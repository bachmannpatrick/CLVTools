skip_on_cran()
data("cdnow")
data("apparelTrans")
data("apparelStaticCov")

context("Correctness - latentAttrition - nocov")
clv.cdnow <- fct.helper.create.clvdata.cdnow(cdnow)

test_that("Same as std interface", {
  skip_on_cran()

  expect_silent(p.std <- pnbd(clv.cdnow, verbose = FALSE))
  expect_silent(p.lA <- latentAttrition(~pnbd(), data=clv.cdnow, verbose=FALSE))
  # all equal but call and runtime
  p.std@call <- p.lA@call
  p.std@optimx.estimation.output[1, "xtime"] <- p.lA@optimx.estimation.output[1, "xtime"]
  expect_true(isTRUE(all.equal(p.std, p.lA)))
})

context("Correctness - latentAttrition - static cov")
clv.apparel.cov <- fct.helper.create.clvdata.apparel.staticcov(apparelTrans, apparelStaticCov, estimation.split = NULL)

test_that("Same as std interface", {
  skip_on_cran()

  expect_silent(p.std <- pnbd(clv.apparel.cov, verbose = FALSE))
  expect_silent(p.lA <- latentAttrition(~pnbd()|.|., data=clv.apparel.cov, verbose=FALSE))
  # all equal but call and runtime
  p.std@call <- p.lA@call
  p.std@optimx.estimation.output[1, "xtime"] <- p.lA@optimx.estimation.output[1, "xtime"]
  expect_true(isTRUE(all.equal(p.std, p.lA)))
})


test_that("Transformations leads to copied data", {
  skip_on_cran()
  expect_silent(p.cov <- latentAttrition(~pnbd()|.|., clv.apparel.cov, verbose=FALSE))
  expect_false(address(clv.apparel.cov) == address(p.cov@clv.data))
  expect_false(address(clv.apparel.cov@data.transactions) == address(p.cov@clv.data@data.transactions))

  expect_silent(p.cov <- latentAttrition(~pnbd()|Gender|Gender, clv.apparel.cov, verbose=FALSE))
  expect_false(address(clv.apparel.cov) == address(p.cov@clv.data))
  expect_false(address(clv.apparel.cov@data.transactions) == address(p.cov@clv.data@data.transactions))

  expect_silent(p.cov <- latentAttrition(~pnbd()|I(Gender+1)|I(Gender+2), clv.apparel.cov, verbose=FALSE))
  expect_false(address(clv.apparel.cov) == address(p.cov@clv.data))
  expect_false(address(clv.apparel.cov@data.transactions) == address(p.cov@clv.data@data.transactions))
})

test_that("Naming covs selects correct cov data", {
  skip_on_cran()
  expect_silent(p.cov <- latentAttrition(~pnbd()|Gender|Gender+Channel, clv.apparel.cov, verbose=FALSE))
  expect_true(all(p.cov@clv.data@names.cov.data.life == "Gender"))
  expect_true(all(p.cov@clv.data@names.cov.data.trans == c("Gender", "Channel")))
})

test_that("'.' selects correct cov data", {
  skip_on_cran()

  expect_silent(p.cov <- latentAttrition(~pnbd()|.|., clv.apparel.cov, verbose=FALSE))
  expect_true(all(p.cov@clv.data@names.cov.data.life == c("Gender", "Channel")))
  expect_true(all(p.cov@clv.data@names.cov.data.trans == c("Gender", "Channel")))

  expect_silent(p.cov <- latentAttrition(~pnbd()|Gender|., clv.apparel.cov, verbose=FALSE))
  expect_true(all(p.cov@clv.data@names.cov.data.life == "Gender"))
  expect_true(all(p.cov@clv.data@names.cov.data.trans == c("Gender", "Channel")))

  expect_silent(p.cov <- latentAttrition(~pnbd()|.|Gender, clv.apparel.cov, verbose=FALSE))
  expect_true(all(p.cov@clv.data@names.cov.data.life == c("Gender", "Channel")))
  expect_true(all(p.cov@clv.data@names.cov.data.trans == "Gender"))

  expect_silent(p.cov <- latentAttrition(~pnbd()|.|.+I(Gender+1), clv.apparel.cov, verbose=FALSE))
  expect_true(all(p.cov@clv.data@names.cov.data.life == c("Gender", "Channel")))
  expect_true(all(p.cov@clv.data@names.cov.data.trans == c("Gender", "Channel", "I.Gender...1.")))
})


test_that("Correct transformations applied", {
  skip_on_cran()
  expect_silent(p.cov <- latentAttrition(~pnbd()|I(Gender+1)|log(Gender+2), clv.apparel.cov, verbose=FALSE))
  expect_true(all(colnames(p.cov@clv.data@data.cov.life) == c("Id", "I.Gender...1.")))
  expect_true(all(colnames(p.cov@clv.data@data.cov.trans) == c("Id", "log.Gender...2.")))

  # does not return a dyncov object
  expect_true(is(p.cov@clv.data, "clv.data.static.covariates") & !is(p.cov@clv.data, "clv.data.dynamic.covariates"))

  expect_true(all(p.cov@clv.data@data.cov.life[, "I.Gender...1."] == clv.apparel.cov@data.cov.life[, "Gender"]+1))
  expect_true(all(p.cov@clv.data@data.cov.trans[, "log.Gender...2."] == log(clv.apparel.cov@data.cov.trans[, "Gender"]+2)))
})

test_that("Correct lambdas used", {
  skip_on_cran()
  expect_silent(p.cov <- latentAttrition(~pnbd()|.|.|regularization(trans=10, life=8), clv.apparel.cov, verbose=FALSE))
  expect_true(p.cov@reg.lambda.life == 8)
  expect_true(p.cov@reg.lambda.trans == 10)
})

test_that("Correct vars constraint", {
  skip_on_cran()
  expect_silent(p.constr <- latentAttrition(~pnbd()|.|.|constraint(Gender), clv.apparel.cov, verbose=FALSE))
  expect_true(p.constr@estimation.used.constraints)
  expect_true(p.constr@names.original.params.constr == "Gender")

  expect_silent(p.constr <- latentAttrition(~pnbd()|.|.|constraint(Gender, Channel), clv.apparel.cov, verbose=FALSE))
  expect_setequal(p.constr@names.original.params.constr, c("Gender", "Channel"))

  # multiple, separate constraints
  expect_silent(p.constr <- latentAttrition(~pnbd()|.|.|constraint(Gender)+constraint(Channel), clv.apparel.cov, verbose=FALSE))
  expect_setequal(p.constr@names.original.params.constr, c("Gender", "Channel"))
})



