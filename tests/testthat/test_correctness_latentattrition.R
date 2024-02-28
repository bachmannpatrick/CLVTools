skip_on_cran()
data("cdnow")
data("apparelTrans")
data("apparelStaticCov")
data("apparelDynCov")


# no cov ---------------------------------------------------------------------------------------------------
clv.cdnow <- fct.helper.create.clvdata.cdnow(cdnow)

test_that("No cov: Same as std interface", {
  skip_on_cran()

  expect_silent(p.std <- pnbd(clv.cdnow, verbose = FALSE))
  expect_silent(p.lA <- latentAttrition(formula = , family=pnbd, data=clv.cdnow, verbose=FALSE))

  # all equal except call and runtime
  p.std@call <- p.lA@call
  p.std@optimx.estimation.output[1, "xtime"] <- p.lA@optimx.estimation.output[1, "xtime"]

  expect_true(isTRUE(all.equal(p.std, p.lA)))
})




# static cov ---------------------------------------------------------------------------------------------------

clv.apparel.cov <- fct.helper.create.clvdata.apparel.staticcov(apparelTrans, apparelStaticCov, estimation.split = NULL)

.fct.test.latentattrition.selects.correct.covs <- function(formula, covs.life, covs.trans){
  expect_silent(p.lA <- latentAttrition(formula = formula, family=pnbd, data = clv.apparel.cov, verbose=FALSE))
  expect_true(all(p.lA@clv.data@names.cov.data.life == covs.life))
  expect_true(all(p.lA@clv.data@names.cov.data.trans == covs.trans))
}


test_that("Static cov: Same as std interface", {
  skip_on_cran()

  expect_silent(p.std <- pnbd(clv.apparel.cov, verbose = FALSE))
  expect_silent(p.lA <- latentAttrition(formula = ~.|., family=pnbd, data = clv.apparel.cov, verbose=FALSE))

  # all equal but call and runtime
  p.std@call <- p.lA@call
  p.std@optimx.estimation.output[1, "xtime"] <- p.lA@optimx.estimation.output[1, "xtime"]

  expect_true(isTRUE(all.equal(p.std, p.lA)))
})


test_that("Transformations yield copied data", {
  skip_on_cran()

  # Selecing all with .
  expect_silent(p.lA <- latentAttrition(formula = ~.|., family=pnbd, data = clv.apparel.cov, verbose=FALSE))
  expect_false(address(clv.apparel.cov) == address(p.lA@clv.data))
  expect_false(address(clv.apparel.cov@data.transactions) == address(p.lA@clv.data@data.transactions))

  # Selecting single covs
  expect_silent(p.lA <- latentAttrition(formula = ~Gender|Gender, family=pnbd, data = clv.apparel.cov, verbose=FALSE))
  expect_false(address(clv.apparel.cov) == address(p.lA@clv.data))
  expect_false(address(clv.apparel.cov@data.transactions) == address(p.lA@clv.data@data.transactions))

  # transformed covs
  expect_silent(p.lA <- latentAttrition(formula = ~I(Gender+1)|I(Gender+2), family=pnbd,data = clv.apparel.cov, verbose=FALSE))
  expect_false(address(clv.apparel.cov) == address(p.lA@clv.data))
  expect_false(address(clv.apparel.cov@data.transactions) == address(p.lA@clv.data@data.transactions))
})


test_that("Naming covs selects correct cov data", {
  skip_on_cran()

  .fct.test.latentattrition.selects.correct.covs(formula = ~Gender|Gender+Channel, covs.life="Gender", covs.trans=c("Gender", "Channel"))
})



test_that("'.' selects correct cov data", {
  skip_on_cran()

  .fct.test.latentattrition.selects.correct.covs(formula = ~.|., covs.life=c("Gender", "Channel"), covs.trans=c("Gender", "Channel"))
  .fct.test.latentattrition.selects.correct.covs(formula = ~Gender|., covs.life="Gender", covs.trans=c("Gender", "Channel"))
  .fct.test.latentattrition.selects.correct.covs(formula = ~.|.+I(Gender+1),
                                                 covs.life=c("Gender", "Channel"), covs.trans=c("Gender", "Channel", "I.Gender...1."))
})


test_that("Selecing same cov multiple times yields only once", {
  skip_on_cran()
  .fct.test.latentattrition.selects.correct.covs(formula = ~Gender+Gender+Channel|., covs.life=c("Gender", "Channel"), covs.trans=c("Gender", "Channel"))
})

test_that("Correct transformations applied", {
  skip_on_cran()
  expect_silent(p.lA <- latentAttrition(formula=~I(Gender+1)|log(Gender+2), family=pnbd, data=clv.apparel.cov, verbose=FALSE))
  expect_true(all(colnames(p.lA@clv.data@data.cov.life) == c("Id", "I.Gender...1.")))
  expect_true(all(colnames(p.lA@clv.data@data.cov.trans) == c("Id", "log.Gender...2.")))

  # does not return a dyncov object
  expect_true(is(p.lA@clv.data, "clv.data.static.covariates") & !is(p.lA@clv.data, "clv.data.dynamic.covariates"))

  # Correct transformations done
  expect_true(all(p.lA@clv.data@data.cov.life[, "I.Gender...1."] == clv.apparel.cov@data.cov.life[, "Gender"]+1))
  expect_true(all(p.lA@clv.data@data.cov.trans[, "log.Gender...2."] == log(clv.apparel.cov@data.cov.trans[, "Gender"]+2)))
})

test_that("Correct cov data when using interactions and all except (. -)", {
  skip_on_cran()
  expect_silent(p.lA <- latentAttrition(formula=~Gender*Channel|.-Gender, family=pnbd, data=clv.apparel.cov, verbose=FALSE))
  expect_setequal(c("Channel", "Gender", "Gender.Channel", "Id"), colnames(p.lA@clv.data@data.cov.life))
  expect_setequal(c("Channel", "Id"), colnames(p.lA@clv.data@data.cov.trans))
})



# dynamic cov ---------------------------------------------------------------------------------------------------
clv.apparel.dyn <- fct.helper.create.clvdata.apparel.dyncov(apparelTrans, apparelDynCov, estimation.split = 52)
clv.dyn.common.cols <- c("Id", "Cov.Date","tp.cov.lower","tp.cov.upper")

.fct.latentattrition.fit.dyncov <- function(formula){
  expect_warning(p.lA <- latentAttrition(formula = formula, family=pnbd, data=clv.apparel.dyn, verbose = FALSE,
                                         optimx.args = fct.helper.dyncov.get.optimxargs.quickfit()),
    regexp="Hessian")
  return(p.lA)
}

test_that("Same as std interface", {
  skip_on_cran()

  expect_warning(p.std <- pnbd(clv.apparel.dyn, verbose = FALSE, optimx.args=fct.helper.dyncov.get.optimxargs.quickfit()), "Hessian")
  p.lA <- .fct.latentattrition.fit.dyncov(formula=~.|.)

  # all equal but call and runtime
  p.std@call <- p.lA@call
  p.std@optimx.estimation.output[1, "xtime"] <- p.lA@optimx.estimation.output[1, "xtime"]

  expect_true(isTRUE(all.equal(p.std, p.lA)))
})

test_that("Correct cov data selected and transformations applied, data copied", {
  skip_on_cran()
  p.lA <- .fct.latentattrition.fit.dyncov(formula=~I(Gender+1)+Channel|log(Gender+2)+High.Season)

  expect_setequal(colnames(p.lA@clv.data@data.cov.life), c(clv.dyn.common.cols, "Channel", "I.Gender...1."))
  expect_setequal(colnames(p.lA@clv.data@data.cov.trans), c(clv.dyn.common.cols, "High.Season","log.Gender...2."))

  expect_false(address(clv.apparel.dyn) == address(p.lA@clv.data))
  expect_false(address(clv.apparel.dyn@data.transactions) == address(p.lA@clv.data@data.transactions))
})

test_that("'.' selects correct cov data", {
  skip_on_cran()
  p.lA <- .fct.latentattrition.fit.dyncov(formula=~.|.+I(Gender+1))
  expect_setequal(colnames(p.lA@clv.data@data.cov.life), c(clv.dyn.common.cols, "Channel", "Gender", "High.Season"))
  expect_setequal(colnames(p.lA@clv.data@data.cov.trans), c(clv.dyn.common.cols, "Channel", "Gender", "High.Season", "I.Gender...1."))
})

test_that("Correct cov data when using interactions and all except (. -)", {
  skip_on_cran()
  p.lA <- .fct.latentattrition.fit.dyncov(formula=~Gender*Channel|.-Gender)
  expect_setequal(c("Channel", "Gender", "Gender.Channel", clv.dyn.common.cols), colnames(p.lA@clv.data@data.cov.life))
  expect_setequal(c("Channel", "High.Season", clv.dyn.common.cols), colnames(p.lA@clv.data@data.cov.trans))
})
