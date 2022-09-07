skip_on_cran()
data("cdnow")
data("apparelTrans")
data("apparelStaticCov")


# no cov ---------------------------------------------------------------------------------------------------
context("Correctness - latentAttrition - nocov")
clv.cdnow <- fct.helper.create.clvdata.cdnow(cdnow, estimation.split=37)

test_that("Same as std interface", {
  skip_on_cran()

  expect_silent(p.std <- pnbd(clv.cdnow, verbose = FALSE))
  expect_silent(p.lA <- latentAttrition(~pnbd(), data=clv.cdnow, verbose=FALSE))
  expect_silent(p.lA.lhs <- latentAttrition(data(split=37)~pnbd(), data=cdnow, verbose=FALSE))
  # all equal but call and runtime
  p.std@call <- p.lA@call <- p.lA.lhs@call
  p.std@clv.data@call <- p.lA@clv.data@call <- p.lA.lhs@clv.data@call
  p.std@optimx.estimation.output[1, "xtime"] <- p.lA@optimx.estimation.output[1, "xtime"] <- p.lA.lhs@optimx.estimation.output[1, "xtime"]
  expect_true(isTRUE(all.equal(p.std, p.lA)))
  expect_true(isTRUE(all.equal(p.std, p.lA.lhs)))
})


# static cov ---------------------------------------------------------------------------------------------------
fct.testthat.correctness.latentattrition.staticcov <- function(l.std.args, f.lhs, clv.apparel.cov){
  test_that("Same as std interface", {
    skip_on_cran()

    expect_silent(p.std <- pnbd(clv.apparel.cov, verbose = FALSE))
    expect_silent(p.lA <- do.call(latentAttrition, c(l.std.args, list(formula=update(as.Formula(~pnbd()|.|.), new=f.lhs)))))
    # all equal but call and runtime
    p.std@call <- p.lA@call <- str2lang("fakecall")
    p.std@clv.data@call <- p.lA@clv.data@call <- str2lang("fakecall")
    p.std@optimx.estimation.output[1, "xtime"] <- p.lA@optimx.estimation.output[1, "xtime"]
    expect_true(isTRUE(all.equal(p.std, p.lA)))
  })

  test_that("Transformations leads to copied data", {
    skip_on_cran()
    expect_silent(p.cov <- do.call(latentAttrition, c(l.std.args, list(formula=update(as.Formula(~pnbd()|.|.), new=f.lhs)))))
    expect_false(address(clv.apparel.cov) == address(p.cov@clv.data))
    expect_false(address(clv.apparel.cov@data.transactions) == address(p.cov@clv.data@data.transactions))

    expect_silent(p.cov <- do.call(latentAttrition, c(l.std.args, list(formula=update(as.Formula(~pnbd()|Gender|Gender), new=f.lhs)))))
    expect_false(address(clv.apparel.cov) == address(p.cov@clv.data))
    expect_false(address(clv.apparel.cov@data.transactions) == address(p.cov@clv.data@data.transactions))

    expect_silent(p.cov <- do.call(latentAttrition, c(l.std.args, list(formula=update(as.Formula(~pnbd()|I(Gender+1)|I(Gender+2)), new=f.lhs)))))
    expect_false(address(clv.apparel.cov) == address(p.cov@clv.data))
    expect_false(address(clv.apparel.cov@data.transactions) == address(p.cov@clv.data@data.transactions))
  })

  test_that("Naming covs selects correct cov data", {
    skip_on_cran()
    expect_silent(p.cov <- do.call(latentAttrition, c(l.std.args, list(formula=update(as.Formula(~pnbd()|Gender|Gender+Channel), new=f.lhs)))))
    expect_true(all(p.cov@clv.data@names.cov.data.life == "Gender"))
    expect_true(all(p.cov@clv.data@names.cov.data.trans == c("Gender", "Channel")))
  })

  test_that("'.' selects correct cov data", {
    skip_on_cran()

    expect_silent(p.cov <- do.call(latentAttrition, c(l.std.args, list(formula=update(as.Formula(~pnbd()|.|.), new=f.lhs)))))
    expect_true(all(p.cov@clv.data@names.cov.data.life == c("Gender", "Channel")))
    expect_true(all(p.cov@clv.data@names.cov.data.trans == c("Gender", "Channel")))

    expect_silent(p.cov <- do.call(latentAttrition, c(l.std.args, list(formula=update(as.Formula(~pnbd()|Gender|.), new=f.lhs)))))
    expect_true(all(p.cov@clv.data@names.cov.data.life == "Gender"))
    expect_true(all(p.cov@clv.data@names.cov.data.trans == c("Gender", "Channel")))

    expect_silent(p.cov <- do.call(latentAttrition, c(l.std.args, list(formula=update(as.Formula(~pnbd()|.|Gender), new=f.lhs)))))
    expect_true(all(p.cov@clv.data@names.cov.data.life == c("Gender", "Channel")))
    expect_true(all(p.cov@clv.data@names.cov.data.trans == "Gender"))

    expect_silent(p.cov <- do.call(latentAttrition, c(l.std.args, list(formula=update(as.Formula(~pnbd()|.|.+I(Gender+1)), new=f.lhs)))))
    expect_true(all(p.cov@clv.data@names.cov.data.life == c("Gender", "Channel")))
    expect_true(all(p.cov@clv.data@names.cov.data.trans == c("Gender", "Channel", "I.Gender...1.")))
  })


  test_that("Correct transformations applied", {
    skip_on_cran()
    expect_silent(p.cov <- do.call(latentAttrition, c(l.std.args, list(formula=update(as.Formula(~pnbd()|I(Gender+1)|log(Gender+2)), new=f.lhs)))))
    expect_true(all(colnames(p.cov@clv.data@data.cov.life) == c("Id", "I.Gender...1.")))
    expect_true(all(colnames(p.cov@clv.data@data.cov.trans) == c("Id", "log.Gender...2.")))

    # does not return a dyncov object
    expect_true(is(p.cov@clv.data, "clv.data.static.covariates") & !is(p.cov@clv.data, "clv.data.dynamic.covariates"))

    expect_true(all(p.cov@clv.data@data.cov.life[, "I.Gender...1."] == clv.apparel.cov@data.cov.life[, "Gender"]+1))
    expect_true(all(p.cov@clv.data@data.cov.trans[, "log.Gender...2."] == log(clv.apparel.cov@data.cov.trans[, "Gender"]+2)))
  })

  test_that("Correct lambdas used", {
    skip_on_cran()
    expect_silent(p.cov <- do.call(latentAttrition, c(l.std.args, list(formula=update(as.Formula(~pnbd()|.|.|regularization(trans=10, life=8)), new=f.lhs)))))
    expect_true(p.cov@reg.lambda.life == 8)
    expect_true(p.cov@reg.lambda.trans == 10)
  })

  test_that("Correct vars constraint", {
    skip_on_cran()
    expect_silent(p.constr <- do.call(latentAttrition, c(l.std.args, list(formula=update(as.Formula(~pnbd()|.|.|constraint(Gender)), new=f.lhs)))))
    expect_true(p.constr@estimation.used.constraints)
    expect_true(p.constr@names.original.params.constr == "Gender")

    expect_silent(p.constr <- do.call(latentAttrition, c(l.std.args, list(formula=update(as.Formula(~pnbd()|.|.|constraint(Gender, Channel)), new=f.lhs)))))
    expect_setequal(p.constr@names.original.params.constr, c("Gender", "Channel"))

    # multiple, separate constraints
    expect_silent(p.constr <- do.call(latentAttrition, c(l.std.args, list(formula=update(as.Formula(~pnbd()|.|.|constraint(Gender)+constraint(Channel)), new=f.lhs)))))
    expect_setequal(p.constr@names.original.params.constr, c("Gender", "Channel"))
  })

  test_that("Correct cov data when using interactions and all except (. -)", {
    skip_on_cran()
    expect_silent(p.inter <- do.call(latentAttrition, c(l.std.args, list(formula=update(as.Formula(~pnbd()|Gender*Channel|.-Gender), new=f.lhs)))))
    expect_setequal(c("Channel", "Gender", "Gender.Channel", "Id"), colnames(p.inter@clv.data@data.cov.life))
    expect_setequal(c("Channel", "Id"), colnames(p.inter@clv.data@data.cov.trans))
  })
}


context("Correctness - latentAttrition - static cov, data=clv.data")
# Test with clv.static.cov object
clv.apparel.cov <- fct.helper.create.clvdata.apparel.staticcov(apparelTrans, apparelStaticCov, estimation.split = NULL)
fct.testthat.correctness.latentattrition.staticcov(l.std.args = list(verbose=FALSE, data=clv.apparel.cov),
                                                   f.lhs=~., clv.apparel.cov=clv.apparel.cov)

context("Correctness - latentAttrition - static cov, data=data.frame")
# Test with data.frame input
fct.testthat.correctness.latentattrition.staticcov(l.std.args = list(data=apparelTrans, cov=apparelStaticCov,verbose=FALSE),
                                                   f.lhs=data(split=NULL, unit=w, format=ymd)~., clv.apparel.cov=clv.apparel.cov)





# dynamic cov ---------------------------------------------------------------------------------------------------

fct.testthat.correctness.latentattrition.dyncov <- function(l.std.args, f.lhs, clv.apparel.dyn){
  skip_on_cran()
  clv.dyn.common.cols <- c("Id", "Cov.Date") #,"tp.cov.lower","tp.cov.upper")

  test_that("Same as std interface", {
    skip_on_cran()

    expect_warning(p.std <- pnbd(clv.apparel.dyn, verbose = FALSE, optimx.args=fct.helper.dyncov.get.optimxargs.quickfit()), "Hessian")
    expect_warning(p.lA <- do.call(latentAttrition, c(l.std.args, list(formula=update(as.Formula(~pnbd()|.|.), new=f.lhs)))), "Hessian")
    # all equal but call and runtime
    p.std@call <- p.lA@call <- str2lang("fakecall")
    p.std@clv.data@call <- p.lA@clv.data@call <- str2lang("fakecall")
    p.std@optimx.estimation.output[1, "xtime"] <- p.lA@optimx.estimation.output[1, "xtime"]
    expect_true(isTRUE(all.equal(p.std, p.lA)))
  })

  test_that("Correct cov data selected and transformations applied, data copied", {
    skip_on_cran()
    expect_warning(p.dyn <- do.call(latentAttrition, c(l.std.args, list(formula=update(as.Formula(~pnbd()|I(Gender+1)+Channel|log(Gender+2)+Marketing), new=f.lhs)))),
                   "Hessian")

    expect_setequal(colnames(p.dyn@clv.data@data.cov.life), c(clv.dyn.common.cols, "Channel", "I.Gender...1."))
    expect_setequal(colnames(p.dyn@clv.data@data.cov.trans), c(clv.dyn.common.cols, "Marketing","log.Gender...2."))

    expect_false(address(clv.apparel.dyn) == address(p.dyn@clv.data))
    expect_false(address(clv.apparel.dyn@data.transactions) == address(p.dyn@clv.data@data.transactions))
  })

  test_that("'.' selects correct cov data", {
    skip_on_cran()
    expect_warning(p.dyn <- do.call(latentAttrition, c(l.std.args, list(formula=update(as.Formula(~pnbd()|.|.+I(Gender+1)), new=f.lhs)))),
                   "Hessian")
    expect_setequal(colnames(p.dyn@clv.data@data.cov.life), c(clv.dyn.common.cols, "Channel", "Gender", "Marketing"))
    expect_setequal(colnames(p.dyn@clv.data@data.cov.trans), c(clv.dyn.common.cols, "Channel", "Gender", "Marketing", "I.Gender...1."))
  })

  test_that("Correct cov data when using interactions and all except (. -)", {
    skip_on_cran()
    expect_warning(p.dyn <- do.call(latentAttrition, c(l.std.args, list(formula=update(as.Formula(~pnbd()|Gender*Channel|.-Gender), new=f.lhs)))),
                   "Hessian")
    expect_setequal(c("Channel", "Gender", "Gender.Channel", "Id", "Cov.Date"), colnames(p.dyn@clv.data@data.cov.life))
    expect_setequal(c("Channel", "Marketing", "Id", "Cov.Date"), colnames(p.dyn@clv.data@data.cov.trans))
  })
}

context("Correctness - latentAttrition - dynamic cov, data=clv.data")
# # Test with clv.dyn.cov object
clv.apparel.dyn <- fct.helper.create.clvdata.apparel.dyncov(apparelTrans, apparelDynCov, estimation.split = 40)
fct.testthat.correctness.latentattrition.dyncov(l.std.args = list(verbose=FALSE, data=clv.apparel.dyn,
                                                                  optimx.args=fct.helper.dyncov.get.optimxargs.quickfit()),
                                                f.lhs=~., clv.apparel.dyn=clv.apparel.dyn)

context("Correctness - latentAttrition - dynamic cov, data=data.frame")
# Test with data.frame input
fct.testthat.correctness.latentattrition.dyncov(l.std.args = list(verbose=FALSE, data=apparelTrans, cov=apparelDynCov,
                                                                  optimx.args=fct.helper.dyncov.get.optimxargs.quickfit()),
                                                f.lhs=data(split=40, unit=w, format=ymd)~., clv.apparel.dyn=clv.apparel.dyn)
