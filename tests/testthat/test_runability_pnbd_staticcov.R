# Setup -----------------------------------------------------------------------------------------------------------------------------------------
data("apparelTrans")
data("apparelStaticCov")

context("Runability - PNBD static cov - Basic runability")

expect_silent(clv.data.apparel        <- clvdata(data.transactions = apparelTrans, date.format = "ymd", time.unit = "W",
                                                 estimation.split = 40))
expect_silent(clv.data.apparel.no.holdout <- clvdata(data.transactions = apparelTrans, date.format = "ymd", time.unit = "W"))

expect_silent(clv.data.cov.holdout    <- SetStaticCovariates(clv.data.apparel, data.cov.life = apparelStaticCov, data.cov.trans = apparelStaticCov,
                                                                 names.cov.life = "Gender", names.cov.trans = "Gender"))
expect_silent(clv.data.cov.no.holdout <- SetStaticCovariates(clv.data.apparel.no.holdout, data.cov.life = apparelStaticCov, data.cov.trans = apparelStaticCov,
                                                                 names.cov.life = "Gender", names.cov.trans = "Gender"))

# Newdata clv data object to test plot/predict
#   Create with new fake data and generally other names
set.seed(0xcaffe)

expect_silent(dt.newdata <- data.table::rbindlist(lapply(paste0(LETTERS,1:100,sep=""), function(cid){
  data.table::data.table(cust.id = cid,
             trans.date = seq.Date(from = as.Date(apparelTrans[, min(Date)]), to = as.Date(apparelTrans[, max(Date)]),
                                   length.out = sample.int(n=5, size = 1, replace=FALSE)))
})))
expect_silent(dt.newdata[, trans.date := format(trans.date, "%Y:%d:%m")])

# Generate fake cov data
expect_silent(dt.covs <- data.table::rbindlist(lapply(paste0(LETTERS,1:100,sep=""), function(cid){
  data.table::data.table(cid, Gender = sample(0:1, size = 1))
})))

# create newdata objects with covariates
expect_silent(clv.newdata.nohold <- SetStaticCovariates(
  clv.data = clvdata(data.transactions = dt.newdata, date.format = "ydm", time.unit = "w",
                     estimation.split = NULL, name.id = "cust.id", name.date = "trans.date",
                     name.price = NULL),
  data.cov.life = dt.covs, data.cov.trans = dt.covs,
  names.cov.life = "Gender", names.cov.trans = "Gender",
  name.id = "cid"))

expect_silent(clv.newdata.withhold <- SetStaticCovariates(
  clv.data = clvdata(data.transactions = dt.newdata, date.format = "ydm", time.unit = "w",
                     estimation.split = 40, name.id = "cust.id", name.date = "trans.date",
                     name.price = NULL),
  data.cov.life = dt.covs, data.cov.trans = dt.covs,
  names.cov.life = "Gender", names.cov.trans = "Gender",
  name.id = "cid"))




# Basic runability -------------------------------------------------------------------------------------------------------

test_that("Works out-of-the box, without additional params", {
  expect_silent(p.hold    <- pnbd(clv.data.cov.holdout, verbose=FALSE))
  expect_silent(p.no.hold <- pnbd(clv.data.cov.no.holdout, verbose=FALSE))
  fct.helper.fitted.all.s3(p.hold,   full.names = c("r", "alpha", "s","beta", "life.Gender", "trans.Gender"),
                           clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold)
  fct.helper.fitted.all.s3(p.no.hold, full.names = c("r", "alpha", "s","beta", "life.Gender", "trans.Gender"),
                           clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold)
})

test_that("Works with custom model start parameters", {
  skip_on_cran()
  expect_silent(pnbd(clv.data.cov.holdout,    start.params.model = c(r=1, alpha = 2, s = 1, beta = 2),verbose=FALSE))
  expect_silent(pnbd(clv.data.cov.no.holdout, start.params.model = c(r=1, alpha = 2, s = 1, beta = 2),verbose=FALSE))
})

test_that("Works with custom model and covariate start parameters", {
  skip_on_cran()
  expect_silent(pnbd(clv.data.cov.holdout,    start.params.model = c(r=1, alpha = 2, s = 1, beta = 2),
                          start.params.life = c(Gender = 1), start.params.trans = c(Gender=1), verbose = FALSE))
  expect_silent(pnbd(clv.data.cov.no.holdout, start.params.model = c(r=1, alpha = 2, s = 1, beta = 2),
                     start.params.life = c(Gender = 1), start.params.trans = c(Gender=1), verbose = FALSE))
})



# Reduces to covariates ------------------------------------------------------------------------------------------------------------------------
test_that("Reduces to relevant covariates only for estimation", {
  # skip_on_cran()

  # Create fantasy covariate to immediately remove again
  dt.fake <- data.table::copy(apparelStaticCov)[, fake:= c(rep(c(1,0), 250/2))]
  expect_silent(clv.data.fake.cov <- SetStaticCovariates(clv.data.apparel, data.cov.life = dt.fake, data.cov.trans = dt.fake,
                                                     names.cov.life = "Gender", names.cov.trans = c("fake","Gender")))
  expect_silent(e.pnbd.fake.cov <-pnbd(clv.data.fake.cov, names.cov.trans = "Gender",verbose=FALSE)) # only keep Gender
  expect_false("fake" %in% names(coef(e.pnbd.fake.cov)))
  expect_false("fake" %in% colnames(e.pnbd.fake.cov@clv.data@data.cov.life))
  expect_false("fake" %in% colnames(e.pnbd.fake.cov@clv.data@data.cov.trans))

  # Same but with lifetime process
  dt.fake <- data.table::copy(apparelStaticCov)[, fake:= c(rep(c(1,0), 250/2))]
  expect_silent(clv.data.fake.cov <- SetStaticCovariates(clv.data.apparel, data.cov.life = dt.fake, data.cov.trans = dt.fake,
                                                     names.cov.life = c("fake","Gender"), names.cov.trans = "Gender"))
  expect_silent(e.pnbd.fake.cov <-pnbd(clv.data.fake.cov, names.cov.life = "Gender",verbose=FALSE)) # only keep Gender
  expect_false("fake" %in% names(coef(e.pnbd.fake.cov)))
  expect_false("fake" %in% colnames(e.pnbd.fake.cov@clv.data@data.cov.life))
  expect_false("fake" %in% colnames(e.pnbd.fake.cov@clv.data@data.cov.trans))
})

# Correlation ---------------------------------------------------------------------------------------------
context("Runability - PNBD static cov - w/ Correlation")

test_that("Works with use.cor=T", {
  skip_on_cran()

  # Holdout does
  expect_silent(pscc <- pnbd(clv.data.cov.holdout, use.cor=TRUE, verbose=FALSE))
  fct.helper.fitted.all.s3(pscc, full.names = c("r", "alpha", "s", "beta", pscc@name.correlation.cor, "life.Gender", "trans.Gender"),
                           clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold)
  # .fct.helper.s3.fitted.coef(clv.fitted = pscc, full.names = c("r", "alpha", "s", "beta", "life.Gender", "trans.Gender", pscc@name.correlation.cor))
  # expect_warning(summary(pscc), regexp = "For some parameters the standard error could not be calculated.")
  # .fct.helper.s3.fitted.print(clv.fitted = pscc)
  # .fct.helper.s3.fitted.nobs(clv.fitted = pscc)
  # .fct.helper.s3.fitted.logLik(clv.fitted = pscc)
  # .fct.helper.s3.fitted.plot(clv.fitted = pscc,
  #                            clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold)
  # .fct.helper.s3.fitted.predict(clv.fitted = pscc,
  #                               clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold)
  # .fct.helper.s3.fitted.vcov(clv.fitted = pscc, full.names = c("r", "alpha", "s", "beta", "life.Gender", "trans.Gender", pscc@name.correlation.cor))

  # expect_silent(pscc <- pnbd(clv.data.cov.no.holdout, use.cor=TRUE, verbose=FALSE))
  # fct.helper.fitted.all.s3(pscc, full.names = c("r", "alpha", "s", "beta", "life.Gender", "trans.Gender", pscc@name.correlation.cor))
})
#
# test_that("Works with use.cor=T and start.params", {
#   skip_on_cran()
#   skip_on_ci()
#   skip_on_covr()
#
#   # NO Holdout + start.param.cor=0.1 works!
#   expect_silent(pscc <- pnbd(clv.data.cov.no.holdout, use.cor=TRUE, start.param.cor = 0.1, verbose=FALSE))
#   fct.helper.fitted.all.s3(clv.fitted = pscc)
#
#   # Vcov does not work but other S3 that do not need vcov() should still work
#   expect_silent(coef(pscc))
#   expect_warning(summary(pscc), regexp = "For some parameters the standard error could not be calculated.")
#   expect_silent(predict(pscc, verbose=FALSE))
#   expect_silent(plot(pscc, verbose=FALSE))
# })


# Interlayers ----------------------------------------------------------------------------------------------------------------------------------
context("Runability - PNBD static cov - w/ Constraint")
test_that("Works with single constraints", {
  skip_on_cran()

  # Without start param
  expect_silent(p.hold    <- pnbd(clv.data.cov.holdout,    names.cov.constr = "Gender",verbose=FALSE))
  expect_silent(p.no.hold <- pnbd(clv.data.cov.no.holdout, names.cov.constr = "Gender",verbose=FALSE))
  fct.helper.fitted.all.s3(p.hold,    full.names = c("r", "alpha", "s","beta", "constr.Gender"),
                           clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold)
  fct.helper.fitted.all.s3(p.no.hold, full.names = c("r", "alpha", "s","beta", "constr.Gender"),
                           clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold)

  # With start param
  expect_silent(pnbd(clv.data.cov.holdout,    names.cov.constr = "Gender", start.params.constr = c(Gender=1),verbose=FALSE))
  expect_silent(pnbd(clv.data.cov.no.holdout, names.cov.constr = "Gender", start.params.constr = c(Gender=1),verbose=FALSE))
})

# ** TODO:
# test_that("Works with two constraints", {
#   # expect_message(pnbd(clv.data.cov.holdout, names.cov.constr = "Gender", start.params.constr = c(Gender=1),
#                           # start.params.model = c(r=1, alpha = 2, s = 1, beta = 2), start.params.life = c("Gender" = 1), start.params.trans = c("Gender"=1)))
# })

# ** TODO:
# test_that("Works with one constraints, other unconstraint", {
#   # expect_message(pnbd(clv.data.cov.holdout, names.cov.constr = "Gender", start.params.constr = c(Gender=1),
#                           # start.params.model = c(r=1, alpha = 2, s = 1, beta = 2), start.params.life = c("Gender" = 1), start.params.trans = c("Gender"=1)))
#  Expect to see in coef. And named in output of print/summary as well
#   expect_(coef())
# })


context("Runability - PNBD static cov - w/ Regularization")
test_that("Works with regularization", {
  expect_silent(p.hold    <- pnbd(clv.data.cov.holdout,    reg.lambdas = c(trans=10, life=10),verbose=FALSE))
  expect_silent(p.no.hold <- pnbd(clv.data.cov.no.holdout, reg.lambdas = c(trans=10, life=10),verbose=FALSE))
  fct.helper.fitted.all.s3(p.hold,    full.names = c("r", "alpha", "s","beta", "life.Gender", "trans.Gender"),
                           clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold)
  fct.helper.fitted.all.s3(p.no.hold, full.names = c("r", "alpha", "s","beta", "life.Gender", "trans.Gender"),
                           clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold)
})

test_that("Works with 0 regularization lambdas", {
  skip_on_cran()
  skip_on_ci()
  skip_on_covr()
  expect_silent(p.hold    <- pnbd(clv.data.cov.holdout,   reg.lambdas = c(trans=0, life=0),verbose=FALSE))
  expect_silent(p.no.hold <- pnbd(clv.data.cov.no.holdout,reg.lambdas = c(trans=0, life=0),verbose=FALSE))
  fct.helper.fitted.all.s3(p.hold,   full.names = c("r", "alpha", "s","beta", "life.Gender", "trans.Gender"),
                           clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold)
  fct.helper.fitted.all.s3(p.no.hold, full.names = c("r", "alpha", "s","beta", "life.Gender", "trans.Gender"),
                           clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold)
})


context("Runability - PNBD static cov - w/ combinations")
test_that("Works with combined interlayers", {
  # Try all combinations of interlayers
  skip_on_cran()
  skip_on_ci()
  skip_on_covr()

  # **TODO: All warnings if use.cor
  # expect_warning(p.hold    <- pnbd(clv.data.cov.holdout,
  #                                  use.cor = TRUE, names.cov.constr = "Gender",verbose=FALSE))
  # expect_warning(p.no.hold <- pnbd(clv.data.cov.no.holdout,
  #                                  use.cor = TRUE, names.cov.constr = "Gender",verbose=FALSE))
  # fct.helper.fitted.all.s3(p.hold,    full.names = c("r", "alpha", "s","beta", "constr.Gender"))
  # fct.helper.fitted.all.s3(p.no.hold, full.names = c("r", "alpha", "s","beta", "constr.Gender"))

  # **TODO:Correlation
  # expect_warning(p.hold    <- pnbd(clv.data.cov.holdout,
  #                                  use.cor = TRUE, reg.lambdas = c(trans=10, life=10),verbose=FALSE))
  # expect_warning(p.no.hold <- pnbd(clv.data.cov.no.holdout,
  #                                  use.cor = TRUE, reg.lambdas = c(trans=10, life=10),verbose=FALSE))
  # fct.helper.fitted.all.s3(p.hold,   full.names = c("r", "alpha", "s","beta", "life.Gender", "trans.Gender"))
  # fct.helper.fitted.all.s3(p.no.hold, full.names = c("r", "alpha", "s","beta", "life.Gender", "trans.Gender"))

  expect_silent(p.hold    <- pnbd(clv.data.cov.holdout,
                               names.cov.constr = "Gender",reg.lambdas = c(trans=10, life=10),verbose=FALSE))
  expect_silent(p.no.hold <- pnbd(clv.data.cov.no.holdout,
                               names.cov.constr = "Gender",reg.lambdas = c(trans=10, life=10),verbose=FALSE))
  fct.helper.fitted.all.s3(p.hold,    full.names = c("r", "alpha", "s","beta", "constr.Gender"),
                           clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold)
  fct.helper.fitted.all.s3(p.no.hold, full.names = c("r", "alpha", "s","beta", "constr.Gender"),
                           clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold)

  # **TODO:Correlation
  # expect_warning(p.hold    <- pnbd(clv.data.cov.holdout,
  #                                  use.cor = TRUE, names.cov.constr = "Gender",reg.lambdas = c(trans=10, life=10),verbose=FALSE))
  # expect_warning(p.no.hold <- pnbd(clv.data.cov.no.holdout,
  #                                  use.cor = TRUE, names.cov.constr = "Gender",reg.lambdas = c(trans=10, life=10),verbose=FALSE))
  # fct.helper.fitted.all.s3(p.hold,    full.names = c("r", "alpha", "s","beta", "constr.Gender"))
  # fct.helper.fitted.all.s3(p.no.hold, full.names = c("r", "alpha", "s","beta", "constr.Gender"))
})


