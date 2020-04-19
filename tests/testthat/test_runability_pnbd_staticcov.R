# Setup -----------------------------------------------------------------------------------------------------------------------------------------
data("apparelTrans")
data("apparelStaticCov")

context("Runability - PNBD static cov - Basic runability")

expect_silent(clv.data.apparel        <- clvdata(data.transactions = apparelTrans, date.format = "ymd", time.unit = "W",
                                                 estimation.split = 40))
expect_silent(clv.data.apparel.no.holdout <- clvdata(data.transactions = apparelTrans, date.format = "ymd", time.unit = "W"))

expect_silent(clv.data.cov.holdout    <- SetStaticCovariates(clv.data.apparel, data.cov.life = apparelStaticCov, data.cov.trans = apparelStaticCov,
                                                             names.cov.life = c("Gender", "Channel"), names.cov.trans = c("Gender", "Channel")))
expect_silent(clv.data.cov.no.holdout <- SetStaticCovariates(clv.data.apparel.no.holdout, data.cov.life = apparelStaticCov, data.cov.trans = apparelStaticCov,
                                                             names.cov.life = c("Gender", "Channel"), names.cov.trans = c("Gender", "Channel")))

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
  data.table::data.table(cid, Gender = sample(0:1, size = 1), Channel =  sample(0:1, size = 1))
})))

# create newdata objects with covariates
expect_silent(clv.newdata.nohold <- SetStaticCovariates(
  clv.data = clvdata(data.transactions = dt.newdata, date.format = "ydm", time.unit = "w",
                     estimation.split = NULL, name.id = "cust.id", name.date = "trans.date",
                     name.price = NULL),
  data.cov.life = dt.covs, data.cov.trans = dt.covs,
  names.cov.life = c("Gender", "Channel"), names.cov.trans = c("Gender", "Channel"),
  name.id = "cid"))

expect_silent(clv.newdata.withhold <- SetStaticCovariates(
  clv.data = clvdata(data.transactions = dt.newdata, date.format = "ydm", time.unit = "w",
                     estimation.split = 40, name.id = "cust.id", name.date = "trans.date",
                     name.price = NULL),
  data.cov.life = dt.covs, data.cov.trans = dt.covs,
  names.cov.life =  c("Gender", "Channel"), names.cov.trans =  c("Gender", "Channel"),
  name.id = "cid"))




# Basic runability -------------------------------------------------------------------------------------------------------

test_that("Works out-of-the box, without additional params", {
  expect_silent(p.hold    <- pnbd(clv.data.cov.holdout, verbose=FALSE))
  expect_silent(p.no.hold <- pnbd(clv.data.cov.no.holdout, verbose=FALSE))
  fct.helper.fitted.all.s3(p.hold,   full.names = c("r", "alpha", "s","beta", p.hold@names.prefixed.params.free.life, p.hold@names.prefixed.params.free.trans),
                           clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold)
  fct.helper.fitted.all.s3(p.no.hold, full.names = c("r", "alpha", "s","beta", p.hold@names.prefixed.params.free.life, p.hold@names.prefixed.params.free.trans),
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
                          start.params.life = c(Gender = 1, Channel=0.4), start.params.trans = c(Gender=1, Channel=2), verbose = FALSE))
  expect_silent(pnbd(clv.data.cov.no.holdout, start.params.model = c(r=1, alpha = 2, s = 1, beta = 2),
                     start.params.life = c(Channel=0.4, Gender = 1), start.params.trans = c(Channel=2, Gender=1), verbose = FALSE))
})


test_that("Works for all optimx optimization methods", {
  skip_on_cran()
  skip_on_covr()
  expect_warning(pnbd(clv.data=clv.data.cov.holdout, optimx.args = list(control=list(all.methods=TRUE)), verbose=FALSE),
                 regexp = "replaced by maximum positive value|Gradient not computable after method nlm|Rcgmin|unused control arguments ignored|Gradient not computable|Estimation failed with NA coefs|Hessian could not be derived", all=TRUE)
})


test_that("Works fully with multiple optimization methods", {
  skip_on_cran()
  expect_silent(p.hold <- pnbd(clv.data=clv.data.cov.holdout, optimx.args = list(method = c("BFGS", "L-BFGS-B", "Nelder-Mead")), verbose=FALSE))
  fct.helper.fitted.all.s3(clv.fitted = p.hold,  full.names = c("r", "alpha", "s","beta", p.hold@names.prefixed.params.free.life, p.hold@names.prefixed.params.free.trans),
                           clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold)
})





# Reduces to covariates ------------------------------------------------------------------------------------------------------------------------
test_that("Reduces to relevant covariates only for estimation", {
  skip_on_cran()

  # Transaction: Fit with Gender covariate only
  expect_silent(e.pnbd.1.less <-pnbd(clv.data.cov.holdout, names.cov.trans = "Gender",verbose=FALSE)) # only keep Gender
  expect_false("Channel" %in% names(coef(e.pnbd.1.less)))
  expect_true("Channel" %in% colnames(e.pnbd.1.less@clv.data@data.cov.life))
  expect_false("Channel" %in% colnames(e.pnbd.1.less@clv.data@data.cov.trans))

  # Lifetime: Same
  expect_silent(e.pnbd.1.less <-pnbd(clv.data.cov.holdout, names.cov.life = "Gender",verbose=FALSE)) # only keep Gender
  expect_false("Channel" %in% names(coef(e.pnbd.1.less)))
  expect_false("Channel" %in% colnames(e.pnbd.1.less@clv.data@data.cov.life))
  expect_true("Channel" %in% colnames(e.pnbd.1.less@clv.data@data.cov.trans))
})

# Correlation ---------------------------------------------------------------------------------------------
context("Runability - PNBD static cov - w/ Correlation")

test_that("Works with use.cor=T", {
  skip_on_cran()

  expect_silent(pscc <- pnbd(clv.data.cov.holdout, use.cor=TRUE, verbose=FALSE))
  fct.helper.fitted.all.s3(pscc, full.names = c("r", "alpha", "s", "beta", pscc@name.correlation.cor,
                                                pscc@names.prefixed.params.free.life, pscc@names.prefixed.params.free.trans),
                           clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold)
})
#
test_that("Works with use.cor=T and start.params", {
  skip_on_cran()
  skip_on_ci()
  skip_on_covr()

  expect_silent(pscc <- pnbd(clv.data.cov.holdout, use.cor=TRUE, start.param.cor = 0.0, verbose=FALSE))
  fct.helper.fitted.all.s3(pscc, full.names = c("r", "alpha", "s", "beta", pscc@name.correlation.cor,
                                                pscc@names.prefixed.params.free.life, pscc@names.prefixed.params.free.trans),
                           clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold)
})


# Interlayers ----------------------------------------------------------------------------------------------------------------------------------
context("Runability - PNBD static cov - w/ Constraint")


test_that("Works with 2 constraints", {
  skip_on_cran()

  expect_silent(p.hold    <- pnbd(clv.data.cov.holdout,   names.cov.constr = c("Gender", "Channel"),verbose=FALSE))
  expect_silent(p.hold    <- pnbd(clv.data.cov.no.holdout,   names.cov.constr = c("Gender", "Channel"),verbose=FALSE))

  fct.helper.fitted.all.s3(p.hold,    full.names = c("r", "alpha", "s","beta", "constr.Gender", "constr.Channel"),
                           clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold)
  fct.helper.fitted.all.s3(p.hold,    full.names = c("r", "alpha", "s","beta", "constr.Gender", "constr.Channel"),
                           clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold)
})


test_that("Works with 1 constraint, 1 free", {
  skip_on_cran()

  # Without start param
  expect_silent(p.hold    <- pnbd(clv.data.cov.holdout,    names.cov.constr = "Gender",verbose=FALSE))
  expect_silent(p.no.hold <- pnbd(clv.data.cov.no.holdout, names.cov.constr = "Gender",verbose=FALSE))
  fct.helper.fitted.all.s3(p.hold,    full.names = c("r", "alpha", "s","beta", "life.Channel", "trans.Channel", "constr.Gender"),
                           clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold)
  fct.helper.fitted.all.s3(p.no.hold, full.names = c("r", "alpha", "s","beta", "life.Channel", "trans.Channel", "constr.Gender"),
                           clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold)

  # With start param
  expect_silent(pnbd(clv.data.cov.holdout,    names.cov.constr = "Gender", start.params.constr = c(Gender=1),verbose=FALSE))
  expect_silent(pnbd(clv.data.cov.no.holdout, names.cov.constr = "Gender", start.params.constr = c(Gender=1),verbose=FALSE))
})




context("Runability - PNBD static cov - w/ Regularization")
test_that("Works with regularization", {

  expect_silent(p.hold    <- pnbd(clv.data.cov.holdout,    reg.lambdas = c(trans=10, life=10),verbose=FALSE))
  expect_silent(p.no.hold <- pnbd(clv.data.cov.no.holdout, reg.lambdas = c(trans=10, life=10),verbose=FALSE))

  fct.helper.fitted.all.s3(p.hold,    full.names = c("r", "alpha", "s","beta", p.hold@names.prefixed.params.free.life, p.hold@names.prefixed.params.free.trans),
                           clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold)
  fct.helper.fitted.all.s3(p.no.hold, full.names = c("r", "alpha", "s","beta", p.no.hold@names.prefixed.params.free.life, p.no.hold@names.prefixed.params.free.trans),
                           clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold)
})

test_that("Works with 0 regularization lambdas", {
  skip_on_cran()
  skip_on_ci()
  skip_on_covr()

  expect_silent(p.hold    <- pnbd(clv.data.cov.holdout,   reg.lambdas = c(trans=0, life=0),verbose=FALSE))
  expect_silent(p.no.hold <- pnbd(clv.data.cov.no.holdout,reg.lambdas = c(trans=0, life=0),verbose=FALSE))
  fct.helper.fitted.all.s3(p.hold,    full.names = c("r", "alpha", "s","beta", p.hold@names.prefixed.params.free.life, p.hold@names.prefixed.params.free.trans),
                           clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold)
  fct.helper.fitted.all.s3(p.no.hold, full.names = c("r", "alpha", "s","beta", p.no.hold@names.prefixed.params.free.life, p.no.hold@names.prefixed.params.free.trans),
                           clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold)
})


context("Runability - PNBD static cov - w/ combinations")

test_that("Works with combined interlayers", {
  # Try all combinations of interlayers
  skip_on_cran()
  skip_on_covr()

  # Constraints + Correlation
  expect_silent(p.hold    <- pnbd(clv.data.cov.holdout,    use.cor = TRUE, names.cov.constr = c("Gender", "Channel"),verbose=FALSE))
  expect_silent(p.no.hold <- pnbd(clv.data.cov.no.holdout, use.cor = TRUE, names.cov.constr = c("Gender", "Channel"), verbose=FALSE))

  fct.helper.fitted.all.s3(p.hold,    full.names = c("r", "alpha", "s","beta",  p.hold@name.correlation.cor, p.hold@names.prefixed.params.constr),
                           clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold)
  fct.helper.fitted.all.s3(p.no.hold, full.names = c("r", "alpha", "s","beta", p.no.hold@name.correlation.cor, p.no.hold@names.prefixed.params.constr),
                           clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold)

  # Regularization + Correlation
  expect_silent(p.hold    <- pnbd(clv.data.cov.holdout,    use.cor = TRUE, reg.lambdas = c(trans=10, life=10),verbose=FALSE))
  expect_silent(p.no.hold <- pnbd(clv.data.cov.no.holdout, use.cor = TRUE, reg.lambdas = c(trans=10, life=10),verbose=FALSE))


  fct.helper.fitted.all.s3(p.hold,    full.names = c("r", "alpha", "s","beta",  p.hold@name.correlation.cor, p.hold@names.prefixed.params.free.life, p.hold@names.prefixed.params.free.trans),
                           clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold)
  fct.helper.fitted.all.s3(p.no.hold, full.names = c("r", "alpha", "s","beta", p.no.hold@name.correlation.cor, p.no.hold@names.prefixed.params.free.life, p.no.hold@names.prefixed.params.free.trans),
                           clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold)


  # Regularization + Constraints
  expect_silent(p.hold    <- pnbd(clv.data.cov.holdout,    names.cov.constr = c("Gender", "Channel"), reg.lambdas = c(trans=10, life=10),verbose=FALSE))
  expect_silent(p.no.hold <- pnbd(clv.data.cov.no.holdout, names.cov.constr = c("Gender", "Channel"), reg.lambdas = c(trans=10, life=10),verbose=FALSE))

  fct.helper.fitted.all.s3(p.hold,    full.names = c("r", "alpha", "s","beta", p.hold@names.prefixed.params.constr),
                           clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold)
  fct.helper.fitted.all.s3(p.no.hold, full.names = c("r", "alpha", "s","beta", p.no.hold@names.prefixed.params.constr),
                           clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold)


  # Regularization + Correlation + Constraints
  expect_silent(p.hold    <- pnbd(clv.data.cov.holdout,    use.cor = TRUE, names.cov.constr = c("Gender", "Channel"),reg.lambdas = c(trans=10, life=10),verbose=FALSE))
  expect_silent(p.no.hold <- pnbd(clv.data.cov.no.holdout, use.cor = TRUE, names.cov.constr = c("Gender", "Channel"),reg.lambdas = c(trans=10, life=10),verbose=FALSE))

  fct.helper.fitted.all.s3(p.hold,    full.names = c("r", "alpha", "s","beta", p.hold@name.correlation.cor, p.hold@names.prefixed.params.constr),
                           clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold)
  fct.helper.fitted.all.s3(p.no.hold, full.names = c("r", "alpha", "s","beta", p.no.hold@name.correlation.cor, p.no.hold@names.prefixed.params.constr),
                           clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold)
})


