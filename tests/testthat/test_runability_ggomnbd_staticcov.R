# Setup -----------------------------------------------------------------------------------------------------------------------------------------
data("apparelTrans")
data("apparelStaticCov")

ggomnbd.param.names.no.cov = c("r", "alpha", "b", "s", "beta")
ggomnbd.param.names.with.cov = c(ggomnbd.param.names.no.cov, "life.Gender", "life.Channel", "trans.Gender", "trans.Channel")

context("Runability - GGompertz/NBD static cov - Basic runability")

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

fct.testthat.runability.common.out.of.the.box.no.hold(method = ggomnbd,
                                                      clv.data.noholdout = clv.data.cov.no.holdout,
                                                      clv.newdata.withhold = clv.newdata.withhold,
                                                      clv.newdata.nohold = clv.newdata.nohold,
                                                      param.names = ggomnbd.param.names.with.cov,
                                                      DERT.not.implemented = TRUE)

fct.testthat.runability.common.out.of.the.box.with.hold(method = ggomnbd,
                                                        clv.data.withholdout = clv.data.cov.holdout,
                                                        clv.newdata.withhold = clv.newdata.withhold,
                                                        clv.newdata.nohold = clv.newdata.nohold,
                                                        param.names = ggomnbd.param.names.with.cov,
                                                        DERT.not.implemented = TRUE)

fct.testthat.runability.common.custom.model.start.params(method = ggomnbd,
                                                         clv.data.withholdout = clv.data.cov.holdout,
                                                         clv.data.noholdout = clv.data.cov.no.holdout,
                                                         start.params.model = c(r = 1, alpha = 1, b = 1, s = 1, beta = 1))

fct.testthat.runability.staticcov.custom.model.covariate.start.params(method = ggomnbd,
                                                                      clv.data.holdout = clv.data.cov.holdout,
                                                                      clv.data.no.holdout = clv.data.cov.no.holdout,
                                                                      start.params.model = c(r = 1, alpha = 4, b = 1, s = 1, beta = 1))

# Reduces to covariates -----------------------------------------------------------------------------------------------------------

fct.testthat.runability.staticcov.reduce.relevant.covariates.estimation(method = ggomnbd,
                                                                        clv.data.holdout = clv.data.cov.holdout)

# Interlayers ---------------------------------------------------------------------------------------------------------------------

context("Runability - GGompertz/NBD static cov - w/ Constraint")

fct.testthat.runability.staticcov.works.with.2.constraints(method = ggomnbd,
                                                           clv.data.holdout = clv.data.cov.holdout,
                                                           clv.data.no.holdout = clv.data.cov.no.holdout,
                                                           clv.newdata.nohold = clv.newdata.nohold,
                                                           clv.newdata.withhold = clv.newdata.withhold,
                                                           param.names = ggomnbd.param.names.no.cov,
                                                           DERT.not.implemented = TRUE)

fct.testthat.runability.staticcov.works.with.1.constraint.1.free(method = ggomnbd,
                                                                 clv.data.holdout = clv.data.cov.holdout,
                                                                 clv.data.no.holdout = clv.data.cov.no.holdout,
                                                                 clv.newdata.nohold = clv.newdata.nohold,
                                                                 clv.newdata.withhold = clv.newdata.withhold,
                                                                 param.names = ggomnbd.param.names.no.cov,
                                                                 DERT.not.implemented = TRUE)

context("Runability - GGompertz/NBD static cov - w/ Regularization")

fct.testthat.runability.staticcov.works.with.regularization(method = ggomnbd,
                                                            clv.data.holdout = clv.data.cov.holdout,
                                                            clv.data.no.holdout = clv.data.cov.no.holdout,
                                                            clv.newdata.nohold = clv.newdata.nohold,
                                                            clv.newdata.withhold = clv.newdata.withhold,
                                                            param.names = ggomnbd.param.names.no.cov,
                                                            DERT.not.implemented = TRUE)

fct.testthat.runability.staticcov.works.with.0.lambdas(method = ggomnbd,
                                                       clv.data.holdout = clv.data.cov.holdout,
                                                       clv.data.no.holdout = clv.data.cov.no.holdout,
                                                       clv.newdata.nohold = clv.newdata.nohold,
                                                       clv.newdata.withhold = clv.newdata.withhold,
                                                       param.names = ggomnbd.param.names.no.cov,
                                                       DERT.not.implemented = TRUE)

context("Runability - GGompertz/NBD static cov - w/ combinations")

fct.testthat.runability.staticcov.works.with.combined.interlayers.without.cor(method = ggomnbd,
                                                                              clv.data.holdout = clv.data.cov.holdout,
                                                                              clv.data.no.holdout = clv.data.cov.no.holdout,
                                                                              clv.newdata.nohold = clv.newdata.nohold,
                                                                              clv.newdata.withhold = clv.newdata.withhold,
                                                                              param.names = ggomnbd.param.names.no.cov,
                                                                              DERT.not.implemented = TRUE)
