fct.testthat.runability.clvfittedspending <- function(name.model, method,
                                                      data.cdnow, data.apparelTrans, data.apparelStaticCov,
                                                      start.params.model,
                                                      failed.optimization.methods.expected.message){

  context(paste0("Runability - ",name.model," - Basic runability"))

  # Data objects: normal data
  expect_silent(clv.data.cdnow.noholdout   <- clvdata( data.cdnow, date.format = "ymd", time.unit = "W"))
  expect_silent(clv.data.cdnow.withholdout <- clvdata(data.cdnow, date.format = "ymd", time.unit = "W",
                                                      estimation.split = 37))

  clv.newdata.nohold   <- fct.helper.create.fake.newdata.nocov(data = data.cdnow, estimation.split = NULL)
  clv.newdata.withhold <- fct.helper.create.fake.newdata.nocov(data = data.cdnow, estimation.split = 37)

  param.names <- names(start.params.model)
  l.args.test.all.s3 <- list(full.names = param.names,
                             clv.newdata.nohold = clv.newdata.nohold, clv.newdata.withhold = clv.newdata.withhold)

  # clv.fitted tests ------------------------------------------------------------------------------------------------
  fct.testthat.runability.clvfitted.out.of.the.box.no.hold(method = method, clv.data.noholdout = clv.data.cdnow.noholdout,
                                                           fct.test.all.s3 = fct.helper.clvfittedspending.all.s3, l.args.test.all.s3 = l.args.test.all.s3)
  fct.testthat.runability.clvfitted.out.of.the.box.with.hold(method = method, clv.data.withholdout = clv.data.cdnow.withholdout,
                                                             fct.test.all.s3 = fct.helper.clvfittedspending.all.s3, l.args.test.all.s3 = l.args.test.all.s3)

  fct.testthat.runability.clvfitted.custom.model.start.params(method = method, clv.data = clv.data.cdnow.noholdout,   start.params.model = start.params.model)
  fct.testthat.runability.clvfitted.custom.model.start.params(method = method, clv.data = clv.data.cdnow.withholdout, start.params.model = start.params.model)

  fct.testthat.runability.clvfitted.custom.optimx.args(method = method, clv.data = clv.data.cdnow.noholdout)


  # fct.testthat.runability.clvfitted.all.optimization.methods(method = method, clv.data = clv.data.cdnow.withholdout,
  #                                                            expected.message=failed.optimization.methods.expected.message)

  fct.testthat.runability.clvfitted.multiple.optimization.methods(method = method, clv.data = clv.data.cdnow.withholdout,
                                                                  l.args.test.all.s3= l.args.test.all.s3, fct.test.all.s3=fct.helper.clvfittedspending.all.s3)

  fct.testthat.runability.clvfitted.hourly.data(method = method, data.cdnow = data.cdnow,
                                                fct.test.all.s3 = fct.helper.clvfittedspending.all.s3, l.args.test.all.s3 = l.args.test.all.s3)


  # Also works with covariate data ------------------------------------------------------------------------------------
  # only check basic workings
  clv.data.cov.no.holdout <- fct.helper.create.clvdata.apparel.staticcov(data.apparelTrans = data.apparelTrans, data.apparelStaticCov = data.apparelStaticCov,
                                                                         estimation.split = NULL)
  clv.data.cov.holdout   <- fct.helper.create.clvdata.apparel.staticcov(data.apparelTrans = data.apparelTrans, data.apparelStaticCov = data.apparelStaticCov,
                                                                        estimation.split = 40)
  clv.newdata.cov.nohold   <- fct.helper.create.fake.newdata.staticcov(data.trans = data.apparelTrans, names.cov = c("Gender", "Channel"),
                                                                       estimation.split = NULL)
  clv.newdata.cov.withhold <- fct.helper.create.fake.newdata.staticcov(data.trans = data.apparelTrans, names.cov = c("Gender", "Channel"),
                                                                       estimation.split = 40)

  l.args.test.all.s3.cov <- list(full.names = param.names, clv.newdata.nohold = clv.newdata.cov.nohold,
                                 clv.newdata.withhold = clv.newdata.cov.withhold)

  fct.testthat.runability.clvfitted.out.of.the.box.no.hold(method = method, clv.data.noholdout = clv.data.cov.no.holdout,
                                                           fct.test.all.s3 = fct.helper.clvfittedspending.all.s3,
                                                           l.args.test.all.s3 = l.args.test.all.s3.cov)
  fct.testthat.runability.clvfitted.out.of.the.box.with.hold(method = method, clv.data.withholdout = clv.data.cov.holdout,
                                                             fct.test.all.s3 = fct.helper.clvfittedspending.all.s3,
                                                             l.args.test.all.s3 = l.args.test.all.s3.cov)

  # And dyncov data as well (has holdout, but can use eith way)
  fitted.dyncov    <- fct.helper.dyncov.load.fitted()
  clv.data.dyn.cov <- fitted.dyncov@clv.data

  l.args.test.all.s3.dyncov <- list(full.names = param.names, clv.newdata.nohold = clv.data.dyn.cov,
                                    clv.newdata.withhold = clv.data.dyn.cov)
  fct.testthat.runability.clvfitted.out.of.the.box.with.hold(method = method, clv.data.withholdout = clv.data.dyn.cov,
                                                             fct.test.all.s3 = fct.helper.clvfittedspending.all.s3,
                                                             l.args.test.all.s3 = l.args.test.all.s3.dyncov)




}
