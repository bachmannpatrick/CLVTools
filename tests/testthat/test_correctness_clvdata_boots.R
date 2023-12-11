skip_on_cran()
data("cdnow")
data("apparelTrans")
data("apparelStaticCov")
data("apparelDynCov")



# create with estimation.split
clv.cdnow <- fct.helper.create.clvdata.cdnow(cdnow, estimation.split=37)

# create with different covs for both processes
clv.apparel.cov <- fct.helper.create.clvdata.apparel.staticcov(apparelTrans, apparelStaticCov, estimation.split = 40,
                                                               names.cov.life = c("Gender"), names.cov.trans = c("Gender", "Channel"))
clv.apparel.dyn <- fct.helper.create.clvdata.apparel.dyncov(apparelTrans, apparelDynCov, estimation.split = 40,
                                                            names.cov.life = c("Gender"), names.cov.trans = c("Gender", "Channel"))

test_that("Bootstrapping clv.time keep same information but no holdout period", {
  skip_on_cran()

  # zero-repeater on 1997-01-06
  expect_silent(clv.sampled <- clv.data.create.bootstrapping.data(clv.cdnow, ids=c("102")))
  expect_true(clv.sampled@clv.time@timepoint.estimation.start == "1997-01-01")
  expect_equal(clv.sampled@clv.time@timepoint.estimation.start, clv.cdnow@clv.time@timepoint.estimation.start)
  expect_equal(clv.sampled@clv.time@timepoint.estimation.end, clv.cdnow@clv.time@timepoint.estimation.end)
  expect_equal(clv.sampled@clv.time@estimation.period.in.tu, clv.cdnow@clv.time@estimation.period.in.tu)
  expect_equal(clv.sampled@clv.time@time.format, clv.cdnow@clv.time@time.format)

  # but no holdout period
  expect_true(clv.sampled@clv.time@holdout.period.in.tu == 0)
  expect_true(clv.sampled@clv.time@timepoint.holdout.start == clv.sampled@clv.time@timepoint.estimation.end)
  expect_true(clv.sampled@clv.time@timepoint.holdout.end == clv.sampled@clv.time@timepoint.estimation.end)
})


test_that("Sampling has no holdout (repeat) transactions", {
  skip_on_cran()

  # customers which make holdout transactions
  expect_silent(clv.sampled <- clv.data.create.bootstrapping.data(clv.cdnow, ids=c("1000", "990")))

  expect_true(clv.sampled@data.transactions[, max(Date)] <= clv.cdnow@clv.time@timepoint.estimation.end)
  expect_true(clv.sampled@data.repeat.trans[, max(Date)] <= clv.cdnow@clv.time@timepoint.estimation.end)
})

test_that("Passing non-existent Ids does not create them in the transcation data", {
  skip_on_cran()

  expect_warning(clv.sampled <- clv.data.create.bootstrapping.data(clv.cdnow, ids=c("1", "2", "abc")))
  # abc may not appear in ids
  expect_setequal(clv.sampled@data.transactions$Id, c("1", "2"))
  expect_false(anyNA(clv.sampled@data.transactions))
})

test_that("Sampling yields same cbs value again for sampled customers", {
  skip_on_cran()

  # bootstrapping data has to be created, such that cbs values remain the same
  dt.cbs.orig <- pnbd_cbs(clv.cdnow)

  expect_equal(dt.cbs.orig[Id %in% c("1", "2")],
               pnbd_cbs(clv.data.create.bootstrapping.data(clv.data=clv.cdnow, ids=c("1", "2"))))

  # sampling all ids
  expect_equal(dt.cbs.orig,
               pnbd_cbs(clv.data.create.bootstrapping.data(clv.data=clv.cdnow, ids=dt.cbs.orig$Id)))
})

test_that("Sampling with replacement creates duplicate entries with new ids", {
  skip_on_cran()

  expect_silent(clv.sampled <- clv.data.create.bootstrapping.data(clv.cdnow, ids=c("1", "1", "2", "2")))

  expect_setequal(clv.sampled@data.transactions$Id, c("1", "1_BOOTSTRAP_ID_2", "2", "2_BOOTSTRAP_ID_2"))
  # Id=3 only has 3 transactions because 1 is in holdout which is cut off
  expect_equal(clv.sampled@data.transactions[, .N, keyby="Id"],
               data.table(Id=c("1", "1_BOOTSTRAP_ID_2", "2", "2_BOOTSTRAP_ID_2"), N=c(3, 3, 2, 2), key = "Id"))
})




test_that("Sampling with and without replacement selects static covariates of the same ids", {
  skip_on_cran()

  # This test is essential because this is never verified in the data when it the object is created

  # without replacement
  expect_silent(clv.sampled <- clv.data.create.bootstrapping.data(clv.apparel.cov, ids=c("1", "10")))
  expect_setequal(clv.sampled@data.cov.life$Id, c("1", "10"))
  expect_setequal(clv.sampled@data.cov.trans$Id, c("1", "10"))
  expect_setequal(clv.sampled@data.cov.life$Id, clv.sampled@data.transactions$Id)
  expect_setequal(clv.sampled@data.cov.trans$Id, clv.sampled@data.transactions$Id)
  expect_setequal(colnames(clv.sampled@data.cov.trans), c("Id", "Gender", "Channel"))
  expect_setequal(colnames(clv.sampled@data.cov.life), c("Id", "Gender"))


  # with replacement
  expect_silent(clv.sampled <- clv.data.create.bootstrapping.data(clv.apparel.cov, ids=c("1", "1", "10", "10")))
  expect_setequal(clv.sampled@data.cov.life$Id, c("1", "1_BOOTSTRAP_ID_2", "10", "10_BOOTSTRAP_ID_2"))
  expect_setequal(clv.sampled@data.cov.trans$Id, c("1", "1_BOOTSTRAP_ID_2", "10", "10_BOOTSTRAP_ID_2"))
  expect_setequal(clv.sampled@data.cov.life$Id, clv.sampled@data.transactions$Id)
  expect_setequal(clv.sampled@data.cov.trans$Id, clv.sampled@data.transactions$Id)
  expect_setequal(colnames(clv.sampled@data.cov.trans), c("Id", "Gender", "Channel"))
  expect_setequal(colnames(clv.sampled@data.cov.life), c("Id", "Gender"))
})

test_that("Sampling selects dynamic covariates of the same ids and only in estimation period", {
  skip_on_cran()

  # This test is essential because this is never verified in the data when it the object is created

  dyn.cols <- c("Id", "Cov.Date", "tp.cov.lower", "tp.cov.upper")

  # without replacement
  expect_silent(clv.sampled <- clv.data.create.bootstrapping.data(clv.apparel.dyn, ids=c("1", "10")))
  expect_setequal(clv.sampled@data.cov.life$Id, c("1", "10"))
  expect_setequal(clv.sampled@data.cov.trans$Id, c("1", "10"))
  expect_setequal(clv.sampled@data.cov.life$Id, clv.sampled@data.transactions$Id)
  expect_setequal(clv.sampled@data.cov.trans$Id, clv.sampled@data.transactions$Id)
  expect_setequal(colnames(clv.sampled@data.cov.trans), c(dyn.cols, "Gender", "Channel"))
  expect_setequal(colnames(clv.sampled@data.cov.life), c(dyn.cols, "Gender"))


  # with replacement
  expect_silent(clv.sampled <- clv.data.create.bootstrapping.data(clv.apparel.dyn, ids=c("1", "1", "10", "10")))
  expect_setequal(clv.sampled@data.cov.life$Id, c("1", "1_BOOTSTRAP_ID_2", "10", "10_BOOTSTRAP_ID_2"))
  expect_setequal(clv.sampled@data.cov.trans$Id, c("1", "1_BOOTSTRAP_ID_2", "10", "10_BOOTSTRAP_ID_2"))
  expect_setequal(clv.sampled@data.cov.life$Id, clv.sampled@data.transactions$Id)
  expect_setequal(clv.sampled@data.cov.trans$Id, clv.sampled@data.transactions$Id)
  expect_setequal(colnames(clv.sampled@data.cov.trans), c(dyn.cols, "Gender", "Channel"))
  expect_setequal(colnames(clv.sampled@data.cov.life), c(dyn.cols, "Gender"))
})


test_that("Sampling selects dynamic covariates leaves full length of covariates", {
  skip_on_cran()

  # This test is essential because this is never verified in the data when it the object is created

  full.length.dates <- clv.apparel.dyn@data.cov.life[, unique(Cov.Date)]

  # without replacement
  expect_silent(clv.sampled <- clv.data.create.bootstrapping.data(clv.apparel.dyn, ids=c("1", "10")))

  expect_true(all(clv.sampled@data.cov.life[, list(same=setequal(Cov.Date, full.length.dates)), by="Id"][, all(same)]))
  expect_true(all(clv.sampled@data.cov.trans[, list(same=setequal(Cov.Date, full.length.dates)), by="Id"][, all(same)]))


  # with replacement
  expect_silent(clv.sampled <- clv.data.create.bootstrapping.data(clv.apparel.dyn, ids=c("1", "1", "10", "10")))
  expect_true(all(clv.sampled@data.cov.life[, list(same=setequal(Cov.Date, full.length.dates)), by="Id"][, all(same)]))
  expect_true(all(clv.sampled@data.cov.trans[, list(same=setequal(Cov.Date, full.length.dates)), by="Id"][, all(same)]))
})

