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

test_that("Sampling all customers (nocov, static, dyn) results in same clv.data object", {
  # nocov
  expect_silent(clv.sampled.nocov <- clv.data.create.bootstrapping.data(clv.cdnow, ids=cdnow[, unique(Id)]))
  expect_silent(clv.sampled.static <- clv.data.create.bootstrapping.data(clv.apparel.cov, ids=apparelTrans[, unique(Id)]))
  expect_silent(clv.sampled.dyn <- clv.data.create.bootstrapping.data(clv.apparel.dyn, ids=apparelTrans[, unique(Id)]))

  clv.sampled.nocov@call <- clv.cdnow@call
  clv.sampled.static@call <- clv.apparel.cov@call
  clv.sampled.dyn@call <- clv.apparel.dyn@call

  expect_equal(clv.sampled.nocov, clv.cdnow)
  expect_equal(clv.sampled.static, clv.apparel.cov)
  expect_equal(clv.sampled.dyn, clv.apparel.dyn)
})

test_that("Bootstrapping clv.time keep same information, incl holdout period", {
  skip_on_cran()

  # zero-repeater on 1997-01-06
  expect_silent(clv.sampled <- clv.data.create.bootstrapping.data(clv.cdnow, ids=c("102")))
  expect_true(clv.sampled@clv.time@timepoint.estimation.start == "1997-01-01")
  expect_equal(clv.sampled@clv.time, clv.cdnow@clv.time)
})


test_that("Sampling keeps holdout (repeat) transactions", {
  skip_on_cran()

  # customers which make holdout transactions
  expect_silent(clv.sampled <- clv.data.create.bootstrapping.data(clv.cdnow, ids=c("1000", "990")))
  expect_true(clv.sampled@data.transactions[Date >= clv.cdnow@clv.time@timepoint.estimation.end, .N] == 8)
})

test_that("Passing non-existent Ids does not create them in the transcation data", {
  skip_on_cran()

  expect_warning(
    clv.sampled <- clv.data.create.bootstrapping.data(clv.cdnow, ids=c("1", "2", "abc")),
    regexp = "Not all given")

  # abc may not appear in ids
  expect_setequal(clv.sampled@data.transactions$Id, c("1", "2"))
  expect_false(anyNA(clv.sampled@data.transactions))
})

test_that("Sampling yields same cbs value again for sampled customers, incl for duplicate ids", {
  # bootstrapping data has to be created, such that cbs values remain the same, also if
  # customers' transactions would result in different estimation.end (such as for customers 1 & 2)
  skip_on_cran()


  dt.cbs.orig <- pnbd_cbs(clv.cdnow)

  # without replacement
  expect_equal(dt.cbs.orig[Id %in% c("1", "2")],
               pnbd_cbs(clv.data.create.bootstrapping.data(clv.data=clv.cdnow, ids=c("1", "2"))))

  # with replacement
  dt.cbs.orig.replaced <- dt.cbs.orig[c("1", "2", "2", "2")]
  dt.cbs.orig.replaced[c(3, 4), Id := c("2_BOOTSTRAP_ID_2", "2_BOOTSTRAP_ID_3")]
  # use == because expect_equal also checks property sorted
  expect_true(all(
    dt.cbs.orig.replaced ==
      pnbd_cbs(clv.data.create.bootstrapping.data(clv.data=clv.cdnow, ids=c("1", "2", "2", "2")))))


  # sampling all ids
  expect_equal(dt.cbs.orig,
               pnbd_cbs(clv.data.create.bootstrapping.data(clv.data=clv.cdnow, ids=dt.cbs.orig$Id)))
})

test_that("Sampling with replacement creates duplicate entries with new ids", {
  skip_on_cran()

  expect_silent(clv.sampled <- clv.data.create.bootstrapping.data(clv.cdnow, ids=c("1", "1", "2", "2")))

  expect_setequal(clv.sampled@data.transactions$Id, c("1", "1_BOOTSTRAP_ID_2", "2", "2_BOOTSTRAP_ID_2"))
  expect_equal(clv.sampled@data.transactions[, .N, keyby="Id"],
               data.table(Id=c("1", "1_BOOTSTRAP_ID_2", "2", "2_BOOTSTRAP_ID_2"), N=c(4, 4, 2, 2), key = "Id"))
})



test_that("Sampling with and without replacement selects static covariates of the same ids", {
  skip_on_cran()

  # This test is essential because this is never verified in the data when it the object is created

  # without replacement
  sampled.ids <- c("1", "10")
  expect_silent(clv.sampled <- clv.data.create.bootstrapping.data(clv.apparel.cov, ids=sampled.ids))
  # same ids in cov data
  expect_setequal(clv.sampled@data.cov.life$Id, sampled.ids)
  expect_setequal(clv.sampled@data.cov.trans$Id, sampled.ids)
  expect_setequal(clv.sampled@data.transactions$Id, clv.sampled@data.cov.life$Id)
  expect_true(all(clv.sampled@data.cov.trans == clv.apparel.cov@data.cov.trans[Id %in% sampled.ids,]))
  expect_true(all(clv.sampled@data.cov.life == clv.apparel.cov@data.cov.life[Id %in% sampled.ids,]))

  # with replacement
  expect_silent(clv.sampled <- clv.data.create.bootstrapping.data(clv.apparel.cov, ids=c("1", "1", "10", "10")))
  expect_setequal(clv.sampled@data.cov.life$Id, c("1", "1_BOOTSTRAP_ID_2", "10", "10_BOOTSTRAP_ID_2"))
  expect_setequal(clv.sampled@data.cov.trans$Id, c("1", "1_BOOTSTRAP_ID_2", "10", "10_BOOTSTRAP_ID_2"))
  expect_setequal(clv.sampled@data.transactions$Id, clv.sampled@data.cov.life$Id)
  expect_setequal(colnames(clv.sampled@data.cov.trans), c("Id", "Gender", "Channel"))
  expect_setequal(colnames(clv.sampled@data.cov.life), c("Id", "Gender"))
})


test_that("Sampling selects correct dynamic covariates", {
  skip_on_cran()

  # This test is essential because this is never verified in the data when the object is created

  dyn.cols <- c("Id", "Cov.Date", "tp.cov.lower", "tp.cov.upper")

  # without replacement
  sampled.ids <- c("1", "10")
  expect_silent(clv.sampled <- clv.data.create.bootstrapping.data(clv.apparel.dyn, ids=sampled.ids))
  # same ids
  expect_setequal(clv.sampled@data.cov.life$Id, sampled.ids)
  expect_setequal(clv.sampled@data.cov.trans$Id, sampled.ids)

  # content of covariate data exactly tge same as original
  expect_true(all(clv.sampled@data.cov.trans[, c("Gender", "Channel")] == clv.apparel.dyn@data.cov.trans[Id %in% sampled.ids, c("Gender", "Channel")]))
  expect_true(all(clv.sampled@data.cov.trans[, c("Gender")] == clv.apparel.dyn@data.cov.trans[Id %in% sampled.ids, c("Gender")]))

  # transaction data
  expect_setequal(clv.sampled@data.transactions$Id, sampled.ids)

  # with replacement
  expect_silent(clv.sampled <- clv.data.create.bootstrapping.data(clv.apparel.dyn, ids=c("1", "1", "10", "10")))# ids in transaction data
  # ids in cov data
  expect_setequal(clv.sampled@data.cov.life$Id, c("1", "1_BOOTSTRAP_ID_2", "10", "10_BOOTSTRAP_ID_2"))
  expect_setequal(clv.sampled@data.cov.trans$Id, c("1", "1_BOOTSTRAP_ID_2", "10", "10_BOOTSTRAP_ID_2"))
  # ids in transaction same as in cov
  expect_setequal(clv.sampled@data.cov.life$Id, clv.sampled@data.transactions$Id)
  expect_setequal(clv.sampled@data.cov.trans$Id, clv.sampled@data.transactions$Id)
  # columns in cov data correct
  expect_setequal(colnames(clv.sampled@data.cov.trans), c(dyn.cols, "Gender", "Channel"))
  expect_setequal(colnames(clv.sampled@data.cov.life), c(dyn.cols, "Gender"))
  # leaves full length of covariates. Same dates for each id, incl duplicated ids
  full.length.dates <- clv.apparel.dyn@data.cov.life[, unique(Cov.Date)]
  expect_true(all(clv.sampled@data.cov.life[, list(same=setequal(Cov.Date, full.length.dates)), by="Id"][, all(same)]))
  expect_true(all(clv.sampled@data.cov.trans[, list(same=setequal(Cov.Date, full.length.dates)), by="Id"][, all(same)]))
})

