data("cdnow")
data("apparelTrans")

# subset ---------------------------------------------------------------------
context("Correctness - clvdata - subset")

test_that("Correct data selected", {
  skip_on_cran()
  clv.cdnow <- fct.helper.create.clvdata.cdnow()

  # full
  # no arg same as full
  expect_true(isTRUE(all.equal(subset(clv.cdnow),
                               subset(clv.cdnow, sample="full"))))

  # Id
  expect_silent(dt.trans <- subset(clv.cdnow, Id == "1"))
  expect_setequal(dt.trans$Id, "1")
  expect_true(dt.trans[, .N] == 4)
  expect_setequal(colnames(dt.trans), c("Id", "Date", "Price"))
  # multiple Ids
  expect_silent(dt.trans <- subset(clv.cdnow, Id %in% c("1", "2")))
  expect_setequal(dt.trans$Id, c("1", "2"))
  expect_true(dt.trans[, .N] == 6)

  # estimation
  expect_silent(dt.trans <- subset(clv.cdnow, Id == "1", sample="estimation"))
  expect_setequal(dt.trans$Id, "1")
  expect_true(dt.trans[, .N] == 3)
  expect_setequal(colnames(dt.trans), c("Id", "Date", "Price"))
  #   multiple Ids
  expect_silent(dt.trans <- subset(clv.cdnow, Id %in% c("1", "2"), sample="estimation"))
  expect_setequal(dt.trans$Id, c("1", "2"))
  expect_true(dt.trans[, .N] == 5)

  # holdout
  expect_silent(dt.trans <- subset(clv.cdnow, Id == "1", sample="holdout"))
  expect_setequal(dt.trans$Id, "1")
  expect_true(dt.trans[, .N] == 1)
  expect_setequal(colnames(dt.trans), c("Id", "Date", "Price"))
  #   multiple Ids
  expect_silent(dt.trans <- subset(clv.cdnow, Id %in% c("1", "111"), sample="holdout"))
  expect_setequal(dt.trans$Id, c("1", "111"))
  expect_true(dt.trans[, .N] == 2)

  # Date
  expect_true(isTRUE(all.equal(subset(clv.cdnow, between(Date, "1997-02-02", "1997-10-10"))[order(Id)],
                               cdnow[Date>="1997-02-02" & Date <= "1997-10-10"][order(Id)])))

  # Price
  expect_true(isTRUE(all.equal(subset(clv.cdnow, between(Price, 50, 100))[order(Id)],
                               cdnow[Price >= 50 & Price <= 100][order(Id)])))

  # columns
  # full
  expect_setequal(colnames(subset(clv.cdnow, select=c("Id"), sample="full")), "Id")
  expect_setequal(colnames(subset(clv.cdnow, select=c("Id", "Date"), sample="full")), c("Id", "Date"))
  # estimation
  expect_setequal(colnames(subset(clv.cdnow, select=c("Id"), sample="estimation")), "Id")
  expect_setequal(colnames(subset(clv.cdnow, select=c("Id", "Date"), sample="estimation")), c("Id", "Date"))
  # holdout
  expect_setequal(colnames(subset(clv.cdnow, select=c("Id"), sample="holdout")), "Id")
  expect_setequal(colnames(subset(clv.cdnow, select=c("Id", "Date", "Price"), sample="holdout")),
                  c("Id", "Date", "Price"))

})

test_that("Same when argument positions are swapped", {
  skip_on_cran()
  clv.cdnow <- fct.helper.create.clvdata.cdnow()

  expect_true(isTRUE(all.equal(subset(clv.cdnow, Id=="111", c("Id", "Date")),
                               subset(clv.cdnow, select=c("Id", "Date"), Id=="111"))))
})




test_that("Always returns a copy of the data", {
  skip_on_cran()
  clv.cdnow <- fct.helper.create.clvdata.cdnow()
  orig.address <- address(clv.cdnow@data.transactions)

  # only both, holdout and estimation always return different object because different data
  expect_false(address(subset(clv.cdnow, subset=TRUE, sample="full")) == orig.address)
  expect_false(address(subset(clv.cdnow, subset=Id=="1", sample="full")) == orig.address)
  expect_false(address(subset(clv.cdnow, select=Id=="1", sample="full")) == orig.address)
})



