data("cdnow")

# summary ---------------------------------------------------------------------------------------------------------
context("Correctness - clvdata - summary")
# **** TODO: Zero-repeaters counted correctly *****
# test_that("Zero repeaters are counted correctly", {
#   skip_on_cran()
#
#   fct.verify.zero.repeaters <- function(date.estimation.split){
#     expect_silent(clv.cdnow <- fct.helper.create.clvdata.cdnow(cdnow, estimation.split = date.estimation.split))
#     expect_silent(res.sum <- summary(clv.cdnow))
#
#     # No zero-repeaters in estimation and holdout
#     expect_true(res.sum$descriptives.transactions[Name == "Total # zero repeaters", "Estimation"] == "-")
#     expect_true(res.sum$descriptives.transactions[Name == "Total # zero repeaters", "Holdout"] == "-")
#
#     if(!is.null(date.estimation.split)){
#       num.zero.rep <- cdnow[Date <= date.estimation.split, .N, by = "Id"][N == 1, .N]
#       perc.zero.rep <- round(num.zero.rep / cdnow[Date <= date.estimation.split, uniqueN(Id)], 2)
#       expect_true(num.zero.rep == res.sum$descriptives.transactions[Name == "Total # zero repeaters", as.numeric(Total)])
#       expect_true(perc.zero.rep == round(res.sum$descriptives.transactions[Name == "Percentage # zero repeaters", as.numeric(Total)], 2))
#       print(res.sum$descriptives.transactions)
#       print(num.zero.rep)
#       print(perc.zero.rep)
#     }else{
#       num.zero.rep <- cdnow[, .N, by = "Id"][N == 1, .N]
#       perc.zero.rep <- round(num.zero.rep / cdnow[, uniqueN(Id)], 2)
#       expect_true(num.zero.rep == res.sum$descriptives.transactions[Name == "Total # zero repeaters", as.numeric(Total)])
#       expect_true(perc.zero.rep == round(res.sum$descriptives.transactions[Name == "Percentage # zero repeaters", as.numeric(Total)], 2))
#     }
#   }
#
#   # Overall
#   fct.verify.zero.repeaters(date.estimation.split = NULL)
#   # In estimation period
#   fct.verify.zero.repeaters(date.estimation.split = lubridate::ymd("1997-09-17"))
# })

test_that("Same transaction summary if all ids or NULL are given", {
  skip_on_cran()
  clv.cdnow <- fct.helper.create.clvdata.cdnow(cdnow)
  expect_silent(res.sum.null <- summary(clv.cdnow, Id=NULL))
  expect_silent(res.sum.all <- summary(clv.cdnow, Id=cdnow[, unique(Id)]))
  expect_true(isTRUE(all.equal(res.sum.null$descriptives.transactions,
                               res.sum.all$descriptives.transactions)))
})

test_that("Correct Ids selected", {
  skip_on_cran()
  clv.cdnow <- fct.helper.create.clvdata.cdnow(cdnow)
  expect_silent(res.sum <- summary(clv.cdnow, Id=c("1", "2", "3")))
  expect_true(setequal(res.sum$selected.ids, c("1", "2", "3")))
  expect_true(length(unique(res.sum$selected.ids)) == length(res.sum$selected.ids))
  # double
  expect_silent(res.sum <- summary(clv.cdnow, Id=c("1", "2", "3", "3", "3")))
  expect_true(setequal(res.sum$selected.ids, c("1", "2", "3")))
  expect_true(length(unique(res.sum$selected.ids)) == length(res.sum$selected.ids))
})

test_that("Different output if ids given", {
  skip_on_cran()
  clv.cdnow <- fct.helper.create.clvdata.cdnow(cdnow)
  expect_silent(df.desc.1 <- summary(clv.cdnow, Id="1")$descriptives.transactions)
  expect_silent(df.desc.123 <- summary(clv.cdnow, Id=c("1", "2", "3"))$descriptives.transactions)
  expect_false(isTRUE(all.equal(desc.1[, "Estimation"], desc.1[, "Estimation"])))
  expect_false(isTRUE(all.equal(desc.1[, "Holdout"], desc.1[, "Holdout"])))
  expect_false(isTRUE(all.equal(desc.1[, "Total"], desc.1[, "Total"])))
})

test_that("Holdout is - if customer has no transactions in holdout period", {
  skip_on_cran()
  clv.cdnow <- fct.helper.create.clvdata.cdnow(cdnow)
  expect_true(all(summary(clv.cdnow, Id="2")$descriptives.transactions[, "Holdout"] == "-"))
})


# as.data.x ---------------------------------------------------------------------------------------------------------
context("Correctness - clvdata - as.data.x")
test_that("Correct data format is returned", {
  skip_on_cran()
  clv.cdnow <- fct.helper.create.clvdata.cdnow(data.cdnow=cdnow)

  expect_false(is.data.table(as.data.frame(clv.cdnow)))
  expect_true(is.data.frame(as.data.frame(clv.cdnow)))

  expect_true(is.data.table(as.data.table(clv.cdnow)))
  # expect_true(is.data.frame(as.data.table(clv.cdnow)))
})


test_that("Correct Ids are returned", {
  skip_on_cran()
  clv.cdnow <- fct.helper.create.clvdata.cdnow(data.cdnow=cdnow)
  target.ids <- c("1", "2", "999")

  expect_setequal(as.data.frame(clv.cdnow, Ids = target.ids)$Id, target.ids)
  expect_setequal(as.data.table(clv.cdnow, Ids = target.ids)$Id, target.ids)

  expect_warning(as.data.frame(clv.cdnow, Ids=c(target.ids, "abc")))
  expect_warning(as.data.table(clv.cdnow, Ids=c(target.ids, "abc")))
})

test_that("Returns correct number of transactinons for given sample", {
  skip_on_cran()

  clv.cdnow <- fct.helper.create.clvdata.cdnow(data.cdnow=cdnow)

  fct.verify.correct.number.trans <- function(fct.as.data.x){
    expect_true(nrow(fct.as.data.x(clv.cdnow)) == nrow(cdnow))

    expect_true(nrow(fct.as.data.x(clv.cdnow)) == nrow(fct.as.data.x(clv.cdnow, sample="full")))

    expect_true(nrow(fct.as.data.x(clv.cdnow, sample="estimation")) +
                  nrow(fct.as.data.x(clv.cdnow, sample="holdout")) ==
                  nrow(fct.as.data.x(clv.cdnow, sample="full")))
  }

  fct.verify.correct.number.trans(fct.as.data.x = as.data.frame)
  fct.verify.correct.number.trans(fct.as.data.x = as.data.table)
})

test_that("Always returns a copy", {
  skip_on_cran()
  clv.cdnow <- fct.helper.create.clvdata.cdnow(data.cdnow=cdnow)
  orig.address <- address(clv.cdnow@data.transactions)

  # data.frame
  expect_false(orig.address == address(as.data.frame(clv.cdnow, sample="full")))
  expect_false(orig.address == address(as.data.frame(clv.cdnow, sample="estimation")))
  expect_false(orig.address == address(as.data.frame(clv.cdnow, sample="holdout")))

  expect_false(orig.address == address(as.data.frame(clv.cdnow, sample="full", Ids = "1")))
  expect_false(orig.address == address(as.data.frame(clv.cdnow, sample="estimation", Ids = "1")))
  expect_false(orig.address == address(as.data.frame(clv.cdnow, sample="holdout", Ids = "1")))

  # data.table
  expect_false(orig.address == address(as.data.table(clv.cdnow, sample="full")))
  expect_false(orig.address == address(as.data.table(clv.cdnow, sample="estimation")))
  expect_false(orig.address == address(as.data.table(clv.cdnow, sample="holdout")))

  expect_false(orig.address == address(as.data.table(clv.cdnow, sample="full", Ids = "1")))
  expect_false(orig.address == address(as.data.table(clv.cdnow, sample="estimation", Ids = "1")))
  expect_false(orig.address == address(as.data.table(clv.cdnow, sample="holdout", Ids = "1")))
})




# subset ---------------------------------------------------------------------
context("Correctness - clvdata - subset")

test_that("Correct data selected", {
  skip_on_cran()
  # with holdout
  clv.cdnow <- fct.helper.create.clvdata.cdnow(cdnow)

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
                               cdnow[Date>="1997-02-02" & Date <= "1997-10-10", !"CDs"][order(Id)])))

  # Price
  expect_true(isTRUE(all.equal(subset(clv.cdnow, between(Price, 50, 100))[order(Id)],
                               cdnow[Price >= 50 & Price <= 100, !"CDs"][order(Id)])))

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

test_that("If no holdout, full and estimation are the same", {
  skip_on_cran()
  clv.cdnow <- fct.helper.create.clvdata.cdnow(cdnow, estimation.split=NULL)
  expect_true(isTRUE(all.equal(subset(clv.cdnow, sample="full"),
                               subset(clv.cdnow, sample="estimation"))))

  # holdout fails
  expect_error(subset(clv.cdnow, sample="holdout"), regexp = "no holdout data")
})


test_that("Same when argument positions are swapped", {
  skip_on_cran()
  clv.cdnow <- fct.helper.create.clvdata.cdnow(cdnow)

  expect_true(isTRUE(all.equal(subset(clv.cdnow, Id=="111", c("Id", "Date")),
                               subset(clv.cdnow, select=c("Id", "Date"), Id=="111"))))
})


test_that("Always returns a copy of the data", {
  skip_on_cran()
  clv.cdnow <- fct.helper.create.clvdata.cdnow(cdnow)
  orig.address <- address(clv.cdnow@data.transactions)

  # only both, holdout and estimation always return different object because different data
  expect_false(address(subset(clv.cdnow, subset=TRUE, sample="full")) == orig.address)
  expect_false(address(subset(clv.cdnow, subset=Id=="1", sample="full")) == orig.address)
  expect_false(address(subset(clv.cdnow, select=Id=="1", sample="full")) == orig.address)
})




