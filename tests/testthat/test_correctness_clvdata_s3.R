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





# plot ---------------------------------------------------------------------
context("Correctness - clvdata - plot")

# . frequency ---------------------------------------------------------------
test_that("frequency plot - actual trans has no 0", {
  skip_on_cran()
  clv.cdnow <- fct.helper.create.clvdata.cdnow(cdnow)

  expect_silent(dt.plot <- plot(clv.cdnow, which="frequency",
                                count.repeat.trans=FALSE, trans.bins=c(1,2,3),
                                plot=FALSE, verbose=FALSE))
  expect_false(any(levels(dt.plot$num.transactions) == "0"))

  # but does with repeat trans
  expect_silent(dt.plot <- plot(clv.cdnow, which="frequency", count.repeat.trans=TRUE,
                                plot=FALSE, verbose=FALSE))
  expect_true(any(levels(dt.plot$num.transactions) == "0"))
})

test_that("frequency plot - remaining label is the highest level and disappears it not needed", {
  skip_on_cran()
  clv.cdnow <- fct.helper.create.clvdata.cdnow(cdnow)

  expect_silent(dt.plot <- plot(clv.cdnow, which="frequency",
                                trans.bins=0:10, label.remaining="AbC123",
                                count.remaining=TRUE,
                                plot=FALSE, verbose=FALSE))
  expect_true(max(levels(dt.plot$num.transactions)) == "AbC123")

  # but disappears if not needed
  expect_silent(dt.plot <- plot(clv.cdnow, which="frequency",
                                trans.bins=0:10, label.remaining="AbC123",
                                count.remaining=FALSE,
                                plot=FALSE, verbose=FALSE))
  expect_true(max(as.numeric(levels(dt.plot$num.transactions))) == 10)
})



# . spending ---------------------------------------------------------------
test_that("Spending plot - different data for different sample", {
  skip_on_cran()
  clv.cdnow <- fct.helper.create.clvdata.cdnow(cdnow)

  expect_silent(dt.none       <- plot(clv.cdnow, which="spending", plot=FALSE, verbose=FALSE))
  expect_silent(dt.estimation <- plot(clv.cdnow, which="spending", sample="estimation", plot=FALSE, verbose=FALSE))
  expect_silent(dt.full       <- plot(clv.cdnow, which="spending", sample="full", plot=FALSE, verbose=FALSE))
  expect_silent(dt.holdout    <- plot(clv.cdnow, which="spending", sample="holdout", plot=FALSE, verbose=FALSE))

  # estimation is default
  expect_true(isTRUE(all.equal(dt.none, dt.estimation)))
  # all differs to all others
  expect_false(isTRUE(all.equal(dt.estimation, dt.full)))
  expect_false(isTRUE(all.equal(dt.estimation, dt.holdout)))
  expect_false(isTRUE(all.equal(dt.full, dt.holdout)))
})


test_that("Spending plot - ggplot styling works correctly", {
  skip_on_cran()
  clv.cdnow <- fct.helper.create.clvdata.cdnow(cdnow)

  # defaults to line
  expect_silent(gg.default <- plot(clv.cdnow, which="spending", verbose=FALSE))
  expect_silent(gg.dots    <- plot(clv.cdnow, which="spending", verbose=FALSE, size=0.1))
  expect_silent(gg.geom    <- plot(clv.cdnow, which="spending", verbose=FALSE, geom="point"))
  # args passed in ...
  expect_silent(gg.color   <- plot(clv.cdnow, which="spending", verbose=FALSE, color="green"))

  expect_s3_class(gg.default$layers[[1]]$geom, "GeomLine")
  expect_s3_class(gg.geom$layers[[1]]$geom, "GeomPoint")
  expect_true(gg.dots$layers[[1]]$aes_params[["size"]] == 0.1)
  expect_true(gg.color$layers[[1]]$aes_params[["colour"]] == "green")
})

test_that("Spending plot - correct num plotted", {
  skip_on_cran()
  clv.cdnow <- fct.helper.create.clvdata.cdnow(cdnow)

  # mean.spending = TRUE
  expect_silent(dt.plot <- plot(clv.cdnow,mean.spending=TRUE, sample="full", which="spending", plot=FALSE, verbose=FALSE))
  expect_setequal(colnames(dt.plot), c("Id", "Spending"))
  expect_true(nrow(dt.plot) == clv.cdnow@data.transactions[, uniqueN(Id)])
  expect_setequal(dt.plot$Id, clv.cdnow@data.transactions[, unique(Id)])

  # mean.spending = FALSE
  #   num trans: Every transaction after aggregating same id/date
  expect_silent(dt.plot <- plot(clv.cdnow, mean.spending=FALSE, sample="full", plot=FALSE, verbose=FALSE, which="spending"))
  expect_setequal(colnames(dt.plot), c("Id", "Spending"))
  expect_true(nrow(dt.plot) == nrow(clv.data.aggregate.transactions(cdnow, has.spending = TRUE)))
  expect_setequal(dt.plot$Id, clv.cdnow@data.transactions[, unique(Id)])
})


# . interpurchasetime -----------------------------------------------------------

test_that("Interpurchasetime plot - zero-repeaters removed", {
  skip_on_cran()
  clv.cdnow <- fct.helper.create.clvdata.cdnow(cdnow)

  expect_silent(dt.plot <- plot(clv.cdnow, which="interpurchasetime", sample="estimation", plot=FALSE, verbose=FALSE))
  expect_s3_class(dt.plot, "data.table")
  expect_setequal(colnames(dt.plot), c("Id", "mean.interpurchase.time"))
  expect_false(anyNA(dt.plot))
  expect_true(dt.plot[mean.interpurchase.time>0,  .N] >  0)
  expect_true(dt.plot[mean.interpurchase.time<=0, .N] == 0)
  # Ids are unique
  expect_true(dt.plot[, uniqueN(Id)] == nrow(dt.plot))
  expect_true(nrow(dt.plot) == nobs(clv.cdnow) - 1432) # 1432: num zero-repeaters from summary() for split=37, 1411 for split=39
})


