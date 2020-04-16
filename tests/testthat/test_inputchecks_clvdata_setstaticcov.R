# Load data ---------------------------------------------------------------------------------------
data("apparelTrans")
data("apparelStaticCov")

context("Inputchecks - SetStaticCovariates - Parameter clv.data")
expect_silent(clv.data.apparel <- clvdata(apparelTrans, date.format = "ymd", time.unit = "w"))


# Parameter clv.data ---------------------------------------------------------------------------------------


test_that("Fails for missing/NA/NULL", {
  expect_error(SetStaticCovariates(clv.data = ,
                                   data.cov.life  = apparelStaticCov, names.cov.life = "Gender",
                                   data.cov.trans = apparelStaticCov, names.cov.trans = "Gender"))
  expect_error(SetStaticCovariates(clv.data = NULL,
                                   data.cov.life  = apparelStaticCov, names.cov.life = "Gender",
                                   data.cov.trans = apparelStaticCov, names.cov.trans = "Gender"))
  expect_error(SetStaticCovariates(clv.data = NA_real_,
                                   data.cov.life  = apparelStaticCov, names.cov.life = "Gender",
                                   data.cov.trans = apparelStaticCov, names.cov.trans = "Gender"))
})

test_that("Fails if not clv.data input", {
  # dataframe / transactions
  expect_error(SetStaticCovariates(clv.data = apparelTrans,
                                   data.cov.life  = apparelStaticCov, names.cov.life = "Gender",
                                   data.cov.trans = apparelStaticCov, names.cov.trans = "Gender"))
  expect_error(SetStaticCovariates(clv.data = list(apparelTrans),
                                   data.cov.life  = apparelStaticCov, names.cov.life = "Gender",
                                   data.cov.trans = apparelStaticCov, names.cov.trans = "Gender"))
})

test_that("Fails if already has covariates", {
  expect_silent(clv.data.apparel.cov <-
                  SetStaticCovariates(clv.data = clv.data.apparel,
                                      data.cov.life  = apparelStaticCov, names.cov.life = "Gender",
                                      data.cov.trans = apparelStaticCov, names.cov.trans = "Gender"))
  expect_error(SetStaticCovariates(clv.data = clv.data.apparel.cov,
                                   data.cov.life  = apparelStaticCov, names.cov.life = "Gender",
                                   data.cov.trans = apparelStaticCov, names.cov.trans = "Gender"),
               regexp = "Cannot set")
})




# Parameter data.cov.life ---------------------------------------------------------------------------------------
context("Inputchecks - SetStaticCovariates - Parameter data.cov.life")
# ** TODO: id type wrong?

test_that("Fails if missing/NULL/NA", {
  expect_error(SetStaticCovariates(clv.data = clv.data.apparel,
                                   data.cov.life  = , names.cov.life = "Gender",
                                   data.cov.trans = apparelStaticCov, names.cov.trans = "Gender"),
               regexp = "missing")
  expect_error(SetStaticCovariates(clv.data = clv.data.apparel,
                                   data.cov.life = NULL, names.cov.life = "Gender",
                                   data.cov.trans = apparelStaticCov, names.cov.trans = "Gender"),
               regexp = "type data.frame or data.table")
  expect_error(SetStaticCovariates(clv.data = clv.data.apparel,
                                   data.cov.life = NA, names.cov.life = "Gender",
                                   data.cov.trans = apparelStaticCov, names.cov.trans = "Gender"),
               regexp = "type data.frame or data.table")
})

test_that("Fails if not dataframe/datetable", {
  expect_error(SetStaticCovariates(clv.data = clv.data.apparel,
                                   data.cov.life  = as.list(apparelStaticCov), names.cov.life = "Gender",
                                   data.cov.trans = apparelStaticCov, names.cov.trans = "Gender"),
               regexp = "type data.frame or data.table")
})

test_that("Fails if empty", {
  expect_error(SetStaticCovariates(clv.data = clv.data.apparel,
                                   data.cov.life  = data.frame(), names.cov.life = "Gender",
                                   data.cov.trans = apparelStaticCov, names.cov.trans = "Gender"),
               regexp = "empty")
  expect_error(SetStaticCovariates(clv.data = clv.data.apparel,
                                   data.cov.life  = data.frame(Id=character(), Gender=character()), names.cov.life = "Gender",
                                   data.cov.trans = apparelStaticCov, names.cov.trans = "Gender"),
               regexp = "empty")
})


test_that("Fails if does not have covariates for all customers", {
  apparelDemo.1missing <- data.table::copy(apparelStaticCov)
  apparelDemo.1missing <- apparelDemo.1missing[-200]
  expect_error(SetStaticCovariates(clv.data = clv.data.apparel,
                                   data.cov.life  = apparelDemo.1missing, names.cov.life = "Gender",
                                   data.cov.trans = apparelStaticCov, names.cov.trans = "Gender"),
               regexp = "Every Id")
})

test_that("Fails if any NA in cov data", {
  apparelDemo.1na <- data.table::copy(apparelStaticCov)
  apparelDemo.1na[200, Gender := NA_real_]
  expect_error(SetStaticCovariates(clv.data = clv.data.apparel,
                                   data.cov.life  = apparelDemo.1na, names.cov.life = "Gender",
                                   data.cov.trans = apparelStaticCov, names.cov.trans = "Gender"),
               regexp = "any NA")
})

test_that("Fails for variable with single category", {
  apparelStaticCov.1cat <- data.table::copy(apparelStaticCov)
  apparelStaticCov.1cat[, Gender := as.character(Gender)]
  apparelStaticCov.1cat[, Gender := "F/M"]
  expect_error(SetStaticCovariates(clv.data = clv.data.apparel,
                                   data.cov.life  = apparelStaticCov.1cat, names.cov.life = "Gender",
                                   data.cov.trans = apparelStaticCov, names.cov.trans = "Gender"),
               regexp = "variables with only a single category")
})


# Parameter data.cov.trans ---------------------------------------------------------------------------------------
context("Inputchecks - SetStaticCovariates - Parameter data.cov.trans")

test_that("Fails if missing/NULL/NA", {
  expect_error(SetStaticCovariates(clv.data = clv.data.apparel,
                                   data.cov.life  = apparelStaticCov, names.cov.life = "Gender",
                                   data.cov.trans = , names.cov.trans = "Gender"),
               regexp = "missing")
  expect_error(SetStaticCovariates(clv.data = clv.data.apparel,
                                   data.cov.life = apparelStaticCov, names.cov.life = "Gender",
                                   data.cov.trans = NULL, names.cov.trans = "Gender"),
               regexp = "type data.frame or data.table")
  expect_error(SetStaticCovariates(clv.data = clv.data.apparel,
                                   data.cov.life = apparelStaticCov, names.cov.life = "Gender",
                                   data.cov.trans = NA, names.cov.trans = "Gender"),
               regexp = "type data.frame or data.table")
})

test_that("Fails if not dataframe/datetable", {
  expect_error(SetStaticCovariates(clv.data = clv.data.apparel,
                                   data.cov.life  = apparelStaticCov, names.cov.life = "Gender",
                                   data.cov.trans = as.list(apparelStaticCov), names.cov.trans = "Gender"),
               regexp = "type data.frame or data.table")
})

test_that("Fails if empty", {
  expect_error(SetStaticCovariates(clv.data = clv.data.apparel,
                                   data.cov.life  = apparelStaticCov, names.cov.life = "Gender",
                                   data.cov.trans = data.frame(), names.cov.trans = "Gender"),
               regexp = "empty")
  expect_error(SetStaticCovariates(clv.data = clv.data.apparel,
                                   data.cov.life  = apparelStaticCov, names.cov.life = "Gender",
                                   data.cov.trans = data.frame(Id=character(), Gender=character()), names.cov.trans = "Gender"),
               regexp = "empty")
})


test_that("Fails if does not have covariates for all customers", {
  apparelDemo.1missing <- data.table::copy(apparelStaticCov)
  apparelDemo.1missing <- apparelDemo.1missing[-200]
  expect_error(SetStaticCovariates(clv.data = clv.data.apparel,
                                   data.cov.life  = apparelStaticCov, names.cov.life = "Gender",
                                   data.cov.trans = apparelDemo.1missing, names.cov.trans = "Gender"),
               regexp = "Every Id")
})

test_that("Fails if any NA in cov data", {
  apparelDemo.1na <- data.table::copy(apparelStaticCov)
  apparelDemo.1na[200, Gender := NA_real_]
  expect_error(SetStaticCovariates(clv.data = clv.data.apparel,
                                   data.cov.life  = apparelStaticCov, names.cov.life = "Gender",
                                   data.cov.trans = apparelDemo.1na, names.cov.trans = "Gender"),
               regexp = "any NA")
})

test_that("Fails for variable with single category", {
  apparelStaticCov.1cat <- data.table::copy(apparelStaticCov)
  apparelStaticCov.1cat[, Gender := as.character(Gender)]
  apparelStaticCov.1cat[, Gender := "F/M"]

  expect_error(SetStaticCovariates(clv.data = clv.data.apparel,
                                   data.cov.life  = apparelStaticCov,      names.cov.life  = "Gender",
                                   data.cov.trans = apparelStaticCov.1cat, names.cov.trans = "Gender"),
               regexp = "variables with only a single category")
})


# Parameter name.cov.life ---------------------------------------------------------------------------------------
context("Inputchecks - SetStaticCovariates - Parameter name.cov.life")
test_that("Fails if missing/NULL/NA/empty",{
  expect_error(SetStaticCovariates(clv.data = clv.data.apparel,
                                   data.cov.life = apparelStaticCov, names.cov.life = ,
                                   data.cov.trans = apparelStaticCov, names.cov.trans = "Gender"),
               regexp = "missing")
  expect_error(SetStaticCovariates(clv.data = clv.data.apparel,
                                   data.cov.life = apparelStaticCov, names.cov.life = NULL ,
                                   data.cov.trans = apparelStaticCov, names.cov.trans = "Gender"),
               regexp = "may not be NULL")
  expect_error(SetStaticCovariates(clv.data = clv.data.apparel,
                                   data.cov.life = apparelStaticCov, names.cov.life = NA_character_ ,
                                   data.cov.trans = apparelStaticCov, names.cov.trans = "Gender"),
               regexp = "any NA")
  expect_error(SetStaticCovariates(clv.data = clv.data.apparel,
                                   data.cov.life = apparelStaticCov, names.cov.life = "" ,
                                   data.cov.trans = apparelStaticCov, names.cov.trans = "Gender"),
               regexp = "could not be found")
})

test_that("Fails if not character vector",{
  # ** Fails properly
  expect_error(SetStaticCovariates(clv.data = clv.data.apparel,
                                   data.cov.life = apparelStaticCov, names.cov.life = list("Gender"),
                                   data.cov.trans = apparelStaticCov, names.cov.trans = "Gender"),
               regexp = "character")
  expect_error(SetStaticCovariates(clv.data = clv.data.apparel,
                                   data.cov.life = apparelStaticCov, names.cov.life = data.frame("Gender"),
                                   data.cov.trans = apparelStaticCov, names.cov.trans = "Gender"),
               regexp = "character")
  expect_error(SetStaticCovariates(clv.data = clv.data.apparel,
                                   data.cov.life = apparelStaticCov, names.cov.life = 2,
                                   data.cov.trans = apparelStaticCov, names.cov.trans = "Gender"),
               regexp = "character")
})

test_that("Fails if contains NA", {
  expect_error(SetStaticCovariates(clv.data = clv.data.apparel,
                                   data.cov.life = apparelStaticCov, names.cov.life = NA_character_,
                                   data.cov.trans = apparelStaticCov, names.cov.trans = "Gender"),
               regexp = "any NA")
  expect_error(SetStaticCovariates(clv.data = clv.data.apparel,
                                   data.cov.life = apparelStaticCov, names.cov.life = c("Gender", NA_character_),
                                   data.cov.trans = apparelStaticCov, names.cov.trans = "Gender"),
               regexp = "any NA")
})

test_that("Fails if names not in data", {
  # ** TODO: Check and fail properly
  expect_error(SetStaticCovariates(clv.data = clv.data.apparel,
                                   data.cov.life = apparelStaticCov, names.cov.life = "GenderBender",
                                   data.cov.trans = apparelStaticCov, names.cov.trans = "Gender"),
               regexp = "could not be found")
  expect_error(SetStaticCovariates(clv.data = clv.data.apparel,
                                   data.cov.life = apparelStaticCov, names.cov.life = "gender",
                                   data.cov.trans = apparelStaticCov, names.cov.trans = "Gender"),
               regexp = "could not be found")
})

test_that("Fails if has duplicate names", {
  # ** TODO: Fail properly
  expect_error(SetStaticCovariates(clv.data = clv.data.apparel,
                                   data.cov.life = apparelStaticCov, names.cov.life = c("Gender", "Gender"),
                                   data.cov.trans = apparelStaticCov, names.cov.trans = "Gender"),
               regexp = "duplicates")
})


# Parameter name.cov.trans ---------------------------------------------------------------------------------------
context("Inputchecks - SetStaticCovariates - Parameter name.cov.trans")
test_that("Fails if missing/NULL/NA/empty",{
  expect_error(SetStaticCovariates(clv.data = clv.data.apparel,
                                   data.cov.life = apparelStaticCov, names.cov.life = "Gender",
                                   data.cov.trans = apparelStaticCov, names.cov.trans = ))
  expect_error(SetStaticCovariates(clv.data = clv.data.apparel,
                                   data.cov.life = apparelStaticCov, names.cov.life ="Gender",
                                   data.cov.trans = apparelStaticCov, names.cov.trans = NULL),
               regexp = "may not be NULL")
  expect_error(SetStaticCovariates(clv.data = clv.data.apparel,
                                   data.cov.life = apparelStaticCov, names.cov.life =  "Gender",
                                   data.cov.trans = apparelStaticCov, names.cov.trans = NA_character_),
               regexp = "any NA")
  expect_error(SetStaticCovariates(clv.data = clv.data.apparel,
                                   data.cov.life = apparelStaticCov, names.cov.life = "Gender" ,
                                   data.cov.trans = apparelStaticCov, names.cov.trans = ""),
               regexp = "could not be found")
})

test_that("Fails if not character vector",{
  expect_error(SetStaticCovariates(clv.data = clv.data.apparel,
                                   data.cov.life = apparelStaticCov, names.cov.life = "Gender",
                                   data.cov.trans = apparelStaticCov, names.cov.trans = list("Gender")),
               regexp = "character")
  expect_error(SetStaticCovariates(clv.data = clv.data.apparel,
                                   data.cov.life = apparelStaticCov, names.cov.life = "Gender",
                                   data.cov.trans = apparelStaticCov, names.cov.trans = data.frame("Gender")),
               regexp = "character")
  expect_error(SetStaticCovariates(clv.data = clv.data.apparel,
                                   data.cov.life = apparelStaticCov, names.cov.life = "Gender",
                                   data.cov.trans = apparelStaticCov, names.cov.trans = 2),
               regexp = "character")
})

test_that("Fails if contains NA", {
  expect_error(SetStaticCovariates(clv.data = clv.data.apparel,
                                   data.cov.life = apparelStaticCov, names.cov.life = "Gender",
                                   data.cov.trans = apparelStaticCov, names.cov.trans = NA_character_),
               regexp = "any NA")
  expect_error(SetStaticCovariates(clv.data = clv.data.apparel,
                                   data.cov.life = apparelStaticCov, names.cov.life = "Gender",
                                   data.cov.trans = apparelStaticCov, names.cov.trans = c("Gender", NA_character_)),
               regexp = "any NA")
})

test_that("Fails if names not in data", {
  expect_error(SetStaticCovariates(clv.data = clv.data.apparel,
                                   data.cov.life = apparelStaticCov, names.cov.life = "Gender",
                                   data.cov.trans = apparelStaticCov, names.cov.trans = "GenderBender"),
               regexp = "could not be found")

  expect_error(SetStaticCovariates(clv.data = clv.data.apparel,
                                   data.cov.life = apparelStaticCov, names.cov.life = "Gender",
                                   data.cov.trans = apparelStaticCov, names.cov.trans = "gender"),
               regexp = "could not be found")
})

test_that("Fails if has duplicate names", {
  expect_error(SetStaticCovariates(clv.data = clv.data.apparel,
                                   data.cov.life = apparelStaticCov, names.cov.life = "Gender",
                                   data.cov.trans = apparelStaticCov, names.cov.trans = c("Gender", "Gender")),
               regexp = "duplicates")
})


# Parameter name.id ---------------------------------------------------------------------------------------
# **TODO: proper inputchecks and fails
l.std.args <- list(clv.data = clv.data.apparel,
                    data.cov.life = apparelStaticCov, names.cov.life = "Gender",
                    data.cov.trans = apparelStaticCov, names.cov.trans = "Gender")

test_that("Fails if NA/NULL", {
  expect_error(do.call(SetStaticCovariates, modifyList(l.std.args, list(name.id=NULL), keep.null = TRUE)),
               regexp = "NULL")
  expect_error(do.call(SetStaticCovariates, modifyList(l.std.args, list(name.id=NA_character_))),
               regexp = "any NA")
  expect_error(do.call(SetStaticCovariates, modifyList(l.std.args, list(name.id=character(0)))),
               regexp = "single element")
})

test_that("Fails if not character", {
  expect_error(do.call(SetStaticCovariates, modifyList(l.std.args, list(name.id=list("Id")), keep.null = TRUE)),
               regexp = "character")
  expect_error(do.call(SetStaticCovariates, modifyList(l.std.args, list(name.id=data.frame("Id")), keep.null = TRUE)),
               regexp = "character")
  expect_error(do.call(SetStaticCovariates, modifyList(l.std.args, list(name.id=1), keep.null = TRUE)),
               regexp = "character")
})

test_that("Fails if multiple", {
  expect_error(do.call(SetStaticCovariates, modifyList(l.std.args, list(name.id=c("Id", "Id")), keep.null = TRUE)),
               regexp = "single element")
  expect_error(do.call(SetStaticCovariates, modifyList(l.std.args, list(name.id=c("Id", "Id")), keep.null = TRUE)),
               regexp = "single element")
  expect_error(do.call(SetStaticCovariates, modifyList(l.std.args, list(name.id=c("Id", "Date")), keep.null = TRUE)),
               regexp = "single element")
})

test_that("Fails if not in transaction data", {
  expect_error(do.call(SetStaticCovariates, modifyList(l.std.args, list(name.id="id"), keep.null = TRUE)),
               regexp = "not be found in the data")

  expect_error(do.call(SetStaticCovariates, modifyList(l.std.args, list(name.id="ID"), keep.null = TRUE)),
               regexp = "not be found in the data")

  expect_error(do.call(SetStaticCovariates, modifyList(l.std.args, list(name.id="di"), keep.null = TRUE)),
               regexp = "not be found in the data")

  expect_error(do.call(SetStaticCovariates, modifyList(l.std.args, list(name.id="customer"), keep.null = TRUE)),
               regexp = "not be found in the data")
})

test_that("Has default argument Id",{
  default.arg <- eval(formals(SetStaticCovariates)[["name.id"]])
  expect_is(default.arg, "character")
  expect_true(default.arg == "Id")
})


