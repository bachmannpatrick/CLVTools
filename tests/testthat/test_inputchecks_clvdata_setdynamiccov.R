skip_on_cran()

data("apparelTrans")
data("apparelDynCov")

clv.data.apparel <- fct.helper.create.clvdata.apparel.nocov(estimation.split = NULL)

fct.expect.error.setdyncov <- function(
    clv.data=clv.data.apparel,
    data.cov.life = apparelDynCov,
    data.cov.trans = apparelDynCov,
    names.cov.life=c("High.Season", "Gender", "Channel"),
    names.cov.trans=c("High.Season", "Gender", "Channel"),
    name.id = "Id",
    name.date = "Cov.Date",
    make.missing = NULL,
    regexp=NULL){

  if(!is.null(make.missing)){
    e <- environment()
    # e[[make.missing]] <- NULL
    rm(list=make.missing, envir = e)
  }

  expect_error(SetDynamicCovariates(
    clv.data = clv.data,
    data.cov.life  = data.cov.life,
    names.cov.life = names.cov.life,
    data.cov.trans = data.cov.trans,
    names.cov.trans = names.cov.trans,
    name.id = name.id,
    name.date = name.date
  ),
  regexp = regexp)
}


# Parameter clv.data ---------------------------------------------------------------------------------------
test_that("Fails if not clv.data input", {
  # missing/NA/NULL
  fct.expect.error.setdyncov(make.missing = "clv.data")
  fct.expect.error.setdyncov(clv.data = NULL)
  fct.expect.error.setdyncov(clv.data = NA_real_)


  # dataframe / transactions
  fct.expect.error.setdyncov(clv.data = apparelTrans)
  fct.expect.error.setdyncov(clv.data = list(apparelTrans))
})


test_that("Fails if already has covariates", {
  fct.expect.error.setdyncov(clv.data = fct.helper.create.clvdata.apparel.staticcov(), regexp = "Cannot set")
  fct.expect.error.setdyncov(clv.data = fct.helper.create.clvdata.apparel.dyncov(), regexp = "Cannot set")
})


# Parameter data.cov.life and data.cov.trans --------------------------------------------------------------------------------

# ** TODO: id type wrong?

test_that("Fails if is wrong type ", {

  # data.cov.life
  fct.expect.error.setdyncov(make.missing="data.cov.life", regexp = "not found")
  fct.expect.error.setdyncov(data.cov.life  = NULL, regexp = "type data.frame or data.table")
  fct.expect.error.setdyncov(data.cov.life  = NA, regexp = "type data.frame or data.table")
  fct.expect.error.setdyncov(data.cov.life  = as.list(apparelDynCov), regexp = "type data.frame or data.table")


  # data.cov.trans
  fct.expect.error.setdyncov(make.missing = "data.cov.trans", regexp = "not found")
  fct.expect.error.setdyncov(data.cov.trans  = NULL, regexp = "type data.frame or data.table")
  fct.expect.error.setdyncov(data.cov.trans  = NA, regexp = "type data.frame or data.table")
  fct.expect.error.setdyncov(data.cov.trans  = as.list(apparelDynCov), regexp = "type data.frame or data.table")

})


test_that("Fails if is empty", {

  # data.cov.life
  fct.expect.error.setdyncov(data.cov.life = data.frame(), regexp = "empty")
  fct.expect.error.setdyncov(data.cov.life = data.frame(Id=character(), Gender=character()), regexp = "empty")

  # data.cov.trans
  fct.expect.error.setdyncov(data.cov.trans = data.frame(), regexp = "empty")
  fct.expect.error.setdyncov(data.cov.trans = data.frame(Id=character(), Gender=character()), regexp = "empty")

})

test_that("Fails if covariate data is to short for all customers",{
  apparelDynCov.tooshort <- data.table::copy(apparelDynCov)
  apparelDynCov.tooshort <- apparelDynCov.tooshort[Cov.Date < "2005-02-01"]
                                                     # clv.data.apparel@clv.time@timepoint.estimation.end - lubridate::dweeks(15)]

  # data.cov.life
  fct.expect.error.setdyncov(data.cov.life = apparelDynCov.tooshort, regexp = "covariate data exactly from")
  # data.cov.trans
  fct.expect.error.setdyncov(data.cov.trans = apparelDynCov.tooshort, regexp = "covariate data exactly from")
})

test_that("Fails if covariate data ends before observation.end", {

  clv.data.apparel.obsE <- fct.helper.create.clvdata.apparel.nocov(
    estimation.split = NULL,
    observation.end = "2012-12-31")

  fct.expect.error.setdyncov(
    clv.data = clv.data.apparel.obsE,
    regexp = "There need to be weekly covariate data exactly")

})

test_that("Fails if there are Ids in the covariates that are not in the transaction data", {
  dt.cov.1additional <- data.table::copy(apparelDynCov[Id == "1"])
  dt.cov.1additional[, Id := "ABC"]

  apparelDynCov.1additional <- rbindlist(list(
    data.table::copy(apparelDynCov),
    dt.cov.1additional
  ))

  # data.cov.life
  fct.expect.error.setdyncov(data.cov.life = apparelDynCov.1additional, regexp = "Every Id")
  # data.cov.trans
  fct.expect.error.setdyncov(data.cov.trans = apparelDynCov.1additional, regexp = "Every Id")
})

test_that("Fails if does not have covariates for all customers", {
  apparelDynCov.1missing <- data.table::copy(apparelDynCov)
  first.customer <- apparelDynCov.1missing[1,]$Id
  # delete all by this customer
  apparelDynCov.1missing <- apparelDynCov.1missing[Id != first.customer]

  # data.cov.life
  fct.expect.error.setdyncov(data.cov.life = apparelDynCov.1missing, regexp = "Every Id")
  # data.cov.trans
  fct.expect.error.setdyncov(data.cov.trans = apparelDynCov.1missing, regexp = "Every Id")
})

test_that("Fails if does not have covariates for all customers and dates", {
  apparelDynCov.1missing <- data.table::copy(apparelDynCov)
  apparelDynCov.1missing <- apparelDynCov.1missing[-1000]

  # data.cov.life
  fct.expect.error.setdyncov(data.cov.life = apparelDynCov.1missing, regexp = "need to have the same number of Dates")
  # data.cov.trans
  fct.expect.error.setdyncov(data.cov.trans = apparelDynCov.1missing, regexp = "need to have the same number of Dates")
})

test_that("Fails if duplicate covariate (Id/Date)", {

  apparelDynCov.duplicate.same <-
    data.table::rbindlist(list(apparelDynCov, apparelDynCov[1000]))

  apparelDynCov.duplicate.diff <-
    data.table::rbindlist(list(apparelDynCov,
                   # Different cov value
                   apparelDynCov[1000,.(Id,Cov.Date, High.Season=1,Gender=0, Channel=0)]))


  # data.cov.life
  fct.expect.error.setdyncov(data.cov.life = apparelDynCov.duplicate.same, regexp = "need to have the same number")
  fct.expect.error.setdyncov(data.cov.life = apparelDynCov.duplicate.diff, regexp = "need to have the same number")

  # data.cov.trans
  fct.expect.error.setdyncov(data.cov.trans = apparelDynCov.duplicate.same, regexp = "need to have the same number")
  fct.expect.error.setdyncov(data.cov.trans = apparelDynCov.duplicate.diff, regexp = "need to have the same number")

})


test_that("Fails if any NA in cov data - Id", {
  apparelDemo.1na <- data.table::copy(apparelDynCov)
  apparelDemo.1na[1000, Id := NA]

  # data.cov.life
  fct.expect.error.setdyncov(data.cov.life = apparelDemo.1na, regexp = "any NA")

  # data.cov.trans
  fct.expect.error.setdyncov(data.cov.trans = apparelDemo.1na, regexp = "any NA")
})

test_that("Fails if any NA in cov data - Date", {
  apparelDemo.1na <- data.table::copy(apparelDynCov)
  apparelDemo.1na[1000, Cov.Date := NA_real_]

  # data.cov.life
  fct.expect.error.setdyncov(data.cov.life = apparelDemo.1na, regexp = "any NA")

  # data.cov.trans
  fct.expect.error.setdyncov(data.cov.trans = apparelDemo.1na, regexp = "any NA")
})

test_that("Fails if any NA in cov data - Cov", {
  apparelDemo.1na <- data.table::copy(apparelDynCov)
  apparelDemo.1na[1000, Channel := NA_real_]
  # data.cov.life
  fct.expect.error.setdyncov(data.cov.life = apparelDemo.1na, regexp = "any NA")
  # data.cov.trans
  fct.expect.error.setdyncov(data.cov.trans = apparelDemo.1na, regexp = "any NA")

  apparelDemo.1na <- data.table::copy(apparelDynCov)
  apparelDemo.1na[1000, Gender := NA_real_]
  # data.cov.life
  fct.expect.error.setdyncov(data.cov.life = apparelDemo.1na, regexp = "any NA")
  # data.cov.trans
  fct.expect.error.setdyncov(data.cov.trans = apparelDemo.1na, regexp = "any NA")


  apparelDemo.1na <- data.table::copy(apparelDynCov)
  apparelDemo.1na[1000, High.Season := NA_real_]
  # data.cov.life
  fct.expect.error.setdyncov(data.cov.life = apparelDemo.1na, regexp = "any NA")
  # data.cov.trans
  fct.expect.error.setdyncov(data.cov.trans = apparelDemo.1na, regexp = "any NA")
})



test_that("Fails for variable with single category", {
  apparelDynCov.1cat <- data.table::copy(apparelDynCov)
  apparelDynCov.1cat[, Gender := as.character(Gender)]
  apparelDynCov.1cat[, Gender := "F/M"]

  # data.cov.life
  fct.expect.error.setdyncov(data.cov.life = apparelDynCov.1cat, regexp = "variables with only a single category")
  # data.cov.trans
  fct.expect.error.setdyncov(data.cov.trans = apparelDynCov.1cat, regexp = "variables with only a single category")
})



# Parameter names.cov.life and names.cov.trans---------------------------------------------------------------------------------------

test_that("Fails if missing/NULL/NA/empty",{
  # names.cov.life
  fct.expect.error.setdyncov(make.missing = "names.cov.life", regexp = "not found")
  fct.expect.error.setdyncov(names.cov.life = NULL, regexp = "may not be NULL")
  fct.expect.error.setdyncov(names.cov.life = NA_character_, regexp = "any NA")
  fct.expect.error.setdyncov(names.cov.life = "", regexp = "could not be found")

  # names.cov.trans
  fct.expect.error.setdyncov(make.missing = "names.cov.trans", regexp = "not found")
  fct.expect.error.setdyncov(names.cov.trans = NULL, regexp = "may not be NULL")
  fct.expect.error.setdyncov(names.cov.trans = NA_character_, regexp = "any NA")
  fct.expect.error.setdyncov(names.cov.trans = "", regexp = "could not be found")
})

test_that("Fails if not character vector",{

  # names.cov.life
  fct.expect.error.setdyncov(names.cov.life = list(c("High.Season", "Gender", "Channel")), regexp = "character")
  fct.expect.error.setdyncov(names.cov.life = data.frame(c("High.Season", "Gender", "Channel")), regexp = "character")
  fct.expect.error.setdyncov(names.cov.life = 2, regexp = "character")

  # names.cov.trans
  fct.expect.error.setdyncov(names.cov.trans = list(c("High.Season", "Gender", "Channel")), regexp = "character")
  fct.expect.error.setdyncov(names.cov.trans = data.frame(c("High.Season", "Gender", "Channel")), regexp = "character")
  fct.expect.error.setdyncov(names.cov.trans = 2, regexp = "character")
})

test_that("Fails if contains NA", {
  # names.cov.life
  fct.expect.error.setdyncov(names.cov.life = NA_character_, regexp = "any NA")
  fct.expect.error.setdyncov(names.cov.life = c("High.Season", "Gender", "", NA_character_), regexp = "any NA")

  # names.cov.trans
  fct.expect.error.setdyncov(names.cov.trans = NA_character_, regexp = "any NA")
  fct.expect.error.setdyncov(names.cov.trans = c("High.Season", "Gender", "", NA_character_), regexp = "any NA")
})

test_that("Fails if names not in data", {

  # names.cov.life
  fct.expect.error.setdyncov(names.cov.life = c("High.Season", "Gender", "ChannelBender"), regexp = "ChannelBender could not be found in the Lifetime covariate")
  fct.expect.error.setdyncov(names.cov.life = c("High.Season", "Bender", "Channel"), regexp = "Bender could not be found in the Lifetime covariate")

  # names.cov.trans
  fct.expect.error.setdyncov(names.cov.trans = c("High.Season", "Gender", "ChannelBender"), regexp = "ChannelBender could not be found in the Transaction covariate")
  fct.expect.error.setdyncov(names.cov.trans = c("High.Season", "Bender", "Channel"), regexp = "Bender could not be found in the Transaction covariate")
})

test_that("Fails if has duplicate names", {

  # names.cov.life
  fct.expect.error.setdyncov(names.cov.life = c("High.Season", "Gender", "Channel", "Channel"), regexp = "Lifetime covariate may not contain any duplicates")
  fct.expect.error.setdyncov(names.cov.life = c("Gender", "Gender"), regexp = "Lifetime covariate may not contain any duplicates")

  # names.cov.trans
  fct.expect.error.setdyncov(names.cov.trans = c("High.Season", "Gender", "Channel", "Channel"), regexp = "Transaction covariate may not contain any duplicates")
  fct.expect.error.setdyncov(names.cov.trans = c("Gender", "Gender"), regexp = "Transaction covariate may not contain any duplicates")
})




# Parameter name.id ---------------------------------------------------------------------------------------

test_that("Fails if NA/NULL", {
  fct.expect.error.setdyncov(name.id = NULL, regexp = "NULL")
  fct.expect.error.setdyncov(name.id = NA_character_, regexp = "any NA")
  fct.expect.error.setdyncov(name.id = character(0), regexp = "exactly 1 element")
})

test_that("Fails if not character", {

  fct.expect.error.setdyncov(name.id = list("Id"), regexp = "character")
  fct.expect.error.setdyncov(name.id = data.frame("Id"), regexp = "character")
  fct.expect.error.setdyncov(name.id = 1, regexp = "character")
})

test_that("Fails if multiple", {
  fct.expect.error.setdyncov(name.id = c("Id", "Id"), regexp = "exactly 1 element")
  fct.expect.error.setdyncov(name.id = c("Id", "Date"), regexp = "exactly 1 element")
})

test_that("Fails if not in transaction data", {


  fct.expect.error.setdyncov(name.id = "id", regexp = "not be found in the data")
  fct.expect.error.setdyncov(name.id = "ID", regexp = "not be found in the data")
  fct.expect.error.setdyncov(name.id = "di", regexp = "not be found in the data")
  fct.expect.error.setdyncov(name.id = "customer", regexp = "not be found in the data")
})

test_that("Has default argument Id",{
  default.arg <- eval(formals(SetDynamicCovariates)[["name.id"]])
  expect_true(is.character(default.arg))
  expect_true(default.arg == "Id")
})


# Parameter name.date ---------------------------------------------------------------------------------------
test_that("Fails if NA/NULL", {

  fct.expect.error.setdyncov(name.date = "id", regexp = "could not be found")
  fct.expect.error.setdyncov(name.date = NA_character_, regexp = "any NA")
  fct.expect.error.setdyncov(name.date = character(0), regexp = "exactly 1 element")
})

test_that("Fails if not character", {

  fct.expect.error.setdyncov(name.date = list("Id"), regexp = "character")
  fct.expect.error.setdyncov(name.date = data.frame("Id"), regexp = "character")
  fct.expect.error.setdyncov(name.date = list(name.date=1), regexp = "character")
})

test_that("Fails if multiple", {

  fct.expect.error.setdyncov(name.date = c("Id", "Id"), regexp = "exactly 1 element")
  fct.expect.error.setdyncov(name.date = c("Id", "Date"), regexp = "exactly 1 element")
})

test_that("Fails if not in transaction data", {

  fct.expect.error.setdyncov(name.date = "id", regexp = "not be found in the data")
  fct.expect.error.setdyncov(name.date = "ID", regexp = "not be found in the data")
  fct.expect.error.setdyncov(name.date = "di", regexp = "not be found in the data")
  fct.expect.error.setdyncov(name.date = "customer", regexp = "not be found in the data")
})

test_that("Has default argument Id",{
  default.arg <- eval(formals(SetDynamicCovariates)[["name.date"]])
  expect_true(is.character(default.arg))
  expect_true(default.arg == "Date")
})


