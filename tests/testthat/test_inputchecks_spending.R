skip_on_cran()
data("cdnow")

clv.cdnow <- fct.helper.create.clvdata.cdnow(cdnow)

# data -----------------------------------------------------------------------------------------------
test_that("Fails if data is not clv.data", {
  skip_on_cran()
  expect_error(spending(~gg(), data=), "object of class clv.data, data.frame, or data.table")
  expect_error(spending(~gg(), data=NULL), "object of class clv.data, data.frame, or data.table")
  expect_error(spending(~gg(), data=123), "object of class clv.data, data.frame, or data.table")
})

test_that("Fails if data() but given clv.data", {
  skip_on_cran()
  expect_error(spending(data()~gg(), clv.cdnow), "do not specify any LHS")
  expect_error(spending(data(split=37)~gg(), clv.cdnow), "do not specify any LHS")
})



# model -----------------------------------------------------------------------------------------------
test_that("Fails if no model in RHS1", {
  skip_on_cran()
  expect_error(spending(~., data = clv.cdnow), "of the following models")
  expect_error(spending(~Id, data = clv.cdnow), "of the following models")
  expect_error(spending(~Id+Price, data = clv.cdnow), "of the following models")
  expect_error(spending(~abc, data = clv.cdnow), "of the following models")
})

test_that("Fails if wrong model in RHS1", {
  # not as function
  expect_error(spending(~gg, data = clv.cdnow), "of the following models")

  # nonexistent function
  expect_error(spending(~pnbd(), data = clv.cdnow), "of the following models")
  expect_error(spending(~bgnbd(), data = clv.cdnow), "of the following models")
  expect_error(spending(~ggomnbd(), data = clv.cdnow), "of the following models")
})

test_that("Fails if anything else but model in RHS1", {
  skip_on_cran()
  expect_error(spending(~gg()+., data = clv.cdnow), "of the following models")
  expect_error(spending(~gg()+id, data = clv.cdnow), "of the following models")
})

test_that("Fails if multiple models in RHS1", {
  skip_on_cran()
  expect_error(spending(~gg()+pnbd(), data = clv.cdnow), "of the following models")
  expect_error(spending(~gg()+bgnbd(), data = clv.cdnow), "of the following models")
})


test_that("Fails if unparsable given to model", {
  skip_on_cran()
  expect_error(spending(~gg(Id)), "can be parsed")
  expect_error(spending(~gg(Id, Price)), "can be parsed")
  expect_error(spending(~gg(clv.cdnow)), "can be parsed")
  expect_error(spending(~gg(clv.cdnow), data = clv.cdnow), "can be parsed")
  expect_error(spending(~gg(remove.first.transaction=True), data = clv.cdnow), "parse")
  expect_error(spending(~gg(start.params.model = abc), data = clv.cdnow), "parse")
})

test_that("Fails if RHS2/3/4", {
  skip_on_cran()
  expect_error(spending(~gg()|.|., data = clv.cdnow), "only contain 1 RHS")
  expect_error(spending(~gg()|Id|Price, data = clv.cdnow), "only contain 1 RHS")
  expect_error(spending(~gg()|.|.|regularization(life=1, trans=2), data = clv.cdnow), "only contain 1 RHS")
})


test_that("Fails if explicit args verbose or optimx.args given to model", {
  skip_on_cran()
  expect_error(spending(~gg(verbose=TRUE), data = clv.cdnow), "verbose")
  expect_error(spending(~gg(optimx.args=list(control=list(trace=6))), data = clv.cdnow), "optimx")
})



test_that("Fails if no data in LHS1",{
  skip_on_cran()
  expect_error(spending(~gg(), data = cdnow), "specify a LHS with")
  expect_error(spending(~gg(), data = cdnow), "specify a LHS with")
  expect_error(spending(gg()~gg(), data = cdnow), "specify exactly data()")
  expect_error(spending(Id~gg(), data = cdnow), "specify exactly data()")
  expect_error(spending(data~gg(), data = cdnow), "specify exactly data()")
})

test_that("Fails if also something else than data in LHS1",{
  skip_on_cran()
  expect_error(spending(data()+Id~gg(), data = cdnow), "specify exactly data()")
  expect_error(spending(data()+Price~gg(), data = cdnow), "specify exactly data()")
  expect_error(spending(data()+pnbd()~gg(), data = cdnow), "specify exactly data()")
})


