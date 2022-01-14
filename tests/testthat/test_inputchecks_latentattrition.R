skip_on_cran()
data("cdnow")
data("apparelTrans")
data("apparelStaticCov")

# **IMPORTANT TODO: FAIL IF COV GIVEN BUT data=CLV.DATA

# nocov, clv.data -----------------------------------------------------------------------------------------------
context("Inputchecks - latentAttrition - nocov, clv.data")
clv.cdnow <- fct.helper.create.clvdata.cdnow(cdnow)

# .data -----------------------------------------------------------------------------------------------
test_that("Fails if data is not clv.data (and no LHS1)", {
  expect_error(latentAttrition(~pnbd(), data=), "clv.data")
  expect_error(latentAttrition(~pnbd(), data=NULL), "clv.data")
  expect_error(latentAttrition(~pnbd(), data=123), "clv.data")
  expect_error(latentAttrition(~pnbd(), data=cdnow), "specify a LHS ")
})

# .RHS1 model -----------------------------------------------------------------------------------------------
test_that("Fails if no model in RHS1", {
  skip_on_cran()
  expect_error(latentAttrition(~., data = clv.cdnow), "of the following models")
  expect_error(latentAttrition(~Id, data = clv.cdnow), "of the following models")
  expect_error(latentAttrition(~Id+Price, data = clv.cdnow), "of the following models")
  expect_error(latentAttrition(~abc, data = clv.cdnow), "of the following models")
})

test_that("Fails if wrong model in RHS1", {
  # not as function
  expect_error(latentAttrition(~pnbd, data = clv.cdnow), "of the following models")
  expect_error(latentAttrition(~bgnbd, data = clv.cdnow), "of the following models")
  expect_error(latentAttrition(~ggomnbd, data = clv.cdnow), "of the following models")

  # nonexistent function
  expect_error(latentAttrition(~gg(), data = clv.cdnow), "of the following models")
  expect_error(latentAttrition(~pndb(), data = clv.cdnow), "of the following models")
  expect_error(latentAttrition(~bnbd(), data = clv.cdnow), "of the following models")
  expect_error(latentAttrition(~ggmnbd(), data = clv.cdnow), "of the following models")
})

test_that("Fails if anything else but model in RHS1", {
  skip_on_cran()
  expect_error(latentAttrition(~pnbd()+., data = clv.cdnow), "of the following models")
  expect_error(latentAttrition(~pnbd()+id, data = clv.cdnow), "of the following models")
  expect_error(latentAttrition(~bgnbd()+xyz, data = clv.cdnow), "of the following models")
  expect_error(latentAttrition(~bgnbd()+a:b, data = clv.cdnow), "of the following models")
})

test_that("Fails if multiple models in RHS1", {
  skip_on_cran()
  expect_error(latentAttrition(~pnbd()+gg(), data = clv.cdnow), "of the following models")
  expect_error(latentAttrition(~pnbd()+bgnbd(), data = clv.cdnow), "of the following models")
  expect_error(latentAttrition(~ggomnbd()+bgnbd(), data = clv.cdnow), "of the following models")
})


test_that("Fails if unparsable given to model", {
  skip_on_cran()
  expect_error(latentAttrition(~pnbd(Id)), "can be parsed")
  expect_error(latentAttrition(~pnbd(Id, Price)), "can be parsed")
  expect_error(latentAttrition(~pnbd(clv.cdnow)), "can be parsed")
  expect_error(latentAttrition(~pnbd(clv.cdnow), data = clv.cdnow), "can be parsed")
  expect_error(latentAttrition(~pnbd(use.cor=True), data = clv.cdnow), "parse")
  expect_error(latentAttrition(~pnbd(use.cor=abc), data = clv.cdnow), "parse")
  expect_error(latentAttrition(~pnbd(start.params.model = abc), data = clv.cdnow), "parse")
})

test_that("Fails if RHS2/3/4 but no covariates in given clv.data", {
  skip_on_cran()
  expect_error(latentAttrition(~pnbd()|.|., data = clv.cdnow), "only contain 1 part")
  expect_error(latentAttrition(~pnbd()|Id|Price, data = clv.cdnow), "only contain 1 part")
  expect_error(latentAttrition(~pnbd()|.|.|regularization(life=1, trans=2), data = clv.cdnow), "only contain 1 part")
})


test_that("Fails if explicit args verbose or optimx.args given to model", {
  skip_on_cran()
  expect_error(latentAttrition(~pnbd(verbose=TRUE), data = clv.cdnow), "verbose")
  expect_error(latentAttrition(~pnbd(optimx.args=list(control=list(trace=6))), data = clv.cdnow), "optimx")
})


# static cov, clv.data -----------------------------------------------------------------------------------------------
context("Inputchecks - latentAttrition - static cov, clv.data")

clv.apparel.cov <- fct.helper.create.clvdata.apparel.staticcov(data.apparelTrans = apparelTrans, data.apparelStaticCov = apparelStaticCov,
                                                               estimation.split = NULL)

# . RHS2/3 ---------------------------------------------------------------------------------------------
test_that("Fails if no RHS2/3 but cov clv.data",{
  expect_error(latentAttrition(~pnbd(), clv.apparel.cov), "transaction and the lifetime covariates")
  expect_error(latentAttrition(~pnbd()|., clv.apparel.cov), "transaction and the lifetime covariates")
})

# . RHS4 -----------------------------------------------------------------------------------------------
test_that("Fails if RHS 4 has wrong content", {
  expect_error(latentAttrition(~pnbd()|.|.|., clv.apparel.cov), "from the following")
  expect_error(latentAttrition(~pnbd()|.|.|reg(life=10, trans=10), clv.apparel.cov), "from the following")
  expect_error(latentAttrition(~pnbd()|.|.|constr(abc), clv.apparel.cov), "from the following")
})


# . regularization() ------------------------------------------------------------------------------------
test_that("Fails if reguarlization does not have args trans and life", {
  expect_error(latentAttrition(~pnbd()|.|.|regularization(), clv.apparel.cov), "life and trans")
  expect_error(latentAttrition(~pnbd()|.|.|regularization(life=10), clv.apparel.cov), "life and trans")
  expect_error(latentAttrition(~pnbd()|.|.|regularization(trans=10), clv.apparel.cov), "life and trans")
  expect_error(latentAttrition(~pnbd()|.|.|regularization(live=10, trans=10), clv.apparel.cov), "life and trans")
  expect_error(latentAttrition(~pnbd()|.|.|regularization(life=10, trans=10, live=10), clv.apparel.cov), "life and trans")
})

test_that("Fails if reguarlization does not have numeric args", {
  expect_error(latentAttrition(~pnbd()|.|.|regularization(life=, trans=10), clv.apparel.cov), "as number")
  expect_error(latentAttrition(~pnbd()|.|.|regularization(life=10, trans=), clv.apparel.cov), "as number")
  expect_error(latentAttrition(~pnbd()|.|.|regularization(life=10, trans=abc), clv.apparel.cov), "as number")
  expect_error(latentAttrition(~pnbd()|.|.|regularization(life=abc, trans=10), clv.apparel.cov), "as number")
  expect_error(latentAttrition(~pnbd()|.|.|regularization(life=abc, trans=abc), clv.apparel.cov), "as number")
  expect_error(latentAttrition(~pnbd()|.|.|regularization(life=TRUE, trans=TRUE), clv.apparel.cov), "as number")
  expect_error(latentAttrition(~pnbd()|.|.|regularization(life=NULL, trans=NULL), clv.apparel.cov), "as number")
})

test_that("Fails if multiple regularization", {
  expect_error(latentAttrition(~pnbd()|.|.|regularization(life=10, trans=10)+regularization(trans=10), clv.apparel.cov), "only once")
  expect_error(latentAttrition(~pnbd()|.|.|regularization(life=10, trans=10)+regularization(life=10), clv.apparel.cov), "only once")
  expect_error(latentAttrition(~pnbd()|.|.|regularization(life=10, trans=10)+regularization(life=10, trans=8), clv.apparel.cov), "only once")
})


# . constraint() ------------------------------------------------------------------------------------
test_that("Fails if constraint() does not have valid content", {
  # **TODO: allow or not??
  # expect_error(latentAttrition(~pnbd()|.|.|constraint(), clv.apparel.cov), "")
  # expect_error(latentAttrition(~pnbd()|.|.|constraint(.), clv.apparel.cov), "")
  expect_error(latentAttrition(~pnbd()|.|.|constraint(NULL), clv.apparel.cov), "could not be found")
  expect_error(latentAttrition(~pnbd()|.|.|constraint(123), clv.apparel.cov), "could not be found")
  expect_error(latentAttrition(~pnbd()|.|.|constraint(abc), clv.apparel.cov), "could not be found")
  expect_error(latentAttrition(~pnbd()|.|.|constraint(Gender, abc), clv.apparel.cov), "could not be found")
  expect_error(latentAttrition(~pnbd()|.|.|constraint(Gender)+constraint(abc), clv.apparel.cov), "could not be found")
  expect_error(latentAttrition(~pnbd()|.|.|constraint(abc)+constraint(Gender), clv.apparel.cov), "could not be found")
  expect_error(latentAttrition(~pnbd()|.|.|constraint(abc)+constraint(xzy), clv.apparel.cov), "could not be found")
  expect_error(latentAttrition(~pnbd()|.|.|constraint(Gender)+constraint(xzy), clv.apparel.cov), "could not be found")
})



# nocov, data.frame -----------------------------------------------------------------------------------------------
context("Inputchecks - latentAttrition - no cov, data.frame")

test_that("Fails if no LHS1",{
  skip_on_cran()
  expect_error(latentAttrition(~pnbd(), cdnow), "specify a LHS with data()")
  expect_error(latentAttrition(~bgnbd(), cdnow), "specify a LHS with data()")
})

test_that("Fails if no data in LHS1",{
  skip_on_cran()
  expect_error(latentAttrition(clvdata()~pnbd(), cdnow), "specify exactly data")
  expect_error(latentAttrition(data~pnbd(), cdnow), "specify exactly data")
  expect_error(latentAttrition(abc()~pnbd(), cdnow), "specify exactly data")
  expect_error(latentAttrition(Id+Date+Price~pnbd(), cdnow), "specify exactly data")
})

test_that("Fails if something else than data in LHS1",{
  skip_on_cran()
  expect_error(latentAttrition(data()+1~pnbd(), cdnow), "specify exactly data")
  expect_error(latentAttrition(data()+Id~pnbd(), cdnow), "specify exactly data")
  expect_error(latentAttrition(data()+Id+Price~pnbd(), cdnow), "specify exactly data")
  expect_error(latentAttrition(data()+abc~pnbd(), cdnow), "specify exactly data")
  expect_error(latentAttrition(data()+pnbd()~pnbd(), cdnow), "specify exactly data")
  expect_error(latentAttrition(data()+pnbd()~gg(), cdnow), "specify exactly data")
})

test_that("Fails if unallowed param in data()",{
  skip_on_cran()
  # single params
  expect_error(latentAttrition(data(abc=37)~pnbd(), cdnow), "is not valid input to data()")
  expect_error(latentAttrition(data(estimation.split=37)~pnbd(), cdnow), "is not valid input to data()")
  expect_error(latentAttrition(data(time.unit=w)~pnbd(), cdnow), "is not valid input to data()")
  expect_error(latentAttrition(data(id=Id)~pnbd(), cdnow), "is not valid input to data()")
  expect_error(latentAttrition(data(price=Spending)~pnbd(), cdnow), "is not valid input to data()")
  # with allowed
  expect_error(latentAttrition(data(unit=w, id=Id)~pnbd(), cdnow), "is not valid input to data()")
  expect_error(latentAttrition(data(id=Id, unit=w)~pnbd(), cdnow), "is not valid input to data()")
  expect_error(latentAttrition(data(unit=w, estimation.split=39)~pnbd(), cdnow), "is not valid input to data()")
})


test_that("Fails if not parsable content in data()",{
  skip_on_cran()
  expect_error(latentAttrition(data(split=w)~pnbd(), cdnow), "parsed")
  expect_error(latentAttrition(data(split=NUL)~pnbd(), cdnow), "parsed")
  # All other params are read as chars and passed on (produce errors in clvdata())
})


# static cov, data.frame -----------------------------------------------------------------------------------------------
context("Inputchecks - latentAttrition - static cov, data.frame")

test_that("Fails if cov is not data.frame/table", {
  skip_on_cran()
  # other input
  expect_error(latentAttrition(data()~pnbd()|.|., data=apparelTrans, cov=NULL), "data.frame or data.table")
  expect_error(latentAttrition(data()~pnbd()|.|., data=apparelTrans, cov=124), "data.frame or data.table")
  expect_error(latentAttrition(data()~pnbd()|.|., data=apparelTrans, cov=list(a=1, b=2)), "data.frame or data.table")
})

# test_that("Fails if cov does not have Id", {
#   skip_on_cran()
#   expect_error(latentAttrition(data()~pnbd()|.|., data=apparelTrans, cov=apparelStaticCov[, !"Id"]), "Id")
# })


test_that("Fails if cov data but missing RHS2/3",{
  skip_on_cran()
  expect_error(latentAttrition(data()~pnbd(), data=apparelTrans, cov=apparelStaticCov), "transaction and the lifetime covariates")
  expect_error(latentAttrition(data()~pnbd()|., data=apparelTrans, cov=apparelStaticCov), "transaction and the lifetime covariates")
  expect_error(latentAttrition(data()~pnbd()|Gender, data=apparelTrans, cov=apparelStaticCov), "transaction and the lifetime covariates")
  expect_error(latentAttrition(data()~pnbd()|Gender|regularization(trans=10, life=2), data=apparelTrans, cov=apparelStaticCov), "transaction and the lifetime covariates")
})

test_that("Fails if RHS2&3 but missing cov data", {
  skip_on_cran()
  expect_error(latentAttrition(data()~pnbd()|.|., data=apparelTrans), "covariate data")
  expect_error(latentAttrition(data()~pnbd()|Gender|Gender, data=apparelTrans), "covariate data")
  expect_error(latentAttrition(data()~pnbd()|Gender|., data=apparelTrans), "covariate data")
})

test_that("Fails if RHS2/3 not in cov data",{
  skip_on_cran()
  expect_error(latentAttrition(data()~pnbd()|gender|., data=apparelTrans, cov=apparelStaticCov), "could be found in the data")
  expect_error(latentAttrition(data()~pnbd()|gender|gender, data=apparelTrans, cov=apparelStaticCov), "could be found in the data")
  expect_error(latentAttrition(data()~pnbd()|.|gender, data=apparelTrans, cov=apparelStaticCov), "could be found in the data")
  expect_error(latentAttrition(data()~pnbd()|high.season|., data=apparelTrans, cov=apparelStaticCov), "could be found in the data")
})



