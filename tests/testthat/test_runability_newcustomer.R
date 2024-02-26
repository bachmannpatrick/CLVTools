skip_on_cran()

# Load required data and fit all models ----------------------------------------------------------------------------
data("cdnow")
data("apparelTrans")
data("apparelStaticCov")
data("apparelDynCov")

# Create clvdata objects with all types of covariates

clv.cdnow <- fct.helper.create.clvdata.cdnow(cdnow)

clv.apparel.static <- fct.helper.create.clvdata.apparel.staticcov(
  data.apparelTrans = apparelTrans,
  data.apparelStaticCov = apparelStaticCov,
  names.cov.life = c("Channel"),
  names.cov.trans = c("Gender", "Channel"),
  estimation.split = 52
)

# Fit all available models
optimx.args.fast <- list(method = "Nelder-Mead", itnmax=10, hessian=FALSE, control=list(kkt=FALSE))

expect_warning(p.cdnow <- pnbd(clv.cdnow, verbose = FALSE, optimx.args = optimx.args.fast), 'Hessian')
expect_warning(bg.cdnow <- bgnbd(clv.cdnow, verbose = FALSE, optimx.args = optimx.args.fast), 'Hessian')
expect_warning(ggom.cdnow <- ggomnbd(clv.cdnow, verbose = FALSE, optimx.args = optimx.args.fast), 'Hessian')

expect_warning(p.apparel.static <- pnbd(clv.apparel.static, verbose = FALSE, optimx.args = optimx.args.fast), 'Hessian')
expect_warning(bg.apparel.static <- bgnbd(clv.apparel.static, verbose = FALSE, optimx.args = optimx.args.fast), 'Hessian')
expect_warning(ggom.apparel.static <- ggomnbd(clv.apparel.static, verbose = FALSE, optimx.args = optimx.args.fast), 'Hessian')

p.apparel.dyn <- fct.helper.dyncov.quickfit.apparel.data(
  data.apparelTrans=apparelTrans,
  data.apparelDynCov=apparelDynCov,
  hessian=FALSE
)



# create newcustomer objects for no covariate, static covariate, and dynamic covariate

test_that("New customer prediction runs for all no cov models", {
  clv.newcustomer <- newcustomer(num.periods = 6.78)
  expect_silent(predict(p.cdnow, newdata=clv.newcustomer, verbose=FALSE))
  expect_silent(predict(bg.cdnow, newdata=clv.newcustomer, verbose=FALSE))
  expect_silent(predict(ggom.cdnow, newdata=clv.newcustomer, verbose=FALSE))
})

test_that("New customer prediction runs for all static models", {
  clv.newcustomer <- newcustomer.static(
    num.periods = 6.78,
    data.cov.life = data.frame(
      Channel=2.34
    ),
    data.cov.trans = data.frame(
      Channel=-0.23,
      Gender=0.56
    )
  )
  expect_silent(predict(p.apparel.static, newdata=clv.newcustomer, verbose=FALSE))
  expect_silent(predict(bg.apparel.static, newdata=clv.newcustomer, verbose=FALSE))
  expect_silent(predict(ggom.apparel.static, newdata=clv.newcustomer, verbose=FALSE))
})


test_that("New customer prediction runs for dyncov", {
  # Choose period outside fitting period
  expect_silent(predict(
    p.apparel.dyn,
    newdata=newcustomer.dynamic(
      num.periods=2.12,
      data.cov.life=data.frame(
        Cov.Date=c("2051-02-12", "2051-02-19", "2051-02-26"),
        Gender=c(0, 0, 0),
        Channel=c(-12.2, 0, 2.4),
        High.Season=c(0, 0, 0)),
      data.cov.trans=data.frame(
        Cov.Date=c("2051-02-12", "2051-02-19", "2051-02-26"),
        Gender=c(0, 0, 0),
        Channel=c(0, 0, 2),
        High.Season=c(0, 0, 0)),
      first.transaction = "2051-02-16"
    ), verbose=FALSE))
})
