# clv.bootstrapped.apply -------------------------------------------------------

# with holdout to not require `prediction.end` and can use same call to `predict`
# also for spending model
bg.cdnow <- fit.cdnow(model=bgnbd)

test_that("clv.bootstrapped.apply(num.boot) may only be an integer > 0", {
  fn.expect.num.boot.greater0 <- function(n){
    expect_error(clv.bootstrapped.apply(
      object = bg.cdnow,
      num.boots = n,
      fn.boot.apply = function(x){x},
      fn.sample = NULL
    ), 'num.boots')
  }

  fn.expect.num.boot.greater0()
  fn.expect.num.boot.greater0(list(5))
  fn.expect.num.boot.greater0(NA_integer_)
  fn.expect.num.boot.greater0(-1)
  fn.expect.num.boot.greater0(0)
})


test_that("clv.bootstrapped.apply(fn.boot.apply) may only be a function", {
  fn.expect.fn.apply <- function(fn.apply){
    expect_error(clv.bootstrapped.apply(
      object = bg.cdnow,
      num.boots = 1000,
      fn.boot.apply = fn.apply,
      fn.sample = NULL
    ), 'fn.boot.apply')
  }

  fn.expect.fn.apply()
  fn.expect.fn.apply(1)
  fn.expect.fn.apply(NA)
  fn.expect.fn.apply(NULL)
  fn.expect.fn.apply(list(sum))
})


test_that("clv.bootstrapped.apply(fn.sample) may only be a function", {
  fn.expect.fn.sample <- function(fn.sample){
    expect_error(clv.bootstrapped.apply(
      object = bg.cdnow,
      num.boots = 1000,
      fn.boot.apply = function(x){x},
      fn.sample = fn.sample
    ), 'fn.sample')
  }

  fn.expect.fn.sample()
  fn.expect.fn.sample(1)
  fn.expect.fn.sample(NA)
  # fn.expect.fn.sample(NULL) # May be NULL
  fn.expect.fn.sample(list(sum))
})


# predict ----------------------------------------------------------------------
gg.cdnow <- fit.cdnow(model=gg)

# . transaction model ----------------------------------------------------------

test_that("predict(uncertainty) one of allowed inputs", {
  fn.expect.predict.uncertainty <- function(clv.fitted, u){
    expect_error(predict(
      clv.fitted,
      uncertainty=u,
    ), regexp = "uncertainty")
  }

  for(clv.fitted in list(bg.cdnow, gg.cdnow)){
    fn.expect.predict.uncertainty(clv.fitted, NULL)
    fn.expect.predict.uncertainty(clv.fitted, NA)
    fn.expect.predict.uncertainty(clv.fitted, FALSE)
    fn.expect.predict.uncertainty(clv.fitted, TRUE)
    fn.expect.predict.uncertainty(clv.fitted, "yes")
    fn.expect.predict.uncertainty(clv.fitted, "bootstrapping")
  }
})


test_that("predict(level) is single numeric in range [0, 1]", {
  fn.expect.predict.level <- function(clv.fitted, l){
    expect_error(predict(
      clv.fitted,
      uncertainty='boots',
      level=l
    ), regexp = "level")
  }

  for(clv.fitted in list(bg.cdnow, gg.cdnow)){
    fn.expect.predict.level(clv.fitted, l=)
    fn.expect.predict.level(clv.fitted, NULL)
    fn.expect.predict.level(clv.fitted, NA)
    fn.expect.predict.level(clv.fitted, NA_real_)
    fn.expect.predict.level(clv.fitted, -1)
    fn.expect.predict.level(clv.fitted, -0.4)
    fn.expect.predict.level(clv.fitted, 1.1)
  }

})

test_that("predict(num.boots) is single positive integer", {
  fn.expect.predict.num.boots <- function(clv.fitted, n){
    expect_error(predict(
      clv.fitted,
      uncertainty='boots',
      num.boots=n
    ), regexp = "num.boots")
  }

  for(clv.fitted in list(bg.cdnow, gg.cdnow)){
    fn.expect.predict.num.boots(clv.fitted)
    fn.expect.predict.num.boots(clv.fitted, NULL)
    fn.expect.predict.num.boots(clv.fitted, NA)
    fn.expect.predict.num.boots(clv.fitted, NA_integer_)
    fn.expect.predict.num.boots(clv.fitted, 0)
    fn.expect.predict.num.boots(clv.fitted, -1)
    fn.expect.predict.num.boots(clv.fitted, 1.23)
    fn.expect.predict.num.boots(clv.fitted, 0.23)
  }
})




