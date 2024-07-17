# clv.bootstrapped.apply -------------------------------------------------------

bg.cdnow <- fit.cdnow(model=bgnbd)

test_that("clv.bootstrapped.apply(num.boot) may only be an integer > 0", {
  fn.expect.num.boot.greater0 <- function(n){
    expect_error(clv.bootstrapped.apply(
      object = bg.cdnow,
      num.boot = n,
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
      num.boot = 1000,
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
      num.boot = 1000,
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
