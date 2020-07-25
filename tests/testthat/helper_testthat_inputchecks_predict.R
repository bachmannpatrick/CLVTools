fct.testthat.inputchecks.clvfitted.predict.newdata.not.clvdata <- function(clv.fitted, data.cdnow){
  test_that("Fails if newdata not a clv.data object", {
    skip_on_cran()
    expect_error(predict(clv.fitted, newdata = NA_character_), regexp = "needs to be a clv data object")
    expect_error(predict(clv.fitted, newdata = character()), regexp = "needs to be a clv data object")
    expect_error(predict(clv.fitted, newdata = "abc"), regexp = "needs to be a clv data object")
    expect_error(predict(clv.fitted, newdata = 123), regexp = "needs to be a clv data object")
    expect_error(predict(clv.fitted, newdata = data.cdnow), regexp = "needs to be a clv data object")
    expect_error(predict(clv.fitted, newdata = unlist(data.cdnow)), regexp = "needs to be a clv data object")
  })
}


fct.testthat.inputchecks.clvfitted.na.in.prediction.params.model <- function(s3method, clv.fitted){
  test_that("Fails if prediction.params.model are NA", {
    skip_on_cran()

    clv.fitted@prediction.params.model[2] <- NA_real_
    if(is(clv.fitted, "clv.fitted.transactions")){
      expect_error(do.call(what = s3method, args = list(clv.fitted, prediction.end = 6)), regexp = "NAs in the estimated model")
    }else{
      # Spending model
      expect_error(do.call(what = s3method, args = list(clv.fitted)), regexp = "NAs in the estimated model")
    }
  })
}
