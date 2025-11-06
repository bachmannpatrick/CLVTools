fct.testthat.runability.clvfittedtransactions.plot.common <- function(clv.fitted, which){
  test_that("Works with/without transactions=TRUE/FALSE",{
    skip_on_cran()
    expect_silent(plot(clv.fitted, which=which, transactions=TRUE, verbose=FALSE))
    expect_silent(plot(clv.fitted, which=which, transactions=FALSE, verbose=FALSE))
  })

  test_that("Works for label set to text",{
    skip_on_cran()
    expect_silent(plot(clv.fitted, which=which, label="ABC", verbose=FALSE))
  })
}

fct.testthat.runability.clvfittedtransactions.plot.other.models <- function(clv.fitted, which, clv.fitted.other){
  skip_on_cran()

  # For unknown reasons the if(clv.data.has.holdout()) seems to not work in the 3rd edition of testthat and branches wrongly
  local_edition(2)

  # same tests but separate for pmf and tracking because use prediction.end=1 to speed up tests which also requires changing expect_silent() to expect_warning()
  # otherwise takes unacceptably long
  fct.expect.other.models <- function(fitted, ...){
    l.args <- list(...)
    l.default.args <- list(x=fitted, which=which, verbose=FALSE)
    if(tolower(which) == "tracking"){
      # speed up: short plotting range
      l.default.args[["prediction.end"]] <- 1

      newdata.has.holdout <- FALSE
      if("newdata" %in% names(l.args)){
        if(clv.data.has.holdout(l.args[["newdata"]])){
          newdata.has.holdout <- TRUE
        }
      }
      if(clv.data.has.holdout(fitted@clv.data) | newdata.has.holdout){
        return(expect_warning(do.call(plot, c(l.args, l.default.args)), regexp = "full holdout"))
      }else{
        return(expect_silent(do.call(plot, c(l.args, l.default.args))))
      }
    }
    if(tolower(which)=="pmf"){
      return(expect_silent(do.call(plot, c(l.args, l.default.args))))
    }
  }

  test_that("works withlabel",{
    skip_on_cran()
    # without label is run in most other cases
    # fct.expect.other.models(clv.fitted, other.models=list(clv.fitted, clv.fitted.other), label=c())
    dt.plot <- fct.expect.other.models(clv.fitted, other.models=list(clv.fitted.other), label=c("this", "other"), plot=FALSE)
    expect_setequal(dt.plot[, unique(variable)], c("Actual", "this", "other"))
  })

  test_that("works with colors",{
    skip_on_cran()
    fct.expect.other.models(clv.fitted, other.models=list("blue"=clv.fitted, "#00ff00"=clv.fitted.other))
    # partly named other models
    fct.expect.other.models(clv.fitted, other.models=list(blue=clv.fitted, clv.fitted.other))
  })

  test_that("works with newdata",{
    skip_on_cran()
    fct.expect.other.models(clv.fitted, other.models=list(clv.fitted.other), newdata=clv.fitted@clv.data)
  })

  test_that("works with additional parameters",{
    skip_on_cran()
    if(tolower(which)=="pmf"){
      fct.expect.other.models(clv.fitted, other.models=list(clv.fitted.other), trans.bins=0:3, label.remaining="rest", calculate.remaining=TRUE, transactions=TRUE)
    }
    if(tolower(which) == "tracking"){
      fct.expect.other.models(clv.fitted, other.models=list(clv.fitted.other), cumulative=TRUE, transactions=TRUE)
    }
  })
}



fct.testthat.runability.clvfittedtransactions.plot.tracking <- function(clv.fitted, clv.newdata.nohold, clv.newdata.withhold){
  # For unknown reasons the if(clv.data.has.holdout()) seems to not work in the 3rd edition of testthat and branches wrongly
  local_edition(2)

  # short prediction.end to speed up tests
  fct.expect.plot.tracking <- function(fitted, ...){
    l.args <- list(...)
    l.default.args <- list(x=fitted, prediction.end=3, verbose=FALSE, plot=FALSE)
    l.default.args <- modifyList(l.default.args, l.args)

    # return(expect_no_error(do.call(plot, l.default.args)))
    if(clv.data.has.holdout(fitted@clv.data)){
      return(expect_warning(do.call(plot, l.default.args), regexp = "full holdout"))
    }else{
      return(expect_silent(do.call(plot, l.default.args)))
    }
  }

  test_that("Works for verbose=TRUE",{
    skip_on_cran()
    expect_message(plot(clv.fitted, verbose=TRUE))
  })

  if(clv.data.has.holdout(clv.fitted@clv.data)){
    test_that("Works without prediction end, out-of-the-box", {
      expect_silent(plot(clv.fitted, verbose=FALSE))
    })
  }

  test_that("Works for cumulative=TRUE",{
    skip_on_cran()
    fct.expect.plot.tracking(clv.fitted, cumulative=TRUE)
    # fct.expect.plot.tracking(clv.fitted, cumulative=FALSE) # is default anyways
  })

  test_that("Works for plot=FALSE and always has 0 repeat trans and expectation on first",{
    skip_on_cran()
    dt.plot <- fct.expect.plot.tracking(clv.fitted, plot=FALSE)
    expect_true(isTRUE(all.equal(dt.plot[period.until == min(period.until), value], c(0,0))))
  })

  test_that("Works with newdata", {
    skip_on_cran()
    expect_silent(dt.plot <- plot(clv.fitted, newdata = clv.newdata.nohold, prediction.end=3, plot=FALSE, verbose=FALSE))
    # Since introducing data.end: Actuals after last transaction are kept and contain NA. Only check model
    expect_false(anyNA(dt.plot[variable != "Actual"]))

    expect_warning(dt.plot <- plot(clv.fitted, newdata = clv.newdata.withhold, prediction.end=3, plot=FALSE, verbose=FALSE), regexp = "full holdout")
    # Since introducing data.end: Actuals after last transaction are kept and contain NA. Only check model
    expect_false(anyNA(dt.plot[variable != "Actual"]))
  })

  test_that("Works for prediction.end in different formats, after holdout", {
    skip_on_cran()
    if(is(clv.fitted@clv.data@clv.time, "clv.time.date")){
      tp.end <- clv.fitted@clv.data@clv.time@timepoint.holdout.end+lubridate::days(26)
    }else{
      tp.end <- clv.fitted@clv.data@clv.time@timepoint.holdout.end+lubridate::hours(26)
    }
    # prediction.end as character, Date, posix, numeric (fct.expect.plot.tracking not working here)
    expect_silent(plot(clv.fitted, prediction.end = as.character(tp.end), verbose=FALSE))
    expect_silent(plot(clv.fitted, prediction.end = as.Date(tp.end, tz=""), verbose=FALSE))
    expect_silent(plot(clv.fitted, prediction.end = tp.end, verbose=FALSE))
    expect_silent(plot(clv.fitted, prediction.end = as.integer(clv.fitted@clv.data@clv.time@holdout.period.in.tu)+4, verbose=FALSE))
  })

  if(clv.data.has.holdout(clv.fitted@clv.data)){
    test_that("Warns if ends in holdout", {
      skip_on_cran()

      if(is(clv.fitted@clv.data@clv.time, "clv.time.date")){
        tp.end <- clv.fitted@clv.data@clv.time@timepoint.estimation.end+lubridate::days(10)
      }else{
        tp.end <- clv.fitted@clv.data@clv.time@timepoint.estimation.end+lubridate::hours(10)
      }

      # prediction.end as character, Date, posix, numeric
      expect_warning(plot(clv.fitted, prediction.end = as.character(tp.end)),
                     regexp = "Not plotting full holdout period.")
      expect_warning(plot(clv.fitted, prediction.end = as.Date(tp.end)),
                     regexp = "Not plotting full holdout period.")
      expect_warning(plot(clv.fitted, prediction.end = 3),
                     regexp = "Not plotting full holdout period.")
    })
  }

  test_that("Works for negative prediction.end", {
    skip_on_cran()
    if(clv.data.has.holdout(clv.fitted@clv.data)){
      # With holdout: Warn about holdout period
      expect_warning(plot(clv.fitted, prediction.end=-20,  verbose=FALSE),
                     regexp = "Not plotting full holdout period.")
    }else{
      # No holdout: Warn about estimation period
      expect_warning(plot(clv.fitted, prediction.end=-20,  verbose=FALSE),
                     regexp = "Not plotting full estimation period.")
    }
  })

  test_that("Correct return structure", {
    skip_on_cran()

    # expect_silent(gg.plot <- plot(clv.fitted, prediction.end=6, plot=TRUE, verbose=FALSE))
    gg.plot <- fct.expect.plot.tracking(clv.fitted, plot=TRUE)
    expect_s3_class(gg.plot, "ggplot")

    dt.plot <- fct.expect.plot.tracking(clv.fitted, plot=FALSE)
    # expect_silent(dt.plot <- plot(clv.fitted, prediction.end=6, plot=FALSE, verbose=FALSE))
    expect_s3_class(dt.plot, "data.table")
    expect_true(all(c("period.until", "variable", "value") %in% colnames(dt.plot)))
    expect_true(all(c("Actual", clv.fitted@clv.model@name.model) %in% dt.plot[, variable]))

    # Num repeat trans may have some NA if prediction.end beyond holdout.end
    expect_false(anyNA(dt.plot[, c("period.until", clv.fitted@clv.model@name.model)]))
    expect_true(ncol(dt.plot) == 3)

    # expect_silent(dt.plot <- plot(clv.fitted, transactions = TRUE, plot=FALSE, verbose=FALSE))
    # expect_silent(dt.plot <- plot(clv.fitted, transactions = FALSE, plot=FALSE, verbose=FALSE))
    # dt.plot <- fct.expect(plot=FALSE, transactions = TRUE)
    # expect_true(ncol(dt.plot) == 3)

    # Same dates/periods for transactions and expectations
    #   = also not some expectations missing
    # **TODO: Add back once prediction periods are corrected.
    #   This needs to be same if only holdout is plotted
    # expect_true(isTRUE(all.equal(dt.plot[variable == unique(variable)[1], "period.until"],
    #                       dt.plot[variable == unique(variable)[2], "period.until"])))
    # expect_true(all(1 == clv.time.interval.in.number.tu(clv.time=clv.fitted@clv.data@clv.time,
    #                                                interv = lubridate::int_diff(dt.plot[variable == unique(variable)[1],
    #                                                                                     period.until]))))
    # expect_true(all(1 == clv.time.interval.in.number.tu(clv.time=clv.fitted@clv.data@clv.time,
    #                                                     interv = lubridate::int_diff(dt.plot[variable == unique(variable)[2],
    #                                                                                          period.until]))))
    # **MAYBE: Check that its the correct dates

  })


  # **   test_that("Correctness: Correct date range in plot data",{})
  # **TODO: TEST THIS DATE TIME BS !! - also, additionally FOR DIFFERENT time.units!
  # test_that("Same values for prediction.end as Date, char, number, or Posixct",{
  #   end.posix <- clv.fitted@clv.data@date.holdout.end+lubridate::weeks(4)
  #   end.date <- as.Date(end.posix, tz="")
  #   end.char <- as.character(end.posix)
  #   expect_silent(res.num   <- plot(clv.fitted, prediction.end = 4, verbose=FALSE))
  #   expect_silent(res.char  <- plot(clv.fitted, prediction.end = as.character(clv.fitted@clv.data@date.holdout.end+lubridate::weeks(4)), verbose=FALSE))
  #   expect_silent(res.Date  <- plot(clv.fitted, prediction.end = as.Date(clv.fitted@clv.data@date.holdout.end+lubridate::weeks(4)), verbose=FALSE))
  #   expect_silent(res.posix <- plot(clv.fitted, prediction.end = end.posix, verbose=FALSE))
  #
  #   expect_equal(res.char, res.num)
  #   expect_equal(res.num, res.Date)
  #   expect_equal(res.Date, res.posix)
  #
  #   expect_true(res.char[, .(N=data.table::uniqueN(period.until)), by="variable"][, diff(N) == 4])
  #   expect_true(res.char[, .(N=data.table::uniqueN(period.until)), by="variable"][, diff(N) == 4])
  # })
}

fct.testthat.runability.clvfittedtransactions.plot.pmf <- function(clv.fitted, clv.newdata.nohold, clv.newdata.withhold){
  test_that("Works for verbose=TRUE",{
    skip_on_cran()
    expect_silent(plot(clv.fitted, which="pmf", verbose=TRUE))
  })

  test_that("Actual ploting works for different trans.bins", {
    expect_silent(plot(clv.fitted, which="pmf", trans.bins=0:5, verbose=FALSE))
    expect_silent(plot(clv.fitted, which="pmf", trans.bins=11:15, verbose=FALSE))
  })

  test_that("Different trans.bins give different results", {
    expect_false(isTRUE(all.equal(plot(clv.fitted, which="pmf", trans.bins=0:5, calculate.remaining=FALSE, plot=FALSE, verbose=FALSE),
                                  plot(clv.fitted, which="pmf", trans.bins=11:15, calculate.remaining=FALSE, plot=FALSE, verbose=FALSE))))
  })

  test_that("Correct trans bins returned", {
    expect_silent(dt.plot <- plot(clv.fitted, which="pmf", trans.bins=0:5,
                                  calculate.remaining=FALSE, plot=FALSE, verbose=FALSE))
    expect_true(is.factor(dt.plot$num.transactions))
    expect_true(is.ordered(dt.plot$num.transactions))
    expect_setequal(levels(dt.plot$num.transactions), as.character(0:5))
  })

  test_that("Not calculating remaining works", {
    expect_silent(dt.plot <- plot(clv.fitted, which="pmf", calculate.remaining=FALSE, label.remaining="Others",
                                  plot=FALSE, verbose=FALSE))
    expect_false("Others" %in% levels(dt.plot$num.transactions))
  })

  test_that("Different label.remaining works", {
    expect_silent(dt.plot <- plot(clv.fitted, which="pmf", trans.bins=0:5,
                                  label.remaining = "Others", plot=FALSE, verbose=FALSE))
    expect_setequal(c(as.character(0:5), "Others"), levels(dt.plot$num.transactions))

  })

  test_that("Newdata leads to different results", {
    expect_silent(dt.plot <- plot(clv.fitted, which="pmf", plot=FALSE, verbose=FALSE))
    expect_false(isTRUE(all.equal(dt.plot,
                                  plot(clv.fitted, which="pmf", newdata=clv.newdata.nohold, plot=FALSE, verbose=FALSE))))
    expect_false(isTRUE(all.equal(dt.plot,
                                  plot(clv.fitted, which="pmf", newdata=clv.newdata.withhold, plot=FALSE, verbose=FALSE))))
  })

}

fct.testthat.runability.clvfittedtransactions.plot <- function(clv.fitted, clv.newdata.nohold, clv.newdata.withhold){

  fct.testthat.runability.clvfittedtransactions.plot.common(clv.fitted=clv.fitted, which="tracking")

  fct.testthat.runability.clvfittedtransactions.plot.tracking(clv.fitted=clv.fitted, clv.newdata.nohold=clv.newdata.nohold, clv.newdata.withhold=clv.newdata.withhold)

  fct.testthat.runability.clvfittedtransactions.plot.other.models(clv.fitted = clv.fitted, clv.fitted.other=clv.fitted, which="tracking")

  test_that("fct.helper.has.pmf", {expect_true(TRUE)})
  if(fct.helper.has.pmf(clv.fitted)){

    fct.testthat.runability.clvfittedtransactions.plot.common(clv.fitted=clv.fitted, which="pmf")

    fct.testthat.runability.clvfittedtransactions.plot.other.models(clv.fitted = clv.fitted, clv.fitted.other=clv.fitted, which="pmf")

    fct.testthat.runability.clvfittedtransactions.plot.pmf(clv.fitted=clv.fitted, clv.newdata.nohold=clv.newdata.nohold, clv.newdata.withhold=clv.newdata.withhold)
  }
}



fct.testthat.runability.clvfittedspending.plot <- function(fitted.spending){

  test_that("Works out-of-the-box", {
    skip_on_cran()
    expect_silent(res.plot <- plot(fitted.spending))
  })

  test_that("Different result for different n", {
    skip_on_cran()
    expect_silent(res.plot.10 <- plot(fitted.spending, n = 10, verbose=FALSE))
    expect_silent(res.plot.20 <- plot(fitted.spending, n = 20, verbose=FALSE))
    expect_false(isTRUE(all.equal(res.plot.10@layers, res.plot.20@layers)))
  })
}
