fct.testthat.runability.clvfittedtransactions.plot <- function(clv.fitted, clv.newdata.nohold, clv.newdata.withhold){

  if(clv.data.has.holdout(clv.fitted@clv.data)){
    test_that("Works without prediction end, out-of-the-box", {
      expect_silent(plot(clv.fitted, verbose=FALSE))
    })
  }

  test_that("Works with/without transactions=TRUE/FALSE",{
    skip_on_cran()
    expect_silent(plot(clv.fitted, transactions=TRUE, verbose=FALSE))
    expect_silent(plot(clv.fitted, transactions=FALSE, verbose=FALSE))
  })

  test_that("Works for cumulative=TRUE/FALSE",{
    skip_on_cran()
    expect_silent(plot(clv.fitted, cumulative=TRUE, verbose=FALSE))
    expect_silent(plot(clv.fitted, cumulative=FALSE, verbose=FALSE))
  })

  test_that("Works for verbose=TRUE",{
    skip_on_cran()
    expect_message(plot(clv.fitted, verbose=TRUE), "until")
  })

  test_that("Works for plot=FALSE and always has 0 repeat trans and expectation on first",{
    skip_on_cran()
    expect_silent(dt.plot <- plot(clv.fitted, plot=FALSE, verbose=FALSE))
    expect_true(isTRUE(all.equal( unlist(dt.plot[period.until == min(period.until), 2:3]),
                                  c(0,0), check.attributes = FALSE)))
  })

  test_that("Works for label set to text",{
    skip_on_cran()
    expect_silent(plot(clv.fitted, label="ABC", verbose=FALSE))
  })

  test_that("Works for prediction.end in different formats, after holdout", {
    skip_on_cran()
    # prediction.end as character, Date, posix, numeric
    expect_silent(plot(clv.fitted, prediction.end = as.character(clv.fitted@clv.data@clv.time@timepoint.holdout.end+lubridate::weeks(26)), verbose=FALSE))
    expect_silent(plot(clv.fitted, prediction.end = as.Date(clv.fitted@clv.data@clv.time@timepoint.holdout.end+lubridate::weeks(26)), verbose=FALSE))
    expect_silent(plot(clv.fitted, prediction.end = clv.fitted@clv.data@clv.time@timepoint.holdout.end+lubridate::weeks(26), verbose=FALSE))
    expect_silent(plot(clv.fitted, prediction.end = as.integer(clv.fitted@clv.data@clv.time@holdout.period.in.tu)+26, verbose=FALSE))
  })


  if(clv.data.has.holdout(clv.fitted@clv.data)){
    test_that("Warns if ends in holdout", {
      skip_on_cran()
      # prediction.end as character, Date, posix, numeric
      expect_warning(plot(clv.fitted, prediction.end = as.character(clv.fitted@clv.data@clv.time@timepoint.holdout.end-lubridate::weeks(6))),
                     regexp = "Not plotting full holdout period.")
      expect_warning(plot(clv.fitted, prediction.end = as.Date(clv.fitted@clv.data@clv.time@timepoint.holdout.end-lubridate::weeks(6))),
                     regexp = "Not plotting full holdout period.")
      expect_warning(plot(clv.fitted, prediction.end = 6),
                     regexp = "Not plotting full holdout period.")
    })
  }

  test_that("Works for negative prediction.end", {
    skip_on_cran()
    if(clv.data.has.holdout(clv.fitted@clv.data)){
      # With holdout: Warn about holdout period
      expect_warning(plot(clv.fitted, prediction.end=-2,  verbose=FALSE),
                     regexp = "Not plotting full holdout period.")
    }else{
      # No holdout: Warn about estimation period
      expect_warning(plot(clv.fitted, prediction.end=-2,  verbose=FALSE),
                     regexp = "Not plotting full estimation period.")
    }
  })


  test_that("Correct return structure", {
    skip_on_cran()
    expect_silent(gg.plot <- plot(clv.fitted, plot=TRUE, verbose=FALSE))
    expect_s3_class(gg.plot, "ggplot")

    expect_silent(dt.plot <- plot(clv.fitted, plot=FALSE, verbose=FALSE))
    expect_s3_class(dt.plot, "data.table")

    # expect_true(all(c("period.until", "variable", "value") %in% colnames(dt.plot)))
    expect_true(all(c("period.until", "Actual Number of Repeat Transactions", clv.fitted@clv.model@name.model)
                    %in% colnames(dt.plot)))
    # Num repeat trans may have some NA if prediction.end beyond holdout.end
    expect_false(anyNA(dt.plot[, c("period.until", clv.fitted@clv.model@name.model)]))

    # expect_silent(dt.plot <- plot(clv.fitted, transactions = TRUE, plot=FALSE, verbose=FALSE))
    expect_true(ncol(dt.plot) == 3)

    expect_silent(dt.plot <- plot(clv.fitted, transactions = FALSE, plot=FALSE, verbose=FALSE))
    expect_true(ncol(dt.plot) == 2)

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

  test_that("Works with newdata", {
    skip_on_cran()
    expect_silent(dt.plot <- plot(clv.fitted, newdata = clv.newdata.nohold, plot=FALSE, verbose=FALSE))
    # expect_false(anyNA(dt.plot))

    expect_silent(dt.plot <- plot(clv.fitted, newdata = clv.newdata.withhold, plot=FALSE, verbose=FALSE))
    # expect_false(anyNA(dt.plot))

    # **TODO: Add test to predict that predictions for all Ids in newdata
    # expect_true(dt.plot[])
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



fct.testthat.runability.clvfittedspending.plot <- function(fitted.spending){

  test_that("Works out-of-the-box", {
    skip_on_cran()
    expect_silent(res.plot <- plot(fitted.spending))
  })

  test_that("Different result for different n", {
    skip_on_cran()
    expect_silent(res.plot.10 <- plot(fitted.spending, n = 10, verbose=FALSE))
    expect_silent(res.plot.20 <- plot(fitted.spending, n = 20, verbose=FALSE))
    expect_false(isTRUE(all.equal(res.plot.10, res.plot.20)))
  })
}
