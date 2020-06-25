skip_on_cran()
data("cdnow")

context("Inputchecks - clvfitted - plot")

expect_silent(pnbd.cdnow <- pnbd(clvdata(cdnow, date.format = "ymd", time.unit = "w"), verbose = FALSE))

l.std.args <- list(pnbd.cdnow, prediction.end=6)
.fct.helper.inputchecks.single.logical(fct = plot, l.std.args = l.std.args, name.param = "transactions")
.fct.helper.inputchecks.single.logical(fct = plot, l.std.args = l.std.args, name.param = "cumulative")
.fct.helper.inputchecks.single.logical(fct = plot, l.std.args = l.std.args, name.param = "plot")
.fct.helper.inputchecks.single.logical(fct = plot, l.std.args = l.std.args, name.param = "verbose")


# **TODO: newdata
# test_that("Fails for prediction.end not char/date/posix", {
#   # **TODO: Regexp
#   expect_error(plot(pnbd.cdnow, prediction.end = list("2004-01-01")))
#   expect_error(plot(pnbd.cdnow, prediction.end = data.frame("2004-01-01")))
# })
# test_that("Fails for prediction.end NA", {
#   expect_error(plot(pnbd.cdnow, prediction.end = NA_real_), regexp = "NA")
#   expect_error(plot(pnbd.cdnow, prediction.end = NA_integer_), regexp = "NA")
#   expect_error(plot(pnbd.cdnow, prediction.end = NA_character_), regexp = "NA")
# })

# test_that("Fails for multiple prediction.end", {
#   expect_error(plot(pnbd.cdnow, prediction.end = c(1,2)), regexp = "single")
#   expect_error(plot(pnbd.cdnow, prediction.end = c(4,5)), regexp = "single")
#   expect_error(plot(pnbd.cdnow, prediction.end = 1:10), regexp = "single")
#
#   expect_error(plot(pnbd.cdnow, prediction.end = c("2004-01-01", "2003-01-01")), regexp = "single")
#   expect_error(plot(pnbd.cdnow, prediction.end = c("2004-01-01", "2004-01-01")), regexp = "single")
#
#   expect_error(plot(pnbd.cdnow, prediction.end = c(as.Date("2004-01-01"), as.Date("2004-01-01"))), regexp = "single")
#   expect_error(plot(pnbd.cdnow, prediction.end = c(as.Date("2004-01-01"), as.Date("2003-01-01"))), regexp = "single")
#
#   expect_error(plot(pnbd.cdnow, prediction.end = as.POSIXct(c(as.Date("2004-01-01"), as.Date("2004-01-01")))), regexp = "single")
#   expect_error(plot(pnbd.cdnow, prediction.end = as.POSIXct(c(as.Date("2004-01-01"), as.Date("2003-01-01")))), regexp = "single")
# })

# test_that("Fails if prediciton.end is not in initial date.format", {
#   expect_error(plot(pnbd.cdnow, prediction.end = format(pnbd.cdnow@clv.data@date.holdout.end+lubridate::weeks(4), "%m-%Y-%d")))
#   expect_error(plot(pnbd.cdnow, prediction.end = format(pnbd.cdnow@clv.data@date.holdout.end+lubridate::weeks(4), "%m-%d-%Y")))
#   expect_error(plot(pnbd.cdnow, prediction.end = format(pnbd.cdnow@clv.data@date.holdout.end+lubridate::weeks(4), "%d-%Y-%m")))
#   expect_error(plot(pnbd.cdnow, prediction.end = format(pnbd.cdnow@clv.data@date.holdout.end+lubridate::weeks(4), "%d-%m-%Y")))
#   expect_error(plot(pnbd.cdnow, prediction.end = format(pnbd.cdnow@clv.data@date.holdout.end+lubridate::weeks(4), "%Y-%d-%m")))
# })
#
#
