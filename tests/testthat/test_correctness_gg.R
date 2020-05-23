# Load required data ---------------------------------------------------------------------------------
data("cdnow")

context("Correctness - Gamma/Gamma - Recover parameters")
clv.cdnow <- clvdata(cdnow,
                     date.format="ymd",
                     time.unit = "week",
                     estimation.split = "1997-09-30",
                     name.id = "Id",
                     name.date = "Date",
                     name.price = "Price")

l.args <- list(clv.data = clv.cdnow, verbose = FALSE)
# Fit on sample only
fitted.nocov.sample <- do.call(what = gg, args = l.args)

# Prediction.end
# Predict sample only
expect_silent(dt.predict.sample <- predict(fitted.nocov.sample, verbose=FALSE,
                                           predict.spending=FALSE, prediction.end="1998-07-06"))
# Predict on full
expect_silent(dt.predict.full <- predict(fitted.nocov.sample, newdata = clv.cdnow, verbose=FALSE,
                                         predict.spending=FALSE, prediction.end="1998-07-06"))


expect_true(nrow(dt.predict.full) == length(unique(cdnow$Id)))

# The sample ones should be the exact same ones in the full
expect_true(isTRUE(all.equal(dt.predict.sample,
                             dt.predict.full[Id %in% dt.predict.sample$Id])))

# No cov: Predict ----------------------------------------------------------------------------------------------
context("Correctness - Gamma/Gamma - predict")

fct.testthat.correctness.common.newdata.same.predicting.fitting(clv.fitted = gg(clv.cdnow),
                                                                clv.newdata = clv.cdnow)


