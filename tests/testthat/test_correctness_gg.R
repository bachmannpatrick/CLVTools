# Load required data ---------------------------------------------------------------------------------
data("cdnow")

context("Correctness - Gamma/Gamma - Recover parameters")
expect_silent(clv.cdnow <- clvdata(data.transactions = cdnow,
                                   date.format = "ymd",
                                   time.unit = "Week",
                                   estimation.split = "1997-09-30",
                                   name.id = "Id",
                                   name.date = "Date",
                                   name.price = "Price"))

#fct.testthat.correctness.nocov.newdata.fitting.sample.predicting.full.data.equal(method = gg,
#                                                                                 cdnow = cdnow,
#                                                                                 clv.cdnow = clv.cdnow)

# No cov: Predict ----------------------------------------------------------------------------------------------
context("Correctness - Gamma/Gamma - predict")

fct.testthat.correctness.common.newdata.same.predicting.fitting(clv.fitted = gg(clv.cdnow),
                                                                clv.newdata = clv.cdnow)


