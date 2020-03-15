library("testthat")
library("CLVTools")
data("cdnow")

clv.apparel <- clvdata(cdnow,
                       date.format="ymd",
                       time.unit = "week",
                       estimation.split = "1997-09-30",
                       name.id = "Id",
                       name.date = "Date",
                       name.price = "Price")

est.bgnbd <- bgnbd(clv.data = clv.apparel, start.params.model = c(r = 1, alpha = 3, a = 1, b= 3),
                   optimx.args = list(control=list(trace=5) ))

summary(est.bgnbd)
coef(est.bgnbd)

context("Correctness - BG/NBD")

test_that("Apparel nocov correct coefs and SE", {
   expect_equal(coef(est.bgnbd), c(r = 0.2425945, alpha = 4.4136019, a = 0.7929199, b = 2.4258881))
})
