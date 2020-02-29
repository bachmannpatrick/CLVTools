library("CLVTools")
data("apparelTrans")

clv.apparel <- clvdata(apparelTrans,
                       date.format="ymd",
                       time.unit = "week",
                       estimation.split = 52,
                       name.id = "Id",
                       name.date = "Date",
                       name.price = "Price")

est.bgnbd <- bgnbd(clv.data = clv.apparel, start.params.model = c(r = 1, alpha = 3, a = 1, b= 3))

summary(est.bgnbd@cbs)

summary(est.bgnbd)
coef(est.bgnbd)

predict(est.bgnbd, prediction.end = "2011-12-31")
plot(est.bgnbd)
