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


predict(est.bgnbd, prediction.end = "2011-12-31")
plot(est.bgnbd)

