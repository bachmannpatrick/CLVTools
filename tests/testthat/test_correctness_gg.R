data("cdnow")
data("apparelTrans")
data("apparelStaticCov")

fct.testthat.correctness.clvfittedspending(name.model = "Gamma-Gamma", method = gg,
                                           data.cdnow=cdnow, data.apparelTrans=apparelTrans, data.apparelStaticCov=apparelStaticCov,
                                           correct.start.params.model = NULL,
                                           # Parameter estimates from Fader & Hardie, 2013 ("The Gamma-Gamma Model of Monetary Value")
                                           correct.params.coef = c(p=6.25, q = 3.74, gamma = 15.44),
                                           correct.LL = -4055.9177)
