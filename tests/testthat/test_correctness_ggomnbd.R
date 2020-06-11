data("cdnow")
data("apparelTrans")
data("apparelStaticCov")


# Coefs are our estimate
fct.testthat.correctness(name.model = "GGompertz/NBD", method=ggomnbd, data.cdnow=cdnow,
                         data.apparelTrans=apparelTrans, data.apparelStaticCov=apparelStaticCov,
                         correct.start.params.model = c(r = 0.5, alpha = 2, b = 0.5, s = 0.5, beta = 0.5),
                         correct.params.nocov.coef = c(r = 0.55313, alpha = 10.5758, b = 0.0000011, s = 0.60682, beta = 0.000013),
                         correct.LL.nocov = -9594.9762)

# Cannot compare against BTYD

context("Correctness - GGompertz/NBD nocov - Recover parameters")

# Bemmaor and Glady (2012)
#   Table 2, p. 1018
fct.testthat.correctness.nocov.correct.coefs(method = ggomnbd,
                                             cdnow = cdnow,
                                             start.params.model = c(r = 0.5, alpha = 2, b = 0.5, s = 0.5, beta = 0.5),
                                             params.nocov.coef = c(r = 0.553, alpha = 10.578, b = 0.0002, s = 0.603, beta = 0.0026),
                                             LL.nocov = -9594.98)

# No reliable data available for comparison. Paper information is not sufficient
#   In the paper, no explanation is given on how the SEs were derived and they diverge substantially from ours for s and beta
# fct.testthat.correctness.nocov.correct.se(method = ggomnbd,
#                                           cdnow = cdnow,
#                                           start.params.model = c(r = 0.5, alpha = 2, b = 0.5, s = 0.5, beta = 0.5),
#                                           params.nocov.se = c(r = 0.049, alpha = 0.949, b = 0.0000, s = 0.039, beta = 0.0004))


context("Correctness - GGompertz/NBD nocov - Expectation")

expect_silent(clv.data.apparel <- clvdata(apparelTrans, date.format = "ymd",
                                          time.unit = "w", estimation.split = 40))
expect_silent(clv.ggomnbd <- ggomnbd(clv.data.apparel, verbose = FALSE))

expect_silent(clv.data.static.cov <- SetStaticCovariates(clv.data.apparel,
                                                         data.cov.life = apparelStaticCov,names.cov.life = c("Gender", "Channel"),
                                                         data.cov.trans = apparelStaticCov,names.cov.trans = c("Gender", "Channel")))
expect_silent(clv.ggomnbd <- ggomnbd(clv.data.static.cov, verbose = FALSE))


test_that("Same result as previously existing R implementation", {



  # Bemmaor and Glady: Implementing in Matlab paper, p.7
  # intg(i,j)=quadgk(@(tau)tau.*exp(bg.*tau).*(betag+exp(bg*tau)-1).^-(sg+1),0,t);
  # gg_xt_cum(i,j)=rg./ag. * (((betag./(betag+exp(bg*t)-1)).^sg).*t+bg.*sg.*betag.^sg.*intg(i,j));

  fct.ggomnbd.expectation <- function(r, alpha, beta, b, s, t_i){
    term1 <- (r / alpha)
    term2 <- ((beta / (beta+exp(b*t_i)-1) )^s) *(t_i)
    term3 <- b * s * (beta^s)
    term4 <- integrate(f = function(tau){tau * exp(b*tau) * ((beta + exp(b*tau) - 1)^(-(s+1)))},
                       lower = 0, upper = t_i,
                       rel.tol = 1e-8, abs.tol = 1e-8)$value

    return(term1 * (term2 + (term3 * term4)))
  }

  vec_t_i <- 0:50


  # Nocov
  expect_silent(expectation_R <- sapply(vec_t_i, fct.ggomnbd.expectation,
                                        r     = clv.ggomnbd@prediction.params.model[["r"]],
                                        alpha = clv.ggomnbd@prediction.params.model[["alpha"]],
                                        beta  = clv.ggomnbd@prediction.params.model[["beta"]],
                                        b     = clv.ggomnbd@prediction.params.model[["b"]],
                                        s     = clv.ggomnbd@prediction.params.model[["s"]]))

  expect_silent(expectation_Rcpp <- ggomnbd_nocov_expectation(r       = clv.ggomnbd@prediction.params.model[["r"]],
                                                              alpha_0 = clv.ggomnbd@prediction.params.model[["alpha"]],
                                                              beta_0  = clv.ggomnbd@prediction.params.model[["beta"]],
                                                              b       = clv.ggomnbd@prediction.params.model[["b"]],
                                                              s       = clv.ggomnbd@prediction.params.model[["s"]],
                                                              vT_i = vec_t_i))
  expect_equal(expectation_R, drop(expectation_Rcpp))




  # Static cov

  m.cov.data.life  <- clv.data.get.matrix.data.cov.life(clv.data=clv.ggomnbd@clv.data, correct.row.names=clv.ggomnbd@cbs$Id,
                                                        correct.col.names=names(clv.ggomnbd@prediction.params.life))
  m.cov.data.trans <- clv.data.get.matrix.data.cov.trans(clv.data=clv.ggomnbd@clv.data, correct.row.names=clv.ggomnbd@cbs$Id,
                                                         correct.col.names=names(clv.ggomnbd@prediction.params.trans))
  alpha_i <- clv.ggomnbd@prediction.params.model[["alpha"]] * exp( -m.cov.data.trans %*% clv.ggomnbd@prediction.params.trans)
  beta_i  <- clv.ggomnbd@prediction.params.model[["beta"]]  * exp( -m.cov.data.life  %*% clv.ggomnbd@prediction.params.life)

  expect_silent(expectation_R <- sapply(seq(vec_t_i), function(i){
    fct.ggomnbd.expectation(r     = clv.ggomnbd@prediction.params.model[["r"]],
                            alpha = alpha_i[i],
                            beta  = beta_i[i],
                            b     = clv.ggomnbd@prediction.params.model[["b"]],
                            s     = clv.ggomnbd@prediction.params.model[["s"]],
                            t_i = vec_t_i[i])}))


  expect_silent(expectation_Rcpp <- CLVTools:::ggomnbd_staticcov_expectation(r       = clv.ggomnbd@prediction.params.model[["r"]],
                                                                             alpha_0 = clv.ggomnbd@prediction.params.model[["alpha"]],
                                                                             b       = clv.ggomnbd@prediction.params.model[["b"]],
                                                                             s       = clv.ggomnbd@prediction.params.model[["s"]],
                                                                             beta_0  = clv.ggomnbd@prediction.params.model[["beta"]],
                                                                             vT_i    = vec_t_i,
                                                                             vCovParams_trans = clv.ggomnbd@prediction.params.trans,
                                                                             vCovParams_life  = clv.ggomnbd@prediction.params.life,
                                                                             mCov_life  = m.cov.data.life[seq_along(vec_t_i), ],
                                                                             mCov_trans = m.cov.data.trans[seq_along(vec_t_i), ]))

  # Differences were verified to stem from numerical integration
  expect_equal(expectation_R, drop(expectation_Rcpp), tolerance = 1e-4)
})


context("Correctness - GGompertz/NBD nocov - CET")
test_that("Same result for CET as previous implementation based on matlab code",{

  # Basically, PALive * Expectation but explicitely formulated

  fct.ggomnbd.CET <- function(r, b, s, alpha_i, beta_i, x, t.x, Tcal, periods, palive){
    alpha_i <- alpha_i + x
    beta_i <- beta_i + exp(b * Tcal) - 1


    # From Matlab code:
    # gg_xt_cum_up(i)=p_i(i).*rstar./astar.*  (((betastar./(betastar+exp(bg*t)-1)).^sg).*t+bg.*sg.*betastar.^sg.*intgup_h(i));
    P1 <- palive * ((r+x) / (alpha_i));
    P2 <- (beta_i / (beta_i + exp(b* periods) - 1.0))^s * periods;

    integrals <- integrate(f = function(tau){tau * exp(b*tau) * ((beta_i + exp(b*tau) - 1)^(-(s+1)))},
                           lower = 0, upper = periods,
                           rel.tol = 1e-8, abs.tol = 1e-8)$value
    P3 <- b * s * (beta_i^s) * integrals;

    return(P1 * (P2 + P3))
  }


  r     <- clv.ggomnbd@prediction.params.model[["r"]]
  alpha <- clv.ggomnbd@prediction.params.model[["alpha"]]
  beta  <- clv.ggomnbd@prediction.params.model[["beta"]]
  b     <- clv.ggomnbd@prediction.params.model[["b"]]
  s     <- clv.ggomnbd@prediction.params.model[["s"]]
  vX    <- clv.ggomnbd@cbs$x
  vT_x  <- clv.ggomnbd@cbs$t.x
  vT_cal<- clv.ggomnbd@cbs$T.cal

  # Nocov
  palive <- CLVTools:::ggomnbd_nocov_PAlive(r = r, alpha_0 = alpha, b = b, s = s, beta_0 = beta,
                                            vX = vX, vT_x = vT_x, vT_cal = vT_cal)

  for(periods in c(0, 0.24, 0.99, 1, 1.23, 2, 3, 5, 10, 50)){
    expect_silent(CET_R <- sapply(seq_along(vX), FUN = function(i){
      fct.ggomnbd.CET(r = r, alpha_i = alpha, b = b, s = s, beta_i = beta,
                      x = vX[i], t.x = vT_x[i], Tcal = vT_cal[i],
                      palive = palive[i], periods = periods)
      }))

    expect_silent(CET_Rcpp <- CLVTools:::ggomnbd_nocov_CET(r = r, alpha_0 = alpha, b = b, s = s, beta_0 = beta,
                                                           vX = vX, vT_x = vT_x, vT_cal = vT_cal,
                                                           dPeriods = periods))
    expect_equal(CET_R, drop(CET_Rcpp))
  }
})


