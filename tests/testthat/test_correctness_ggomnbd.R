data("cdnow")
data("apparelTrans")
data("apparelStaticCov")


# Coefs are our estimate
fct.testthat.correctness.clvfittedtransactions(name.model = "GGompertz/NBD", method=ggomnbd, data.cdnow=cdnow,
                                               data.apparelTrans=apparelTrans, data.apparelStaticCov=apparelStaticCov,
                                               correct.start.params.model = c(r = 0.5, alpha = 2, b = 0.5, s = 0.5, beta = 0.5),
                                               correct.params.nocov.coef = c(r = 0.55313, alpha = 10.5758, b = 0.0000011, s = 0.60682, beta = 0.000013),
                                               correct.LL.nocov = -9594.9762,
                                               kkt2.true = FALSE)


context("Correctness - GGompertz/NBD nocov - Recover parameters")

# Bemmaor and Glady (2012)
#   Table 2, p. 1018
fct.testthat.correctness.clvfitted.correct.coefs(method = ggomnbd,
                                                 cdnow = cdnow,
                                                 start.params.model = c(r = 0.5, alpha = 2, b = 0.5, s = 0.5, beta = 0.5),
                                                 params.nocov.coef = c(r = 0.553, alpha = 10.578, b = 0.0002, s = 0.603, beta = 0.0026),
                                                 LL.nocov = -9594.98)

# No reliable data available for comparison. Paper information is not sufficient
#   In the paper, no explanation is given on how the SEs were derived and they diverge substantially from ours for s and beta
# fct.testthat.correctness.clvfitted.nocov.correct.se(method = ggomnbd,
#                                           cdnow = cdnow,
#                                           start.params.model = c(r = 0.5, alpha = 2, b = 0.5, s = 0.5, beta = 0.5),
#                                           params.nocov.se = c(r = 0.049, alpha = 0.949, b = 0.0000, s = 0.039, beta = 0.0004))



# Compare vs Matlab code ---------------------------------------------------------------------------------------


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

fct.ggomnbd.expectation.R <- function(params_i.t){
  return(params_i.t[, sapply(seq_along(t_i), function(i){
    return(fct.ggomnbd.expectation(r=r[i],b=b[i],s=s[i], alpha=alpha_i[i], beta=beta_i[i], t_i=t_i[i]))})])
}

# To test expectation correctly, fake that some customers only come alive later
apparelTrans.later <- copy(apparelTrans)
apparelTrans.later[Id %in% c("1", "10", "100"), Date := Date + lubridate::weeks(10)]
clv.apparel.static <- fct.helper.create.clvdata.apparel.staticcov(data.apparelTrans = apparelTrans.later,
                                                                  data.apparelStaticCov = apparelStaticCov,
                                                                  estimation.split = 38)
expect_silent(clv.ggomnbd <- ggomnbd(clv.data = clv.apparel.static, verbose = FALSE))

r     <- clv.ggomnbd@prediction.params.model[["r"]]
alpha <- clv.ggomnbd@prediction.params.model[["alpha"]]
beta  <- clv.ggomnbd@prediction.params.model[["beta"]]
b     <- clv.ggomnbd@prediction.params.model[["b"]]
s     <- clv.ggomnbd@prediction.params.model[["s"]]
vX    <- clv.ggomnbd@cbs$x
vT_x  <- clv.ggomnbd@cbs$t.x
vT_cal<- clv.ggomnbd@cbs$T.cal

expect_silent(m.cov.data.life  <- clv.data.get.matrix.data.cov.life(clv.data=clv.ggomnbd@clv.data, correct.row.names=clv.ggomnbd@cbs$Id,
                                                                    correct.col.names=names(clv.ggomnbd@prediction.params.life)))
expect_silent(m.cov.data.trans <- clv.data.get.matrix.data.cov.trans(clv.data=clv.ggomnbd@clv.data, correct.row.names=clv.ggomnbd@cbs$Id,
                                                                     correct.col.names=names(clv.ggomnbd@prediction.params.trans)))
expect_silent(alpha_i <- alpha * exp( -m.cov.data.trans %*% clv.ggomnbd@prediction.params.trans))
expect_silent(beta_i  <- beta  * exp( -m.cov.data.life  %*% clv.ggomnbd@prediction.params.life))



# . Expectation -----------------------------------------------------------------------------------------
context("Correctness - GGompertz/NBD nocov - Expectation")

test_that("Expectation in Rcpp matches expectation in R (nocov)", {
  skip_on_cran()

  expect_silent(clv.cdnow <- clvdata(data.transactions = cdnow,
                                     date.format = "ymd", time.unit = "W", estimation.split = 38))
  expect_silent(fitted.cdnow <- ggomnbd(clv.data = clv.cdnow, verbose = FALSE))

  params_i <- fitted.cdnow@cbs[, c("Id", "T.cal", "date.first.actual.trans")]
  params_i[, r       := fitted.cdnow@prediction.params.model[["r"]]]
  params_i[, alpha_i := fitted.cdnow@prediction.params.model[["alpha"]]]
  params_i[, b       := fitted.cdnow@prediction.params.model[["b"]]]
  params_i[, s       := fitted.cdnow@prediction.params.model[["s"]]]
  params_i[, beta_i  := fitted.cdnow@prediction.params.model[["beta"]]]

  # Differences were verified to stem from numerical integration
  fct.testthat.correctness.clvfittedtransactions.same.expectation.in.R.and.Cpp(fct.expectation.R = fct.ggomnbd.expectation.R,
                                                                               params_i = params_i,
                                                                               obj.fitted = fitted.cdnow)
})

test_that("Expectation in Rcpp matches expectation in R (staticcov)", {
  skip_on_cran()

  params_i <- clv.ggomnbd@cbs[, c("Id", "T.cal", "date.first.actual.trans")]
  params_i[, r       := r]
  params_i[, s       := s]
  params_i[, b       := b]
  params_i[, alpha_i := alpha_i]
  params_i[, beta_i  := beta_i]

  # Differences were verified to stem from numerical integration
  fct.testthat.correctness.clvfittedtransactions.same.expectation.in.R.and.Cpp(fct.expectation.R = fct.ggomnbd.expectation.R,
                                                                               params_i = params_i,
                                                                               obj.fitted = clv.ggomnbd,
                                                                               tolerance = 1e-6)
})






# .CET ------------------------------------------------------------------------------------------
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



  # Nocov
  palive_nocov <- ggomnbd_nocov_PAlive(r = r, alpha_0 = alpha, b = b, s = s, beta_0 = beta,
                                       vX = vX, vT_x = vT_x, vT_cal = vT_cal)

  for(periods in c(0, 0.24, 0.99, 1, 1.23, 2, 3, 5, 10, 50)){
    expect_silent(CET_R <- sapply(seq_along(vX), FUN = function(i){
      fct.ggomnbd.CET(r = r, alpha_i = alpha, b = b, s = s, beta_i = beta,
                      x = vX[i], t.x = vT_x[i], Tcal = vT_cal[i],
                      palive = palive_nocov[i], periods = periods)
    }))

    expect_silent(CET_Rcpp <- ggomnbd_nocov_CET(r = r, alpha_0 = alpha, b = b, s = s, beta_0 = beta,
                                                vX = vX, vT_x = vT_x, vT_cal = vT_cal,
                                                dPeriods = periods))
    expect_equal(CET_R, drop(CET_Rcpp))
  }

  # Static cov
  palive_staticcov <- ggomnbd_staticcov_PAlive(r = r, alpha_0 = alpha, b = b, s = s, beta_0 = beta,
                                               vX = vX, vT_x = vT_x, vT_cal = vT_cal,
                                               vCovParams_trans = clv.ggomnbd@prediction.params.trans,
                                               vCovParams_life  = clv.ggomnbd@prediction.params.life,
                                               mCov_life = m.cov.data.life,
                                               mCov_trans = m.cov.data.trans)
  for(periods in c(0, 0.24, 0.99, 1, 1.23, 2, 3, 5, 10, 50)){
    expect_silent(CET_R <- sapply(seq_along(vX), FUN = function(i){
      fct.ggomnbd.CET(r = r, alpha_i = alpha_i[i], b = b, s = s, beta_i = beta_i[i],
                      x = vX[i], t.x = vT_x[i], Tcal = vT_cal[i],
                      palive = palive_staticcov[i], periods = periods)
    }))

    expect_silent(CET_Rcpp <- ggomnbd_staticcov_CET(r = r, alpha_0 = alpha, b = b, s = s, beta_0 = beta,
                                                    vX = vX, vT_x = vT_x, vT_cal = vT_cal,
                                                    vCovParams_trans = clv.ggomnbd@prediction.params.trans,
                                                    vCovParams_life  = clv.ggomnbd@prediction.params.life,
                                                    mCov_life = m.cov.data.life,
                                                    mCov_trans = m.cov.data.trans,
                                                    dPeriods = periods))
    expect_equal(CET_R, drop(CET_Rcpp))
  }
})


# .LL ------------------------------------------------------------------------------------------
context("Correctness - GGompertz/NBD nocov - LL")
test_that("Same LL as in matlab code", {

  fct.ggomnbd.LL.ind <- function(r, b, s, alpha, beta, x, t.x, tcal){

    fct.ll.integral <- function(alpha_i, beta_i, x_i, t.x_i, tcal_i){
      # intl(i,1)=quadgk(@(y)((y+alpha).^-(r+x_i(i))).*((beta+exp(b.*y)-1).^-
      #                                                   (s+1)).*exp(b.*y),k_i(i),T_i(i));
      integrate(function(y){
        return(((y+alpha_i)^-(r+x_i)) * ((beta_i+exp(b*y)-1)^-(s+1)) * exp(b*y))
      },lower = t.x_i, upper = tcal_i)$value
    }

    LLIntergrals <- sapply(seq_along(x), function(i){
      fct.ll.integral(alpha_i = alpha[i], beta_i = beta[i],
                      x_i = x[i], t.x_i = t.x[i], tcal_i = tcal[i])
    })


    l1=lgamma(r+x)-lgamma(r)+r*(log(alpha)-log(alpha+tcal))-
      x*log(alpha+tcal)+s*(log(beta)-log(beta-1+exp(b*tcal)));
    l2=lgamma(r+x)-
      lgamma(r)+log(b)+r*log(alpha)+log(s)+s*log(beta) + log(LLIntergrals)
    return((log(exp(l1)+exp(l2))))
  }

  # No cov
  expect_silent(LL_R <- fct.ggomnbd.LL.ind(r = r, b=b, s=s,
                                           alpha = rep_len(alpha, length(vX)), beta = rep_len(beta, length(vX)),
                                           x = vX, t.x = vT_x, tcal = vT_cal))

  expect_silent(LL_Rcpp <- ggomnbd_nocov_LL_ind(vLogparams = log(c(r = r, alpha_0 = alpha, b = b, s = s, beta_0 = beta)),
                                                vX = vX, vT_x = vT_x, vT_cal = vT_cal))
  expect_equal(LL_R, drop(LL_Rcpp))

  # Static cov
  expect_silent(LL_R <- fct.ggomnbd.LL.ind(r = r, b=b, s=s,
                                           alpha = alpha_i, beta = beta_i,
                                           x = vX, t.x = vT_x, tcal = vT_cal))

  expect_silent(LL_Rcpp <- ggomnbd_staticcov_LL_ind(vParams = c(log(c(r = r, alpha_0 = alpha, b = b, s = s, beta_0 = beta)),
                                                                clv.ggomnbd@prediction.params.life, clv.ggomnbd@prediction.params.trans),
                                                    vX = vX, vT_x = vT_x, vT_cal = vT_cal,
                                                    mCov_life = m.cov.data.life, mCov_trans = m.cov.data.trans))
  # Difference are verified to stem from numerical integration
  expect_equal(drop(LL_R), drop(LL_Rcpp), check.attributes=FALSE, tolerance = 1e-4)
})


# .PAlive ------------------------------------------------------------------------------------------
context("Correctness - GGompertz/NBD nocov - PAlive")
test_that("Same PAlive as in matlab code", {

  fct.ggomnbd.PAlive <- function(r, b, s, alpha, beta, x, t.x, tcal){
    # l1=gammaln(rg+x_i)-gammaln(rg)+rg.*(log(ag)-log(ag+T_i))-
    #   x_i.*log(ag+T_i)+sg.*(log(betag)-log(betag-1+exp(bg.*T_i)));
    # l2=gammaln(rg+x_i)-gammaln(rg)+log(bg)+rg.*log(ag)+log(sg)+sg.*log(betag);
    # p1=gamma(rg+x_i)./gamma(rg).*((ag./(ag+T_i)).^rg).*((1./(ag+T_i)).^(x_i)).*
    #   (betag./(betag-1+exp(bg.*T_i))).^sg;
    l1 = lgamma(r+x)-lgamma(r)+r*(log(alpha)-log(alpha+tcal))-
      x*log(alpha+tcal)+s*(log(beta) - log(beta - 1 + exp(b*tcal)))
    l2 = lgamma(r+x)-lgamma(r)+log(b)+r*log(alpha)+log(s)+s*log(beta)
    p1 = gamma(r+x)/gamma(r)*((alpha / (alpha+tcal))^r)*((1/(alpha+tcal))^(x))*(beta/(beta-1+exp(b*tcal)))^s

    fct.palive.integral <- function(alpha_i, beta_i, x_i, t.x_i, tcal_i){
      # intl(i)=quadgk(@(y)((y+ag).^-(rg+x_i(i))).*((betag+exp(bg.*y)-1).^-
      #                                               (sg+1)).*exp(bg.*y),k_i(i),T_i(i));
      integrate(function(y){
        return((y+alpha_i)^-(r+x_i)*((beta_i+exp(b*y)-1)^-(s+1))*exp(b*y))
      }, t.x_i, tcal_i, rel.tol = 1e-8, abs.tol = 1e-8)$value
    }
    LLIntergrals <- sapply(seq_along(x), function(i){
      fct.palive.integral(alpha_i = alpha[i], beta_i = beta[i],
                          x_i = x[i], t.x_i = t.x[i], tcal_i = tcal[i])
    })
    # l_i(i)=exp(l1(i))+exp(l2(i)+log(intl(i)))
    # p_i(i)=p1(i)./l_i(i);
    l_i <- exp(l1)+exp(l2 + log(LLIntergrals))
    palive <- p1/l_i
    return(palive)
  }

  # No cov
  expect_silent(PAlive_R <- fct.ggomnbd.PAlive(r = r, b=b, s=s,
                                               alpha = rep_len(alpha, length(vX)), beta = rep_len(beta, length(vX)),
                                               x = vX, t.x = vT_x, tcal = vT_cal))

  expect_silent(PAlive_Rcpp <- CLVTools:::ggomnbd_nocov_PAlive(r = r, alpha_0 = alpha, b = b, s = s, beta_0 = beta,
                                                               vX = vX, vT_x = vT_x, vT_cal = vT_cal))
  expect_equal(PAlive_R, drop(PAlive_Rcpp))

  # Static cov
  expect_silent(LL_R <- fct.ggomnbd.PAlive(r = r, b=b, s=s,
                                           alpha = alpha_i, beta = beta_i,
                                           x = vX, t.x = vT_x, tcal = vT_cal))

  expect_silent(LL_Rcpp <- CLVTools:::ggomnbd_staticcov_PAlive(r = r, alpha_0 = alpha, b = b, s = s, beta_0 = beta,
                                                               vCovParams_life = clv.ggomnbd@prediction.params.life,
                                                               vCovParams_trans = clv.ggomnbd@prediction.params.trans,
                                                               vX = vX, vT_x = vT_x, vT_cal = vT_cal,
                                                               mCov_life = m.cov.data.life, mCov_trans = m.cov.data.trans))
  # Difference are verified to stem from numerical integration
  expect_equal(drop(LL_R), drop(LL_Rcpp), check.attributes=FALSE, tolerance = 1e-4)


})

