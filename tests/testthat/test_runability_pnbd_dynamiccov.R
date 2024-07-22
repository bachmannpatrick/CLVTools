# Load Data --------------------------------------------------------------------
data("apparelTrans")
data("apparelDynCov")

skip_on_cran()


# Basic runability of standard specification -----------------------------------

fitted.dyncov <- fct.helper.dyncov.quickfit.apparel.data(
  hessian=TRUE,
  names.cov.life = c("Marketing", "Gender"),
  names.cov.trans = c("Marketing", "Gender", "Channel")
)


fct.helper.runability.dyncov.all.downstream(
  fitted.dyncov=fitted.dyncov,
  names.params=
    c("r", "alpha", "s", "beta",
      "life.Marketing",  "life.Gender",
      "trans.Marketing", "trans.Gender",  "trans.Channel")
)

fct.testthat.runability.dynamiccov.LL.is.correct(
  clv.fitted = fitted.dyncov
)


# Additional specifications ----------------------------------------------------
test_that("Dyncov works with additional model specifications", {

  clv.apparel.dyn <- fct.helper.create.clvdata.apparel.dyncov()

  fn.fit.dyncov.spec <- function(...){
    covs.life <- c("Marketing", "Gender")
    covs.trans <- c("Marketing", "Gender", "Channel")

    expect_silent(fitted.dyncov <- fit.apparel.dyncov(
      names.cov.life = covs.life,
      names.cov.trans = covs.trans,
      optimx.args=fct.helper.dyncov.get.optimxargs.quickfit(hessian=TRUE),
      ...
    ))

    # downstream usage of additional specification works
    names.params <- c('r', 'alpha', 's', 'beta')
    if(fitted.dyncov@clv.model@estimation.used.correlation){
      names.params <- c(names.params, 'Cor(life,trans)')
    }
    if(fitted.dyncov@estimation.used.constraints){
      covs.constr <- list(...)['names.cov.constr']
      names.params <- c(names.params, paste0('constr.', covs.constr))
      # all others which are not constrained
      names.params <- c(
        names.params,
        paste0('life.', setdiff(covs.life, covs.constr)),
        paste0('trans.', setdiff(covs.trans, covs.constr))
      )
    }else{
      names.params <- c(
        names.params,
        paste0('life.', covs.life),
        paste0('trans.', covs.trans)
      )
    }

    fct.helper.runability.dyncov.all.downstream(
      fitted.dyncov=fitted.dyncov,
      names.params=names.params
    )

    # LLsum is not expected to match logLik if regularization was used
    if(!fitted.dyncov@estimation.used.regularization){
      fct.testthat.runability.dynamiccov.LL.is.correct(
        clv.fitted = fitted.dyncov
      )
    }
  }

  # regularization -------------------------------------------------------------
  fn.fit.dyncov.spec(reg.lambdas = c(trans=10, life=20))

  # constrained covs -----------------------------------------------------------
  fn.fit.dyncov.spec(names.cov.constr = "Gender")

  # correlation ----------------------------------------------------------------
  fn.fit.dyncov.spec(use.cor=TRUE)
  #
  # # combination ----------------------------------------------------------------
  fn.fit.dyncov.spec(
    use.cor=TRUE,
    names.cov.constr = "Gender",
    reg.lambda = c(trans=10, life=20))
})
