fct.helper.dyncov.create.longer.dyncov.data <- function(num.additional, data.apparelDynCov){

  # Add additional <num.additional> weeks of fake cov data for all Ids
  dt.additional.cov <- expand.grid(Id = unique(data.apparelDynCov$Id),
                                   Cov.Date = seq(from=data.apparelDynCov[, max(Cov.Date)]+lubridate::weeks(1),
                                                  length.out = num.additional, by = "week"), stringsAsFactors = FALSE)
  setDT(dt.additional.cov)
  dt.additional.cov[, High.Season := rep(c(0,1,1,0),.N/4)]
  dt.additional.cov[, Gender    := rep(c(0,1),.N/2)]
  dt.additional.cov[, Channel   := rep(c(0,1),.N/2)]

  return(rbindlist(list(data.apparelDynCov, dt.additional.cov), use.names = TRUE))
}

fct.helper.dyncov.LLdata.from.clvdata <- function(clv.data, params){
  return(pnbd_dyncov_getLLdata(clv.fitted=fct.helper.dyncov.quickfit(clv.data, hessian=FALSE), params=params))
}

