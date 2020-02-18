# Returns only the individual LL values per customer and no other data.
pnbd_dyncov_LL_ind <- function(params, obj){
  LL <- NULL
  cbsdata_ind <- pnbd_dyncov_LL(params=params, obj=obj)
  return(cbsdata_ind[, LL])
}
