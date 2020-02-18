pnbd_dyncov_LL_sum <- function(params, obj){
  return(-sum(pnbd_dyncov_LL_ind(params=params, obj=obj)))
}
