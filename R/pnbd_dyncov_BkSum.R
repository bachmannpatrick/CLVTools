.pnbd_dyncov_LL_BkSum <- function(data.work.trans, BkT=T){

  AuxTrans <- Num.Walk <- adj.Walk1 <- d <- adj.Max.Walk <- tjk <- delta <- Id <- V1 <- NULL
  #Check for BkT. If FALSE, auxilary transaction need to be removed,
  #               if TRUE we use all transactions inlcuding auxilary one.
  if(BkT == FALSE)
    data.work <- data.work.trans[AuxTrans==F]
  else
    data.work <- data.work.trans

  # num.walk same for data.working.1 and .2
  max.walk <- data.work[, max(Num.Walk)]

  if( max.walk == 1 || max.walk == 2)
  {
    #Sum B1, Bn
    Bsum <- data.work[, sum(adj.Walk1 * d, #B1
                            adj.Max.Walk * (tjk-d-delta*(Num.Walk-2)), #Bkn/Bjn
                            na.rm=T), by=Id]
  }else{

    # create strings of structure: "c(adj.Walk2, adj.Walk3, ..., adj.Walk(max.walk-1))"
    middle.walks <- paste0("c(", paste0("adj.Walk", 2:(max.walk-1), collapse=", "), ")")
    #Sum B1, B2, .., Bn
    Bsum <- data.work[, sum( adj.Walk1 * d, #B1
                             eval(parse(text=middle.walks)), #B2,B3,..B(n-1)
                             adj.Max.Walk * (tjk-d-delta*(Num.Walk-2)), #Bkn/Bjn
                             na.rm=T), by=Id]
  }

  #If Bjsum is calculated not all customers might be used!
  # -> merge with all customers and fill the missing ones with 0
  if(BkT == F )
  {
    unique.customers <- unique(data.work.trans[, "Id", with=F])
    if( nrow(unique.customers) != nrow(Bsum))
    {
      Bsum<-merge(x = unique.customers, y=Bsum, by="Id", all.x=T)
      Bsum[is.na(V1), V1:=0]
    }
  }

  #rename accordingly
  if(BkT == T)
    setnames(Bsum, "V1", "Bksum")
  else
    setnames(Bsum, "V1", "Bjsum")

  return(Bsum)
}
