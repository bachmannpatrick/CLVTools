.pnbd_dyncov_LL_Bi <- function(data.work.trans.aux, cbs.t.x, i){

  t.x <- Aji <- adj.Walk1 <- d <- Id <- Aki <- Num.Walk <- adj.Max.Walk <- delta <- NULL

  # because there are only AuxTrans there is exactly one single row for each customer
  # -> there is no need for by=Id
  #Also replacing Walk_i with Max.Walk was ommited by changing >=/< relations in Aki

  results <- data.table(Id = data.work.trans.aux$Id, Aji=0, Aki=0)
  data.work.trans.aux[, t.x:=cbs.t.x]

  ##Aji Part ------------------------------------------------------------------

  if(i == 1 || i == 2)
  {
    #Aji only consists of Aj1
    #no sum by needed as only AuxTrans are used
    #No need to remove NAs as it will be removed when summing with Aki
    results[, Aji:= data.work.trans.aux[, adj.Walk1 * d]]
  }else{

    middle.walks <- paste( "c(", paste0("adj.Walk", 2:(i-1), collapse = ","), ")")
    results[, Aji:= data.work.trans.aux[, sum(adj.Walk1*d, eval(parse(text=middle.walks)), na.rm=T), by=Id]$V1]
  }

  # Aki Part ------------------------------------------------------------------

  if(i==1){
    # omit delta part
    results[, Aki:= data.work.trans.aux[, adj.Walk1 * (-cbs.t.x - d)] ]
  }else{
    # include delta part
    ind.num.se.i <- data.work.trans.aux[,.I[Num.Walk <= i]]
    ind.num.g.i <- data.work.trans.aux[,.I[Num.Walk > i]]

    results[ind.num.se.i, Aki:= data.work.trans.aux[ind.num.se.i,  adj.Max.Walk
                                                    * (-cbs.t.x[ind.num.se.i] - d - delta*(Num.Walk-2))] ]
    results[ind.num.g.i, Aki:= data.work.trans.aux[ind.num.g.i, get(paste0("adj.Walk", i))
                                                   * (-cbs.t.x[ind.num.g.i] - d - delta*(i-2))]]
  }

  return(results[, sum(Aki, Aji, na.rm=T), by=Id]$V1)
}
