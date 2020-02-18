.pnbd_dyncov_LL_Bi <- function(data.work.trans.aux, cbs.t.x, i){

  t.x <- Aji <- adj.Walk1 <- d <- Id <- Aki <- Num.Walk <- adj.Max.Walk <- delta <- NULL

  #as there are only AuxTrans there is exactly one single row for each customer
  # -> there is no need for by=Id
  #Also replacing Walk_i with Max.Walk was ommited by changing >=/< relations in Aki

  results <- data.table(Id = data.work.trans.aux$Id, Aji=0, Aki=0)
  data.work.trans.aux[, t.x:=cbs.t.x]

  ##Aji Part
  ##########################

  if(i == 1 || i == 2)
  {
    #Aji only consists of Aj1
    #no sum by needed as only AuxTrans are used
    #No need to remove NAs as it will be removed when summing with Aki
    results[, Aji:= data.work.trans.aux[, adj.Walk1 * d]]
  }else{

    middle.walks <- paste( "c(", paste0("adj.Walk", 2:(i-1), collapse = ","), ")")
    #print(microbenchmark(data.work.trans.aux[, sum(adj.Walk1*d, adj.Walk2,adj.Walk3,adj.Walk4,adj.Walk5,adj.Walk6,adj.Walk7,adj.Walk8,adj.Walk9,adj.Walk10,adj.Walk11, adj.Walk12, na.rm=T)]))
    #print(microbenchmark(data.work.trans.aux[, sum(adj.Walk1*d, eval(parse(text=aw)), na.rm=T)] ) )
    # print(microbenchmark(data.work.trans.aux[, sum(aw), with=F] + data.work.trans.aux[, adj.Walk1*d] ))
    #    print(data.work.trans.aux[, paste0("adj.Walk", 2:(i-1)), with=F] )
    results[, Aji:= data.work.trans.aux[, sum(adj.Walk1*d, eval(parse(text=middle.walks)), na.rm=T), by=Id]$V1]
    #results[, Aji:= data.work.trans.aux[, sum(adj.Walk1 * d, eval(paste0("adj.Walk", 2:(i-1))),  na.rm = T), by=Id]$V1]
  }

  ##Aki Part
  ##########################

  if(i==1){
    #omit delta part
    #?? **TODO** ?? no Num.Walk < 1 -> no case distinguish?
    #ind.num.s.i <- data.work.trans.aux[,.I[Num.Walk < i]]
    #ind.num.ge.i <- data.work.trans.aux[,.I[Num.Walk >= i]]
    results[, Aki:= data.work.trans.aux[, adj.Walk1 * (-cbs.t.x - d)] ]
    #  results[ind.num.ge.i, Aki:= data.work.trans.aux[ind.num.ge.i, adj.Walk1 * (-cbs.t.x - d)] ]
    # results[ind.num.s.i, Aki:= data.work.trans.aux[ind.num.s.i, adj.Walk1 * (-cbs.t.x - d)] ]

  }else{
    #include delta part
    ind.num.se.i <- data.work.trans.aux[,.I[Num.Walk <= i]]
    ind.num.g.i <- data.work.trans.aux[,.I[Num.Walk > i]]

    results[ind.num.se.i, Aki:= data.work.trans.aux[ind.num.se.i,  adj.Max.Walk
                                                    * (-cbs.t.x[ind.num.se.i] - d - delta*(Num.Walk-2))] ]
    results[ind.num.g.i, Aki:= data.work.trans.aux[ind.num.g.i, get(paste0("adj.Walk", i))
                                                   * (-cbs.t.x[ind.num.g.i] - d - delta*(i-2))]]
  }

  return(results[, sum(Aki, Aji, na.rm=T), by=Id]$V1)
}
