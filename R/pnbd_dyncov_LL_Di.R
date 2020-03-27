#D1
#max.walk=1 & max.walk=2:
#             i=1: Dk1, Dkn=0 -> Dk1
#             i>1: Dk1, Dkn
#max.walk>2:
#             i=1: Dk1, Dk2, ... Dk(max.walk-1), Dkn=0
#             i>1: Dk1, Dk2, ... Dk(max.walk-1), Dkn
#
.pnbd_dyncov_LL_Di1_gen <- function(data.work.life.real, i, kxT){
  Num.Walk <- d <- Di.adj.Walk1 <- Id <- Di.Max.Walk <- NULL

  max.walk <- data.work.life.real[, max(Num.Walk)]

  if(max.walk == 1  || max.walk == 2)
  {
    if(i == 1)
      # do not include DKn (Dkn=0) -> only Dk1
      Di1 <- data.work.life.real[, sum(d*Di.adj.Walk1, na.rm=T), by=Id]
    else{
      # include Dkn as well
      Di1 <- data.work.life.real[, sum(d*Di.adj.Walk1, Di.Max.Walk, na.rm=T), by=Id]
    }
  }else{
    # create strings of structure: "c(adj.Walk2, adj.Walk3, ..., adj..Walk(max.walk-1))"
    middle.walks <- paste0("c(", paste0("adj.Walk", 2:(max.walk-1), collapse=", "), ")")
    if(i == 1){
      # do not include Dkn
      # Dk2..Dk(max.walk-1)
      Di1 <- data.work.life.real[, sum(d*Di.adj.Walk1, eval(parse(text=middle.walks)), na.rm=T), by=Id]
    }else{

      # Achtung: We must also ignore Dkn, in the case kxT==1!!
      Di1 <- data.work.life.real[, sum(d*Di.adj.Walk1, eval(parse(text=middle.walks)), Di.Max.Walk, na.rm=T), by=Id]
      Di1[which(kxT==1)] <- data.work.life.real[which(kxT==1), sum(d*Di.adj.Walk1, eval(parse(text=middle.walks)), na.rm=T), by=Id]
    }
  }
  return(Di1$V1)
}




#i=1:Cj1=0, Cki
#i=2: 0, 0, Cki
#i>2: 0, Cj2, Cj3, .. Cj(i-1), Cki
#
#Cji: sum(exp(gamma2*Walk_i))
#
#Cki(Num.Walk >= i) : exp(gamma2*Walk_i) * (-d.omega- delta*(k0x+i-3))
#Cki(Num.Walk < i)  : exp(gamma2*Max.Walk) * (-d - delta*(k0x+Num.Walk-3))
#where
#   k0x = Num.Walk (real trans)
#   d.omega = d (real trans)
#   delta=(k0x+i-1>1)
.pnbd_dyncov_LL_Di2_gen <- function(data.work.aux, i, d.omega, k0x){

  delta <- Num.Walk <- Cki <- Di.Max.Walk <- Id <- NULL

  #data.work.aux is a subset of the whole data.work -> ie can manipulate by ref

  adj.walk_i = paste0("adj.Walk",i)

  #Cki ---------------------------------------------------------------------------------

  # nrow(data.aux.only) = nrow(data.real.only)
  # ie nrow = number of customers for both!
  data.work.aux[, d.omega:=d.omega] #data.real.only$d
  data.work.aux[, k0x:=k0x]         #data.real.only$Num.Walk
  #Careful! delta=0 if k_{0,x}+i-1=1, thus it can vary for different individuals
  data.work.aux[, delta:=as.numeric(k0x+i-1>1)]

  data.work.aux[Num.Walk>i, Cki := (-d.omega - delta*(k0x+i-3))  * get(adj.walk_i) ] #FACTOR * Ci
  data.work.aux[Num.Walk<=i,  Cki := (-d.omega - delta*(k0x+Num.Walk-3)) * Di.Max.Walk]

  data.work.aux[is.na(Cki), Cki:=0]


  #Di2: Cji + Cki -----------------------------------------------------------------------

  if( i == 1 || i == 2){
    #Di2 = sum(Cj1=0,Cj2=0, Cki), hence D2 = Cki
    return(data.work.aux[, Cki])
  }else{
    #Di2 = sum(0,Cj2, Cj3, .., Cj(i-1), Cki )
    middle.walks.a.cki <- paste0("c(", paste0("adj.Walk", 2:(i-1), collapse=", "), ", Cki)" )
    Di2 <- data.work.aux[, sum(eval(parse(text=middle.walks.a.cki)), na.rm=T), by=Id]
  }
  return(Di2$V1)
}


.pnbd_dyncov_LL_Di <- function(data.work.life, i){

  AuxTrans <- Num.Walk <- d <- NULL

  #Num.Walk == 1 & AuxTrans==T -> Max.Walk = NA

  # Note: Not very elegant, but I am adding the Num.Walk for Auxtrans=True (kxT) as well. We need to know in this function
  # whether this is 1 or not. -> Actually I just found out that this does not make a difference at all. If we don't do this
  # then DT for Num.Walk=kxT=1 would be wrong, but in this case we only use D1 anyway which is correct...

  Di1 <- .pnbd_dyncov_LL_Di1_gen(data.work.life.real = data.work.life[AuxTrans==F], i = i, kxT = data.work.life[AuxTrans==T, Num.Walk])

  Di2 <- .pnbd_dyncov_LL_Di2_gen(data.work.aux = data.work.life[AuxTrans==T], i = i,
                                 d.omega =data.work.life[AuxTrans==F, d], k0x = data.work.life[AuxTrans==F, Num.Walk])

  return(Di1 + Di2)

}
