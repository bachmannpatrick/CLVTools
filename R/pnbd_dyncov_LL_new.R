walk_to_long <- function(dt.walks, process, cov.name){
  num.walks <- dt.walks[, max(Num.Walk)]
  id.vars <- c("Id", "Date", "AuxTrans")
  dt.melted <- melt(dt.walks,
                    id.vars = id.vars,
                    measure.vars = c(paste0("Walk", seq(1, num.walks)), "Max.Walk"))

  # dt.melted[, walk_id := paste0(as.character(.GRP), "_", process, "_", cov.name), by=id.vars]
  dt.melted[, walk_id := .GRP, by=id.vars]

  dt.melted[dt.walks, tjk:=i.tjk, on=id.vars]
  dt.melted[dt.walks, d:=i.d, on=id.vars]
  dt.melted[dt.walks, delta:=i.delta, on=id.vars]

  dt.melted[, process := process]
  dt.melted[, cov.name := cov.name]

  # as many groups as walks
  stopifnot(dt.melted[, uniqueN(walk_id)] == dt.walks[, .N])
  return(dt.melted[])
}


long_walks_split <- function(dt.long.cov.wide, names.cov){
  l.split.covs <- split(dt.long.cov.wide[, .SD, .SDcols=c("Id", names.cov)], by="Id", sorted = TRUE, keep.by=FALSE)
  l.split.walkinfo <- split(unique(dt.long.cov.wide[, c("Id", "from", "to", "tjk", "d", "delta", "AuxTrans")]), by="Id", sorted = TRUE, keep.by=FALSE)
  stopifnot(all(names(l.split.walkinfo) == names(l.split.covs)))
  names.id <- names(l.split.covs)

  l.user.data <- lapply(names.id, function(n){
    list(covs=l.split.covs[[n]],
         info=l.split.walkinfo[[n]])
  })
  names(l.user.data) <- names.id
  return(l.user.data)
}

long_walks_to_rcpp <- function(l.melted.process){
  id.vars <- c("Id", "Date", "AuxTrans")

  dt.melted <- rbindlist(l.melted.process)
  # ensure d, tjk, delta are the same for all covs, per walk
  #   so can also include in long table per walk
  stopifnot(dt.melted[, uniqueN(d), by=id.vars][, all(V1==1)])
  stopifnot(dt.melted[, uniqueN(tjk), by=id.vars][, all(V1==1)])
  stopifnot(dt.melted[, uniqueN(delta), by=id.vars][, all(V1==1)])

  dt.long.cov.wide <- tryCatch(dcast(Id+Date+AuxTrans+variable+tjk+d+delta~cov.name, data=dt.melted,
                                     value.var = "value"),
                               # stop if message about fun.aggregate is printed
                               message = function(m)stop(m))

  dt.long.cov.wide <- dt.long.cov.wide[complete.cases(dt.long.cov.wide)]
  setkeyv(dt.long.cov.wide, id.vars)


  # every combo still has Max.Walk and Walk1
  stopifnot(dt.long.cov.wide[variable=="Walk1", .N] == uniqueN(dt.long.cov.wide[, .SD, .SDcols=id.vars]))
  stopifnot(dt.long.cov.wide[variable=="Max.Walk", .N] == uniqueN(dt.long.cov.wide[, .SD, .SDcols=id.vars]))

  # Per Id: Mark cutoffs
  #   mark walks
  #   by Id: mark position of walks relative to beginning
  #   add from to by walks
  dt.long.cov.wide[, walk_id := .GRP, by=id.vars]
  dt.long.cov.wide[variable=="Max.Walk", variable:="WalkMax.Walk"]
  dt.long.cov.wide[order(Id, Date, AuxTrans, variable), id_relative_pos := seq(from=1, to=.N), by="Id"]
  dt.long.cov.wide[variable=="WalkMax.Walk", variable:="Max.Walk"]
  # ensure order really correct: Max.Walk last
  stopifnot(dt.long.cov.wide[, tail(variable,n=1) == "Max.Walk", by=id.vars][, all(V1)])
  # by walk_id_ from: Walk1, to:Max.Walk
  dt.long.cov.wide[, from := min(id_relative_pos), by="walk_id"]
  dt.long.cov.wide[, to := max(id_relative_pos), by="walk_id"]

  return(dt.long.cov.wide)
  # names.cov <- dt.melted[,  unique(cov.name)]


  # Cannot keep AuxTrans in separate table because cutoffs will not match anymore
  # l.walks.aux <- long_walks_split(dt.long.cov.wide[AuxTrans==TRUE], names.cov=names.cov)
  # l.walks.real <- long_walks_split(dt.long.cov.wide[AuxTrans==FALSE], names.cov=names.cov)

  # - separate for real and aux trans
  # - verify correctness only for aux trans first (sum up some walks in Rcpp + manually)
}


pnbd_dyncov_LL_new <- function(params, clv.fitted, return.all.intermediate.results=FALSE){
  # cran silence
  Num.Walk <- AuxTrans <- Di.Max.Walk <- adj.Max.Walk <- Di.adj.Walk1 <- adj.Walk1 <- A1T <- x <- A1sum <- AuxTrans <- transaction.cov.dyn <- Id <- Bjsum <- Bksum <- AkT <- NULL
  adj.transaction.cov.dyn <- dT <- d <- B1 <- t.x <- BT <- a1 <- akt <- aT <- T.cal <- C1T <- CkT <- adj.lifetime.cov.dyn <- D1 <- DT <- DkT <- b1 <- bkT <- bT <- a1T <- NULL
  b1T <- alpha_1 <- beta_1 <- alpha_2 <- beta_2 <- F2.1 <- F2.2 <- F2.3 <- i <- Ai <- Bi <- ai <- Ci <- Id <- Di <- bi <- log.F0 <- F1 <- F2 <- F3 <- LL <- NULL
  Akprod <- Z <- splus1 <- log.F1 <- log.F3 <- max.AB <- NULL

  model.params <- params[clv.fitted@clv.model@names.prefixed.params.model]
  # The param names after duplication in the constraint interlayer
  life.params  <- params[clv.fitted@names.prefixed.params.after.constr.life]
  trans.params <- params[clv.fitted@names.prefixed.params.after.constr.trans]

  r         <- exp(model.params[["log.r"]])
  alpha_0   <- exp(model.params[["log.alpha"]])
  s         <- exp(model.params[["log.s"]])
  beta_0    <- exp(model.params[["log.beta"]])

  for(i in seq_along(clv.fitted@data.walks.life))
    setkeyv(clv.fitted@data.walks.life[[i]], c("Id", "Date"))
  for(i in seq_along(clv.fitted@data.walks.trans))
    setkeyv(clv.fitted@data.walks.trans[[i]], c("Id", "Date"))

  # Make copy of clv.fitted's cbs as will be extensively modified / ie all data saved in
  cbs <- copy(clv.fitted@cbs)

  #add num walk to cbs
  cbs[, Num.Walk := clv.fitted@data.walks.trans[[1]][AuxTrans==TRUE, Num.Walk]]
  setkeyv(cbs, c("Id", "Num.Walk"))

  # create a single, large data.table ---------------------------------------------------
  #   containing all the data needed for calculation

  # for future: USE SET TO COPY FASTER

  data.work.trans <- data.table(clv.fitted@data.walks.trans[[1]][, "Id"],
                                clv.fitted@data.walks.trans[[1]][, "Date"],
                                clv.fitted@data.walks.trans[[1]][, "AuxTrans"],
                                clv.fitted@data.walks.trans[[1]][, "Num.Walk"],
                                clv.fitted@data.walks.trans[[1]][, "d"],
                                clv.fitted@data.walks.trans[[1]][, "delta"],
                                clv.fitted@data.walks.trans[[1]][, "tjk"],
                                adj      = .calc.adjusted.walks(walks=clv.fitted@data.walks.trans, gammas = trans.params), #adj.Walk1, adj.Walk2, ... adj.Max.Walk
                                adj      = .calc.adjusted.data(data = clv.fitted@data.walks.trans, data.names = c("transaction.cov.dyn"), gammas = trans.params))


  data.work.life <- data.table(clv.fitted@data.walks.life[[1]][, "Id"],
                               clv.fitted@data.walks.life[[1]][, "Date"],
                               clv.fitted@data.walks.life[[1]][, "AuxTrans"],
                               clv.fitted@data.walks.life[[1]][, "Num.Walk"],
                               clv.fitted@data.walks.life[[1]][, "d"],
                               adj      = .calc.adjusted.walks(walks = clv.fitted@data.walks.life, gammas = life.params), #adj.Walk1, adj.Walk2, ... adj.Max.Walk
                               adj      = .calc.adjusted.data( data  = clv.fitted@data.walks.life, data.names = c("lifetime.cov.dyn"), gammas = life.params) )

  setkeyv(data.work.trans, c("Id", "Date", "AuxTrans", "Num.Walk"))
  setkeyv(data.work.life,  c("Id", "Date", "AuxTrans", "Num.Walk"))

  ########
  # #instead of changing Max.Walk in pnbd_LL_Di a new column Di.Max.Walk is introduced which contains these changes to Max.Walk
  # #this way a copy can be avoided in _Di
  # #any customers Num.Walk is 1 (independent of AuxTrans)? -> Make AuxTrans Di.max.walk=NA
  # #(this was Jeffs strange implementation in _Di before)
  # data.work.life [, Di.Max.Walk:=adj.Max.Walk]

  # #if you have any Num.Walk==1, set your RealTrans  Di.Max.Walk = NA
  # #get anybody with Num.Walk == 1 and for this IDs set Di.Max.Walk = NA where AuxTrans==F
  # any.num.walk.e.1 <- data.work.life[Num.Walk==1, Id,  by=Id]$Id
  # data.work.life[AuxTrans==F & Id %in% any.num.walk.e.1, Di.Max.Walk := as.double(NA)]
  #########

  #instead of changing Max.Walk in pnbd_LL_Di a new column Di.Max.Walk is introduced which contains these changes to Max.Walk
  #this way a copy can be avoided in _Di
  #Where Num.Walk == 1 set max.walk = NA
  data.work.life[, Di.Max.Walk:=adj.Max.Walk]
  data.work.life[, Di.adj.Walk1:=adj.Walk1]
  data.work.life[Num.Walk==1 & AuxTrans==FALSE, Di.Max.Walk:=as.double(NA)]
  data.work.life[Num.Walk==1 & AuxTrans==TRUE, Di.adj.Walk1:=as.double(NA)]

  # Transaction or Purchase Process ---------------------------------------------------
  cbs[, A1T:= data.work.trans[AuxTrans==T, adj.Walk1]]

  cbs[x==0, A1sum:= 0]
  # calc g1*sum(trans.cov.dyn)[1] + g1*sum(trans.cov.dyn)[2] + g1*sum(trans.cov.dyn)[3]
  # exp() missing, ie not adj. function
  cbs[x!=0, A1sum:= rowSums(mapply(function(w,g){w[AuxTrans==F, g* sum(transaction.cov.dyn),by=Id ]$V1}, w=clv.fitted@data.walks.trans, g=trans.params ) )]

  cbs[, Bjsum:=.pnbd_dyncov_LL_BkSum(data.work.trans = data.work.trans, BkT = F)$Bjsum]
  cbs[, Bksum:=.pnbd_dyncov_LL_BkSum(data.work.trans = data.work.trans, BkT = T)$Bksum]

  cbs[, AkT:=data.work.trans[AuxTrans==T, adj.transaction.cov.dyn]]

  cbs[, dT:= data.work.trans[AuxTrans==T, d]]


  names.walk.cols.trans <- grep(pattern = "adj.Walk", value=TRUE, fixed=TRUE, x=colnames(data.work.trans))
  data.work.trans.aux <- data.work.trans[AuxTrans==T]

  # cbs[, B1:=.pnbd_dyncov_LL_Bi(data.work.trans.aux = data.work.trans[AuxTrans==T], cbs.t.x = t.x, i = 1)]
  cbs[, B1:=pnbd_dyncov_LL_Bi_cpp(i=1,
                                  t_x=t.x, d=data.work.trans.aux$d, delta=data.work.trans.aux$delta,
                                  n_walks=data.work.trans.aux$Num.Walk, max_walks=data.work.trans.aux$adj.Max.Walk,
                                  walks = as.matrix(data.work.trans.aux[, .SD, .SDcols=names.walk.cols.trans]))]
  # cbs[, BT:=.pnbd_dyncov_LL_Bi(data.work.trans.aux = data.work.trans[AuxTrans==T], cbs.t.x = t.x, i = data.work.trans[, max(Num.Walk)])]
  cbs[, BT:=pnbd_dyncov_LL_Bi_cpp(i=data.work.trans[, max(Num.Walk)],
                                  t_x=t.x, d=data.work.trans.aux$d, delta=data.work.trans.aux$delta,
                                  n_walks=data.work.trans.aux$Num.Walk, max_walks=data.work.trans.aux$adj.Max.Walk,
                                  walks = as.matrix(data.work.trans.aux[, .SD, .SDcols=names.walk.cols.trans]))]

  cbs[, a1:= Bjsum + B1 + A1T * (t.x + dT - 1)]

  cbs[, akt:= Bjsum + BT + AkT * (t.x + dT + data.work.trans[AuxTrans==TRUE, Num.Walk] - 2)]

  cbs[, aT:= Bjsum + BT + (T.cal * AkT)]



  # Lifetime Process ---------------------------------------------------
  #   Num.walk in cbs is kxT!
  cbs[, C1T:= data.work.life[AuxTrans==T, adj.Walk1]]

  cbs[, CkT:= data.work.life[AuxTrans==T, adj.lifetime.cov.dyn]]
  names.walk.cols.life <- grep(pattern = "adj.Walk", value=TRUE, fixed=TRUE, x=colnames(data.work.life))
  data.work.life.real <- data.work.life[AuxTrans==FALSE]
  data.work.life.aux  <- data.work.life[AuxTrans==TRUE]

  # cbs[, D1:= .pnbd_dyncov_LL_Di(data.work.life = data.work.life, i = 1)]
  cbs[, D1:= pnbd_dyncov_LL_Di_cpp(i=1,
                                   real_d=data.work.life.real$d,
                                   real_n_walks=data.work.life.real$Num.Walk,
                                   real_max_walks=data.work.life.real$Di.Max.Walk,
                                   real_adj_walk1=data.work.life.real$Di.adj.Walk1,
                                   real_walks=as.matrix(data.work.life.real[, .SD, .SDcols=names.walk.cols.life]),
                                   aux_d=data.work.life.aux$d,
                                   aux_n_walks=data.work.life.aux$Num.Walk,
                                   aux_max_walks=data.work.life.aux$Di.Max.Walk,
                                   aux_walks=as.matrix(data.work.life.aux[, .SD, .SDcols=names.walk.cols.life]))]

  # cbs[, DT:= .pnbd_dyncov_LL_Di(data.work.life = data.work.life, i = data.work.life[, max(Num.Walk)] ) ]
  cbs[, DT:= pnbd_dyncov_LL_Di_cpp(i=data.work.life[, max(Num.Walk)],
                                   real_d=data.work.life.real$d,
                                   real_n_walks=data.work.life.real$Num.Walk,
                                   real_max_walks=data.work.life.real$Di.Max.Walk,
                                   real_adj_walk1=data.work.life.real$Di.adj.Walk1,
                                   real_walks=as.matrix(data.work.life.real[, .SD, .SDcols=names.walk.cols.life]),
                                   aux_d=data.work.life.aux$d,
                                   aux_n_walks=data.work.life.aux$Num.Walk,
                                   aux_max_walks=data.work.life.aux$Di.Max.Walk,
                                   aux_walks=as.matrix(data.work.life.aux[, .SD, .SDcols=names.walk.cols.life]))]

  cbs[, DkT:= CkT*T.cal + DT]

  cbs[, b1:=D1 + C1T*(t.x + dT - 1)]

  cbs[, bkT:=DT + CkT*(t.x+dT + Num.Walk - 2)]

  cbs[, bT:= DT + T.cal*CkT]


  # F2 ------------------------------------------------------------------
  #   For Num.Walk == 1: F2 = F2.1
  #   For Num.Walk >  1: F2 = F2.1, F2.2, F2.3

  #Prepare
  cbs[, splus1 := s+1] # used to call hypergeom functions with vectors
  cbs.f2.num.e.1 <- subset(cbs, Num.Walk == 1)
  cbs.f2.num.g.1 <- subset(cbs, Num.Walk > 1)

  # F2 for Num.Walk == 1 ---------------------------------------------------
  cbs.f2.num.e.1[, a1T     :=Bjsum + B1 + T.cal*A1T]
  cbs.f2.num.e.1[, b1T     :=D1+T.cal*C1T]

  cbs.f2.num.e.1[, alpha_1 :=a1 +A1T*(1-dT) + alpha_0]
  cbs.f2.num.e.1[, beta_1  := (b1 + (1-dT)*C1T + beta_0) * A1T/C1T]

  cbs.f2.num.e.1[, alpha_2 :=a1T + alpha_0]
  cbs.f2.num.e.1[, beta_2  := (b1T  + beta_0)*A1T/C1T]
  if(nrow(cbs.f2.num.e.1[alpha_1 >= beta_1]) > 0){
    # cbs.f2.num.e.1[alpha_1 >= beta_1, F2.1:= (A1T/C1T)^s * .hyp.alpha.ge.beta(.SD, r=r, s=s, alpha_0=alpha_0)]
    cbs.f2.num.e.1[alpha_1 >= beta_1, F2.1:= (A1T/C1T)^s * hyp_alpha_ge_beta_cpp(alpha_1=alpha_1, beta_1=beta_1,
                                                                                 alpha_2=alpha_2, beta_2=beta_2,
                                                                                 x=x,r=r, s=s)]

  }
  if(nrow(cbs.f2.num.e.1[alpha_1 < beta_1]) > 0){
    # cbs.f2.num.e.1[alpha_1 <  beta_1, F2.1:= (A1T/C1T)^s * .hyp.beta.g.alpha(.SD, r=r, s=s, alpha_0=alpha_0)]
    cbs.f2.num.e.1[alpha_1 <  beta_1, F2.1:= (A1T/C1T)^s * hyp_beta_g_alpha_cpp(alpha_1=alpha_1, beta_1=beta_1,
                                                                                alpha_2=alpha_2, beta_2=beta_2,
                                                                                x=x,r=r, s=s)]
  }



  # F2.1 (only for Num.Walk > 1) ---------------------------------------------------
  cbs.f2.num.g.1[, alpha_1:=a1 + (1-dT)*A1T + alpha_0]
  cbs.f2.num.g.1[, beta_1:= (b1 + (1-dT)*C1T + beta_0) * A1T/C1T]

  cbs.f2.num.g.1[, alpha_2:=a1 + A1T + alpha_0]
  cbs.f2.num.g.1[, beta_2:=(b1 + C1T + beta_0)*A1T/C1T]

  if(nrow(cbs.f2.num.g.1[alpha_1 >= beta_1]) > 0){
    # cbs.f2.num.g.1[alpha_1 >= beta_1, F2.1:= (A1T/C1T)^s * .hyp.alpha.ge.beta(cbs =.SD, r=r, s=s, alpha_0=alpha_0)]
    cbs.f2.num.g.1[alpha_1 >= beta_1, F2.1:= (A1T/C1T)^s * hyp_alpha_ge_beta_cpp(alpha_1=alpha_1, beta_1=beta_1,
                                                                                 alpha_2=alpha_2, beta_2=beta_2,
                                                                                 x=x,r=r, s=s)]

  }
  if(nrow(cbs.f2.num.g.1[alpha_1 < beta_1]) > 0){
    # cbs.f2.num.g.1[alpha_1 < beta_1,  F2.1:= (A1T/C1T)^s * .hyp.beta.g.alpha(cbs =.SD, r=r, s=s, alpha_0=alpha_0)]
    cbs.f2.num.g.1[alpha_1 < beta_1,  F2.1:= (A1T/C1T)^s * hyp_beta_g_alpha_cpp(alpha_1=alpha_1, beta_1=beta_1,
                                                                                alpha_2=alpha_2, beta_2=beta_2,
                                                                                x=x,r=r, s=s)]
  }


  # F2.2 (only for Num.Walk > 1) ---------------------------------------------------
  cbs.f2.num.g.1[, alpha_1:= akt + alpha_0]
  cbs.f2.num.g.1[, beta_1:=  (bkT + beta_0)*AkT/CkT]

  cbs.f2.num.g.1[, alpha_2:= (aT + alpha_0)]
  cbs.f2.num.g.1[, beta_2:=  (bT + beta_0)*AkT/CkT]

  if(nrow(cbs.f2.num.g.1[alpha_1 >= beta_1]) > 0){
    # cbs.f2.num.g.1[alpha_1 >= beta_1, F2.2:= (AkT/CkT)^s * .hyp.alpha.ge.beta(cbs =.SD, r=r, s=s, alpha_0=alpha_0)]
    cbs.f2.num.g.1[alpha_1 >= beta_1, F2.2:= (AkT/CkT)^s * hyp_alpha_ge_beta_cpp(alpha_1=alpha_1, beta_1=beta_1,
                                                                                 alpha_2=alpha_2, beta_2=beta_2,
                                                                                 x=x,r=r, s=s)]
  }
  if(nrow(cbs.f2.num.g.1[alpha_1 < beta_1]) > 0){
    # cbs.f2.num.g.1[alpha_1 < beta_1,  F2.2:= (AkT/CkT)^s * .hyp.beta.g.alpha(cbs =.SD, r=r, s=s, alpha_0=alpha_0)]
    cbs.f2.num.g.1[alpha_1 < beta_1,  F2.2:= (AkT/CkT)^s * hyp_beta_g_alpha_cpp(alpha_1=alpha_1, beta_1=beta_1,
                                                                                alpha_2=alpha_2, beta_2=beta_2,
                                                                                x=x,r=r, s=s)]
  }


  # F2.3 (only for Num.Walk > 1) ---------------------------------------------------
  # to init for loop and default 0 for all
  cbs.f2.num.g.1[, F2.3:=0]

  max.walk <- data.work.life[,max(Num.Walk)]
  all.walks <- c(paste0("Walk", 1:(max.walk)))

  if(nrow(cbs.f2.num.g.1) != 0){

    work.trans.aux <- data.work.trans[AuxTrans==TRUE]
    work.life.aux  <- data.work.life[AuxTrans==TRUE]
    work.life.real  <- data.work.life[AuxTrans==FALSE]

    F2.3.vec <- F2_3_vecs_cpp(# cbs
      n_walks_cbs = cbs.f2.num.g.1$Num.Walk,
      dT_cbs      = cbs.f2.num.g.1$dT,
      Bjsum_cbs   = cbs.f2.num.g.1$Bjsum,
      x_cbs       = cbs.f2.num.g.1$x,
      t_x_cbs     = cbs.f2.num.g.1$t.x,

      # walks trans real
      n_walks_trans   = work.trans.aux$Num.Walk,
      d_trans         = work.trans.aux$d,
      delta_trans     = work.trans.aux$delta,
      max_walks_trans = work.trans.aux$adj.Max.Walk,
      walks_trans     = as.matrix(work.trans.aux[, .SD, .SDcols=names.walk.cols.trans]),

      # walks life real
      n_walks_life_real   = work.life.real$Num.Walk,
      d_life_real         = work.life.real$d,
      max_walks_life_real = work.life.real$Di.Max.Walk,
      adj_walk1_life_real = work.life.real$Di.adj.Walk1,
      walks_life_real     = as.matrix(work.life.real[, .SD, .SDcols=names.walk.cols.life]),

      # walks life aux
      n_walks_life_aux   = work.life.aux$Num.Walk,
      d_life_aux         = work.life.aux$d,
      max_walks_life_aux = work.life.aux$Di.Max.Walk,
      walks_life_aux     = as.matrix(work.life.aux[, .SD, .SDcols=names.walk.cols.life]),

      # model params
      r=r, alpha=alpha_0,
      s=s, beta=beta_0)

    cbs.f2.num.g.1$F2.3 <- F2.3.vec

  }#if


  #Put F2 results together
  if(cbs[, any(Num.Walk == 1)])
    cbs[Num.Walk==1, `:=`(F2.1=cbs.f2.num.e.1$F2.1,
                          F2.2=as.numeric(0),
                          F2.3=as.numeric(0))]

  # if(nrow(cbs[Num.Walk > 1])>0)
  if(cbs[, any(Num.Walk > 1)])
    cbs[Num.Walk >1, `:=`(F2.1=cbs.f2.num.g.1$F2.1,
                          F2.2=cbs.f2.num.g.1$F2.2,
                          F2.3=cbs.f2.num.g.1$F2.3)]

  # NEW PART ------------------------------------------------------------------------------------------------
  cbs[, Adj.Walk1.Aux.Trans:= data.work.trans[AuxTrans==T, adj.Walk1]]
  cbs[, adj.transaction.cov.dyn := data.work.trans[AuxTrans==T, adj.transaction.cov.dyn]]
  cbs[, Adj.Walk1.Aux.Life := data.work.life[AuxTrans==T, adj.Walk1]]
  cbs[, adj.lifetime.cov.dyn:= data.work.life[AuxTrans==T, adj.lifetime.cov.dyn]]

  cbs[, LL.i := pnbd_dyncov_LL_i(
    r = r, alpha_0=alpha_0, s=s, beta_0=beta_0,
    x = x, t_x = t.x, T_cal=T.cal,
    num_walks = Num.Walk,
    adj_transaction_cov_dyn = adj.transaction.cov.dyn,
    adj_lifetime_cov_dyn = adj.lifetime.cov.dyn,
    dT = dT,
    A1T_R = Adj.Walk1.Aux.Trans,
    C1T_R = Adj.Walk1.Aux.Life,
    A1sum_R = A1sum,
    Bjsum = Bjsum,
    Bksum = Bksum,
    B1 = B1,
    BT = BT,
    D1 = D1,
    DT = DT,
    F2_3 = F2.3),
    by="Id"]

  cbs[, LL := LL.i]



  # Try cheating for stabilty -----------------------------------------------------
  # Replace infinite LL values with the most extreme (finite) LL value
  perc.infinite <- cbs[, mean(is.infinite(LL))]

  if(perc.infinite > 0){

    warning(paste0("There are ", round(perc.infinite*100, digits=5)," percent +/- infinity values"),
            immediate. = FALSE)

    # If we have less than 5 % infinite values impute them with the max value we have in the likelihood
    if(perc.infinite <= 0.05){

      # most extreme value in the likelihood (without the infinity values)
      most.extreme.LL <- cbs[is.finite(LL), max(abs(LL))]

      # If the value we have is -infinity set the value to the largest negative value...
      cbs[is.infinite(LL) & sign(LL) == -1, LL := -abs(most.extreme.LL)]
      # ...if +infinity set it to largest positive value.
      cbs[is.infinite(LL) & sign(LL) == 1,  LL :=  abs(most.extreme.LL)]
    }
    # Else, if > 5%, let it propagate
  }

  setkeyv(cbs, "Id")
  return(cbs)

  if(!return.all.intermediate.results){
    cbsdata <- data.table(Id=cbs$Id,LL=cbs$LL, Akprod=exp(cbs$A1sum), Bksum=cbs$Bksum, DkT=cbs$DkT, Z=cbs$F2)
    setkey(cbsdata,"Id")
    return(cbsdata)
  }else{
    cbs[, Akprod := exp(A1sum)]
    cbs[, Z      := F2]
    setkeyv(cbs, "Id")
    return(cbs)
  }
}


# pnbd_dyncov_LL_new <- function(params, clv.fitted, return.all.intermediate.results=FALSE){
#
#   model.params <- params[clv.fitted@clv.model@names.prefixed.params.model]
#   # The param names after duplication in the constraint interlayer
#   life.params  <- params[clv.fitted@names.prefixed.params.after.constr.life]
#   trans.params <- params[clv.fitted@names.prefixed.params.after.constr.trans]
#
#   r         <- exp(model.params[["log.r"]])
#   alpha_0   <- exp(model.params[["log.alpha"]])
#   s         <- exp(model.params[["log.s"]])
#   beta_0    <- exp(model.params[["log.beta"]])
#
#   for(i in seq_along(clv.fitted@data.walks.life))
#     setkeyv(clv.fitted@data.walks.life[[i]], c("Id", "Date"))
#   for(i in seq_along(clv.fitted@data.walks.trans))
#     setkeyv(clv.fitted@data.walks.trans[[i]], c("Id", "Date"))
#
#   # Make copy of clv.fitted's cbs as will be extensively modified / ie all data saved in
#   cbs <- copy(clv.fitted@cbs)
#
#   #add num walk to cbs
#   cbs[, Num.Walk := clv.fitted@data.walks.trans[[1]][AuxTrans==TRUE, Num.Walk]]
#   setkeyv(cbs, c("Id", "Num.Walk"))
#
#   # create a single, large data.table ---------------------------------------------------
#   #   containing all the data needed for calculation
#
#   # for future: USE SET TO COPY FASTER
#
#   data.work.trans <- data.table(clv.fitted@data.walks.trans[[1]][, "Id"],
#                                 clv.fitted@data.walks.trans[[1]][, "Date"],
#                                 clv.fitted@data.walks.trans[[1]][, "AuxTrans"],
#                                 clv.fitted@data.walks.trans[[1]][, "Num.Walk"],
#                                 clv.fitted@data.walks.trans[[1]][, "d"],
#                                 clv.fitted@data.walks.trans[[1]][, "delta"],
#                                 clv.fitted@data.walks.trans[[1]][, "tjk"],
#                                 adj      = .calc.adjusted.walks(walks=clv.fitted@data.walks.trans, gammas = trans.params), #adj.Walk1, adj.Walk2, ... adj.Max.Walk
#                                 adj      = .calc.adjusted.data(data = clv.fitted@data.walks.trans, data.names = c("transaction.cov.dyn"), gammas = trans.params))
#
#
#   data.work.life <- data.table(clv.fitted@data.walks.life[[1]][, "Id"],
#                                clv.fitted@data.walks.life[[1]][, "Date"],
#                                clv.fitted@data.walks.life[[1]][, "AuxTrans"],
#                                clv.fitted@data.walks.life[[1]][, "Num.Walk"],
#                                clv.fitted@data.walks.life[[1]][, "d"],
#                                adj      = .calc.adjusted.walks(walks = clv.fitted@data.walks.life, gammas = life.params), #adj.Walk1, adj.Walk2, ... adj.Max.Walk
#                                adj      = .calc.adjusted.data( data  = clv.fitted@data.walks.life, data.names = c("lifetime.cov.dyn"), gammas = life.params) )
#
#   setkeyv(data.work.trans, c("Id", "Date", "AuxTrans", "Num.Walk"))
#   setkeyv(data.work.life,  c("Id", "Date", "AuxTrans", "Num.Walk"))
#
#   data.work.life[, Di.Max.Walk:=adj.Max.Walk]
#   data.work.life[, Di.adj.Walk1:=adj.Walk1]
#   data.work.life[Num.Walk==1 & AuxTrans==FALSE, Di.Max.Walk:=as.double(NA)]
#   data.work.life[Num.Walk==1 & AuxTrans==TRUE, Di.adj.Walk1:=as.double(NA)]
#
#   # Transaction or Purchase Process ---------------------------------------------------
#   names.walk.cols.trans <- grep(pattern = "adj.Walk", value=TRUE, fixed=TRUE, x=colnames(data.work.trans))
#   data.work.trans.aux <- data.work.trans[AuxTrans==T]
#
#   cbs[, B1:=pnbd_dyncov_LL_Bi_cpp(i=1,
#                                   t_x=t.x, d=data.work.trans.aux$d, delta=data.work.trans.aux$delta,
#                                   n_walks=data.work.trans.aux$Num.Walk, max_walks=data.work.trans.aux$adj.Max.Walk,
#                                   walks = as.matrix(data.work.trans.aux[, .SD, .SDcols=names.walk.cols.trans]))]
#
#   cbs[, BT:=pnbd_dyncov_LL_Bi_cpp(i=data.work.trans[, max(Num.Walk)],
#                                   t_x=t.x, d=data.work.trans.aux$d, delta=data.work.trans.aux$delta,
#                                   n_walks=data.work.trans.aux$Num.Walk, max_walks=data.work.trans.aux$adj.Max.Walk,
#                                   walks = as.matrix(data.work.trans.aux[, .SD, .SDcols=names.walk.cols.trans]))]
#
#
#
#   # Lifetime Process ---------------------------------------------------
#   names.walk.cols.life <- grep(pattern = "adj.Walk", value=TRUE, fixed=TRUE, x=colnames(data.work.life))
#   data.work.life.real <- data.work.life[AuxTrans==FALSE]
#   data.work.life.aux  <- data.work.life[AuxTrans==TRUE]
#
#   # cbs[, D1:= .pnbd_dyncov_LL_Di(data.work.life = data.work.life, i = 1)]
#   cbs[, D1:= pnbd_dyncov_LL_Di_cpp(i=1,
#                                    real_d=data.work.life.real$d,
#                                    real_n_walks=data.work.life.real$Num.Walk,
#                                    real_max_walks=data.work.life.real$Di.Max.Walk,
#                                    real_adj_walk1=data.work.life.real$Di.adj.Walk1,
#                                    real_walks=as.matrix(data.work.life.real[, .SD, .SDcols=names.walk.cols.life]),
#                                    aux_d=data.work.life.aux$d,
#                                    aux_n_walks=data.work.life.aux$Num.Walk,
#                                    aux_max_walks=data.work.life.aux$Di.Max.Walk,
#                                    aux_walks=as.matrix(data.work.life.aux[, .SD, .SDcols=names.walk.cols.life]))]
#
#   # cbs[, DT:= .pnbd_dyncov_LL_Di(data.work.life = data.work.life, i = data.work.life[, max(Num.Walk)] ) ]
#   cbs[, DT:= pnbd_dyncov_LL_Di_cpp(i=data.work.life[, max(Num.Walk)],
#                                    real_d=data.work.life.real$d,
#                                    real_n_walks=data.work.life.real$Num.Walk,
#                                    real_max_walks=data.work.life.real$Di.Max.Walk,
#                                    real_adj_walk1=data.work.life.real$Di.adj.Walk1,
#                                    real_walks=as.matrix(data.work.life.real[, .SD, .SDcols=names.walk.cols.life]),
#                                    aux_d=data.work.life.aux$d,
#                                    aux_n_walks=data.work.life.aux$Num.Walk,
#                                    aux_max_walks=data.work.life.aux$Di.Max.Walk,
#                                    aux_walks=as.matrix(data.work.life.aux[, .SD, .SDcols=names.walk.cols.life]))]
#
#
#
#   # F2.3 (only for Num.Walk > 1) ---------------------------------------------------
#   cbs[, splus1 := s+1] # used to call hypergeom functions with vectors
#   cbs.f2.num.g.1 <- subset(cbs, Num.Walk > 1)
#   # to init for loop and default 0 for all
#   cbs.f2.num.g.1[, F2.3:=0]
#
#   max.walk <- data.work.life[,max(Num.Walk)]
#   all.walks <- c(paste0("Walk", 1:(max.walk)))
#
#   if(nrow(cbs.f2.num.g.1) != 0){
#
#     work.trans.aux <- data.work.trans[AuxTrans==TRUE]
#     work.life.aux  <- data.work.life[AuxTrans==TRUE]
#     work.life.real  <- data.work.life[AuxTrans==FALSE]
#
#     F2.3.vec <- F2_3_vecs_cpp(# cbs
#       n_walks_cbs = cbs.f2.num.g.1$Num.Walk,
#       dT_cbs      = cbs.f2.num.g.1$dT,
#       Bjsum_cbs   = cbs.f2.num.g.1$Bjsum,
#       x_cbs       = cbs.f2.num.g.1$x,
#       t_x_cbs     = cbs.f2.num.g.1$t.x,
#
#       # walks trans real
#       n_walks_trans   = work.trans.aux$Num.Walk,
#       d_trans         = work.trans.aux$d,
#       delta_trans     = work.trans.aux$delta,
#       max_walks_trans = work.trans.aux$adj.Max.Walk,
#       walks_trans     = as.matrix(work.trans.aux[, .SD, .SDcols=names.walk.cols.trans]),
#
#       # walks life real
#       n_walks_life_real   = work.life.real$Num.Walk,
#       d_life_real         = work.life.real$d,
#       max_walks_life_real = work.life.real$Di.Max.Walk,
#       adj_walk1_life_real = work.life.real$Di.adj.Walk1,
#       walks_life_real     = as.matrix(work.life.real[, .SD, .SDcols=names.walk.cols.life]),
#
#       # walks life aux
#       n_walks_life_aux   = work.life.aux$Num.Walk,
#       d_life_aux         = work.life.aux$d,
#       max_walks_life_aux = work.life.aux$Di.Max.Walk,
#       walks_life_aux     = as.matrix(work.life.aux[, .SD, .SDcols=names.walk.cols.life]),
#
#       # model params
#       r=r, alpha=alpha_0,
#       s=s, beta=beta_0)
#
#     cbs.f2.num.g.1$F2.3 <- F2.3.vec
#
#   }
#
#
#   # if(cbs[, any(Num.Walk > 1)])
#   #   cbs[Num.Walk >1, `:=`(F2.1=cbs.f2.num.g.1$F2.1,
#   #                         F2.2=cbs.f2.num.g.1$F2.2,
#   #                         F2.3=cbs.f2.num.g.1$F2.3)]
#   cbs[, F2.3 := 0]
#   cbs[cbs.f2.num.g.1, F2.3 := i.F2.3, on="Id"]
#
#
#
#
#   # Try cheating for stabilty -----------------------------------------------------
#   # Replace infinite LL values with the most extreme (finite) LL value
#   perc.infinite <- cbs[, mean(is.infinite(LL))]
#
#   if(perc.infinite > 0){
#
#     warning(paste0("There are ", round(perc.infinite*100, digits=5)," percent +/- infinity values"),
#             immediate. = FALSE)
#
#     # If we have less than 5 % infinite values impute them with the max value we have in the likelihood
#     if(perc.infinite <= 0.05){
#
#       # most extreme value in the likelihood (without the infinity values)
#       most.extreme.LL <- cbs[is.finite(LL), max(abs(LL))]
#
#       # If the value we have is -infinity set the value to the largest negative value...
#       cbs[is.infinite(LL) & sign(LL) == -1, LL := -abs(most.extreme.LL)]
#       # ...if +infinity set it to largest positive value.
#       cbs[is.infinite(LL) & sign(LL) == 1,  LL :=  abs(most.extreme.LL)]
#     }
#     # Else, if > 5%, let it propagate
#   }
#
#
#   if(!return.all.intermediate.results){
#     cbsdata <- data.table(Id=cbs$Id,LL=cbs$LL, Akprod=exp(cbs$A1sum), Bksum=cbs$Bksum, DkT=cbs$DkT, Z=cbs$F2)
#     setkey(cbsdata,"Id")
#     return(cbsdata)
#   }else{
#     cbs[, Akprod := exp(A1sum)]
#     cbs[, Z      := F2]
#     setkeyv(cbs, "Id")
#     return(cbs)
#   }
# }


