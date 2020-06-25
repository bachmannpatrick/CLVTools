pnbd_dyncov_LL_sum <- function(params, clv.fitted){
  return(-sum(pnbd_dyncov_LL_ind(params=params, clv.fitted=clv.fitted)))
}


# Returns only the individual LL values per customer and no other data.
pnbd_dyncov_LL_ind <- function(params, clv.fitted){
  LL <- NULL
  cbsdata_ind <- pnbd_dyncov_LL(params=params, clv.fitted=clv.fitted)
  return(cbsdata_ind[, LL])
}

#' @importFrom foreach foreach %dopar%
pnbd_dyncov_LL <- function(params, clv.fitted, return.all.intermediate.results=FALSE){
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

  cbs[, B1:=.pnbd_dyncov_LL_Bi(data.work.trans.aux = data.work.trans[AuxTrans==T], cbs.t.x = t.x, i = 1)]
  cbs[, BT:=.pnbd_dyncov_LL_Bi(data.work.trans.aux = data.work.trans[AuxTrans==T], cbs.t.x = t.x, i = data.work.trans[, max(Num.Walk)])]

  cbs[, a1:= Bjsum + B1 + A1T * (t.x + dT - 1)]

  cbs[, akt:= Bjsum + BT + AkT * (t.x + dT + data.work.trans[AuxTrans==TRUE, Num.Walk] - 2)]

  cbs[, aT:= Bjsum + BT + (T.cal * AkT)]



  # Lifetime Process ---------------------------------------------------
  #   Num.walk in cbs is kxT!
  cbs[, C1T:= data.work.life[AuxTrans==T, adj.Walk1]]

  cbs[, CkT:= data.work.life[AuxTrans==T, adj.lifetime.cov.dyn]]

  cbs[, D1:= .pnbd_dyncov_LL_Di(data.work.life = data.work.life, i = 1)]
  cbs[, DT:= .pnbd_dyncov_LL_Di(data.work.life = data.work.life, i = data.work.life[, max(Num.Walk)] ) ]
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
    cbs.f2.num.e.1[alpha_1 >= beta_1, F2.1:= (A1T/C1T)^s * .hyp.alpha.ge.beta(.SD, r=r, s=s, alpha_0=alpha_0)]
  }
  if(nrow(cbs.f2.num.e.1[alpha_1 < beta_1]) > 0){
    cbs.f2.num.e.1[alpha_1 <  beta_1, F2.1:= (A1T/C1T)^s * .hyp.beta.g.alpha(.SD, r=r, s=s, alpha_0=alpha_0)]
  }



  # F2.1 (only for Num.Walk > 1) ---------------------------------------------------
  cbs.f2.num.g.1[, alpha_1:=a1 + (1-dT)*A1T + alpha_0]
  cbs.f2.num.g.1[, beta_1:= (b1 + (1-dT)*C1T + beta_0) * A1T/C1T]

  cbs.f2.num.g.1[, alpha_2:=a1 + A1T + alpha_0]
  cbs.f2.num.g.1[, beta_2:=(b1 + C1T + beta_0)*A1T/C1T]

  if(nrow(cbs.f2.num.g.1[alpha_1 >= beta_1]) > 0){
    cbs.f2.num.g.1[alpha_1 >= beta_1, F2.1:= (A1T/C1T)^s * .hyp.alpha.ge.beta(cbs =.SD, r=r, s=s, alpha_0=alpha_0)]
  }
  if(nrow(cbs.f2.num.g.1[alpha_1 < beta_1]) > 0){
    cbs.f2.num.g.1[alpha_1 < beta_1,  F2.1:= (A1T/C1T)^s * .hyp.beta.g.alpha(cbs =.SD, r=r, s=s, alpha_0=alpha_0)]
  }


  # F2.2 (only for Num.Walk > 1) ---------------------------------------------------
  cbs.f2.num.g.1[, alpha_1:= akt + alpha_0]
  cbs.f2.num.g.1[, beta_1:=  (bkT + beta_0)*AkT/CkT]

  cbs.f2.num.g.1[, alpha_2:= (aT + alpha_0)]
  cbs.f2.num.g.1[, beta_2:=  (bT + beta_0)*AkT/CkT]

  if(nrow(cbs.f2.num.g.1[alpha_1 >= beta_1]) > 0){
    cbs.f2.num.g.1[alpha_1 >= beta_1, F2.2:= (AkT/CkT)^s * .hyp.alpha.ge.beta(cbs =.SD, r=r, s=s, alpha_0=alpha_0)]
  }
  if(nrow(cbs.f2.num.g.1[alpha_1 < beta_1]) > 0){
    cbs.f2.num.g.1[alpha_1 < beta_1,  F2.2:= (AkT/CkT)^s * .hyp.beta.g.alpha(cbs =.SD, r=r, s=s, alpha_0=alpha_0)]
  }


  # F2.3 (only for Num.Walk > 1) ---------------------------------------------------
  # to init for loop and default 0 for all
  cbs.f2.num.g.1[, F2.3:=0]

  max.walk <- data.work.life[,max(Num.Walk)]
  all.walks <- c(paste0("Walk", 1:(max.walk)))

  if(nrow(cbs.f2.num.g.1) != 0){

    F2.3.vecs <-
      # %dopar% also applies sequentially with a warning if no parallel backend registered
      foreach(i = 2:max(cbs.f2.num.g.1$Num.Walk-1))%dopar%{

        work.trans.i <- data.work.trans[AuxTrans==T & ((Num.Walk-1) >= i)]
        work.life.i  <- data.work.life[AuxTrans==T & ((Num.Walk-1) >= i)]
        cbs.i        <- cbs.f2.num.g.1[Num.Walk-1 >= i]

        # Transaction Process ------------------------------------------
        cbs.i[, Ai:= work.trans.i[, get(paste0("adj.Walk", i))]]
        cbs.i[is.na(Ai), Ai:=0]
        cbs.i[, Bi:= .pnbd_dyncov_LL_Bi( data.work.trans.aux = work.trans.i, cbs.t.x = t.x, i = i)]
        cbs.i[, ai:=Bjsum + Bi + Ai*(t.x + dT + (i-2))]


        # Lifetime Process ------------------------------------------

        cbs.i[, Ci:=work.life.i[, get(paste0("adj.Walk", i))]]
        cbs.i[is.na(Ci), Ci:=0]

        # For Di: in the current implementation we also need to consider 0 to x.
        # Problem; we need the first row for each customer as well (see above)
        #  -> get all data (not only Num.Walk > i) for the IDs in work.life.i
        walk.lifetime.all.ids.i <- work.life.i[, Id]
        tmp.data.life <- data.work.life[Id %in% walk.lifetime.all.ids.i]
        tmp.data.life <- data.work.life[walk.lifetime.all.ids.i]
        setkeyv(data.work.life, c("Id", "Date", "AuxTrans", "Num.Walk"))

        cbs.i[, Di:=.pnbd_dyncov_LL_Di(data.work.life = tmp.data.life, i = i)]
        cbs.i[, bi:=Di + Ci*(t.x + dT + (i-2))]

        # Alpha & Beta ------------------------------------------------

        cbs.i[, alpha_1:=ai + alpha_0]
        cbs.i[, beta_1:=(bi + beta_0)*Ai/Ci]
        cbs.i[, alpha_2:=ai + Ai + alpha_0]
        cbs.i[, beta_2:=(bi + Ci +beta_0)*Ai/Ci]
        if(nrow(cbs.i[alpha_1 >= beta_1]) > 0){
          cbs.i[alpha_1 >= beta_1, F2.3:=(Ai/Ci)^(s) * .hyp.alpha.ge.beta(cbs=.SD, r=r, s=s, alpha_0=alpha_0)]
        }
        if(nrow(cbs.i[alpha_1 < beta_1]) > 0){
          cbs.i[alpha_1 <  beta_1, F2.3:=(Ai/Ci)^(s) * .hyp.beta.g.alpha(cbs=.SD, r=r, s=s, alpha_0=alpha_0)]
        }

        #write results to separate vector because data.table (cbs.f2.num.g.1)
        # is manipulated by reference across threads! (ie shared memory) what may abort session

        res <- rep_len(0, nrow(cbs.f2.num.g.1))
        res[cbs.f2.num.g.1[, .I[Num.Walk-1 >= i]]] <- cbs.i$F2.3 #write at right position

        return(res)

      }#for
  }#if



  # put together the results from the multiple cores again:
  cbs.f2.num.g.1$F2.3 <- Reduce("+", F2.3.vecs)

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

  # LL -----------------------------------------------------------------------------------------------------
  #
  #         LL = log(F0)+log((F1 * F2) + F3)
  #
  # We rely on various tricks to improve numerical stability
  #
  # 1. Improvement
  #   F0 quickly is too large to represent because of exp() and gamma(f(x))
  #   Because it is only used as log(F0) it can be directly rewritten:
  #
  #   F0      = ((alpha_0)^(r)*(beta_0)^(s) * (gamma(x+r)))/gamma(r) * exp(A1sum)
  #   log(F0) = r*log(alpha_0) + s*log(beta_0) + log(gamma(x+r)) - log(gamma(r)) + A1sum- (x+r)*log((Bksum + alpha_0))
  #
  #   and using the lgamma function to calculate log(gamma())
  #
  #
  # 2. Improvement
  #   log((F1 * F2) + F3) can be to large to represent. It can be rewritten
  #   using the log trick:
  #     log(A + B) = log(max(A,B)) + log(1+(min(A,B)/max(A,B)))
  #
  #   where A = (F1*F2) and B = F3 in this case and using log1p(x) instead
  #   of log(1+x) for better log approximation in case of small x:
  #
  #   LL = log.F0 + log(A+B)    # where A=F1*F2, B=F3
  #   LL = log.F0 + log(max(A,                 B))  + log(1+(min(A,B)/max(A,B))) #as described on 290-292
  #   LL = log.F0 + max(log(A),            log(B))  + log(1+(min(A,B)/max(A,B)))
  #   LL = log.F0 + max(log(F1*F2),        log(B))  + log(1+(min(A,B)/max(A,B)))
  #   LL = log.F0 + max(log(F1) + log(F2), log(B))  + log(1+(min(A,B)/max(A,B)))
  #     Hence:
  #   LL = log.F0 + max(log(F1) + log(F2), log(F3))  + log(1+(min(F1*F2,F3)/max(F1*F2,F3)))
  #
  #   log(F1) and log(F3) can be simplified to logged sums as they are products. log(F2) cannot.
  #   F1 = s/(r+s+x)                                        =>   log.F1 = log(s) - log(r+s+x)
  #   F3 = 1 /((DkT  + beta_0)^(s)*(BkSum+alpha_0)^(x+1r))  =>   log.F3 = -s*log(DkT + beta_0) - (x+r)*log(Bksum+alpha_0)
  #
  #
  # 3. Improvement
  #   The F2 can be negative/zero for some observations and log(F2) cannot be calculated. Therefore, case
  #     differentiation is done for F2. In general, log((F1*F2) + F3) is because (F1*F2) + F3 > 0 as otherwise
  #     the whole likelihood does not make sense. Also we have that always F1 > 0 and B=F3 > 0 so A=F1*F2 <= 0 is
  #     possible but at the same time A+B > 0.
  #
  #   If F2 > 0: Same calculation as before.
  #
  #   If F2 < 0:  A=F1*F2 <= 0 and B=F3 > 0 but abs(F3) > abs(F1*F2)
  #     log(max(A,B))  + log(1+(min(A,B)/max(A,B)))
  #     log(B)         + log(1+A/B)                 with -1 < (A/B) < 0
  #     log(F3)        + log(1+(F1*F2/F3))
  #
  #    If F2 = 0: Based on the original LL
  #       LL = log.F0 + log((F1*F2) + F3)
  #       LL = log.F0 + log(0 + F3)
  #       LL = log.F0 + log.F3
  #
  # 4. Improvement
  #   For the case F2 < 0, the product F1*F2 in log(1+min()/max()) can still be to large to represent.
  #     They are elimenated by artificially exp() and then log components
  #
  #     log(F3)        + log(1 +         ( F1 * F2 / F3 ))
  #     log(F3)        + log(1 + exp( log(F1))* F2 / exp( log(F3)))
  #     log(F3)        + log(1 + exp( log.F1  - log.F3) * F2)
  #
  #
  #   For the case F2 > 0, the product F1*F2 in log((F1 * F2) + F3) can still be to large
  #     to represent. They are eliminated using the log-sum-of-exponents (LSE) trick.
  #     log(A + B)
  #     log(exp(log(A)) + exp(log(B)))
  #       -> LSE
  #     max(log(A),         log(B)) + log(exp(log(A          - max(log(A),         log(B)))) + exp(log(B- max(log(A),         log(B)))))
  #     max(log(F1*F2),     log(B)) + log(exp(log(F1*F2      - max(log(F1*F2),     log(B)))) + exp(log(B- max(log(F1*F2),     log(B)))))
  #     max(log.F1+log(F2), log.F3) + log(exp(log.F1+log(F2) - max(log.F1+log(F2), log.F3))) + exp(log(B- max(log.F1+log(F2), log.F3))))
  #
  #   Or alternative:
  #     max(log(F1) + log(F2), log(B))  + log(1+       (min(A,B)                  / max(A,B)))
  #     max(log(F1) + log(F2), log(B))  + log(1+exp(log(min(A,B)                  / max(A,B))))
  #     max(log(F1) + log(F2), log(B))  + log(1+exp(log(min(A,B))                 - log(max(A,B))))
  #     max(log(F1) + log(F2), log(B))  + log(1+exp(min(log(A),log(B))            - max(log(A),log(B))))
  #     max(log(F1) + log(F2), log(B))  + log(1+exp(min(log(F1*F2),log(F3))       - max(log(F1*F2),log(F3))))
  #     max(log(F1) + log(F2), log(B))  + log(1+exp(min(log.F1 + log(F2), log.F3) - max(log.F1 + log(F2),log.F3)))
  #     cbs[F2 >  0,  LL.other :=log.F0 +  pmax(log.F1 + log(F2), log.F3)  + log1p(exp(pmin(log.F1 + log(F2), log.F3) - pmax(log.F1 + log(F2),log.F3)))]


  cbs[, log.F0 := r*log(alpha_0) + s*log(beta_0) + lgamma(x+r) - lgamma(r) + A1sum]
  cbs[, log.F1 := log(s) - log(r+s+x)]
  cbs[, F2     := F2.1 + F2.2 + F2.3]

  cbs[, log.F3 := -s*log(DkT + beta_0) - (x+r)*log(Bksum+alpha_0)]

  cbs[F2 == 0,  LL := log.F0 + log.F3]

  cbs[F2 <  0,  LL := log.F0 + log.F3 + log1p(exp(log.F1-log.F3)*F2)]

  cbs[F2 >  0,  max.AB := pmax(log.F1+log(F2), log.F3)]
  cbs[F2 >  0,  LL     := log.F0 + max.AB  + log(exp(log.F1+log(F2)-max.AB) + exp(log.F3 - max.AB))]


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


#
# CONCEPT
#
# Adjusted Walks: exp(cov1.trans.Wx * gamma.trans.1) * exp(cov2.trans.Wx * gamma.trans.2) * ...
#                 =exp(cov1.trans.Wx * gamma.trans.1 + cov2.trans.Wx * gamma.trans.2 + ...)
#
# adjusted.WalkX <- exp(cov1.trans.Wx * gamma.trans.1 + cov2.trans.Wx * gamma.trans.2 +  ...)
.calc.adjusted.data <- function(data, data.names, gammas){

  num.cov <- length(data)

  if(num.cov == 0)
    stop("Not implemented here!")

  #adjusted.Walks: exp(cov1.walks * gamma.1 + cov2.walks * gamma.2 + ...)
  if( num.cov == 1 ){
    adjusted.data <- exp( data[[1]][, .SD, .SDcols = data.names] * gammas[1])
  }else{
    #use as base to add the rest upon
    adjusted.data <- data[[1]][, .SD, .SDcols = data.names] * gammas[1]
    for(i in 2:num.cov)
      adjusted.data <- adjusted.data +  (data[[i]][, .SD, .SDcols = data.names] * gammas[i])

    adjusted.data <- exp(adjusted.data)
  }

  return(adjusted.data)
}

.calc.adjusted.walks <- function(walks, gammas){

  num.walk <- max(walks[[1]]$Num.Walk)

  walks.to.adjust <- paste0("Walk", 1:num.walk)
  walks.to.adjust <- c(walks.to.adjust, "Max.Walk")

  return(.calc.adjusted.data(data = walks, data.names = walks.to.adjust, gammas = gammas))
}


#FACTOR * (
#                 hyp2F1(r+s+x,s+1,r+s+x+1,(alpha_1-beta_1)/alpha_1) / (alpha_1^(r+s+x))
#               - hyp2F1(r+s+x,s+1,r+s+x+1,(alpha_2-beta_2)/alpha_2) / (alpha_2^(r+s+x))
#               )
.hyp.alpha.ge.beta <- function(cbs, alpha_0, r, s)
{
  x <- alpha_1 <- beta_1 <- alpha_2 <- beta_2 <- z.1 <- z.2 <- log.C <- hyp.z1 <- hyp.z2 <- NULL

  # hyp crashes with empty data.table
  if(nrow(cbs) > 0){

    cbs.z <- copy(cbs)
    cbs.z[,z.1 := (alpha_1-beta_1)/alpha_1]
    cbs.z[,z.2 := (alpha_2-beta_2)/alpha_2]

    cbs.z[,log.C :=  lgamma(r+s+x+1) + lgamma(s) - lgamma(r+s+x) - lgamma(s+1) ]

    l.hyp.z1 <- vec_gsl_hyp2f1_e(r+s+cbs.z$x, cbs.z$splus1, r+s+cbs.z$x+1, cbs.z$z.1)
    l.hyp.z2 <- vec_gsl_hyp2f1_e(r+s+cbs.z$x, cbs.z$splus1, r+s+cbs.z$x+1, cbs.z$z.2)

    cbs.z[, hyp.z1 := l.hyp.z1$value / (alpha_1^(r+s+x))]
    cbs.z[, hyp.z2 := l.hyp.z2$value / (alpha_2^(r+s+x))]

    # GSL_EMAXITER (11) or GSL_EDOM (1, input domain error)
    if(any(l.hyp.z1$status == 11 | l.hyp.z1$status == 1)){
      cbs.z[l.hyp.z1$status == 11 | l.hyp.z1$status == 1,
            hyp.z1 := (1-z.1)^(r+x)*exp(log.C) / beta_1^(r+s+x)]
    }

    if(any(l.hyp.z2$status == 11 | l.hyp.z2$status == 1)){
      cbs.z[l.hyp.z2$status == 11 | l.hyp.z2$status == 1,
            hyp.z2 := (1-z.2)^(r+x)*exp(log.C) / beta_2^(r+s+x)]
    }
    return(cbs.z[, hyp.z1 - hyp.z2])
  }else
    return(cbs) #return the empty data.table

}

#FACTOR * (
#                 hyp2F1(r+s+x,r+x,r+s+x+1,(beta_1-alpha_1)/beta_1) / (beta_1^(r+s+x))
#               - hyp2F1(r+s+x,r+x,r+s+x+1,(beta_2-alpha_2)/beta_2) / (beta_2^(r+s+x))
#               )
.hyp.beta.g.alpha <- function(cbs, r, s, alpha_0)
{
  x <- alpha_1 <- beta_1 <- alpha_2 <- beta_2 <- z.1 <- z.2 <- log.C <- hyp.z1 <- hyp.z2 <- NULL

  # hyp crashes with empty data.table
  if(nrow(cbs) > 0){
    cbs.z <- copy(cbs)
    cbs.z[,z.1 := (beta_1-alpha_1)/beta_1]
    cbs.z[,z.2 := (beta_2-alpha_2)/beta_2]

    cbs.z[,log.C :=  lgamma(r+s+x+1) + lgamma(r+x-1) - lgamma(r+s+x) - lgamma(r+x) ]

    l.hyp.z1 <- vec_gsl_hyp2f1_e(r+s+cbs.z$x,r+cbs.z$x,r+s+cbs.z$x+1, cbs.z$z.1)
    l.hyp.z2 <- vec_gsl_hyp2f1_e(r+s+cbs.z$x,r+cbs.z$x,r+s+cbs.z$x+1, cbs.z$z.2)

    cbs.z[, hyp.z1 := l.hyp.z1$value / (beta_1^(r+s+x))]
    cbs.z[, hyp.z2 := l.hyp.z2$value / (beta_2^(r+s+x))]

    # GSL_EMAXITER (11) or GSL_EDOM (1, input domain error)
    if(any(l.hyp.z1$status == 11 | l.hyp.z1$status == 1)){
      cbs.z[l.hyp.z1$status == 11 | l.hyp.z1$status == 1,
            hyp.z1 := (1-z.1)^(s+1)*exp(log.C) / (alpha_1)^(r+s+x)]
    }

    if(any(l.hyp.z2$status == 11 | l.hyp.z2$status == 1)){
      cbs.z[l.hyp.z2$status == 11 | l.hyp.z2$status == 1,
            hyp.z2 := (1-z.2)^(s+1)*exp(log.C) / (alpha_2)^(r+s+x)]
    }
    return(cbs.z[, hyp.z1 - hyp.z2])
  }else
    return(cbs) #return the empty data.table

}

