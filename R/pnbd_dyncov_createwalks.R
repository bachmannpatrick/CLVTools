pnbd_dyncov_getLLcallargs <-function(clv.fitted){
  i.walk_from <- i.walk_to <- i.d <- i.delta <- i.tjk <- NULL
  walkinfo_trans_real_from <- walkinfo_trans_real_to <- i.id_from <- i.id_to <- NULL

  pnbd_dyncov_addwalkinfo_single <- function(dt.cbs, dt.walk, name, cols.walkinfo){
    dt.walkinfo <- unique(dt.walk[, .SD, .SDcols=cols.walkinfo])

    for(wi in setdiff(cols.walkinfo, "Id")){
      col.name <- paste0("walk_",name,"_", gsub("_", "", gsub("walk", "", wi)))
      dt.cbs[dt.walkinfo, (col.name) := get(paste0("i.", wi)), on="Id"]
    }

    return(dt.cbs)
  }

  # Add where to find customer's walk info
  cols.wi.life <- c("Id", "walk_from", "walk_to", "delta")
  cols.wi.trans <- c(cols.wi.life, "tjk", "d1")
  dt.cbs <- copy(clv.fitted@cbs)
  dt.cbs <- pnbd_dyncov_addwalkinfo_single(dt.cbs, dt.walk=clv.fitted@data.walks.life.aux,  name="aux_life", cols.walkinfo = cols.wi.life)
  dt.cbs <- pnbd_dyncov_addwalkinfo_single(dt.cbs, dt.walk=clv.fitted@data.walks.life.real, name="real_life", cols.walkinfo = cols.wi.life)
  dt.cbs <- pnbd_dyncov_addwalkinfo_single(dt.cbs, dt.walk=clv.fitted@data.walks.trans.aux, name="aux_trans", cols.walkinfo = cols.wi.trans)

  # Add to cbs
  #   zero-repeaters have no real trans walks and will have NA in walkinfo_trans_real_from/to
  #   which are skipped in the cpp implementation
  #   dt.customerinfo: where in walkinfo matrix to find customer's walk info
  dt.walkinfo.real.trans <- pnbd_dyncov_getrealtranswalks_walkinfo(clv.fitted@data.walks.trans.real)
  dt.customerinfo.real.trans <- unique(dt.walkinfo.real.trans[, c("Id", "id_from", "id_to")])
  dt.cbs[dt.customerinfo.real.trans, walkinfo_trans_real_from := i.id_from, on="Id"]
  dt.cbs[dt.customerinfo.real.trans, walkinfo_trans_real_to   := i.id_to,   on="Id"]

  # IN SAME ORDER AS READ OUT IN WALK ctor
  cols.walk.ordered.life  <- c("from", "to", "delta")
  cols.walk.ordered.trans <- c(cols.walk.ordered.life, "d1", "tjk")
  m.walkinfo.aux.life   <- data.matrix(dt.cbs[, .SD, .SDcols=paste0("walk_aux_life_",  cols.walk.ordered.life, sep="")])
  m.walkinfo.real.life  <- data.matrix(dt.cbs[, .SD, .SDcols=paste0("walk_real_life_", cols.walk.ordered.life, sep="")])
  m.walkinfo.aux.trans  <- data.matrix(dt.cbs[, .SD, .SDcols=paste0("walk_aux_trans_", cols.walk.ordered.trans, sep="")])
  m.walkinfo.real.trans <- data.matrix(dt.walkinfo.real.trans[, c("walk_from", "walk_to", "delta", "d1", "tjk")])

  # **TODO: check col sorting is same as parameters! (correct.col.names == )
  m.cov.data.aux.life    <- data.matrix(clv.fitted@data.walks.life.aux[,   .SD, .SDcols=clv.fitted@clv.data@names.cov.data.life])
  m.cov.data.real.life   <- data.matrix(clv.fitted@data.walks.life.real[,  .SD, .SDcols=clv.fitted@clv.data@names.cov.data.life])
  m.cov.data.aux.trans   <- data.matrix(clv.fitted@data.walks.trans.aux[,  .SD, .SDcols=clv.fitted@clv.data@names.cov.data.trans])
  m.cov.data.real.trans  <- data.matrix(clv.fitted@data.walks.trans.real[, .SD, .SDcols=clv.fitted@clv.data@names.cov.data.trans])

  return(list(X = dt.cbs$x,
              t_x = dt.cbs$t.x,
              T_cal = dt.cbs$T.cal,
              d_omega = dt.cbs$d_omega,

              walkinfo_aux_life = m.walkinfo.aux.life,
              walkinfo_real_life = m.walkinfo.real.life,
              walkinfo_aux_trans = m.walkinfo.aux.trans,

              walkinfo_trans_real_from = dt.cbs$walkinfo_trans_real_from,
              walkinfo_trans_real_to = dt.cbs$walkinfo_trans_real_to,
              walkinfo_real_trans = m.walkinfo.real.trans,

              covdata_aux_life = m.cov.data.aux.life,
              covdata_real_life = m.cov.data.real.life,
              covdata_aux_trans = m.cov.data.aux.trans,
              covdata_real_trans = m.cov.data.real.trans))
}


pnbd_dyncov_getLLdata <- function(clv.fitted, params){
  # get LL with all values, not just ind LL or summed LL
  l.LL.args <- pnbd_dyncov_getLLcallargs(clv.fitted)
  l.LL.args[["params"]] = params
  l.LL.args[["return_intermediate_results"]] = TRUE

  dt.LLdata <- data.table(Id = clv.fitted@cbs$Id,
                          do.call(what = pnbd_dyncov_LL_ind, args = l.LL.args),
                          key = "Id")
  setnames(dt.LLdata, "F2", "Z")
  return(dt.LLdata)
}

pnbd_dyncov_creatwalks_add_tjk <- function(dt.walk, clv.time){
  tjk <- tp.previous.trans <- tp.this.trans <- NULL
  # time between Trans and the previous Trans / from date.lagged to date
  dt.walk[, tjk := clv.time.interval.in.number.tu(clv.time = clv.time,
                                                  interv = interval( start = tp.previous.trans,
                                                                     end   = tp.this.trans))]
  return(dt.walk)
}

pnbd_dyncov_creatwalks_add_d1 <- function(dt.walk, clv.time){
  d1 <- tp.previous.trans <- NULL
  # **TODO: +1 or not?
  # time between date.lagged and period end (ceiling(data.lagged+1))
  # d shall be 1 if it is exactly on the time.unit boundary!
  # Plus.Eps is already "+ 1"
  dt.walk[, d1 := clv.time.interval.in.number.tu(clv.time = clv.time,
                                                 interv = interval( start = tp.previous.trans,
                                                                    end   = clv.time.ceiling.date(clv.time=clv.time,
                                                                                                  timepoint=tp.previous.trans)))]
  return(dt.walk)
}


pnbd_dyncov_creatwalks_matchcovstocuts <- function(dt.cov, dt.cuts, names.cov){
  # **TODO: Update comment: Walks are covs that have an effect from coming alive until last transaction [0, T] (or (0. T))
  by.covs <- c("Id", "tp.cov.lower", "tp.cov.upper")
  by.cuts <- c("Id", "tp.cut.lower", "tp.cut.upper")
  setkeyv(dt.cov, by.covs)
  setkeyv(dt.cuts, by.cuts)

  dt.matched <- foverlaps(x = dt.cuts, y = dt.cov, by.x = by.cuts, by.y = by.covs,
                          type="any", mult="all", nomatch = NULL)
  return(dt.matched)
}

pnbd_dyncov_createwalks_singletrans <- function(dt.cov, dt.tp.first.last,
                                                name.lower, name.upper,
                                                names.cov, clv.time){
  tp.cut.lower <- tp.cut.upper <- tp.this.trans <- tp.previous.trans <- NULL

  dt.cuts <- copy(dt.tp.first.last)
  dt.cuts[, tp.cut.lower := get(name.lower) + clv.time.epsilon(clv.time)]
  dt.cuts[, tp.cut.upper := get(name.upper)]
  dt.cuts[get(name.lower) == get(name.upper), tp.cut.lower := get(name.lower)]

  dt.walks <- pnbd_dyncov_creatwalks_matchcovstocuts(dt.cov = dt.cov, dt.cuts = dt.cuts,
                                                     names.cov = names.cov)

  dt.walks[, tp.this.trans := get(name.upper)]
  dt.walks[, tp.previous.trans := get(name.lower)]

  return(dt.walks)
}

pnbd_dyncov_covariate_add_interval_bounds <- function(dt.cov, clv.time){
  tp.cov.lower <- tp.cov.upper <- Cov.Date <- NULL

  # Covariate intervals are closed intervals + Cov.Date marks beginning (Covs are "forward-looking")
  #   => [Cov.Date, Next Cov.Date - eps] <=> [Cov.Date, Cov.Date + 1 Period - eps]

  dt.cov[, tp.cov.lower := Cov.Date]

  # Do not use shift() because leaves NA which will then have to be fixed
  single.timeperiod <- clv.time.number.timeunits.to.timeperiod(clv.time, user.number.periods=1L)
  dt.cov[, tp.cov.upper := tp.cov.lower + single.timeperiod - clv.time.epsilon(clv.time)]

  # **TODO: test: same as if shifted + short manual
  #         test: no NAs in tp columns
  # dt.cov[, tp.cov.upper := shift(tp.cov.lower, n=-1L), by="Id"]
  # dt.cov[, tp.cov.upper := tp.cov.upper - clv.time.epsilon(clv.time)]

  return(dt.cov)
}


pnbd_dyncov_createwalks_real_trans <- function(clv.data, dt.trans, dt.tp.first.last){
  num.trans <- tp.this.trans <- tp.previous.trans <- tp.cut.lower <- tp.cut.upper <- is.first <- Date <- NULL
  clv.time <- clv.data@clv.time
  dt.cov <- clv.data@data.cov.trans

  # Covariates affecting repeat-transactions
  #   Zero-repeaters have 0 real walks
  #   No walk for the first tranaction
  #   A transaction is influenced some time between the last and actual trans
  #     -> Interval [last trans + eps, this trans]

  # remove zero-repeaters
  dt.cuts.real <- dt.trans[dt.tp.first.last[num.trans > 1, "Id"], on="Id", nomatch=NULL]
  dt.tp.first.last[, num.trans := NULL]

  # If 2 transactions are on the same date, shift+1 will lead to Date.Start > Date.End
  #   Cannot/Should have no 2 transactions on same tp because are aggregated
  #   No aux trans, only real trans present
  #     min dist is 1 eps, hence can fall together again
  setkeyv(dt.cuts.real, cols=c("Id", "Date"))
  dt.cuts.real[, tp.this.trans := Date]
  dt.cuts.real[, tp.previous.trans := shift(tp.this.trans, n=1), by="Id"]
  dt.cuts.real[, tp.cut.lower := tp.previous.trans + clv.time.epsilon(clv.time)]
  dt.cuts.real[, tp.cut.upper := tp.this.trans]

  # remove cut for first transaction
  #   - which has NA in tp.previous.trans because of shift (and cannot match to cov)
  #   - for which no walk shall be created
  # dt.cuts.real <- dt.cuts.real[!is.na(tp.previous.trans)]
  dt.cuts.real[, is.first := Date == min(Date), by="Id"]
  dt.cuts.real <- dt.cuts.real[is.first == FALSE]
  dt.cuts.real[, is.first := NULL]

  # **TODO: test: Even if trans only 1 eps apart, gives walk
  #         test: Number of walks = num repeat trans
  # dt.cuts.real[tp.cut.lower > tp.cut.upper, tp.cut.lower := tp.cut.upper]??
  # print(dt.cuts.real)
  # print(dt.cuts.real[is.na(tp.cut.lower)])

  dt.walks.real <- pnbd_dyncov_creatwalks_matchcovstocuts(dt.cov = dt.cov,
                                                          dt.cuts = dt.cuts.real,
                                                          names.cov = clv.data.get.names.cov.trans(clv.data))

  dt.walks.real <- pnbd_dyncov_creatwalks_add_tjk(dt.walks.real, clv.time)
  dt.walks.real <- pnbd_dyncov_creatwalks_add_d1(dt.walks.real, clv.time)

  return(dt.walks.real)
}

pnbd_dyncov_createwalks_real_life <- function(clv.data, dt.tp.first.last, dt.walks.aux.life){
  # Real walks for the lifetime process are all covs before the one covariate where the last transaction happens in
  #   Given the aux walk, they are the residual covs since coming alive
  #   May have no overlap with the aux walk
  #   Only ever needed in Di() where they are summed additionally to the aux walk
  #   For some customers, there might be no real walk, if the aux walk starts in the customer's first period
  #   Still, every customer may have 1 real walk at maximum
  # **TODO: Test real + aux gives original data (until estimation end)

  # Create as residuals given the aux walks
  #   Since coming alive (ie no covs before first transaction)

  tp.first.trans <- i.tp.first.trans <- tp.cov.upper <- tp.this.trans <- i.tp.last.trans <- NULL

  # All dates per customer, except the ones already in the aux walks
  #   **TODO: Requires minimum data.table version XXX (bug before that in data.table)
  dt.dates.residual <- fsetdiff(x = clv.data@data.cov.life[, c("Id", "tp.cov.lower", "tp.cov.upper")],
                                y = dt.walks.aux.life[, c("Id", "tp.cov.lower", "tp.cov.upper")])

  # Cannot use non-equi join with ("Cov.Date >= tp.first.trans") because Cov.Date marks beginning of cov interval
  #   need to use interval join (foverlaps)

  dt.dates.residual[dt.tp.first.last, tp.first.trans := i.tp.first.trans, on="Id"]
  dt.dates.residual <- dt.dates.residual[tp.first.trans <= tp.cov.upper]

  # Create walks from Id/Cov.Date combos that are left
  dt.cov <- clv.data@data.cov.life[dt.dates.residual[, c("Id", "tp.cov.lower")],
                                   on = c("Id", "tp.cov.lower"), nomatch=NULL]


  # Only such that are at least since first transaction
  #   using non-equi join
  #   requires minimum data.table version 1.9.8!
  # **TODO: Verify that >= and not > (ie same as cov start/end intervals for foverlaps())
  # **TODO: Add test: same first cov date as real walks for transaction process + has no gaps in Cov.Dates


  # Result where tp.first.trans in col Cov.Date
  # dt.dates.residual <- dt.dates.residual[dt.tp.first.last[, c("Id", "tp.first.trans")],
  #                                        on = c("Id == Id", "Cov.Date >= tp.first.trans"),
  #                                        nomatch = NULL]


  dt.cov[dt.tp.first.last, tp.this.trans := i.tp.last.trans,  on="Id"]

  return(dt.cov)
}

pnbd_dyncov_createwalks_auxwalk <- function(dt.cov, dt.tp.first.last, names.cov, clv.time){
  tp.estimation.end <- tp.cut.lower <- tp.last.trans <- tp.cut.upper <- tp.this.trans <- tp.previous.trans <- NULL

  dt.tp.first.last[, tp.estimation.end := clv.time@timepoint.estimation.end]

  dt.cuts <- copy(dt.tp.first.last)
  dt.cuts[, tp.cut.lower := tp.last.trans + clv.time.epsilon(clv.time)]
  dt.cuts[, tp.cut.upper := tp.estimation.end]
  dt.cuts[tp.last.trans == tp.estimation.end, tp.cut.lower := tp.last.trans]

  dt.walks.aux <- pnbd_dyncov_creatwalks_matchcovstocuts(dt.cov = dt.cov, dt.cuts = dt.cuts,
                                                         names.cov = names.cov)

  dt.walks.aux[, tp.this.trans := tp.estimation.end]
  dt.walks.aux[, tp.previous.trans := tp.last.trans]

  dt.walks.aux[, tp.estimation.end := NULL]
  return(dt.walks.aux)
}

pnbd_dyncov_createwalks_addwalkfromto <- function(dt.walks){
  Id <- tp.this.trans <- tp.cov.lower <- abs_pos <- walk_from <- walk_to <- NULL
  dt.walks[order(Id, tp.this.trans, tp.cov.lower), abs_pos := seq(from=1, to=.N)]
  dt.walks[, walk_from := min(abs_pos), by="walk_id"]
  dt.walks[, walk_to := max(abs_pos), by="walk_id"]
  return(dt.walks)
}

pnbd_dyncov_createwalks <- function(clv.data){
  walk_id <- walk_from <- walk_to <- .N <- abs_pos <- Date <- delta <- tp.cov.lower <- NULL

  # Walk info, per walk
  #   - from, to: where to find walk data
  #   - delta: more than 1 element
  #   Transaction:
  #     - tjk
  #     - d1
  # d_omega is per customer


  # Extract transactions and first/last for all walk types & processes because
  #   may be memory and computation intensive
  dt.trans <- clv.data.get.transactions.in.estimation.period(clv.data)
  dt.tp.first.last <- dt.trans[, list(tp.first.trans = min(Date),
                                      tp.last.trans = max(Date),
                                      num.trans = .N),
                               by="Id"]

  names.cov.trans <- clv.data.get.names.cov.trans(clv.data)
  names.cov.life <- clv.data.get.names.cov.life(clv.data)

  dt.walks.aux.life <- pnbd_dyncov_createwalks_auxwalk(dt.cov = clv.data@data.cov.life,
                                                       dt.tp.first.last=dt.tp.first.last,
                                                       names.cov=names.cov.life,
                                                       clv.time=clv.data@clv.time)

  dt.walks.real.life <- pnbd_dyncov_createwalks_real_life(clv.data=clv.data,
                                                          dt.tp.first.last=dt.tp.first.last,
                                                          dt.walks.aux.life = dt.walks.aux.life)


  dt.walks.real.trans <- pnbd_dyncov_createwalks_real_trans(clv.data,
                                                            dt.trans=dt.trans,
                                                            dt.tp.first.last=dt.tp.first.last)

  dt.walks.aux.trans <- pnbd_dyncov_createwalks_auxwalk(dt.cov=clv.data@data.cov.trans,
                                                        dt.tp.first.last=dt.tp.first.last,
                                                        names.cov=names.cov.trans,
                                                        clv.time=clv.data@clv.time)
  dt.walks.aux.trans <- pnbd_dyncov_creatwalks_add_tjk(dt.walks.aux.trans, clv.time = clv.data@clv.time)
  dt.walks.aux.trans <- pnbd_dyncov_creatwalks_add_d1(dt.walks.aux.trans, clv.time = clv.data@clv.time)


  # Create actual walk tables ------------------------------------------------------------

  cols.walk.def <- c("Id", "tp.this.trans") # what defines data that belongs to a walk (tp.this.trans required because of multiple real trans walk)
  cols.keys <- c("walk_id", cols.walk.def, "tp.cov.lower")
  cols.life <- c(cols.walk.def, "tp.cov.lower", "tp.cov.upper", "walk_from", "walk_to", "abs_pos", names.cov.life, "delta")
  cols.trans <- c(cols.walk.def, "tp.cov.lower", "tp.cov.upper", "walk_from", "walk_to", "abs_pos", names.cov.trans, "delta", "tjk", "d1")

  pnbd_dyncov_finishwalks <- function(dt.walk){
    # Add walk_id
    #   per what defines a walk
    setkeyv(dt.walk, cols.walk.def)
    dt.walk[,  walk_id := .GRP, by=cols.walk.def]
    setkeyv(dt.walk, cols.keys)

    if(nrow(dt.walk) == 0){
      return(dt.walk) # min() and max() give warning if table is empty (there is not a single real walk for lifetime)
    }

    # Add delta
    #   delta: if Num.Walk > 1 -> 1, otherwise 0
    dt.walk[, delta := as.numeric(.N > 1), by="walk_id"]

    # Add absolute position in data
    #   sort each walk together and then also by cov date
    dt.walk[order(walk_id, tp.cov.lower), abs_pos := seq(.N)]

    # Add from to of walk
    dt.walk[, walk_from := min(abs_pos), by="walk_id"]
    dt.walk[, walk_to   := max(abs_pos), by="walk_id"]

    return(dt.walk)
  }

  return(list("data.walks.life.aux"   = pnbd_dyncov_finishwalks(dt.walks.aux.life)[,   .SD, .SDcols=cols.life],
              "data.walks.life.real"  = pnbd_dyncov_finishwalks(dt.walks.real.life)[,  .SD, .SDcols=cols.life],
              "data.walks.trans.aux"  = pnbd_dyncov_finishwalks(dt.walks.aux.trans)[,  .SD, .SDcols=cols.trans],
              "data.walks.trans.real" = pnbd_dyncov_finishwalks(dt.walks.real.trans)[, .SD, .SDcols=cols.trans]))
}



pnbd_dyncov_getrealtranswalks_walkinfo <- function(dt.walk){
  abs_pos <- Id <- id_from <- id_to <- NULL
  # minimum info, per walk. Keep Id to create customerinfo (match with cbs)

  dt.walkinfo <- unique(dt.walk[, c("Id", "walk_from", "walk_to", "delta", "tjk", "d1")])
  setkeyv(dt.walkinfo, "Id")

  # Add abs_pos + id from and to here to ensure correct
  #   abs_pos requires sorting by id to ensure all walkinfo of same id is together
  dt.walkinfo[order(Id), abs_pos := seq(.N)]

  dt.walkinfo[, id_from := min(abs_pos), by="Id"]
  dt.walkinfo[, id_to   := max(abs_pos), by="Id"]

  return(dt.walkinfo)
}

