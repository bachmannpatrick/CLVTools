pnbd_dyncov_getLLcallargs <-function(clv.fitted){
  walkinfo_life_from <- walkinfo_life_to <- walkinfo_trans_from <- walkinfo_trans_to <- i.id_from <- i.id_to <- NULL

  dt.walkinfo.life  <- pnbd_dyncov_get_walkinfo(clv.fitted@data.walks.life)
  dt.walkinfo.trans <- pnbd_dyncov_get_walkinfo(clv.fitted@data.walks.trans)

  # Add where to find customer's walk info
  dt.cbs <- copy(clv.fitted@cbs)
  dt.customerinfo.life  <- pnbd_dyncov_get_customerinfo(dt.walkinfo.life)
  dt.cbs[dt.customerinfo.life,  walkinfo_life_from  := i.id_from, on="Id"]
  dt.cbs[dt.customerinfo.life,  walkinfo_life_to    := i.id_to,   on="Id"]

  dt.customerinfo.trans <- pnbd_dyncov_get_customerinfo(dt.walkinfo.trans)
  dt.cbs[dt.customerinfo.trans, walkinfo_trans_from := i.id_from, on="Id"]
  dt.cbs[dt.customerinfo.trans, walkinfo_trans_to   := i.id_to,   on="Id"]

  # IN SAME ORDER AS READ OUT IN Walk::Walk()
  cols.walk.ordered <- c("walk_from", "walk_to", "tjk", "d", "delta", "AuxTrans")
  m.walkinfo.life  <- data.matrix(dt.walkinfo.life[, .SD, .SDcols=cols.walk.ordered])
  m.walkinfo.trans <- data.matrix(dt.walkinfo.trans[,.SD, .SDcols=cols.walk.ordered])

  # **TODO: check col sorting (correct.col.names == )
  m.cov.data.life  <- data.matrix(clv.fitted@data.walks.life[,  .SD, .SDcols=clv.fitted@clv.data@names.cov.data.life])
  m.cov.data.trans <- data.matrix(clv.fitted@data.walks.trans[, .SD, .SDcols=clv.fitted@clv.data@names.cov.data.trans])

  return(list(X = dt.cbs$x,
              t_x = dt.cbs$t.x,
              T_cal = dt.cbs$T.cal,

              walkinfo_trans_from = dt.cbs$walkinfo_trans_from,
              walkinfo_trans_to   = dt.cbs$walkinfo_trans_to,
              walkinfo_life_from  = dt.cbs$walkinfo_life_from,
              walkinfo_life_to    = dt.cbs$walkinfo_life_to,

              walk_info_life  = m.walkinfo.life,
              walk_info_trans = m.walkinfo.trans,

              cov_data_life = m.cov.data.life,
              cov_data_trans = m.cov.data.trans))
}

pnbd_dyncov_creatwalks_add_tjk <- function(dt.walk, clv.time){
  tjk <- tp.previous.trans <- tp.this.trans <- NULL
  # time between Trans and the previous Trans / from date.lagged to date
  dt.walk[, tjk := clv.time.interval.in.number.tu(clv.time = clv.time,
                                                  interv = interval( start = tp.previous.trans,
                                                                     end   = tp.this.trans))]
  return(dt.walk)
}

pnbd_dyncov_creatwalks_add_d <- function(dt.walk, clv.time){
  d <- tp.previous.trans <- NULL
  # **TODO: +1 or not?
  # time between date.lagged and period end (ceiling(data.lagged+1))
  # d shall be 1 if it is exactly on the time.unit boundary!
  # Plus.Eps is already "+ 1"
  dt.walk[, d := clv.time.interval.in.number.tu(clv.time = clv.time,
                                                interv = interval( start = tp.previous.trans,
                                                                   end   = clv.time.ceiling.date(clv.time=clv.time,
                                                                                                 timepoint=tp.previous.trans)))]
  return(dt.walk)
}


pnbd_dyncov_creatwalks_matchcovstocuts <- function(dt.cov, dt.cuts, names.cov){
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

  dt.walks <- pnbd_dyncov_creatwalks_add_tjk(dt.walks, clv.time = clv.time)
  dt.walks <- pnbd_dyncov_creatwalks_add_d(dt.walks, clv.time = clv.time)

  return(dt.walks)
}

pnbd_dyncov_covariate_add_interval_bounds <- function(dt.cov, clv.time){
  tp.cov.lower <- tp.cov.upper <- Cov.Date <- NULL

  # Covariate intervals are closed intervals + Cov.Date marks beginning (Covs are forward-looking)
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
  num.trans <- tp.this.trans <- tp.previous.trans <- tp.cut.lower <- tp.cut.upper <- is.first <- AuxTrans <- Date <- NULL
  clv.time <- clv.data@clv.time
  dt.cov <- clv.data@data.cov.trans

  # Real Walks -----------------------------------------------------------------------
  # Covariates affecting repeat-transactions
  #   Zero-repeaters have 0 real walks (**TODO: what, really..??)
  #   No walk for the first tranaction (**TODO: what, really, also..??)

  # remove zero-repeaters
  dt.cuts.real <- dt.trans[dt.tp.first.last[num.trans > 1, "Id"], on="Id", nomatch=NULL]
  dt.tp.first.last[, num.trans := NULL]

  # Cannot/Should have no 2 transactions on same tp because are aggregated,
  #   min dist is 1 eps, can fall together again
  setkeyv(dt.cuts.real, cols=c("Id", "Date"))
  dt.cuts.real[, tp.this.trans := Date]
  dt.cuts.real[, tp.previous.trans := shift(tp.this.trans, n=1), by="Id"]
  dt.cuts.real[, tp.cut.lower := tp.previous.trans + clv.time.epsilon(clv.time)]
  dt.cuts.real[, tp.cut.upper := tp.this.trans]

  # remove cut for first transaction
  #   - which has NA in tp.previous.trans (cannot match to cov)
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
  dt.walks.real <- pnbd_dyncov_creatwalks_add_d(dt.walks.real, clv.time)
  dt.walks.real[, AuxTrans := FALSE]

  return(dt.walks.real)
}

pnbd_dyncov_createwalks_real_life <- function(clv.data, dt.tp.first.last){
  AuxTrans <- NULL

  # **TOOD: Keep Zero-repeaters? Because for transaction process, only repeat-transactions (zero repeaters may have no walks)
  dt.walks.real <- pnbd_dyncov_createwalks_singletrans(dt.cov=clv.data@data.cov.life,
                                                       dt.tp.first.last=dt.tp.first.last,
                                                       name.lower="tp.first.trans",
                                                       name.upper="tp.last.trans",
                                                       names.cov=clv.data.get.names.cov.life(clv.data),
                                                       clv.time=clv.data@clv.time)
  dt.walks.real[, AuxTrans := FALSE]
}

pnbd_dyncov_createwalks_auxwalk <- function(dt.cov, dt.tp.first.last, names.cov, clv.time){
  tp.estimation.end <- AuxTrans <- NULL

  dt.tp.first.last[, tp.estimation.end := clv.time@timepoint.estimation.end]
  dt.walks.aux <- pnbd_dyncov_createwalks_singletrans(dt.cov=dt.cov,
                                                      dt.tp.first.last=dt.tp.first.last,
                                                      name.lower="tp.last.trans",
                                                      name.upper="tp.estimation.end",
                                                      names.cov = names.cov,
                                                      clv.time=clv.time)
  dt.walks.aux[, tp.estimation.end := NULL]
  dt.walks.aux[, AuxTrans := TRUE]
  return(dt.walks.aux)
}

pnbd_dyncov_createwalks_addwalkfromto <- function(dt.walks){
  Id <- tp.this.trans <- AuxTrans <- tp.cov.lower <- abs_pos <- walk_from <- walk_to <- NULL
  dt.walks[order(Id, tp.this.trans, AuxTrans, tp.cov.lower), abs_pos := seq(from=1, to=.N)]
  dt.walks[, walk_from := min(abs_pos), by="walk_id"]
  dt.walks[, walk_to := max(abs_pos), by="walk_id"]
  return(dt.walks)
}

pnbd_dyncov_makewalks <- function(clv.data){
  walk_id <- walk_from <- walk_to <- .N <- abs_pos <- Date <- delta <- tp.cov.lower <- NULL
  #   ** TODO: Is it correct, that life only ever uses d but not delta and tjk?
  #   data.work.trans <- data.table(clv.fitted@data.walks.trans[[1]][, "Id"],
  #                                 clv.fitted@data.walks.trans[[1]][, "d"],
  #                                 clv.fitted@data.walks.trans[[1]][, "delta"],
  #                                 clv.fitted@data.walks.trans[[1]][, "tjk"],
  #
  #   data.work.life <- data.table(clv.fitted@data.walks.life[[1]][, "Id"],
  #                                clv.fitted@data.walks.life[[1]][, "d"],
  #


  # Extract transactions and first/last for all walk types & processes because
  #   may be memory and computation intensive
  dt.trans <- clv.data.get.transactions.in.estimation.period(clv.data)
  dt.tp.first.last <- dt.trans[, list(tp.first.trans = min(Date),
                                      tp.last.trans = max(Date),
                                      num.trans = .N),
                               by="Id"]

  names.cov.trans <- clv.data.get.names.cov.trans(clv.data)
  names.cov.life <- clv.data.get.names.cov.life(clv.data)

  dt.walks.real.trans <- pnbd_dyncov_createwalks_real_trans(clv.data,
                                                            dt.trans=dt.trans,
                                                            dt.tp.first.last=dt.tp.first.last)

  dt.walks.real.life <- pnbd_dyncov_createwalks_real_life(clv.data=clv.data,
                                                          dt.tp.first.last=dt.tp.first.last)

  dt.walks.aux.life <- pnbd_dyncov_createwalks_auxwalk(dt.cov = clv.data@data.cov.life,
                                                       dt.tp.first.last=dt.tp.first.last,
                                                       names.cov=names.cov.life,
                                                       clv.time=clv.data@clv.time)

  dt.walks.aux.trans <- pnbd_dyncov_createwalks_auxwalk(dt.cov=clv.data@data.cov.trans,
                                                        dt.tp.first.last=dt.tp.first.last,
                                                        names.cov=names.cov.trans,
                                                        clv.time=clv.data@clv.time)


  # Create actual walk tables ------------------------------------------------------------
  cols.walk.def <- c("Id", "tp.this.trans", "AuxTrans") # what defines data that belongs to a walk
  cols.keys <- c("walk_id", cols.walk.def, "tp.cov.lower")
  cols.life <- c(cols.walk.def, "tp.cov.lower", names.cov.life, "tjk", "d")
  cols.trans <- c(cols.walk.def, "tp.cov.lower", names.cov.trans, "tjk", "d")

  dt.walks.life <- rbindlist(list(dt.walks.real.life[, .SD, .SDcols = cols.life],
                                  dt.walks.aux.life[, .SD, .SDcols = cols.life]),
                             use.names = TRUE, fill=FALSE)
  dt.walks.trans <- rbindlist(list(dt.walks.real.trans[, .SD, .SDcols = cols.trans],
                                   dt.walks.aux.trans[, .SD, .SDcols = cols.trans]),
                              use.names = TRUE, fill=FALSE)


  # Add walk_id
  #   per what defines a walk (incl AuxTrans because AuxTrans can be on same as last real trans)
  setkeyv(dt.walks.life, cols.walk.def)
  setkeyv(dt.walks.trans, cols.walk.def)
  dt.walks.life[,  walk_id := .GRP, by=cols.walk.def]
  dt.walks.trans[, walk_id := .GRP, by=cols.walk.def]

  setkeyv(dt.walks.life, cols.keys)
  setkeyv(dt.walks.trans, cols.keys)

  # Add delta
  #   delta: if Num.Walk > 1 -> 1, otherwise 0
  # ** TODO: What counts as Num.Walk? Actual content or 1 cov (walk1 + max.walk = 1 or 2)
  dt.walks.life[,  delta := as.numeric(.N > 1), by="walk_id"]
  dt.walks.trans[, delta := as.numeric(.N > 1), by="walk_id"]

  # Add absolute position in data
  #   sort each walk together and then by cov date
  dt.walks.life[ order(walk_id, tp.cov.lower), abs_pos := seq(.N)]
  dt.walks.trans[order(walk_id, tp.cov.lower), abs_pos := seq(.N)]

  # Add from to of walk
  dt.walks.life[,  walk_from := min(abs_pos), by="walk_id"]
  dt.walks.life[,  walk_to   := max(abs_pos), by="walk_id"]

  dt.walks.trans[, walk_from := min(abs_pos), by="walk_id"]
  dt.walks.trans[, walk_to   := max(abs_pos), by="walk_id"]

  return(list("data.walks.life" = dt.walks.life,
              "data.walks.trans" = dt.walks.trans))
}

pnbd_dyncov_get_walkinfo <- function(dt.walks){
  abs_pos <- Id <- id_from <- id_to <- NULL
  # minimum info, per walk. Keep Id to create customerinfo (match with cbs)

  dt.walkinfo <- unique(dt.walks[, c("Id", "walk_from", "walk_to", "tjk", "d", "delta", "AuxTrans")])
  setkeyv(dt.walkinfo, "Id")

  # Add abs_pos + id from and to here to ensure correct
  #   abs_pos requires sorting by id to ensure all ids are together
  dt.walkinfo[order(Id), abs_pos := seq(.N)]

  dt.walkinfo[, id_from := min(abs_pos), by="Id"]
  dt.walkinfo[, id_to   := max(abs_pos), by="Id"]

  return(dt.walkinfo)
}

pnbd_dyncov_get_customerinfo <-function(dt.walkinfo){
  # where in walkinfo to find customer's walk info
  #   def: unique minimum info (from, to)
  return(unique(dt.walkinfo[, c("Id", "id_from", "id_to")]))
}


