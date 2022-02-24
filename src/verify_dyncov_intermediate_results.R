ids <- names(l.walks.life)
# ids <- c("1", "10")
# ids <- "101"
l.LL <- lapply(ids, function(id){
  stopifnot(id %in% names(l.walks.life) && id %in%  names(l.walks.trans))
  # print(id)
  m.walkinfo_real_life <- as.matrix(l.walks.life[[id]]$info[AuxTrans==F, c("from", "to")])
  m.walkinfo_aux_life  <- as.matrix(l.walks.life[[id]]$info[AuxTrans==T, c("from", "to")])

  m.walkinfo_aux_trans <- as.matrix(l.walks.trans[[id]]$info[AuxTrans==T, c("from", "to", "d", "tjk")])
  if(l.walks.trans[[id]]$info[AuxTrans==F, .N] > 0){
    m.walkinfo_real_trans <- as.matrix(l.walks.trans[[id]]$info[AuxTrans==F, c("from", "to", "d", "tjk")])
  }else{
    m.walkinfo_real_trans <- as.matrix(NA_real_)
  }

  res <- CLVTools:::LL_i_single_walk(r=p.dyncov.g0@prediction.params.model[["r"]], alpha_0=p.dyncov.g0@prediction.params.model[["alpha"]],
                                         s=p.dyncov.g0@prediction.params.model[["s"]], beta_0=p.dyncov.g0@prediction.params.model[["beta"]],
                                         x=dt.LL.original[Id==id]$x, t_x=dt.LL.original[Id==id]$t.x, T_cal = dt.LL.original[Id==id]$T.cal,
                                     d_omega = dt.LL.original[Id==id]$d_omega,
                                     num_walks=dt.LL.original[Id==id]$Num.Walk,
                                     B1=dt.LL.original[Id==id]$B1, BT=dt.LL.original[Id==id]$BT,
                                     D1=dt.LL.original[Id==id]$D1, DT=dt.LL.original[Id==id]$DT,
                                     F2_3 = dt.LL.original[Id==id]$F2.3,
                                         params_life = c(0.1, 0.1, 0.1), params_trans = c(0.1, 0.1, 0.1),
                                         cov_data_trans = as.matrix(l.walks.trans[[id]]$covs), cov_data_life = as.matrix(l.walks.life[[id]]$covs),
                                     walkinfo_real_life = m.walkinfo_real_life, walkinfo_aux_life = m.walkinfo_aux_life,
                                     walkinfo_aux_trans = m.walkinfo_aux_trans, walkinfo_real_trans = m.walkinfo_real_trans
                                     )
  data.table(Id=id, t(res))
})

dt.LL.cpp <- rbindlist(l.LL)
cols.compare <- c("LL", "Akprod", "Bksum", "B1", "BT", "D1", "DT",  "log.F0", "log.F1", "log.F3",
                  "F2", "F2.1", "F2.2", "F2.3",  "dT") #"DkT", "bkT", "bT", "aT",
all.equal(dt.LL.original[order(Id), .SD, .SDcols=cols.compare],
          dt.LL.cpp[order(Id), .SD, .SDcols=cols.compare],
          check.attributes = F)

for(cc in cols.compare){
  print(cc)
  print(all.equal(dt.LL.original[order(Id), .SD, .SDcols=cc],
                  dt.LL.cpp[order(Id), .SD, .SDcols=cc],
                  check.attributes = F)     )
}


var <- "LL"
dt.LL.original[dt.LL.cpp, paste0(var, ".cpp") := get(paste0("i.", var)), on = "Id"]
dt.LL.original[abs(get(var)-get(paste0(var, ".cpp"))) > 1.3]


# Summary statistics of old walks ---------------------------------------------

# Long format info of old walks
#   id, cov.name, date, auxtrans, info, value
cov.id <- c("Id","Date", "AuxTrans")

old_walks_to_long_info <- function(l.walks){
  dt.long <- rbindlist(lapply(names(l.walks), function(n){
    dt.walks <- copy(l.walks[[n]])
    cols.walks <- c(colnames(dt.walks)[grep(pattern = "^Walk*", x=colnames(dt.walks))], "Max.Walk")
    dt.info <- dt.walks[, .(num.walks = as.numeric(Num.Walk),
                            d = d,
                            tjk = tjk,
                            sum.cov = sum(.SD, na.rm=TRUE) - if(Num.Walk==1){Max.Walk}else{0}),
                        .SDcols=cols.walks,
                        by=cov.id]


    # dt.cov <- dt.walks[, .(sum.cov = sum(.SD, na.rm=T)), .SDcols=cols.walks, by=cov.id]
    # dt.info[dt.cov, sum.cov := i.sum.cov, on=cov.id]

    dt.info[, name.cov := n]
    return(melt(dt.info,
                id.vars = c(cov.id, "name.cov"),
                variable.factor = FALSE,
                value.factor = FALSE,
                measure.vars = c("num.walks", "d", "tjk", "sum.cov")))
  }))
  dt.long[, fake_walk_id := .GRP, by=c(cov.id, "name.cov")]
  return(dt.long)
}

dt.life <- old_walks_to_long_info(pnbd.extended@data.walks.life)
dt.trans <- old_walks_to_long_info(pnbd.extended@data.walks.trans)


new_walks_to_long_info <- function(dt.walks, names.cov, aux.trans){
  return(rbindlist(lapply(names.cov, function(n){
    dt.info <- dt.walks[, .(Id,
                            Date=tp.this.trans,
                            AuxTrans = aux.trans,
                            num.walks = as.numeric(.N),
                            d = if("d1" %in% colnames(dt.walks)){d1}else{NA_real_},
                            tjk = if("tjk" %in% colnames(dt.walks)){tjk}else{NA_real_},
                            sum.cov = sum(get(n))),
                        by="walk_id"]
    dt.info[, name.cov := n]
    dt.info <- unique(dt.info)
    return(melt(dt.info,
                id.vars = c("Id","Date", "AuxTrans", "walk_id", "name.cov"),
                variable.factor = FALSE,
                value.factor = FALSE,
                measure.vars = c("num.walks", "d", "tjk", "sum.cov")))

  })))
}


dt.life.real <- new_walks_to_long_info(pnbd.extended.cpp@data.walks.life.real,
                                       names.cov = c("direct.marketing", "high.season", "low.season", "gender"),
                                       aux.trans = F)
dt.life.aux <- new_walks_to_long_info(pnbd.extended.cpp@data.walks.life.aux,
                                       names.cov = c("direct.marketing", "high.season", "low.season", "gender"),
                                       aux.trans = T)
dt.trans.real <- new_walks_to_long_info(pnbd.extended.cpp@data.walks.trans.real,
                                        names.cov = c("direct.marketing", "high.season", "low.season", "gender"),
                                        aux.trans = F)
dt.trans.aux <- new_walks_to_long_info(pnbd.extended.cpp@data.walks.trans.aux,
                                        names.cov = c("direct.marketing", "high.season", "low.season", "gender"),
                                        aux.trans = T)


fct.walk.stats.per.id <- function(dt.walks.long){
  return(
    dt.walks.long[, .(num.aux.trans = sum(AuxTrans == T),
                      num.real.walks = sum(AuxTrans == F),
                      sum.num.walks = .SD[variable=="num.walks", sum(value)],
                      sum.length.walks.aux = .SD[variable=="num.walks"&AuxTrans==T, sum(value)],
                      sum.length.walks.real = .SD[variable=="num.walks"&AuxTrans==F, sum(value)],
                      sum.d = .SD[variable=="d", sum(value)],
                      sum.d.aux = .SD[variable=="d"&AuxTrans==T, sum(value)],
                      sum.d.real = .SD[variable=="d"&AuxTrans==F, sum(value)],
                      sum.tjk = .SD[variable=="tjk", sum(value)],
                      sum.cov.values = .SD[variable=="sum.cov", sum(value)],
                      sum.aux.cov.values = .SD[variable=="sum.cov"&AuxTrans==T, sum(value)],
                      sum.real.cov.values = .SD[variable=="sum.cov"&AuxTrans==F, sum(value)]
                      ),
                  by="Id"]
  )
}

dt.stats.old.life <- fct.walk.stats.per.id(dt.life)
dt.stats.old.trans <- fct.walk.stats.per.id(dt.trans)

dt.stats.new.life <- fct.walk.stats.per.id(rbindlist(list(dt.life.aux, dt.life.real)))
dt.stats.new.trans <- fct.walk.stats.per.id(rbindlist(list(dt.trans.aux, dt.trans.real)))


fct.walks.compare.summary.stats <- function(dt.old, dt.new){
  dt.compare <- data.table::merge.data.table(x = dt.old, y = dt.new, by="Id",
                                                  suffixes = c("_OLD", "_NEW"), sort=T)
  for(n in setdiff(colnames(dt.old), "Id")){
    print(n)
    print(dt.compare[get(paste0(n, "_", "NEW")) != get(paste0(n, "_", "OLD")), .N])
  }
}
fct.walks.compare.summary.stats(dt.old = dt.stats.old.life, dt.new = dt.stats.new.life)
fct.walks.compare.summary.stats(dt.old = dt.stats.old.trans, dt.new = dt.stats.new.trans)

