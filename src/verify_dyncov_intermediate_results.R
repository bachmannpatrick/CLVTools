ids <- names(l.walks.life)
# ids <- c("1", "10")
# ids <- "101"
l.LL <- lapply(ids, function(id){
  stopifnot(id %in% names(l.walks.life) && id %in%  names(l.walks.trans))
  # print(id)
  m.walkinfo_real_life <- as.matrix(l.walks.life[[id]]$info[AuxTrans==F, c("from", "to", "delta")])
  m.walkinfo_aux_life  <- as.matrix(l.walks.life[[id]]$info[AuxTrans==T, c("from", "to", "delta")])

  m.walkinfo_aux_trans <- as.matrix(l.walks.trans[[id]]$info[AuxTrans==T, c("from", "to", "delta", "d", "tjk")])
  if(l.walks.trans[[id]]$info[AuxTrans==F, .N] > 0){
    m.walkinfo_real_trans <- as.matrix(l.walks.trans[[id]]$info[AuxTrans==F, c("from", "to", "delta", "d", "tjk")])
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

