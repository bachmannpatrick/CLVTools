ids <- names(l.walks.life)
# ids <- c("1", "10")
l.LL <- lapply(ids, function(id){
  stopifnot(id %in% names(l.walks.life) && id %in%  names(l.walks.trans))
  res <- CLVTools:::LL_i_single_walk(r=p.dyncov.g0@prediction.params.model[["r"]], alpha_0=p.dyncov.g0@prediction.params.model[["alpha"]],
                                         s=p.dyncov.g0@prediction.params.model[["s"]], beta_0=p.dyncov.g0@prediction.params.model[["beta"]],
                                         x=dt.LL.original[Id==id]$x, t_x=dt.LL.original[Id==id]$t.x, T_cal = dt.LL.original[Id==id]$T.cal, num_walks=dt.LL.original[Id==id]$Num.Walk,
                                         B1 = dt.LL.original[Id==id]$B1, BT=dt.LL.original[Id==id]$BT, DT=dt.LL.original[Id==id]$DT, D1 = dt.LL.original[Id==id]$D1, F2_3 = dt.LL.original[Id==id]$F2.3,
                                         params_life = c(0.1, 0.1, 0.1), params_trans = c(0.1, 0.1, 0.1),
                                         cov_data_trans = as.matrix(l.walks.trans[[id]]$covs), cov_data_life = as.matrix(l.walks.life[[id]]$covs),
                                         walk_info_trans = as.matrix(l.walks.trans[[id]]$info), walk_info_life = as.matrix(l.walks.life[[id]]$info),
                              return_intermediate_results = T)
  data.table(Id=id, t(res))
})

dt.LL.cpp <- rbindlist(l.LL)
cols.compare <- c("LL", "Akprod", "A1sum", "Bksum", "B1", "BT", "D1", "DT",  "log.F0", "log.F1", "log.F3",
                  "F2", "F2.1", "F2.2", "F2.3",  "dT") #"DkT", "bkT", "bT", "aT",
all.equal(dt.LL.original[, .SD, .SDcols=cols.compare],
          dt.LL.cpp[, .SD, .SDcols=cols.compare],
          check.attributes = F)

