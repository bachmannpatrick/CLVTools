pnbd_dyncov_assert_walk_assumptions <- function(clv.fitted){
  abs_pos <- walk_id <- walk_from <- walk_to <- num_walks <- Id <- tp.cov.lower <- tp.this.trans <- NULL
  first_cov_real <- i.first_cov_real <- first_cov_aux <- first_cov_real <- NULL
  i.date.first.actual.trans <- x <- tjk <- d_omega <- d1 <- first_trans <- NULL

  assert_ret <- tryCatch(expr = {

    # All walks:
    #   keys: Id, walk_id, tp.this.trans, tp.cov.lower (and therefore also sorted)
    #   sorted, increasing:
    #     strictly monotonic: abs_pos
    #     monotonic: walk_id, walk_from, walk_to
    assert_allwalks <- function(dt.walks){
      # coreelements
      stopifnot(setequal(key(dt.walks), c("Id", "walk_id", "tp.this.trans", "tp.cov.lower")))
      stopifnot(dt.walks[, !is.unsorted(abs_pos,   strictly = TRUE)])
      stopifnot(dt.walks[, !is.unsorted(walk_id,   strictly = FALSE)])
      stopifnot(dt.walks[, !is.unsorted(walk_from, strictly = FALSE)])
      stopifnot(dt.walks[, !is.unsorted(walk_to,   strictly = FALSE)])

      stopifnot(!anyNA(dt.walks))
      # backwards looking from second transaction
      stopifnot(dt.walks[tp.this.trans < tp.cov.lower, .N] == 0)
    }

    assert_allwalks(clv.fitted@data.walks.life.aux)
    assert_allwalks(clv.fitted@data.walks.trans.aux)
    assert_allwalks(clv.fitted@data.walks.life.real)
    assert_allwalks(clv.fitted@data.walks.trans.real)


    # all aux walks:
    #   every Id only once
    #   every Id in walks
    #   same length per customer
    stopifnot(clv.fitted@data.walks.life.aux[, list(num_walks = uniqueN(walk_id)), by="Id"][, all(num_walks==1)])
    stopifnot(clv.fitted@data.walks.life.aux[, uniqueN(Id)] == nobs(clv.fitted))
    stopifnot(setequal(clv.fitted@data.walks.life.aux[, unique(Id)], clv.fitted@cbs$Id))

    stopifnot(clv.fitted@data.walks.trans.aux[, list(num_walks = uniqueN(walk_id)), by="Id"][, all(num_walks==1)])
    stopifnot(clv.fitted@data.walks.trans.aux[, uniqueN(Id)] == nobs(clv.fitted))
    stopifnot(setequal(clv.fitted@data.walks.trans.aux[, unique(Id)], clv.fitted@cbs$Id))

    stopifnot(identical(clv.fitted@data.walks.life.aux[, .N, keyby="Id"],
                        clv.fitted@data.walks.trans.aux[, .N, keyby="Id"]))


    # lifetime aux walk:
    #   no date overlap with real lifetime walks
    dt.tmp <- clv.fitted@data.walks.life.aux[, list(first_cov_aux = min(tp.cov.lower)), keyby="Id"]
    dt.tmp[clv.fitted@data.walks.life.real[, list(first_cov_real = min(tp.cov.lower)) , keyby="Id"], first_cov_real := i.first_cov_real, on="Id"]
    # some first_cov_real are NA because have no real walk
    stopifnot(dt.tmp[first_cov_aux < first_cov_real, .N] == 0)

    # lifetime real walk:
    #   exactly 1 walk per customer
    #   all ids except where aux walk reaches to the first transactions (coming alive)
    #   (number of customers = num customers in trans real walks where .N>1)
    #   n real walk + n aux walk >= ceiling(Tcal)
    stopifnot(clv.fitted@data.walks.life.real[, list(num_walks=uniqueN(walk_id)), keyby="Id"][, all(num_walks == 1)])
    # dt.tmp[clv.fitted@clv.data@data.transactions[, list(last_trans = max(Date)), by="Id"], last_trans := i.last_trans, on="Id"]
    dt.tmp[clv.fitted@cbs, first_trans := i.date.first.actual.trans, on="Id"]
    stopifnot(setequal(clv.fitted@data.walks.life.real[, unique(Id)],
                       dt.tmp[first_cov_aux > first_trans, Id]))

    # trans real walks:
    #   every Id with x>0 is in ...
    #            ... with x-1 walks
    stopifnot(setequal(clv.fitted@data.walks.trans.real[, unique(Id)], clv.fitted@cbs[x>0, Id]))
    stopifnot(identical(clv.fitted@data.walks.trans.real[, list(num_walks=as.double(uniqueN(walk_id))), keyby="Id"],
                        clv.fitted@cbs[x>0, list(num_walks=as.double(x)), keyby="Id"]))

    # trans walks
    #   tjk >= 0 (== 0 when t.x=T)
    stopifnot(clv.fitted@data.walks.trans.aux[tjk < 0, .N] == 0)
    stopifnot(clv.fitted@data.walks.trans.real[tjk < 0, .N] == 0)

    # d1 and d_omega measures are in (0,1]
    stopifnot(clv.fitted@cbs[, all(d_omega > 0 & d_omega <= 1)])
    stopifnot(clv.fitted@data.walks.trans.aux[, all(d1 > 0 & d1 <= 1)])
    stopifnot(clv.fitted@data.walks.trans.real[, all(d1 > 0 & d1 <= 1)])

  },
  error = function(e){return(e)})

  if(is(assert_ret, "error")){
    message("Caught the following error, please report issue on Github: \n", assert_ret)
    stop("Abort")
  }

}

pnbd_dyncov_getLLcallargs_LLsum <- function(clv.fitted){
  # Get all arguments (except `params`) to call `pnbd_dyncov_LL_negsum`.
  #
  # This includes "compressing" the input data (incl walks) across customers and it
  # only returns the compressed data.
  #
  # Data of customers which have a unique pattern is returned, together with `vN`.
  # This reduces the number of observations ("customers") for which the LL is calculated.
  # The returned walk data is reduced to contain only data of these customers. It
  # has different position information than the full walk tables.
  # Using only the walk data of these customers also reduces the matrix
  # multiplications done for `adj_covdata_*`.


  hash_life_real <- hash_life_aux <- hash_trans_real <- hash_trans_aux <- i.hash <- NULL
  .N <- Id <- NULL

  pnbd_dyncov_assert_walk_assumptions(clv.fitted)


  # Compress CBS ------------------------------------------------------------------

  # Add hashs to cbs to find customers with unique (LL input) data combinations
  #   use first() because the hashs in the walk table are all the same per customer
  #   For real trans walks, there is no walk available for zero-repeaters. This does not matter,
  #   as long as the same value (NA) is used for hash_trans_real.
  dt.cbs <- copy(clv.fitted@cbs)
  dt.cbs[unique(clv.fitted@data.walks.life.real[, c("Id", "hash")]), hash_life_real := i.hash, on="Id"]
  dt.cbs[unique(clv.fitted@data.walks.life.aux[, c("Id", "hash")]), hash_life_aux := i.hash, on="Id"]
  dt.cbs[unique(clv.fitted@data.walks.trans.real[, c("Id", "hash")]), hash_trans_real := i.hash, on="Id"]
  dt.cbs[unique(clv.fitted@data.walks.trans.aux[, c("Id", "hash")]), hash_trans_aux := i.hash, on="Id"]

  # Compress
  dt.compressed <- dt.cbs[
    ,list(n=.N, Id = first(Id)),
    by=c("x", "t.x", "T.cal", "d_omega", "hash_life_real", "hash_life_aux", "hash_trans_real", "hash_trans_aux")
    ]

  # Create walks of compressed customers ------------------------------------------

  # Select walks of customers with unique pattern
  dt.walks.life.real <- copy(clv.fitted@data.walks.life.real[Id %in% dt.compressed$Id])
  dt.walks.life.aux <- copy(clv.fitted@data.walks.life.aux[Id %in% dt.compressed$Id])
  dt.walks.trans.real <- copy(clv.fitted@data.walks.trans.real[Id %in% dt.compressed$Id])
  dt.walks.trans.aux <- copy(clv.fitted@data.walks.trans.aux[Id %in% dt.compressed$Id])

  # Update walk core elements, especially abs_pos, walk_from/to which are relevant
  # for reading the data in the LL
  dt.walks.life.real <- pnbd_dyncov_createwalks_add_corelements(dt.walks.life.real)
  dt.walks.life.aux <- pnbd_dyncov_createwalks_add_corelements(dt.walks.life.aux)
  dt.walks.trans.real <- pnbd_dyncov_createwalks_add_corelements(dt.walks.trans.real)
  dt.walks.trans.aux <- pnbd_dyncov_createwalks_add_corelements(dt.walks.trans.aux)


  # Add vN to call args --------------------------------------------------------
  ll.args <- .pnbd_dyncov_getLLcallargs_from_walks(
    dt.cbs = dt.compressed,
    dt.walks.life.aux=dt.walks.life.aux,
    dt.walks.life.real=dt.walks.life.real,
    dt.walks.trans.aux=dt.walks.trans.aux,
    dt.walks.trans.real=dt.walks.trans.real,
    names.cov.data.life = clv.fitted@clv.data@names.cov.data.life,
    names.cov.data.trans = clv.fitted@clv.data@names.cov.data.trans
  )

  # This requires that the data in the call args has the same order as dt.compressed!
  # Hence, dt.cbs may not be re-ordered in .pnbd_dyncov_getLLcallargs_from_walks!
  ll.args$vN <- dt.compressed$n

  return(ll.args)
}

.pnbd_dyncov_getLLcallargs_from_walks <- function(dt.cbs, dt.walks.life.aux, dt.walks.life.real, dt.walks.trans.aux, dt.walks.trans.real, names.cov.data.life, names.cov.data.trans){
  # Create args to pass to Rcpp LL functions pnbd_dyncov_LL_negsum and pnbd_dyncov_LL_ind
  #
  # dt.cbs has to retain its order (Id)!

  i.walk_from <- i.walk_to <- i.d <- i.tjk <- NULL
  walkinfo_trans_real_from <- walkinfo_trans_real_to <- i.id_from <- i.id_to <- NULL

  dt.cbs <- copy(dt.cbs)

  pnbd_dyncov_addwalkinfo_single <- function(dt.cbs, dt.walk, name, cols.walkinfo){
    # Get walkinfo from walk table
    dt.walkinfo <- unique(dt.walk[, .SD, .SDcols=cols.walkinfo])

    # Merge with suffix (but using update-join)
    for(wi in setdiff(cols.walkinfo, "Id")){
      col.name <- paste0("walk_",name,"_", gsub("_", "", gsub("walk", "", wi)))
      dt.cbs[dt.walkinfo, (col.name) := get(paste0("i.", wi)), on="Id"]
    }

    return(dt.cbs)
  }

  # Add walkinfo to CBS ----------------------------------------------------------------------
  # walkinfo: Where (index) to find customer's walk data & per-customer walk data
  cols.wi.life <- c("Id", "walk_from", "walk_to")
  cols.wi.trans <- c(cols.wi.life, "tjk", "d1")

  dt.cbs <- pnbd_dyncov_addwalkinfo_single(dt.cbs, dt.walk=dt.walks.life.aux,  name="aux_life", cols.walkinfo = cols.wi.life)
  dt.cbs <- pnbd_dyncov_addwalkinfo_single(dt.cbs, dt.walk=dt.walks.life.real, name="real_life", cols.walkinfo = cols.wi.life)
  dt.cbs <- pnbd_dyncov_addwalkinfo_single(dt.cbs, dt.walk=dt.walks.trans.aux, name="aux_trans", cols.walkinfo = cols.wi.trans)

  # walkinfo for real trans walks
  #   - Customers can have multiple real trans walks. Therefore walkinfo here is a matrix per customer, where each row contains a walkinfo
  #   - dt.walkinfo.real.trans: Walkinfo matrix with all walks info.
  #   - dt.customerinfo.real.trans: Where in dt.walkinfo.real.trans to find customer's walk info
  #   - zero-repeaters have no real trans walks and will have NA in walkinfo_trans_real_from/to
  #     which are skipped in the cpp implementation
  dt.walkinfo.real.trans <- pnbd_dyncov_getrealtranswalks_walkinfo(dt.walks.trans.real)
  dt.customerinfo.real.trans <- unique(dt.walkinfo.real.trans[, c("Id", "id_from", "id_to")])
  dt.cbs[dt.customerinfo.real.trans, walkinfo_trans_real_from := i.id_from, on="Id"]
  dt.cbs[dt.customerinfo.real.trans, walkinfo_trans_real_to   := i.id_to,   on="Id"]

  # Convert all data to vectors & matrices -------------------------------------------------
  # IN SAME ORDER AS READ OUT IN WALK constructor

  cols.walk.ordered.life  <- c("from", "to")
  cols.walk.ordered.trans <- c(cols.walk.ordered.life, "d1", "tjk")
  m.walkinfo.aux.life   <- data.matrix(dt.cbs[, .SD, .SDcols=paste0("walk_aux_life_",  cols.walk.ordered.life, sep="")])
  m.walkinfo.real.life  <- data.matrix(dt.cbs[, .SD, .SDcols=paste0("walk_real_life_", cols.walk.ordered.life, sep="")])
  m.walkinfo.aux.trans  <- data.matrix(dt.cbs[, .SD, .SDcols=paste0("walk_aux_trans_", cols.walk.ordered.trans, sep="")])
  m.walkinfo.real.trans <- data.matrix(dt.walkinfo.real.trans[, c("walk_from", "walk_to", "d1", "tjk")])

  # Verify col sorting is same as parameters!
  # TODO: What?? Are prediction.params.life available when optimizing? Move to walk verification?
  # stopifnot(names(clv.fitted@prediction.params.life) == clv.fitted@clv.data@names.cov.data.life)
  # stopifnot(names(clv.fitted@prediction.params.trans) == clv.fitted@clv.data@names.cov.data.trans)

  m.cov.data.aux.life    <- data.matrix(dt.walks.life.aux[, .SD, .SDcols=names.cov.data.life])
  m.cov.data.real.life   <- data.matrix(dt.walks.life.real[,  .SD, .SDcols=names.cov.data.life])
  m.cov.data.aux.trans   <- data.matrix(dt.walks.trans.aux[,  .SD, .SDcols=names.cov.data.trans])
  m.cov.data.real.trans  <- data.matrix(dt.walks.trans.real[, .SD, .SDcols=names.cov.data.trans])

  # Assemble LL call args ------------------------------------------------------------------

  return(list(
      X = dt.cbs$x,
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
      covdata_real_trans = m.cov.data.real.trans
  ))
}

pnbd_dyncov_getLLcallargs_ind <-function(clv.fitted){

  pnbd_dyncov_assert_walk_assumptions(clv.fitted)

  return(
    .pnbd_dyncov_getLLcallargs_from_walks(
      dt.cbs = clv.fitted@cbs,
      dt.walks.life.real = clv.fitted@data.walks.life.real,
      dt.walks.life.aux = clv.fitted@data.walks.life.aux,
      dt.walks.trans.real = clv.fitted@data.walks.trans.real,
      dt.walks.trans.aux = clv.fitted@data.walks.trans.aux,
      names.cov.data.life = clv.fitted@clv.data@names.cov.data.life,
      names.cov.data.trans = clv.fitted@clv.data@names.cov.data.trans
  ))
}


pnbd_dyncov_getLLdata <- function(clv.fitted, params){
  # get LL with all values, not just ind LL or summed LL
  l.LL.args <- pnbd_dyncov_getLLcallargs_ind(clv.fitted)
  l.LL.args[["params"]] <- params
  l.LL.args[["return_intermediate_results"]] <- TRUE

  # DATA HAS TO BE SAME ORDER AS PARAMS
  stopifnot(all(names(params) ==
                  c(clv.fitted@clv.model@names.prefixed.params.model,
                    clv.fitted@names.prefixed.params.after.constr.life,
                    clv.fitted@names.prefixed.params.after.constr.trans)))


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


pnbd_dyncov_createwalks_add_corelements <- function(dt.walks){
  walk_id <- abs_pos <- walk_from <- walk_to <- NULL

  # Add elements shared by all walks: walk_id, abs_pos, walk_from, walk_to
  # Do at single point to ensure sorting guarantees

  # Add walk ids:
  #   Does not require sorting, only marks what belongs together
  dt.walks[,  walk_id := .GRP, by=c("Id", "tp.this.trans")]

  # Sort walks:
  #  For each customer, walks in chronological order, actual data in walk in chronological order:
  #   Id, tp transaction, tp.cov.lower
  #  For completeness also add walk_id as key
  setkeyv(dt.walks, c("Id", "tp.this.trans", "tp.cov.lower", "walk_id"))


  # absolute position in data
  dt.walks[, abs_pos := seq(.N)]

  # walk_from/to: Start and end position in data of each walk

  # min() and max() give warning if table is empty (if there is not a single real lifetime walk)
  if(nrow(dt.walks) > 0){
    # Add from to of walk
    dt.walks[, walk_from := min(abs_pos), by="walk_id"]
    dt.walks[, walk_to   := max(abs_pos), by="walk_id"]
  }else{
    dt.walks[, walk_from := numeric(0)]
    dt.walks[, walk_to   := numeric(0)]
  }

  return(dt.walks)
}

pnbd_dyncov_walk_d <- function(clv.time, tp.relevant.transaction){

  # . d ---------------------------------------------------------------------
  # d shall be 1 if it is exactly on the time unit lower boundary
  # see comments on d_ in #205

  # lubridate::ceiling_date() has the argument change_on_boundary since Version 1.5.6 (2016-04-06)
  #   this makes +1 of previous implementations obsolete
  return(clv.time.interval.in.number.tu(clv.time=clv.time,
                                        interv = interval(start = tp.relevant.transaction,
                                                          end = clv.time.ceiling.date(clv.time=clv.time,
                                                                                      timepoint=tp.relevant.transaction))))
}

pnbd_dyncov_creatwalks_add_d1 <- function(dt.walk, clv.time){
  d1 <- tp.previous.trans <- NULL

  # d1
  #   "For any two successive transactions (j − 1), j, this is the time of the
  #    transaction (j-1) to the end of the first interval"
  #
  #    Number of periods between walk's first transaction and the end of the cov interval it is in

  dt.walk[, d1 := pnbd_dyncov_walk_d(clv.time=clv.time, tp.relevant.transaction=tp.previous.trans)]

  return(dt.walk)
}


pnbd_dyncov_creatwalks_matchcovstocuts <- function(dt.cov, dt.cuts, names.cov){
  # Match the cov data to the points given in dt.cuts

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
  # Create the walks for a given single transaction of customers
  # walk includes: id, abs_pos, from, to

  tp.cut.lower <- tp.cut.upper <- tp.this.trans <- tp.previous.trans <- NULL

  dt.cuts <- copy(dt.tp.first.last)
  dt.cuts[, tp.cut.lower := get(name.lower)]
  # dt.cuts[, tp.cut.lower := get(name.lower) + clv.time.epsilon(clv.time)]
  dt.cuts[, tp.cut.upper := get(name.upper)]
  dt.cuts[get(name.lower) == get(name.upper), tp.cut.lower := get(name.lower)]

  dt.walks <- pnbd_dyncov_creatwalks_matchcovstocuts(dt.cov = dt.cov, dt.cuts = dt.cuts,
                                                     names.cov = names.cov)

  dt.walks[, tp.this.trans := get(name.upper)]
  dt.walks[, tp.previous.trans := get(name.lower)]

  dt.walks <- pnbd_dyncov_createwalks_add_corelements(dt.walks)

  return(dt.walks)
}

pnbd_dyncov_covariate_add_interval_bounds <- function(dt.cov, clv.time){
  tp.cov.lower <- tp.cov.upper <- Cov.Date <- NULL

  # Covariate intervals are closed intervals + Cov.Date marks beginning (Covs are "forward-looking")
  #   => [Cov.Date, Next Cov.Date - eps] <=> [Cov.Date, Cov.Date + 1 Period - eps]

  dt.cov[, tp.cov.lower := Cov.Date]

  # Do not use shift() because leaves NA which will then have to be fixed
  #   has a test which verifies that same result as if shifting
  single.timeperiod <- clv.time.number.timeunits.to.timeperiod(clv.time, user.number.periods=1L)
  dt.cov[, tp.cov.upper := tp.cov.lower + single.timeperiod - clv.time.epsilon(clv.time)]

  return(dt.cov)
}


pnbd_dyncov_createwalks_real_trans <- function(clv.data, dt.trans, dt.tp.first.last){
  num.trans <- tp.this.trans <- tp.previous.trans <- tp.cut.lower <- tp.cut.upper <- is.first <- Date <- NULL
  clv.time <- clv.data@clv.time
  dt.cov <- clv.data@data.cov.trans

  # Covariates affecting repeat-transactions
  #   Zero-repeaters have no real walks
  #   No walk for the first transaction
  #   A transaction is influenced some time between the last and actual trans
  #     -> Interval [last trans + eps, this trans]

  # remove zero-repeaters
  dt.cuts.real <- dt.trans[dt.tp.first.last[num.trans > 1, "Id"], on="Id", nomatch=NULL]
  dt.tp.first.last[, num.trans := NULL]

  # If 2 transactions are on the same date, shift+1 will lead to Date.Start > Date.End
  #   Cannot/Should have no 2 transactions on same tp because are aggregated
  #   No aux trans, only real trans present and therefore there is no aux trans on T (no distance between transactions) in this data
  #   min dist between transactions is 1 eps, hence tp.cut.lower and tp.cut.upper can fall together when shifting. Although the length of this interval is 0,
  #   foverlaps() matches these to covariates and produces walks of length 1
  setkeyv(dt.cuts.real, cols=c("Id", "Date"))
  dt.cuts.real[, tp.this.trans := Date]
  dt.cuts.real[, tp.previous.trans := shift(tp.this.trans, n=1), by="Id"]
  # walk is [t_j, t_k] and not [t_j+eps, t_k]
  # dt.cuts.real[, tp.cut.lower := tp.previous.trans + clv.time.epsilon(clv.time)] # if we add + eps, lower > upper if they are only 1 eps apart (because we also shift)
  dt.cuts.real[, tp.cut.lower := tp.previous.trans]
  dt.cuts.real[, tp.cut.upper := tp.this.trans]

  # remove cut for first transaction
  #   - which has NA in tp.previous.trans because of shift()ing (and then cannot match to cov anyway)
  #   - for which no walk shall be created
  # dt.cuts.real <- dt.cuts.real[!is.na(tp.previous.trans)]

  # May be empty if there are no real trans walks (all zero-repeaters)
  if(nrow(dt.cuts.real) > 0){
    dt.cuts.real[, is.first := tp.this.trans == min(tp.this.trans), by="Id"]
  }else{
    dt.cuts.real[, is.first := logical(0), by="Id"]
  }
  dt.cuts.real <- dt.cuts.real[is.first == FALSE]
  dt.cuts.real[, is.first := NULL]

  dt.walks.real <- pnbd_dyncov_creatwalks_matchcovstocuts(dt.cov = dt.cov,
                                                          dt.cuts = dt.cuts.real,
                                                          names.cov = clv.data.get.names.cov.trans(clv.data))

  dt.walks.real <- pnbd_dyncov_createwalks_add_corelements(dt.walks.real)
  dt.walks.real <- pnbd_dyncov_creatwalks_add_tjk(dt.walks.real, clv.time)
  dt.walks.real <- pnbd_dyncov_creatwalks_add_d1(dt.walks.real, clv.time)

  return(dt.walks.real)
}

pnbd_dyncov_createwalks_real_life <- function(clv.data, dt.tp.first.last, dt.walks.aux.life){
  tp.first.aux.cov.lower <- i.tp.first.aux.cov.lower <- tp.cov.lower <- Cov.Date <- NULL

  # Real walks for the lifetime process are all covs before the one covariate where the last transaction happens in
  #   Given the aux walk, they are the residual covs since coming alive
  #   May have no overlap with the aux walk
  #   Only ever needed in Di() where they are summed additionally to the aux walk
  #   For some customers, there might be no real walk, if the aux walk starts in the customer's first period
  #   Still, every customer may have 1 real walk at maximum

  # Create from residuals given the aux walks
  #   Eligible covs:
  #     Per customer: Only transactions before the ones in aux walks (strict <)
  #   using non-equi join requires minimum data.table version 1.9.8!
  dt.tp.first.aux <- dt.walks.aux.life[, list(tp.first.aux.cov.lower = min(tp.cov.lower)), keyby="Id"]
  dt.cov <- clv.data@data.cov.life[dt.tp.first.aux, nomatch=NULL,
                                   on=c("Id==Id", "tp.cov.lower < tp.first.aux.cov.lower")]
  # Subset with join adds/sets tp.cov.lower=tp.first.aux.cov.lower: Overwrite with correct Cov.Date
  dt.cov[, tp.cov.lower := Cov.Date]

  # Alternative:
  # dt.cov <- copy(clv.data@data.cov.life)
  # dt.cov[dt.tp.first.aux, tp.first.aux.cov.lower := i.tp.first.aux.cov.lower, on="Id"]
  # dt.cov[, is.before.first.aux := tp.cov.lower < tp.first.aux.cov.lower]
  # dt.cov <- dt.cov[is.before.first.aux == TRUE]
  # dt.cov[, is.before.first.aux := NULL]
  # dt.cov[, tp.first.aux.cov.lower := NULL]

  # Create walk table only with eligible covariates
  dt.walks.real <- pnbd_dyncov_createwalks_singletrans(dt.cov=dt.cov,
                                                       dt.tp.first.last=dt.tp.first.last,
                                                       name.lower="tp.first.trans",
                                                       name.upper="tp.last.trans",
                                                       names.cov=clv.data.get.names.cov.life(clv.data),
                                                       clv.time=clv.data@clv.time)

  return(dt.walks.real)
}

pnbd_dyncov_createwalks_auxwalk <- function(dt.cov, dt.tp.first.last, names.cov, clv.time){
  tp.estimation.end <- NULL

  dt.tp.first.last[, tp.estimation.end := clv.time@timepoint.estimation.end]

  dt.walks.aux <- pnbd_dyncov_createwalks_singletrans(dt.cov=dt.cov,
                                                      dt.tp.first.last=dt.tp.first.last,
                                                      name.lower="tp.last.trans",
                                                      name.upper="tp.estimation.end",
                                                      names.cov = names.cov,
                                                      clv.time=clv.time)

  dt.walks.aux[, tp.estimation.end := NULL]
  return(dt.walks.aux)
}


#' @importFrom digest digest
pnbd_dyncov_createwalks <- function(clv.data){
  walk_id <- walk_from <- walk_to <- .N <- abs_pos <- Date <- tp.cov.lower <- hash <- NULL

  # Walk info, per walk
  #   - from, to: where to find walk data
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
                                                          dt.walks.aux.life=dt.walks.aux.life)


  dt.walks.real.trans <- pnbd_dyncov_createwalks_real_trans(clv.data,
                                                            dt.trans=dt.trans,
                                                            dt.tp.first.last=dt.tp.first.last)

  dt.walks.aux.trans <- pnbd_dyncov_createwalks_auxwalk(dt.cov=clv.data@data.cov.trans,
                                                        dt.tp.first.last=dt.tp.first.last,
                                                        names.cov=names.cov.trans,
                                                        clv.time=clv.data@clv.time)
  dt.walks.aux.trans <- pnbd_dyncov_creatwalks_add_tjk(dt.walks.aux.trans, clv.time = clv.data@clv.time)
  dt.walks.aux.trans <- pnbd_dyncov_creatwalks_add_d1(dt.walks.aux.trans, clv.time = clv.data@clv.time)

  # Add walk hashs ----------------------------------------------------------------------
  # Hashs are calculated at the level of the customer across all walks. They are based
  # on all data that make up a walk (incl tjk, d1)


  dt.walks.aux.life[,   hash := digest(.SD), by='Id', .SDcols = names.cov.life]
  dt.walks.real.life[,  hash := digest(.SD), by='Id', .SDcols = names.cov.life]
  dt.walks.aux.trans[,  hash := digest(.SD), by='Id', .SDcols = c(names.cov.trans, "tjk", "d1")]
  dt.walks.real.trans[, hash := digest(.SD), by='Id', .SDcols = c(names.cov.trans, "tjk", "d1")]


  # Create actual walk tables ------------------------------------------------------------
  cols.common <-  c("abs_pos", "Id", "tp.this.trans", "walk_id", "tp.cov.lower", "tp.cov.upper", "walk_from", "walk_to")
  cols.life  <- c(cols.common, names.cov.life, "hash")
  cols.trans <- c(cols.common, names.cov.trans, "tjk", "d1", "hash")

  return(list("data.walks.life.aux"   = dt.walks.aux.life[,   .SD, .SDcols=cols.life],
              "data.walks.life.real"  = dt.walks.real.life[,  .SD, .SDcols=cols.life],
              "data.walks.trans.aux"  = dt.walks.aux.trans[,  .SD, .SDcols=cols.trans],
              "data.walks.trans.real" = dt.walks.real.trans[, .SD, .SDcols=cols.trans]))
}



pnbd_dyncov_getrealtranswalks_walkinfo <- function(dt.walk){
  abs_pos <- Id <- id_from <- id_to <- NULL
  # minimum info, per walk. Keep Id to create customerinfo (match with cbs)

  dt.walkinfo <- unique(dt.walk[, c("Id", "walk_from", "walk_to", "tjk", "d1")])

  # Add abs_pos + id from and to here to ensure correct
  #   abs_pos requires sorting by id to ensure all walkinfo of same id is together
  setkeyv(dt.walkinfo, "Id")
  dt.walkinfo[, abs_pos := seq(.N)]

  dt.walkinfo[, id_from := min(abs_pos), by="Id"]
  dt.walkinfo[, id_to   := max(abs_pos), by="Id"]

  return(dt.walkinfo)
}

