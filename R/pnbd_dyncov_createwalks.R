# Creates all walks for given transaction and one type of covariate data
# transaction data:       Id, Date
# Covariate.names: vector of names to select columns from data
# data.transactions will not be used as the standard transaction data:
#     for trans covs: will be reduced to repeat trans with the set flag is.first.trans
#     for life covs - reduced to 2 (sometimes 3) transactions
#' @importFrom methods is
pnbd_dyncov_createwalks <- function(clv.time, data.transactions, data.dyn.cov, names.dyn.cov){

  Date <- Id <- Cov.Date <- Mapping.Transaction.Id <- is.first.trans <- NULL # from add dyn cov
  Trans.Date <- Prev.Trans.Date.Plus.Eps <- This.Cov.Start.Date <- Next.Cov.Date.Minus.Eps <- N <- NULL # from here
  d <- tjk <- Walk1 <- Max.Walk <- Num.Walk <- delta <- NULL # for walk table

  cov.dt <- data.dyn.cov # readability, no copy

  # Setup data for date interval merging ------------------------------------
  #
  #   Covariate intervals:
  #     Covs date indicates when it got active - has influence until next cov
  #       -> Interval from (this cov) to (next Cov - 1sec)
  #
  #   Transaction intervals:
  #     A transaction is influenced somewhen between the last and actual trans
  #       -> Interval from (last Trans + 1sec) to (this Trans)
  #
  #   This needs to be done before removing irrelevant transactions

  # sort before so shift refers to the right dates
  setkeyv(data.transactions, c("Id", "Date", "AuxTrans"))
  setkeyv(cov.dt, c("Id", "Cov.Date"))


  data.transactions[, Prev.Trans.Date.Plus.Eps := shift(Date, type="lag") + 1L, by=Id]
  data.transactions[, Trans.Date   := Date ]

  cov.dt[, This.Cov.Start.Date := Cov.Date]
  cov.dt[, Next.Cov.Date.Minus.Eps := shift(Cov.Date, type = "lead") - 1L, by=Id]

  #
  # Corrections

  # due to shift Date.End is missing the last entry. Fill with Prev.Trans.Date.Plus.Eps (?? ok?? or better + 1week)
  # this should always be after cal.end and therefore not have any affect
  cov.dt[is.na(Next.Cov.Date.Minus.Eps), Next.Cov.Date.Minus.Eps := This.Cov.Start.Date]
  data.transactions[is.na(Prev.Trans.Date.Plus.Eps), Prev.Trans.Date.Plus.Eps := Trans.Date] #ymd("1970-01-01")]

  # when 2 transactions are on the same date, shift+1 will lead to Date.Start > Date.End
  data.transactions[Prev.Trans.Date.Plus.Eps > Trans.Date, Prev.Trans.Date.Plus.Eps := Trans.Date]
  cov.dt[This.Cov.Start.Date > Next.Cov.Date.Minus.Eps, This.Cov.Start.Date := Next.Cov.Date.Minus.Eps]

  # needs to be redone as rows were added
  setkeyv(data.transactions, c("Id", "Prev.Trans.Date.Plus.Eps", "Trans.Date"))
  setkeyv(cov.dt, c("Id", "Cov.Date", "This.Cov.Start.Date", "Next.Cov.Date.Minus.Eps"))



  # Transaction specific measures -------------------------------------------
  #   This cannot be done in the walks creation code as the date
  #     will be missing there

  # . d ---------------------------------------------------------------------
  # time between date.lagged and period end (ceiling(data.lagged+1))
  # d shall be 1 if it is exactly on the time.unit boundary!
  # Plus.Eps is already "+ 1"
  data.transactions[, d := clv.time.interval.in.number.tu(clv.time = clv.time,
                                                 interv = interval( start = Prev.Trans.Date.Plus.Eps - 1L,
                                                                    end   = clv.time.ceiling.date(clv.time=clv.time,
                                                                                                  timepoint=Prev.Trans.Date.Plus.Eps)))]

  # . tjk ---------------------------------------------------------------------
  # time between Trans and the previous Trans / from date.lagged to date
  data.transactions[, tjk := clv.time.interval.in.number.tu(clv.time = clv.time,
                                                             interv = interval( start = Prev.Trans.Date.Plus.Eps - 1L,
                                                                                end   = Trans.Date))]


  # Remove first transaction of each customer ---------------------------------
  #
  #   For Transaction walks:
  #       Because only repeat transactions are relevant
  #
  #   For Lifetime walks:
  #       Only 2 transactions are needed: AuxTrans and the one before
  #       The first transaction needs to be added as a duplicate to create
  #         the intervals correctly
  #         One of the duplicates has is.first.trans = T, the other one not
  #
  #   Aux Trans need to be added first, otherwise some customers are lost

  data.transactions <- data.transactions[is.first.trans == FALSE]



  # Merge transaction to covariates ------------------------------------------

  #Merge using Date intervals
  # NEW IN cleanup-dyncov ---------------------------------------------------
  # **TODO: IMPLEMENT CLEANER!
  # CONVERT DATES TO POSIXCT!
  if(is(clv.time, "clv.time.date")){
    covs.mapped <- foverlaps(x=cov.dt, y=data.transactions[Trans.Date <= floor_date(force_tz(as.POSIXct.Date(clv.time@timepoint.estimation.end), tzone = "UTC"), unit="day")],  # only up to Cal.End
                             by.x = c("Id", "This.Cov.Start.Date", "Next.Cov.Date.Minus.Eps"),
                             by.y = c("Id", "Prev.Trans.Date.Plus.Eps", "Trans.Date"),
                             type = "any",
                             nomatch = NA,    # fill with NA where no transaction could be found
                             mult    = "all") # keep all records when multiple transactions are present for the same date

    # cut off any Covariate after cal.end
    covs.mapped <- covs.mapped[Cov.Date <= floor_date(force_tz(as.POSIXct.Date(clv.time@timepoint.estimation.end),
                                                               tzone = "UTC"), unit="day")]
  }else{
    covs.mapped <- foverlaps(x=cov.dt, y=data.transactions[Trans.Date <= clv.time@timepoint.estimation.end],  # only up to Cal.End
                             by.x = c("Id", "This.Cov.Start.Date", "Next.Cov.Date.Minus.Eps"),
                             by.y = c("Id", "Prev.Trans.Date.Plus.Eps", "Trans.Date"),
                             type = "any",
                             nomatch = NA,    # fill with NA where no transaction could be found
                             mult    = "all") # keep all records when multiple transactions are present for the same date

    # cut off any Covariate after cal.end
    covs.mapped <- covs.mapped[Cov.Date <= clv.time@timepoint.estimation.end]
  }

  #for some/a lot of covariates there are no transactions mapped to it - remove them, otherwise to many walks (rows) are added
  covs.mapped <- covs.mapped[!is.na(Mapping.Transaction.Id)]

  # Create Walks -------------------------------------------------------------
  #   Create the whole walks table for all covariates given

  #find the largest walk
  no.walks <- covs.mapped[, .N, by=c("Id", "Mapping.Transaction.Id")][, max(N)]
  # find the number of walks required
  no.rows <- nrow(covs.mapped[,.N, by=c("Id", "Mapping.Transaction.Id")])

  #Create a walks table for every covariate
  covariate.walks <- lapply(X=names.dyn.cov, FUN=function(cov.name){

    # Walks -------------------------------------------
    #   Write the covariates to the columns

    # pre allocate the whole walk data.table in advance and fill with NAs
    walks           <- as.data.table(matrix(data  = NA_real_, nrow = no.rows, ncol = no.walks))
    walk.names      <- paste0("Walk", seq(no.walks))
    colnames(walks) <- walk.names

    # Helper to write the .SD vector to the walk data.table
    # Walks will be ordered by group / .GRP is row number
    write.walk <- function(SD, GRP){
      for(j in seq_along(SD)) { #all covariates in SD
        set(x=walks, i=GRP, j=j, value=as.numeric(SD[j]))
      }
    }
    covs.mapped[, write.walk(SD = unlist(.SD), GRP = .GRP), by=c("Id", "Mapping.Transaction.Id"), .SDcols = cov.name]


    # Num.Walk -------------------------------------------
    #   Count number of walks (non NA entries)
    walks[, Num.Walk := as.integer(rowSums(!is.na(.SD))), .SDcols = walk.names]


    # Max.Walk -------------------------------------------
    #   Write the first non NA value to Max.Walk

    # Idea: - Go backwards through walk cols and move the value to Max.Walk (regardless of content)
    #       - Restrain for rows where Max.Walk is still NA (=last non-NA element not found yet)
    #       - Next col (backwards)

    walks[, Max.Walk := NA_real_] # NA in max.walk indicates that the first non-na element has not been found yet
    relrows = walks[, .I]         # at the beginning, all rows are considered

    #go backwards through all walk columns
    col.indices <- order(which(colnames(walks) %in% walk.names), decreasing = T)
    for (col in col.indices){

      #set max.walk for all relevant rows to the value in the current column (also NA possible!)
      set(walks, j="Max.Walk", i=relrows, value = walks[[col]][relrows])

      #set NA - if this was the first non-na: thats what is needed, else doesnt matter because was NA before
      set(walks, j=col, i=relrows, value=NA_real_)

      #in next col/loop only use the rows where max.walk has not been set yet (NA, because copied from column)
      relrows <- walks[,.I[is.na(Max.Walk)]]
    }

    # where there is only 1 walk, a value is needed in both, Walk1 and Max.Walk
    #   copy covariate in Max.Walk back to Walk1
    walks[Num.Walk == 1, Walk1 := Max.Walk]

    # Add further columns from transactions (covs.mapped) -----------------------------------------
    #   Add one row from last(!) transaction in
    #   cov table

    transaction.spec.data <- covs.mapped[ , .SD[.N],  by=c("Id", "Mapping.Transaction.Id")]
    walks[, c("Id", "AuxTrans", "Date", "tjk", "d") := transaction.spec.data[, c("Id", "AuxTrans", "Date", "tjk", "d") ] ]

    # do separatly as get() is needed and cannot use get on vectors
    # this is actually just Max.Walk!
    walks[, c("Cov.on.trans.date") := transaction.spec.data[, get(cov.name)]]


    # delta: if Num.Walk > 1 -> 1, otherwise 0
    walks[Num.Walk  > 1, delta:= 1]
    walks[Num.Walk == 1, delta:= 0]


    #set col order: Id, Date, Cov.on.trans.date, AuxTrans, Walk1:n, Max.Walk, Num.Walk, delta, tjk, d
    setcolorder(walks, c("Id", "Date", "AuxTrans", "Cov.on.trans.date", walk.names, "Max.Walk", "Num.Walk", "delta", "tjk", "d"))
    walks[, Date := force_tz(Date, tzone="UTC")]
    walks[, Date := suppressMessages(clv.time.convert.user.input.to.timepoint(clv.time, Date))]
    setkeyv(walks, c("Id", "Date", "AuxTrans"))
    return(walks)
  })


  # cosmetics for list of walks
  names(covariate.walks) <- names.dyn.cov

  # thats it
  return(covariate.walks)

}
