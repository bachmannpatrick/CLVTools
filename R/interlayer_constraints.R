# LL.params are the unfixed parameters containing the unfixed params only once
#' @importFrom utils modifyList
interlayer_constraints <- function(next.interlayers, LL.params, LL.function.sum, names.original.params.constr, names.prefixed.params.constr, ...){

  # Catch elipsis params immediately
  all.other.args <- list(...)

  # Construct new param set --------------------------------------------------------------
  #   Only a single param is given for the fixed params
  #
  #   The vec LL.params contains all model params, all correctly named non-fixed params, and also
  #     the to-be-fixed, incorrectly named (missing prefix) params with a single param only per covariate
  #
  #   Construct param vec:
  #   - Non fixed params
  #   - Fixed params
  #     - Add to-be-fixed parameters twice, once for life and once for trans
  #     - Add "life." and "trans." prefixes to names

  # Add prefix to the names
  fixed.params.names.life   <- paste("life",   names.original.params.constr,  sep=".")
  fixed.params.names.trans  <- paste("trans",  names.original.params.constr,  sep=".")

  # check if names exist
  if(!(all(names.prefixed.params.constr %in% names(LL.params))))
    stop("The named constrained params are not among the parameters")

  # All params but the fixed ones
  new.LL.params <- LL.params[setdiff(names(LL.params), names.prefixed.params.constr)]

  # Add the fixed params twice (passed in the orignal param vec as well)
  new.LL.params[fixed.params.names.life]  <- LL.params[names.prefixed.params.constr]
  new.LL.params[fixed.params.names.trans] <- LL.params[names.prefixed.params.constr]

  # Call next interlayer ------------------------------------------------------
  #   Use do.call to integrate ... args

  next.interlayer.call.args <- list(next.interlayers = next.interlayers,
                                    LL.params = new.LL.params,
                                    LL.function.sum = LL.function.sum)

  next.interlayer.call.args <- modifyList(next.interlayer.call.args,
                                          all.other.args)

  return(do.call(what = interlayer_callnextinterlayer, args = next.interlayer.call.args))
}
