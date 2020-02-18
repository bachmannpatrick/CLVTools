#' @importFrom stats model.frame model.matrix reformulate
convert_userinput_covariatedata_dummies <- function(dt.cov.data, names.cov){

  # Use model.frame/model.matrix to convert cov data
  #   numeric to numeric, char/factors to k-1 dummies

  # Always need intercept!
  #   to always get k-1 dummies, as no intercept implies k dummies in the
  #   case of only a single catgorical covariate
  f.covs <- reformulate(termlabels = names.cov,
                        response = NULL,
                        intercept = TRUE)

  mf <- model.frame(f.covs, data = dt.cov.data)
  mm <- model.matrix(object = f.covs, data = dt.cov.data)

  # Combine averything else (Id, maybe Cov.Date) and raw converted numeric covariate data
  dt.cov <- cbind(dt.cov.data[, .SD, .SDcols=setdiff(colnames(dt.cov.data), names.cov)], # everything except cov data
                  mm[, setdiff(colnames(mm), "(Intercept)"), drop=FALSE]) # everything except the Intercept

  names.cov <- setdiff(colnames(dt.cov), c("Id", "Cov.Date"))

  return(list(data.cov = dt.cov, names.cov=names.cov))
}


# Returns the DATE until when to plot
#   Reason is that dyncov models need it in inputchecks to verify data length
#     and not only for plot length (=num periods)
# @importFrom lubridate is.Date is.POSIXt
# convert_user_data_predictionend_plot <- function(obj, prediction.end){
#
#   # prediction end
#   #   return date of until when to plot
#   #   date:   until this date (ceiling_tu(date))
#   #   posixt: until this date (ceiling_tu(date))
#   #   null:
#   #           holdout:    until end of holdout period (ceiling_tu(date.holdout.end))
#   #           no holdout: until end of estimation period (ceiling_tu(date.estimation.end))
#   #   number:
#   #           holdout:    this many tu since holdout start
#   #           no holdout: this many tu since estimation end
#
#   if(is.null(prediction.end)){
#     if(obj@has.holdout)
#       predict.date <- ceiling_tu(obj=obj, d=obj@date.holdout.end)
#     else
#       predict.date <- ceiling_tu(obj=obj, d=obj@date.estimation.end)
#   }else{
#
#     if(is.Date(prediction.end) | is.POSIXt(prediction.end) | is.character(prediction.end)){
#       predict.date <- .convert_userinput_datadate(date.data = prediction.end, date.format = obj@date.format) #parse_date_time(x = gen.date, orders = obj@date.format, quiet = T)
#       predict.date <- ceiling_tu(obj=obj, d=predict.date)
#     }else{
#         if(is.numeric(prediction.end)){
#           # Numer
#           if(obj@has.holdout)
#             predict.date <- ceiling_tu(obj=obj, d=obj@date.holdout.start + tu_to_duration(obj=obj, number.of.tu = prediction.end))
#           else
#             predict.date <- ceiling_tu(obj=obj, d=obj@date.estimation.end + tu_to_duration(obj=obj, number.of.tu = prediction.end))
#         }else{
#           # Not numeric or anything else..?
#           stop("Something went wrong, should not end up here. Please report.")
#         }
#     }
#   }
#   return(predict.date)
# }
