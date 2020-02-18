#' @include class_clv_time.R
#' @importFrom lubridate interval
clv.time.get.prediction.table <- function(clv.time, user.prediction.end){

  # Interpretation of periods
  #   Transactions can happen on estimation end
  #     Start of 1 period forward hence only is 1 timepoint after estimation.end
  #     End of 1 period forward should, including the start and end itself, represent a single period
  #     => This is + 1 lubridate::period to estimation.end
  #         Result is the end of 1 period forward, if end is counted towards the period
  #         Example:
  #           Estimation end: Sun 2019-10-06
  #           1 week forward
  #             First timepoint of 1 period: Mon 2019-10-07
  #             Last timepoint of full 1 period: Sun 2019-10-13
  #               In lubridate:: Have to use estimation.end
  #               ymd("2019-10-06")+lubridate::period(1, "weeks") = ymd("2019-10-13")
  #             First timepoint of 1st period: Mon 2019-10-07
  #             Last timepoint of full 2nd period: Sun 2019-10-20 (14d = 2weeks)
  #               ymd("2019-10-06")+lubridate::period(2, "weeks") = ymd("2019-10-20")
  #
  #   If the timepoint on which to end is given, it is counted towards / included in the prediction period.
  #     The length/number of periods has to account for this and the fact that the period only starts after
  #       the estimation end.
  #     Example:
  #       Estimation end: Wed 2019-06-12
  #       Prediction end: Wed 2019-06-19
  #         -> Prediction period is (Wed 2019-06-12 - Wed 2019-06-19] = [Thu 2019-06-13 - Wed 2019-06-19]
  #             Length = 7d = 1 week
  #             In lubridate: have to use estimation.end
  #                as.numeric(as.period(interval(start = ymd("2019-06-12"), end = ymd("2019-06-19"))), "week") = 1
  #       Estimation end: Wed 2019-06-12
  #       Prediction end: Fr 2019-06-28 (16d)
  #         -> Prediction period is (Wed 2019-06-12 - Wed 2019-06-28] = [Thu 2019-06-13 - Wed 2019-06-28]
  #             Length = 16d = 2+2/7 week
  #                as.numeric(as.period(interval(start = ymd("2019-06-12"), end = ymd("2019-06-28"))), "week") = 2.285
  #

  # Documentation:
  #   Example: The estimation period ends on Wed Xx-xx-xxx, then the first of the prediction period is on
  #   Thu and the prediction period ends on which is included. the last

  # Prediction end given:
  #   Timepoint: Until and including this point. Length can be inferred from this.
  #   Numeric: This many periods. Due to limitations in lubridate's periods (and the Date class which
  #     represents only full days), only whole periods can be added up (because what is 0.2345 weeks even?)

  if(!is.numeric(user.prediction.end)){

    if(!is.null(user.prediction.end)){
      # Calcuate number of periods bases on given date
      #  parse date, char, posixt. then count number of timeunits
      prediction.end.date <- clv.time.convert.user.input.to.timepoint(clv.time=clv.time,
                                                                      user.timepoint=user.prediction.end)
    }else{
      # Whether there is a holdout sample is checked in the predict inputchecks
      prediction.end.date <- clv.time@timepoint.holdout.end
    }



    # As explained above, estimation.end has to be used as the start of the interval
    #   to correctly count the numer of periods which are [estimation.end+1TP, prediction.end.date]
    number.of.time.units <- clv.time.interval.in.number.tu(clv.time=clv.time,
                                                           interv=interval(start = clv.time@timepoint.estimation.end,
                                                                           end   = prediction.end.date))

    return(data.table(period.first = clv.time@timepoint.holdout.start,
                      period.last  = prediction.end.date,
                      period.length=number.of.time.units))

  }else{

    # prediction.end is numeric

    # Make periods integer because of lubridate::period limitations
    #   This is the same behavior as in clv.time.set.sample.periods
    if(user.prediction.end %% 1 != 0)
      warning("The parameter prediction.end may not indicate partial periods. Digits after the decimal point are cut off.",
              call. = FALSE)

    number.of.time.units <- as.integer(user.prediction.end)

    # Alternatives to cutting off digits / partial
    #   - Strictly allow only full periods (ie stop in inputchecks)
    #   - Use lubridate::duration and round to closest full timepoint (day/second)
    #       and recalculate exact length from this TP

    # Timepoint that marks the end of this many periods
    #   As explained above, the periods have to be added to estimation.end
    prediction.end.date <- clv.time@timepoint.estimation.end +
      clv.time.number.timeunits.to.timeperiod(clv.time = clv.time,
                                              user.number.periods = number.of.time.units)

    return(data.table(period.first = clv.time@timepoint.holdout.start,
                      period.last  = prediction.end.date,
                      period.length=number.of.time.units))
  }
}
