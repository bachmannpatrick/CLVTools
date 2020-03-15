setMethod(f = "clv.controlflow.predict.set.prediction.params", signature = signature(obj="clv.fitted"), definition = function(obj){
  obj@prediction.params.model <- coef(obj)[obj@clv.model@names.original.params.model]
  return(obj)
})


setMethod(f = "clv.controlflow.predict.check.inputs", signature = signature(obj="clv.fitted"),
          definition = function(obj, prediction.end, continuous.discount.factor,predict.spending, verbose){
            err.msg <- c()

            err.msg <- c(err.msg, .check_user_data_single_boolean(b=verbose, var.name="verbose"))

            err.msg <- c(err.msg, check_user_data_predictionend(obj=obj, prediction.end=prediction.end))

            # Cannot predict if no prediction.end (=null) and no holdout
            if(is.null(prediction.end) & obj@clv.data@has.holdout == FALSE)
              err.msg <- c(err.msg, "Cannot predict without prediction.end if there is no holdout!")

            err.msg <- c(err.msg, check_user_data_continuousdiscountfactor(continuous.discount.factor=continuous.discount.factor))

            err.msg <- c(err.msg, .check_user_data_single_boolean(b = predict.spending,
                                                                  var.name = "predict.spending"))

            # predict.spending has to be single logical already
            check_err_msg(err.msg)

            # Check the data in the fitted model if it has spending
            if(predict.spending == TRUE & obj@clv.data@has.spending == FALSE)
              err.msg <- c(err.msg, "Cannot predict spending if there is no spending data!")

            check_err_msg(err.msg)
            # nothing to return
          })
