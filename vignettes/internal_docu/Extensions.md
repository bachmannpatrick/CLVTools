

# Extend CLVTools with your own models

This guide will show you, how to easily integrate your own models into the CLVTools package. As an example, we will integrate the BG/NBD model as implemented by BTYD (https://github.com/cran/BTYD)

## Overview

 - 1. [Estimation](#1-estimation)
	 -  1.1. [Create a model class](#1-1-create-model) 
   	 -  1.2. [Implement estimation methods on your class](#1-2-implement-estimation)
   	 -  1.3. [Create your fitted model class](#1-3-create-model-fitted)
   	 -  1.4. [Implement the fitted class](#1-4-implement-model-fitted)
   	 -  1.5. [Entrypoint for model usage](#1-5-entrypoint-model-usage)
   	 -  1.6. [Example: Estimation](#1-6-example-estimation)
  -  2. [Expectation](#2-expectation)
	  - 2.1 [Implement the expectation method](#2-1-implement-expectation)
  -  3. [Prediction](#3-prediction)
	  - 3.1 [Implement the prediction method](#3-1-implement-prediction)
  - 4. [Example: Putting it all together](#4-example)


## <a name="1-estimation"></a> 1. Estimation 

### <a name="1-1-create-model"></a>1.1 Create a model class
First, create a new R script with a class called `clv.model.{your-model-name}` where the part in the brackets is the name of your model. In the class definition, make sure to inherit from `clv.model` in order to get the basic model functionality.

```R
setClass(Class = "clv.model.bgnbd.no.cov", contains = "clv.model",
         slots = list(),
         prototype = list(
           name.model = character(),
           names.original.params.model = character(0),
           names.prefixed.params.model = character(0),
           start.params.model = numeric(0)
         ))

#' @importFrom methods new
clv.model.bgnbd.no.cov <- function(){
  return(new("clv.model.bgnbd.no.cov",
             name.model = "BG/NBD Standard",
             names.original.params.model = c(r="r", alpha="alpha", a="a", b="b"),
             names.prefixed.params.model = c("log.r", "log.alpha", "log.a", "log.b"),
             start.params.model = c(r=1, alpha = 3, a = 1, b = 3)))
}
```

### <a name="1-2-implement-estimation"></a>1.2 Implement estimation methods on your class
In order for your model class to provide estimation data, you will need to implement the following methods on your class:

**clv.model.check.input.args**: Contains validation for the input parameters of your model.
```R
#' @include all_generics.R
setMethod(f = "clv.model.check.input.args", signature = signature(clv.model="clv.model.bgnbd.no.cov"), definition = function(clv.model, clv.fitted, start.params.model, use.cor, start.param.cor, optimx.args, verbose, ...){
  err.msg <- c()

  # Have to be > 0 as will be logged
  if(any(start.params.model <= 0)){
    err.msg <- c(err.msg, "Please provide only model start parameters greater than 0 as they will be log()-ed for the optimization!")
  }

  if(use.cor){
    err.msg <- c(err.msg, "Correlation is not supported for the BG/NBD model")
  }

  if(length(list(...)) > 0){
    stop("Any further parameters passed in ... are ignored because they are not needed by this model.", call. = FALSE, immediate. = TRUE)
  }

  check_err_msg(err.msg)
})

```

**clv.model.put.estimation.input**: 
```R
setMethod(f = "clv.model.put.estimation.input", signature = signature(clv.model="clv.model.bgnbd.no.cov"), definition = function(clv.model, clv.fitted, verbose, ...){
  # nothing to put specifically for this model
  return(clv.fitted)
})
```
**clv.model.transform.start.params.model**: Usually used to apply `log()` method on the parameters.
```R
#' @importFrom stats setNames
setMethod("clv.model.transform.start.params.model", signature = signature(clv.model="clv.model.bgnbd.no.cov"), definition = function(clv.model, original.start.params.model){
  # Log all user given or default start params
  return(setNames(log(original.start.params.model[clv.model@names.original.params.model]),
                  clv.model@names.prefixed.params.model))
})
```

**clv.model.backtransform.start.params.model**: Usually used to apply `exp()` method on the parameters. (Reverts the `log()` call of clv.model.transform.start.params.model)
```R
#' @importFrom stats setNames
setMethod("clv.model.backtransform.estimated.params.model", signature = signature(clv.model="clv.model.bgnbd.no.cov"), definition = function(clv.model, prefixed.params.model){
  # exp all prefixed params
  return(exp(prefixed.params.model[clv.model@names.prefixed.params.model]))
})
```
**clv.model.prepare.optimx.args**: This is where you initialize optimx arguments. You specifiy your log likelihood (LL) functions both on individual (`LL.function.ind`) and on aggregated levels (`LL.function.sum`). Also, you provide the CBS (Customer-By-Sufficiency) matrix. 
**CAUTION**: Your parameters are very likely logarithmic, so you need to consider this in your LL functions or you will get incorrect values.
```R
setMethod(f = "clv.model.prepare.optimx.args", signature = signature(clv.model="clv.model.bgnbd.no.cov"), definition = function(clv.model, clv.fitted, prepared.optimx.args,...){

  # Only add LL function args, everything else is prepared already, incl. start parameters
  optimx.args <- modifyList(prepared.optimx.args,
                            list(LL.function.sum = bgnbd_nocov_LL_sum,
                                 LL.function.ind = bgnbd_nocov_LL_ind, # if doing correlation
                                 obj    = clv.fitted,
                                 vX     = clv.fitted@cbs$x,
                                 vT_x   = clv.fitted@cbs$t.x,
                                 vT_cal = clv.fitted@cbs$T.cal,

                                 # parameter ordering for the callLL interlayer
                                 LL.params.names.ordered = c(log.r = "log.r",log.alpha =  "log.alpha", log.a = "log.a", log.b = "log.b")),
                            keep.null = TRUE)
  return(optimx.args)
})
```

**clv.model.process.post.estimation**
```R
setMethod("clv.model.process.post.estimation", signature = signature(clv.model="clv.model.bgnbd.no.cov"), definition = function(clv.model, clv.fitted, res.optimx){
  # No additional step needed (ie store model specific stuff, extra process)
  return(clv.fitted)
})
```

**clv.model.put.newdata**
```R
# .clv.model.put.newdata --------------------------------------------------------------------------------------------------------
setMethod(f = "clv.model.put.newdata", signature = signature(clv.model = "clv.model.bgnbd.no.cov"), definition = function(clv.model, clv.fitted, verbose){
  # clv.data in clv.fitted is already replaced with newdata here
  # Need to only redo cbs if given new data
  clv.fitted@cbs <- bgnbd_cbs(clv.data = clv.fitted@clv.data)
  return(clv.fitted)
})
```

**clv.model.vcov.jacobi.diag**
```R
setMethod(f = "clv.model.vcov.jacobi.diag", signature = signature(clv.model="clv.model.bgnbd.no.cov"), definition = function(clv.model, clv.fitted, prefixed.params){
  # Create matrix with the full required size
  m.diag <- diag(x = 0, ncol = length(prefixed.params), nrow=length(prefixed.params))
  rownames(m.diag) <- colnames(m.diag) <- names(prefixed.params)

  # Add the transformations for the model to the matrix
  #   All model params need to be exp()
  m.diag[clv.model@names.prefixed.params.model,
         clv.model@names.prefixed.params.model] <- diag(x = exp(prefixed.params[clv.model@names.prefixed.params.model]),
                                                        nrow = length(clv.model@names.prefixed.params.model),
                                                        ncol = length(clv.model@names.prefixed.params.model))
  return(m.diag)
})
```

### <a name="1-3-create-model-fitted"></a>1.3 Create your fitted model class
Create a new R script with a class called `clv.{your-model-name}` where the part in the brackets is the name of your model. In the class definition, make sure to inherit from `clv.fitted` in order to get the basic functionality. This class will be responsible to create the CBS (Customer-By-Sufficiency) matrix, which we need for the optimization.

```R
#' @keywords internal
#' @importFrom methods setClass
#' @include class_clv_model_bgnbd.R class_clv_data.R class_clv_fitted.R
setClass(Class = "clv.bgnbd", contains = "clv.fitted",
         slots = c(
           cbs = "data.table"),

         # Prototype is labeled not useful anymore, but still recommended by Hadley / Bioc
         prototype = list(
           cbs = data.table()))
```

### <a name="1-4-implement-model-fitted"></a>1.4 Implement the fitted model
We now need a method to generate a CBS matrix, as well as a constructor for the class. We will call the CBS generating method from the constructor and return an instance of `clv.{your-model-name}`

**Constructor**
```R
clv.bgnbd <- function(cl, clv.data){

  dt.cbs.bgnbd <- bgnbd_cbs(clv.data = clv.data)
  clv.model    <- clv.model.bgnbd.no.cov()

  return(new("clv.bgnbd",
             clv.fitted(cl=cl, clv.model=clv.model, clv.data=clv.data),
             cbs = dt.cbs.bgnbd))
}
```

**CBS method**
```R
bgnbd_cbs <- function(clv.data){
  Date <- Price <- x <- date.first.actual.trans <- date.last.transaction <- NULL
  # Customer-By-Sufficiency (CBS) Matrix
  #   Only for transactions in calibration period
  #   Only repeat transactions are relevant
  #
  #   For every customer:
  #     x:        Number of repeat transactions := Number of actual transactions - 1
  #     t.x:      Time between first actual and last transaction
  #     T.cal:    Time between first actual transaction and end of calibration period
  #     Spending: Average (mean) spending per transaction (of all transactions, not only repeat)
  #
  #     All time is expressed in time units
  trans.dt <- clv.data@data.transactions[Date <= clv.data@clv.time@timepoint.estimation.end]

  #Initial cbs, for every Id a row
  if(clv.data.has.spending(clv.data)){
    cbs <- trans.dt[ , list(x                        =.N,
                            date.first.actual.trans  = min(Date),
                            date.last.transaction    = max(Date),
                            Spending                 = mean(Price, na.rm=TRUE)),
                     by="Id"]
  }else{
    cbs <- trans.dt[ , list(x                        =.N,
                            date.first.actual.trans  = min(Date),
                            date.last.transaction    = max(Date)),
                     by="Id"]
  }

  # Only repeat transactions -> Number of transactions - 1
  cbs[, x := x - 1]

  # t.x, T.cal
  cbs[, ':='(t.x      = clv.time.interval.in.number.tu(clv.time=clv.data@clv.time, interv=interval(start = date.first.actual.trans, end = date.last.transaction)),
             T.cal    = clv.time.interval.in.number.tu(clv.time=clv.data@clv.time, interv=interval(start = date.first.actual.trans, end = clv.data@clv.time@timepoint.estimation.end)))]

  setkeyv(cbs, c("Id", "date.first.actual.trans"))
  if(clv.data.has.spending(clv.data))
    setcolorder(cbs, c("Id","x","t.x","T.cal","Spending","date.first.actual.trans", "date.last.transaction"))
  else
    setcolorder(cbs, c("Id","x","t.x","T.cal", "date.first.actual.trans", "date.last.transaction"))

  return(cbs)
}
```

### <a name="1-5-entrypoint-model-usage"></a>1.5 Entrypoint for model usage
We now have both a model and a fitted class, the missing piece in the puzzle is the entry point that makes use of the functionality that we implemented. We use a generic method for this purpose. Create a new R script called `f_interface_{your-model-name}.R` with the following contents:
```R
#' @exportMethod bgnbd
setGeneric("bgnbd", def = function(clv.data, start.params.model=c(), use.cor = FALSE, start.param.cor=c(),
                                  optimx.args=list(), verbose=TRUE, ...)
  standardGeneric("bgnbd"))
  
#' @aliases bgnbd bgnbd,clv.data-method
#' @include class_clv_data.R class_clv_model_bgnbd.R
#' @export
setMethod("bgnbd", signature = signature(clv.data="clv.data"), definition = function(clv.data,
                                                                                    start.params.model=c(),
                                                                                    use.cor = FALSE,
                                                                                    start.param.cor=c(),
                                                                                    optimx.args=list(),
                                                                                    verbose=TRUE,...){
  cl <- match.call(call = sys.call(-1), expand.dots = TRUE)

  obj <- clv.bgnbd(cl=cl, clv.data=clv.data)

  return(clv.template.controlflow.estimate(clv.fitted = obj, cl=cl, start.params.model = start.params.model, use.cor = use.cor,
                                           start.param.cor = start.param.cor, optimx.args = optimx.args, verbose=verbose, ...))
})
```

### <a name="1-6-example-estimation"></a>1.6 Example: Estimation
If you set up your model correctly, you should be able to estimate parameters:
```R
library("CLVTools")
data("cdnow")

clv.apparel <- clvdata(cdnow,
                       date.format="ymd",
                       time.unit = "week",
                       estimation.split = "1997-09-30",
                       name.id = "Id",
                       name.date = "Date",
                       name.price = "Price")

est.bgnbd <- bgnbd(clv.data = clv.apparel, start.params.model = c(r = 1, alpha = 3, a = 1, b= 3),
                   optimx.args = list(control=list(trace=5) ))

summary(est.bgnbd)
```

The output should look like this:
```
BG/NBD Standard  Model 

Call:
bgnbd(clv.data = clv.apparel, start.params.model = c(r = 1, alpha = 3, 
    a = 1, b = 3), optimx.args = list(control = list(trace = 5)))

Fitting period:                               
Estimation start  1997-01-01   
Estimation end    1997-09-30   
Estimation length 38.8571 Weeks

Coefficients:
      Estimate Std. Error  z-val Pr(>|z|)    
r      0.24259    0.05176  4.687 2.78e-06 ***
alpha  4.41360    0.08570 51.504  < 2e-16 ***
a      0.79292    0.23424  3.385 0.000712 ***
b      2.42589    0.29078  8.343  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Optimization info:                 
LL     -9582.4292
AIC    19172.8584
BIC    19195.9190
KKT 1  TRUE      
KKT 2  TRUE      
fevals 32.0000   
Method BFGS      

Used Options:                 
Correlation FALSE
```

## <a name="2-expectation"></a> 2 Expectation

### <a name="2-1-implement-expectation"></a>2.1 Implement the expectation method
In your model class, create the method `clv.model.expectation`:

```R
#' @include all_generics.R
setMethod("clv.model.expectation", signature(clv.model="clv.model.bgnbd.no.cov"), function(clv.model, clv.fitted, dt.expectation.seq, verbose){
  r <- alpha <- a <- b <- date.first.repeat.trans<- date.first.actual.trans <- T.cal <- t_i<- period.first.trans<-NULL

  params_i <- clv.fitted@cbs[, c("Id", "T.cal", "date.first.actual.trans")]

  params_i[, r := clv.fitted@prediction.params.model[["r"]]]
  params_i[, alpha := clv.fitted@prediction.params.model[["alpha"]]]
  params_i[, a := clv.fitted@prediction.params.model[["a"]]]
  params_i[, b := clv.fitted@prediction.params.model[["b"]]]

  fct.bgnbd.expectation <- function(params_i.t){
    term1 <- params_i.t[,(a + b - 1)/(a - 1)]
    term2 <- params_i.t[,(alpha/(alpha + t_i))^r]
    term3 <- params_i.t[, vec_gsl_hyp2f1_e(r, b, a+b-1, t_i/(alpha+t_i) )$value]

    return(term1 * (1 - term2 * term3))
  }

  return(DoExpectation(dt.expectation.seq = dt.expectation.seq, params_i = params_i,
                       fct.expectation = fct.bgnbd.expectation, clv.time = clv.fitted@clv.data@clv.time))
})
```

## <a name="3-prediction"></a> 3 Prediction

### <a name="3-1-implement-prediction"></a> 3.1 Implement the prediction method
In your model class, create the method `clv.model.predict.clv`:

```R
#' @include all_generics.R
setMethod("clv.model.predict.clv", signature(clv.model="clv.model.bgnbd.no.cov"), function(clv.model, clv.fitted, dt.prediction, continuous.discount.factor, verbose){
  r <- alpha <- a <- b <- period.length <- CET <- x <- t.x <- T.cal <- PAlive <- DERT <- NULL

  # To be sure they are both sorted the same when calling cpp functions
  setkeyv(dt.prediction, "Id")
  setkeyv(clv.fitted@cbs, "Id")

  predict.number.of.periods <- dt.prediction[1, period.length]

  # Add CET
  dt.prediction[, CET := bgnbd_nocov_CET(r = clv.fitted@prediction.params.model[["r"]],
                                         alpha = clv.fitted@prediction.params.model[["alpha"]],
                                         a = clv.fitted@prediction.params.model[["a"]],
                                         b = clv.fitted@prediction.params.model[["b"]],
                                         nPeriods = predict.number.of.periods,
                                         vX = clv.fitted@cbs[, x],
                                         vT_x = clv.fitted@cbs[, t.x],
                                         vT_cal = clv.fitted@cbs[, T.cal])]


  # Add PAlive
  dt.prediction[, PAlive := bgnbd_nocov_PAlive(r = clv.fitted@prediction.params.model[["r"]],
                                               alpha = clv.fitted@prediction.params.model[["alpha"]],
                                               a = clv.fitted@prediction.params.model[["a"]],
                                               b = clv.fitted@prediction.params.model[["b"]],
                                               vX = clv.fitted@cbs[, x],
                                               vT_x = clv.fitted@cbs[, t.x],
                                               vT_cal = clv.fitted@cbs[, T.cal])]
  # Add DERT
  dt.prediction[, DERT := 0]

  return(dt.prediction)
})
```
## <a name="4-example"></a>4. Example: Putting it all together
If you implemented all the above mentioned classes and methods, you will be able to make use of the implemented functionality:

```R
library("CLVTools")
data("cdnow")

clv.apparel <- clvdata(cdnow,
                       date.format="ymd",
                       time.unit = "week",
                       estimation.split = "1997-09-30",
                       name.id = "Id",
                       name.date = "Date",
                       name.price = "Price")

est.bgnbd <- bgnbd(clv.data = clv.apparel, start.params.model = c(r = 1, alpha = 3, a = 1, b= 3),
                   optimx.args = list(control=list(trace=5) ))

summary(est.bgnbd)
coef(est.bgnbd)


predict(est.bgnbd, prediction.end = "2011-12-31")
plot(est.bgnbd)
```
