
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Walktrough for the CLVTools package

## Setup the R environment

Install the stable version from CRAN:

``` r
install.packages("CLVTools")
```

Install the development version from GitHub (using the `devtools`
(Wickham, Hester, and Chang 2019) package):

``` r
install.packages("devtools")
devtools::install_github("CLVTools", ref = "master")
```

Load the package

``` r
library("CLVTools")
```

# Load sample data provided in the package

As Input data `CLVTools` requires customers’ transaction history. Every
transaction record consists of a purchase date and customer ID.
Optionally, the price of the transaction may be included to allow for
prediction of future customer spending using an additional Gamma/Gamma
model(Fader, Hardie, and Lee 2005b; Colombo and Jiang 1999). Using the
full history of transaction data allows for comprehensive plots and
summary statistics, which allow the identification of possible issues
prior to model estimation. Data may be provided as `data.frame` or
`data.table` (Dowle and Srinivasan 2019).

It is common practice to split time series data into two parts, an
estimation and a holdout period. The model is estimated based on the
data from the estimation period while the data from the holdout period
allows to rigorously assess model performance. Once model performance is
checked on known data one can proceed to predict data without a holdout
period. The length of the estimation period is heavily dependent on the
characteristics of the analyzed dataset. We recommend to choose an
estimation period that contains in minimum the lenght of the average
inter-purchase time. Note that all customers in the dataset need to
purchase at least once during the estimation period, i.e. these models
do not account for prospects who have not yet a purchase record.

Some models included in `CLVTools` allow to model the impact of
covariates. These covariates may explain heterogeneity among the
customers and therefore increase the predictive accuracy of the model.
At the same time we may also identify and quantify the effects of these
covariates on customer purchase and customer attrition. `CLVTools`
distinguishes between time-invariant and time-varying covariates.
Time-invariant covariates include customer characteristics such as
demographics that do not change over time. Time-varying covariates are
allowed to change over time. They include for example direct marketing
information or seasonal patterns.

For the following example, we use data of a retailer in the apparel
industry. The dataset contains transactional detail records for every
customer consisting of customer id, date of purchase and the total
monetary value of the transaction.The apparel dataset is available in
the `CLVTools` package. Use the `data(apparelTrans)` to load it:

``` r
data("apparelTrans")
apparelTrans
#>          Id       Date Price
#>     1:    1 2010-01-02     1
#>     2:    1 2010-02-27     1
#>     3:   10 2010-01-02     1
#>     4:  100 2010-01-02     1
#>     5: 1000 2010-01-02     1
#>    ---                      
#> 19569: 1772 2015-11-02     1
#> 19570: 1772 2015-12-13     1
#> 19571: 1772 2016-02-29     1
#> 19572: 1772 2016-08-24     1
#> 19573: 1773 2010-01-02     1
```

## Initialize the CLV-Object

Before we estimate a model, we are required to initialize a data object
using the `clvdata()` command. The data object contains the prepared
transactional data and is later used as input for model fitting. Make
sure to store the generated object in a variable, e.g. in our example
`clv.apparel`.

Through the argument `data.transactions` a `data.frame` or `data.table`
which contains the transaction records, is specified. In our example
this is `data.transactions=apparelTrans`. The argument `date.format` is
used to indicate the format of the date variable in the data used. The
date format in the apparel dataset is given as “year-month-day” (i.e.,
“2010-01-02”), therefore we set `date.format="ymd"`. Other
combinations such as `date.format="dmy"` are possible. See the
documentation of `lubridate` (Grolemund and Wickham 2011) for all
details. `time.unit` is the scale used to measure time between two
dates. For this dataset and in most other cases The argument
`time.unit="week"` is the preferred choice. Abbreviations may be used
(i.e. “w”). `estimation.split` indicates the length of the estimation
period. Either the length of the estimation period (in previous
specified time units) or the date at which the estimation period ends
can be specified. If no value is provided, the whole dataset is used as
estimation period (i.e. no holdout period). In this example, we use an
estimation period of 52 weeks. Finally, the three name arguments
indicate the column names for customer ID, date and price in the
supplied dataset.

``` r
clv.apparel <- clvdata(apparelTrans,  
                       date.format="ymd", 
                       time.unit = "week",
                       estimation.split = 52,
                       name.id = "Id",
                       name.date = "Date",
                       name.price = "Price")
#> The time of day stored in the provided data (of type POSIXct) is ignored (cut off).
```

## Check the `clvdata` Object

To get details on the `clvdata` object, print it to the console

``` r
clv.apparel
#> CLV Transaction Data 
#>                           
#> Total # customers    2201 
#> Total # transactions 19176
#> 
#>                                 
#> Time unit         Weeks         
#>                                 
#> Estimation start  2010-01-02    
#> Estimation end    2011-01-01    
#> Estimation length 52.0000 Weeks 
#>                                 
#> Holdout start     2011-01-02    
#> Holdout end       2016-10-08    
#> Holdout length    300.8571 Weeks
```

Alternatively the `summary()` command provides full detailed summary
statistics for the provided transactional detail. `summary()` is
available at any step in the process of estimating a probabilistic
customer attrition model with `CLVTools`. The result output is updated
accordingly and additional information is added to the summary
statistics.`nobs()` extracts the number of observations.

``` r
summary(clv.apparel)
#> CLV Transaction Data 
#>                                 
#> Time unit         Weeks         
#> Estimation length 52.0000 Weeks 
#> Holdout length    300.8571 Weeks
#> 
#> Transaction Data Summary 
#>                                    Estimation      Holdout         Total     
#> Number of customers                -               -               2201      
#> First Transaction in period        2010-01-02      2011-01-02      2010-01-02
#> Last Transaction in period         2011-01-01      2016-10-08      2016-10-08
#> Total # Transactions               6038            13138           19176     
#> Mean # Transactions per cust       2.743           17.778          8.712     
#> (SD)                               2.994           18.019          15.340    
#> Mean Spending per Transaction      1.012           1.025           1.021     
#> (SD)                               0.111           0.161           0.147     
#> Total Spending                     6113.000        13460.000       19573.000 
#> Total # zero repeaters             1082            1462            975       
#> Percentage # zero repeaters        0.492           0.664           0.443
```

For the apparel dataset we observe a total of 2201 customers who made in
total 10845 purchases. Approximately 61% of the customers are zero
repeaters, which means that the majority of the customers do not return
to the store after their first purchase. The mean inter-purchase of
about 26 weeks is shorter than the estimation period and confirms that a
52-week estimation period was a good choice.

## Estimate Model Parameters

After initializing the object, we are able to estimate the first
probabilistic latent attrtition model. We start with the standard
Pareto/NBD model (Schmittlein, Morrison, and Colombo 1987) and therefore
use the command `pnbd()` to fit the model and estimate model parameters.
`clv.data` specifies the initialized object prepared in the last step.
Optionally, starting values for the model parameters and control
settings for the optimization algorithm may be provided: The argument
`start.params.model` allows to assign a vector (e.g. `c(alpha=1, beta=2,
s=1, beta=2)` in the case of the Pareto/NBD model) of starting values
for the optimization. This is useful if prior knowledge on the
parameters of the distributions are available. By default starting
values are set to 1 for all parameters. The argument `optimx.args`
provides an option to control settings for the optimization routine. It
passes a list of arguments to the optimizer. All options known from the
package `optimx` (Nash and Varadhan 2011; Nash 2014) may be used. This
option enables users to specify specific optimization algorithms, set
upper and/or lower limits or enable tracing information on the progress
of the optimization. In the case of the standard Pareto/NBD model,
`CLVTools` uses by default the optimization method `L-BFGS-G` (Byrd et
al. 1995). If the result of the optimization is infeasible, the
optimization automatically switches to the more robust but often slower
`Nelder-Mead` method (Nelder and Mead 1965). `verbose` shows additional
output.

``` r
est.pnbd <- pnbd(clv.data = clv.apparel)
#> Starting estimation...
#> Estimation finished!
est.pnbd
#> Pareto NBD Standard  Model
#> 
#> Call:
#> rmarkdown::render("/Users/bachmannpatrick/Documents/Git-Repo/CLVTools/Walkthrough.rmd", 
#>     encoding = "UTF-8")
#> 
#> Coefficients:
#>       r    alpha        s     beta  
#>  0.7051  14.4027   0.1825   2.9016  
#> KKT1: TRUE 
#> KKT2: TRUE 
#> 
#> Used Options:
#> Correlation:     FALSE
```

If we assign starting parameters and addititonal arguments for the
optimizer we use:

``` r
est.pnbd <- pnbd(clv.data = clv.apparel, 
                     start.params.model = c(r=1, alpha = 2, s = 1, beta = 2), 
                     optimx.args = list(control=list(trace=5),
                                       method="Nelder-Mead", 
                                       upper=NULL, 
                                       lower=NULL))
```

Parameter estimates may be reported by either printing the estimated
object (i.e. `est.pnbd`) directly in the console or by calling
`summary(est.pnbd)` to get a more detailed report including the
likelihood value as well as AIC and BIC. Alternatively parameters may be
directly extracted using `coef(est.pnbd)`. Also `loglik()`, `confint()`
and `vcov()` are available to directly access the Loglikelihood value,
confidence intervals for the parameters and to calculate the
Variance-Covariance Matrix for the fitted model. For the standard
Pareto/NBD model, we get 4 parameters ![r, \\alpha,
s](https://latex.codecogs.com/png.latex?r%2C%20%5Calpha%2C%20s
"r, \\alpha, s") and
![\\beta](https://latex.codecogs.com/png.latex?%5Cbeta "\\beta"). where
![r,\\alpha](https://latex.codecogs.com/png.latex?r%2C%5Calpha
"r,\\alpha") represent the shape and scale parameter of the gamma
distribution that determines the purchase rate and
![s,\\beta](https://latex.codecogs.com/png.latex?s%2C%5Cbeta "s,\\beta")
of the attrition rate across individual customers.
![r/\\alpha](https://latex.codecogs.com/png.latex?r%2F%5Calpha
"r/\\alpha") can be interpreted as the mean purchase and
![s/\\beta](https://latex.codecogs.com/png.latex?s%2F%5Cbeta "s/\\beta")
as the mean attrition rate. A significance level is provided for each
parameter estimates. In the case of the apparelTrans dataset we observe
a an average purchase rate of
![r/\\alpha=0.024](https://latex.codecogs.com/png.latex?r%2F%5Calpha%3D0.024
"r/\\alpha=0.024") transactions and an average attrition rate of
![s/\\beta=0.067](https://latex.codecogs.com/png.latex?s%2F%5Cbeta%3D0.067
"s/\\beta=0.067") per customer per week. KKT 1 and 2 indicate the
Karush-Kuhn-Tucker optimality conditions of the first and second order
(Kuhn and Tucker 1951). If those criteria are not met, the optimizer has
probably not arrived at an optimal solution. If this is the case it is
usually a good idea to rerun the estimation using alternative starting
values.

``` r
#Full detailed summary of the parameter estimates
summary(est.pnbd)
#> Pareto NBD Standard  Model 
#> 
#> Call:
#> rmarkdown::render("/Users/bachmannpatrick/Documents/Git-Repo/CLVTools/Walkthrough.rmd", 
#>     encoding = "UTF-8")
#> 
#> Fitting period:                               
#> Estimation start  2010-01-02   
#> Estimation end    2011-01-01   
#> Estimation length 52.0000 Weeks
#> 
#> Coefficients:
#>       Estimate Std. Error  z-val Pr(>|z|)    
#> r      0.70514    0.06380 11.053  < 2e-16 ***
#> alpha 14.40267    1.08727 13.247  < 2e-16 ***
#> s      0.18245    0.02629  6.941  3.9e-12 ***
#> beta   2.90160    1.24254  2.335   0.0195 *  
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Optimization info:                  
#> LL     -14966.6305
#> AIC    29941.2609 
#> BIC    29964.0476 
#> KKT 1  TRUE       
#> KKT 2  TRUE       
#> fevals 24.0000    
#> Method L-BFGS-B   
#> 
#> Used Options:                 
#> Correlation FALSE

#Extract the coefficients only
coef(est.pnbd)
#>          r      alpha          s       beta 
#>  0.7051437 14.4026736  0.1824524  2.9016049
#Alternative: oefficients(est.pnbd.obj)
```

To extract only the coefficients, we can use `coef()`. To access the
confidence intervals for all parameters `confint()` is available.

``` r
#Extract the coefficients only
coef(est.pnbd)
#>          r      alpha          s       beta 
#>  0.7051437 14.4026736  0.1824524  2.9016049
#Alternative: oefficients(est.pnbd.obj)

#Extrac the confidence intervals
confint(est.pnbd)
#>            2.5 %     97.5 %
#> r      0.5801043  0.8301830
#> alpha 12.2716723 16.5336749
#> s      0.1309304  0.2339743
#> beta   0.4662669  5.3369429
```

In order to get the Likelihood value and the corresponding
Variance-Covariance Matrix we use the follwing commands:

``` r
# LogLikelihood at maximum
logLik(est.pnbd)
#> 'log Lik.' -14966.63 (df=4)

# Variance-Covariance Matrix at maximum
vcov(est.pnbd)
#>                   r        alpha             s        beta
#> r      0.0040700279  0.059480177 -0.0001474098 -0.04135688
#> alpha  0.0594801774  1.182146362 -0.0014764094 -0.45395331
#> s     -0.0001474098 -0.001476409  0.0006910167  0.02620098
#> beta  -0.0413568796 -0.453953310  0.0262009762  1.54391117
```

## Predicting Customer Behavior

Once the model parameters are estimated, we are able to predict future
customer behavior on an individual level. To do so, we use `predict()`
on the object with the estimated parameters (i.e. `est.pnbd`). The
prediction period may be varied by specifying `prediction.end`. It is
possible to provide either an end-date or a duration using the same time
unit as specified when initializing the object (i.e `prediction.end =
"2013-12-31"` or `prediction.end = 104`). By default, the prediction is
made until the end of the dataset specified in the `clvdata()` command.
The argument `continuous.discount.factor` allows to adjust the discount
rate used to estimated the discounted expected transactions (DERT). The
default value is `0.1` (=10%). Probabilistic customer attrition model
predict in general three expected characteristics for every customer:

  - “conditional expected transactions” (CET), which is the number of
    transactions to expect form a customer during the prediction period,
  - “probability of a customer being alive” (PAlive) at the end of the
    estimation period and
  - “discounted expected residual transactions” (DERT) for every
    customer, which is the total number of transactions for the residual
    lifetime of a customer discounted to the end of the estimation
    period.

If spending information was provided when initializing the object,
`CLVTools` provides prediction for

  - predicted spending estimated by a Gamma/Gamma model (Colombo and
    Jiang 1999; Fader, Hardie, and Lee 2005a) and
  - the customer lifetime value (CLV).

If a holdout period is available additionally the true numbers of
transactions (“actual.x”) and true spendings (“actual.spendings”) during
the holdout period are reported.

To use the parameter estimates on new data (e.g., an other customer
cohort), the argument `newdata` optionally allows to provide a new
`clvdata` object.

``` r
results <- predict(est.pnbd)
#> Predicting from 2011-01-02 until (incl.) 2016-10-08 (301.00 Weeks).
print(results)
#>          Id period.first period.last period.length actual.x actual.spending
#>    1:     1   2011-01-02  2016-10-08           301        0               0
#>    2:    10   2011-01-02  2016-10-08           301        0               0
#>    3:   100   2011-01-02  2016-10-08           301        0               0
#>    4:  1000   2011-01-02  2016-10-08           301       12              12
#>    5: 10000   2011-01-02  2016-10-08           301        0               0
#>   ---                                                                      
#> 2197:   177   2011-01-02  2016-10-08           301       30              30
#> 2198:  1770   2011-01-02  2016-10-08           301       48              51
#> 2199:  1771   2011-01-02  2016-10-08           301        0               0
#> 2200:  1772   2011-01-02  2016-10-08           301       34              34
#> 2201:  1773   2011-01-02  2016-10-08           301        0               0
#>          PAlive       CET       DERT predicted.Spending predicted.CLV
#>    1: 0.4715353  2.934885 0.11770104           1.000001    0.11770111
#>    2: 0.4007203  1.031419 0.04136415           1.007831    0.04168807
#>    3: 0.4007203  1.031419 0.04136415           1.007831    0.04168807
#>    4: 0.8483062  5.279947 0.21174772           1.000001    0.21174784
#>    5: 0.4007203  1.031419 0.04136415           1.007831    0.04168807
#>   ---                                                                
#> 2197: 0.9909719 31.488592 1.26282279           1.000000    1.26282288
#> 2198: 0.9722832 30.894752 1.23900735           1.111110    1.37667367
#> 2199: 0.4007203  1.031419 0.04136415           1.007831    0.04168807
#> 2200: 0.9796300 38.279877 1.53518144           1.000000    1.53518153
#> 2201: 0.4007203  1.031419 0.04136415           1.007831    0.04168807
```

To change the duration of the prediction time, we use the
`predicton.end` argument. We can either provide a time period (15 weeks
in this example):

``` r
predict(est.pnbd, prediction.end = "2011-12-31")
```

or provide a date indication the end of the prediction period:

``` r
predict(est.pnbd, prediction.end = "2011-12-31")
```

## Model Plotting

`clvdata` objects may be plotted using the `plot()` command. Similar to
`summary()`, the output of `plot()` adapts on the current modeling step.
It provides a descriptive plot of the actual transactional data if the
model has not yet been fitted. Once the model has been estimated,
`plot()` provides an aggregated incremental tracking plot of the actual
data and the model based on the estimated parameters. The timespan for
the plot may be altered using the `prediction.end` argument by providing
either a duration or an end date. By default the plot is generated for
the entire timespan of the provided dataset specified in the `clvdata()`
command. The dashed line indicates the end of the estimation period.
Alternatively cumulative actual and expected transactions can be plotted
by setting `cumulative` to `TRUE`. The argument `transactions` disable
for plotting actual transactions (`transactions=FALSE`). For further
plotting options see the documentation

plot(x, prediction.end = NULL, cumulative = FALSE, transactions = TRUE,
label = NULL, plot = TRUE, verbose = TRUE, …)

``` r
plot(est.pnbd)
#> Plotting from 2010-01-02 until 2016-10-09.
```

<img src="man/figures/WALKTHROUGH-plot-model-1.png" width="100%" />

To plot the *cumulative* expected transactions 15 time units (15 weeks
in this example) ahead of the end of the estimation plot, we use:

``` r
plot(est.pnbd, prediction.end = 15, cumulative = TRUE)
```

Alternatively, it is possible to specify a date for the
`prediction.end`argument. Note that dates are rounded to the next full
time unit (i.e. week):

``` r
plot(est.pnbd, prediction.end = "2011-12-31", cumulative = TRUE)
```

## Covariates

`CLVTools` provides the option to include covariates into probabilistic
customer attrition models. Covariates may affect the purchase or the
attrition process, or both. It is also possible to include different
covariates for the two processes. However, support for covariates is
dependent on the model. Not all implemented models provide the option
for covariates. In general, `CLVTools` distinguishes between two types
of covariates: time-invariant and time-varying. The former include
factors that do not change over time such as customer demographics or
customer acquisition information. The latter may change over time and
include marketing activities or seasonal patterns.

Data for time-invariant covariates must contain a unique customer ID and
a single value for each covariate. It should be supplied as a
`data.frame` or `data.table`. In the example of the apparel retailer we
use demographic information (i.e. gender) as time-invariant covariate
for both, the purchase and the attrition process. Use the
`data("apparelDemographics")` command to load the time-invariant
covariates. In this example gender is already coded as a dummy variable
with `male=0` and `female=1`.

``` r
data("apparelDemographics")
apparelDemographics
#>          Id Gender
#>    1:     1      1
#>    2:    10      1
#>    3:   100      1
#>    4:  1000      0
#>    5: 10000      1
#>   ---             
#> 2197:   177      1
#> 2198:  1770      1
#> 2199:  1771      0
#> 2200:  1772      1
#> 2201:  1773      1
```

Data for time-varying contextual factors requires a time-series of
contextual factor values for every customer. I.e. if the time-varying
contextual factors are allowed to change every week, a value for every
customer for every week is required. Note that all contextual factors
are required to use the same time intervals for the time-series. In the
example of the apparel retailer we use information on direct marketing
(`DM`) and seasonal patterns (`High.Season`) as time-varying contextual
factors. Additionally, we add gender as time-invariant contextual
factors. Note that the data structure of invariant contextual factors
needs to be aligned with the structure of time-varying contextual
factors. Use `data("apparelDynCov")` command to load the time-varying
contextual factors. In this example `Cov.Date` indicates the first day
of a weekly contextual factor time-interval.

``` r
data("apparelDynCov")
```

To add the covariates to an initialized `clvdata` object the commands
`SetStaticCovariates()` and `SetDynamicCovariates()` are available. The
two commands are mutually exclusive. The argument `clv.data` specifies
the initialized object and the argument `data.cov.life` respectively
`data.cov.trans` specifies the data source for the covariates for the
attrition and the purchase process. Covariates are added separately for
the purchase and the attrition process. Therefore if a covariate should
affect both processes it has to be added in both arguments:
`data.cov.life` *and* `data.cov.trans`. The arguments `names.cov.life`
and `names.cov.trans` specify the column names of the contextual factors
for the two processes. In our example, we use the same covariates for
both processes. Accordingly, we specify the time-invariant covariate
“gender” as follows:

``` r
pnbd.static<- SetStaticCovariates(clv.data = clv.apparel, 
                                      data.cov.life = apparelDemographics, 
                                      data.cov.trans = apparelDemographics,
                                      names.cov.life = "Gender", 
                                      names.cov.trans ="Gender", 
                                      name.id = "Id")
```

To specify the time-varying contextual factors for seasonal patterns and
direct marketing, we use the following:

``` r
pnbd.dyn <- SetDynamicCovariates(clv.data = clv.apparel, 
                                     data.cov.life = apparelDynCov,
                                     data.cov.trans = apparelDynCov, 
                                     names.cov.life = c("DM", "High.Season", "Gender"), 
                                     names.cov.trans = c("DM","High.Season", "Gender"), 
                                     name.id = "Id",
                                     name.date = "Cov.Date")
```

In order to include time-invariant covariates in a time-varying model,
they may be recoded as a time-varying covariate with a constant value in
every time period.

Once the covariates are added to the model the estimation process is
almost identical to the standard model without contextual factors. The
only difference is that the provided object now data for contains either
time-invariant or time-varying covariates and the option to define start
parameters for the covariates of both processes using the arguments
`start.params.life` and `start.params.trans`. If not set, the staring
values are set to 1. To define starting parameters for the covariates,
the name of the corresponding factor has to be used. For example in the
case of time-invariant covariates:

``` r
est.pnbd.static <- pnbd(pnbd.static, 
                         start.params.model = c(r=1, alpha = 2, s = 1, beta = 2),
                         start.params.life = c(Gender=0.6),
                         start.params.trans = c(Gender=0.6))
#> Starting estimation...
#> Estimation finished!
```

To inspect the estimated model we use `summary()`, however all other
commands such as `print()`, `coef()`, `loglike()`, `confint()` and
`vcov()` are also available. Now, output contains also parameters for
the covariates for both processes. Since covariates are added separately
for the purchase and the attrition process, there are also separate
model parameters for the two processes. These parameters are directly
interpretable as rate elasticity of the corresponding factors: A 1%
change in a contextual factor
![\\bf{X}^{P}](https://latex.codecogs.com/png.latex?%5Cbf%7BX%7D%5E%7BP%7D
"\\bf{X}^{P}") or
![\\bf{X}^{L}](https://latex.codecogs.com/png.latex?%5Cbf%7BX%7D%5E%7BL%7D
"\\bf{X}^{L}") changes the purchase or the attrition rate by
![\\gamma\_{purch}\\bf{X}^{P}](https://latex.codecogs.com/png.latex?%5Cgamma_%7Bpurch%7D%5Cbf%7BX%7D%5E%7BP%7D
"\\gamma_{purch}\\bf{X}^{P}") or
![\\gamma\_{life}\\bf{X}^{L}](https://latex.codecogs.com/png.latex?%5Cgamma_%7Blife%7D%5Cbf%7BX%7D%5E%7BL%7D
"\\gamma_{life}\\bf{X}^{L}") percent, respectively (Gupta 1991). In the
example of the apparel retailer, we observe that female customer
significantly purchase more (trans.Gender=0.5524). However, they also
churn significantly sooner (life.Gender=0.9343). Note: female customers
are coded as 1, male customers as 0.

``` r
summary(est.pnbd.static)
#> Pareto NBD with Static Covariates  Model 
#> 
#> Call:
#> rmarkdown::render("/Users/bachmannpatrick/Documents/Git-Repo/CLVTools/Walkthrough.rmd", 
#>     encoding = "UTF-8")
#> 
#> Fitting period:                               
#> Estimation start  2010-01-02   
#> Estimation end    2011-01-01   
#> Estimation length 52.0000 Weeks
#> 
#> Coefficients:
#>              Estimate Std. Error  z-val Pr(>|z|)    
#> r             0.72076    0.06794 10.608  < 2e-16 ***
#> alpha        22.41031    2.19411 10.214  < 2e-16 ***
#> s             0.19470    0.02952  6.596 4.23e-11 ***
#> beta          7.17662    4.66631  1.538   0.1241    
#> life.Gender   0.93426    0.46955  1.990   0.0466 *  
#> trans.Gender  0.55240    0.09095  6.074 1.25e-09 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Optimization info:                  
#> LL     -14947.8395
#> AIC    29907.6789 
#> BIC    29941.8589 
#> KKT 1  TRUE       
#> KKT 2  TRUE       
#> fevals 38.0000    
#> Method L-BFGS-B   
#> 
#> Used Options:                     
#> Correlation     FALSE
#> Regularization  FALSE
#> Constraint covs FALSE
```

To predict future customer behavior we use `predict()`. Time-varying
covariates have to be available for the entire prediction prediction
period. If the data initially provided in the `SetDynamicCovariates()`
command does not cover the complete prediction period, the argument
`new.data` offers the possibility to supply new data for the
time-varying covariates in the from of a `clvdata` object.

## Add Correlation to the model

To relax the assumption of independence between the purchase and the
attrition process, `CLVTools` provides the option to specify the
argument `use.cor` in the command t fit the model (i.e. `pnbd`). In case
of `use.cor=TRUE`, a Sarmanov approach is used to correlate the two
processes. `start.param.cor` allows to optionally specify a starting
value for the correlation parameter.

``` r
est.pnbd.cor <- pnbd(pnbd.static, 
                     use.cor= TRUE)
summary(est.pnbd.cor)
```

The parameter `Cor(life,trans)` is added to the parameter estimates that
may be directly interpreted as a correlation. In the example of the
apparel retailer the correlation parameter is not significant and the
correlation is very close to zero, indicating that the purchase and the
attrition process are independent.

## Advanced Options for Contextual Factors

`CLVTools` provides two additional estimation options for models
containing covariates (time-invariant or time-varying): regularization
and constraints for the parameters of the covariates. Both options are
included in the command to fit the model (i.e., `pnbd()`. Support for
this option is dependent on the model. They may be used simultaneously.

  - The argument `reg.lambdas` provides the possibility to specify
    separate `\lambda_{reg}` for the two processes (i.e. `reg.lambdas =
    c(trans=100, life=100)`. The larger the `\lambda_{reg}` the stronger
    the effects of the regularization. Regularization only affects the
    parameters of the covariates.
  - The argument `names.cov.constr` implements equality constraints for
    contextual factors with regards to the two processes. For example
    the variable “gender” is forced to have the same effect on the
    purchase as well as on the attrition process. To do so, the option
    `names.cov.constr` is available (i.e.
    `names.cov.constr=c("Gender")`). To provide starting parameters for
    the constrained variable use `start.params.constr`.

To enable regularization for the covariates, we use the following
command:

``` r
est.pnbd.reg <- pnbd(pnbd.static, 
                         start.params.model = c(r=1, alpha = 2, s = 1, beta = 2),
                         reg.lambdas = c(trans=100, life=100))
#> Starting estimation...
#> Estimation finished!
summary(est.pnbd.reg)
#> Pareto NBD with Static Covariates  Model 
#> 
#> Call:
#> rmarkdown::render("/Users/bachmannpatrick/Documents/Git-Repo/CLVTools/Walkthrough.rmd", 
#>     encoding = "UTF-8")
#> 
#> Fitting period:                               
#> Estimation start  2010-01-02   
#> Estimation end    2011-01-01   
#> Estimation length 52.0000 Weeks
#> 
#> Coefficients:
#>                Estimate Std. Error z-val Pr(>|z|)
#> r             6.921e-01  2.886e+00 0.240    0.810
#> alpha         1.426e+01  5.000e+01 0.285    0.776
#> s             1.920e-01  1.333e+00 0.144    0.885
#> beta          3.466e+00  6.880e+01 0.050    0.960
#> life.Gender  -5.116e-06  7.071e-02 0.000    1.000
#> trans.Gender  1.616e-04  7.070e-02 0.002    0.998
#> 
#> Optimization info:               
#> LL     -6.8000 
#> AIC    25.5999 
#> BIC    59.7799 
#> KKT 1  TRUE    
#> KKT 2  TRUE    
#> fevals 45.0000 
#> Method L-BFGS-B
#> 
#> Used Options:                        
#> Correlation     FALSE   
#> Regularization  TRUE    
#>    lambda.life  100.0000
#>    lambda.trans 100.0000
#> Constraint covs FALSE
```

To constrain “Gender” to have the same effect on both processes we use
the following command. Note, that the output now only contains one
parameter for “Gender” as it is constrained to be the same for both
processes.

``` r
est.pnbd.constr <- pnbd(pnbd.static, 
                         start.params.model = c(r=1, alpha = 2, s = 1, beta = 2),
                         start.params.constr = c(Gender=0.6),
                         names.cov.constr=c("Gender"))
#> Starting estimation...
#> Estimation finished!
summary(est.pnbd.constr)
#> Pareto NBD with Static Covariates  Model 
#> 
#> Call:
#> rmarkdown::render("/Users/bachmannpatrick/Documents/Git-Repo/CLVTools/Walkthrough.rmd", 
#>     encoding = "UTF-8")
#> 
#> Fitting period:                               
#> Estimation start  2010-01-02   
#> Estimation end    2011-01-01   
#> Estimation length 52.0000 Weeks
#> 
#> Coefficients:
#>               Estimate Std. Error  z-val Pr(>|z|)    
#> r              0.73449    0.06838 10.741  < 2e-16 ***
#> alpha         22.07199    2.14019 10.313  < 2e-16 ***
#> s              0.19123    0.02808  6.810 9.75e-12 ***
#> beta           4.68942    2.08858  2.245   0.0248 *  
#> constr.Gender  0.51999    0.08352  6.226 4.78e-10 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Optimization info:                  
#> LL     -14948.2632
#> AIC    29906.5264 
#> BIC    29935.0097 
#> KKT 1  TRUE       
#> KKT 2  TRUE       
#> fevals 28.0000    
#> Method L-BFGS-B   
#> 
#> Used Options:                           
#> Correlation          FALSE 
#> Regularization       FALSE 
#> Constraint covs      TRUE  
#>    Constraint params Gender
```

### Literature

<div id="refs" class="references">

<div id="ref-byrd1995limited">

Byrd, Richard H, Peihuang Lu, Jorge Nocedal, and Ciyou Zhu. 1995. “A
Limited Memory Algorithm for Bound Constrained Optimization.” *SIAM
Journal on Scientific Computing* 16 (5). SIAM: 1190–1208.

</div>

<div id="ref-Colombo1999">

Colombo, Richard, and Weina Jiang. 1999. “A stochastic RFM model.”
*Journal of Interactive Marketing* 13 (3): 2–12.

</div>

<div id="ref-data.table">

Dowle, Matt, and Arun Srinivasan. 2019. *Data.table: Extension of
‘Data.frame‘*. <https://CRAN.R-project.org/package=data.table>.

</div>

<div id="ref-Fader2005c">

Fader, Peter S., Bruce G.S. Hardie, and KL Lee. 2005a. “"Counting Your
Customers" the Easy Way: An Alternative to the Pareto/NBD Model.”
*Marketing Science* 24 (2): 275–84.

</div>

<div id="ref-Fader2005b">

———. 2005b. “RFM and CLV: Using Iso-Value Curves for Customer Base
Analysis.” *Journal of Marketing Research* 42 (4): 415–30.

</div>

<div id="ref-lubridate">

Grolemund, Garrett, and Hadley Wickham. 2011. “Dates and Times Made Easy
with lubridate.” *Journal of Statistical Software* 40 (3): 1–25.
<http://www.jstatsoft.org/v40/i03/>.

</div>

<div id="ref-Gupta1991">

Gupta, Sunil. 1991. “Stochastic Models of Interpurchase Time with
Time-Dependent Covariates.” *Journal of Marketing Research* 28 (1):
1–15.

</div>

<div id="ref-KKT">

Kuhn, H. W., and A. W. Tucker. 1951. “Nonlinear Programming.” In *Second
Berkeley Symposium on Mathematical Statistics and Probability*, edited
by J. Neyman, 481–92.

</div>

<div id="ref-optimx2">

Nash, John C. 2014. “On Best Practice Optimization Methods in R.”
*Journal of Statistical Software* 60 (2): 1–14.
<http://www.jstatsoft.org/v60/i02/>.

</div>

<div id="ref-optimx1">

Nash, John C., and Ravi Varadhan. 2011. “Unifying Optimization
Algorithms to Aid Software System Users: optimx for R.” *Journal of
Statistical Software* 43 (9): 1–14. <http://www.jstatsoft.org/v43/i09/>.

</div>

<div id="ref-nelder1965simplex">

Nelder, John A, and Roger Mead. 1965. “A Simplex Method for Function
Minimization.” *The Computer Journal* 7 (4). Oxford University Press:
308–13.

</div>

<div id="ref-Schmittlein1987">

Schmittlein, David C., Donald G. Morrison, and Richard Colombo. 1987.
“Counting Your Customers: Who-Are They and What Will They Do Next?”
*Management Science* 33 (1): 1–24.

</div>

<div id="ref-devtools">

Wickham, Hadley, Jim Hester, and Winston Chang. 2019. *Devtools: Tools
to Make Developing R Packages Easier*.
<https://CRAN.R-project.org/package=devtools>.

</div>

</div>
