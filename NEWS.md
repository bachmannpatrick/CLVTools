# CLVTools 0.12.1

### NEW FEATURES
* `newcustomer()` prediction: Include the initial transaction in the predicted number of orders



# CLVTools 0.12.0

### NEW FEATURES
* `clvdata(data.end)`: Add parameter `data.end` to specify a data end beyond the last actual transaction
* `summary()`: Always set `zval` and `pval` to NA for the main model parameters
* `hessian()`: Add method to calculate hessian matrix for already fitted models
* Add 3 new vignettes covering: Advanced modelling techniques, model intuition, and the internal class system

### BUG FIXES
* Fix CRAN notes: Replace `arma::is_finite()` -> `std::isfinite()`
* Dyncov PNBD: Rename `predicted.CLV` -> `predicted.period.CLV`
* `predict()`: Rename `{predicted, actual}.total.spending` -> `{predicted, actual}.period.spending`



# CLVTools 0.11.2

### NEW FEATURES
* `newcustomer.spending()`: Predict average spending per transaction for customers without order history
* Improved optimizer defaults (higher iteration count) for PNBD dyncov



# CLVTools 0.11.1

### NEW FEATURES
* Updated the apparel example data
* Prediction bootstrapping: Calculate confidence intervals using regular rather than "reversed-quantiles"

### BUG FIXES
* Prediction bootstrapping: Re-fit model using exact original specification
* GGomNBD: Set limit in integration method to size of workspace



# CLVTools 0.11.0

### NEW FEATURES
* More memory efficient and faster creation of repeat transactions in `clv.data`
* Use existing repeat transactions when calling `gg` with `remove.first.transaction = TRUE`
* Simplify the formula interfaces `latentAttrition()` and `spending()`
* Add `predicted.total.spending` to predictions
* Harmonize parameter names used in various S3 methods
* Bootstrapping: Add facilities to estimate parameter uncertainty for all models
* Ability to predict future transactions of customers with no existing transaction history
* New start parameters for all latent attrition models
* Pareto/NBD dyncov: Improved numeric stability of PAlive
* GGomNBD: Implement erratum by Jost Adler to predict CET correctly
* GGomNBD: Improve numerical stability and runtime of LL integral
* GGomNBD: Implement PMF as derived by Jost Adler
* lrtest(): Likelihood ratio testing for latent attrition models
* Accept `data.table::IDate` as data inputs to `clvdata`
* `summary.clv.data`:Much faster by improving the calculation of the mean inter-purchase time
* Reduced fitting times for all models by using a compressed CBS as input to the LL sum
* Faster hessian calculation if a model was using correlation

### BUG FIXES
* Estimating the Pareto/NBD dyncov with correlation was not possible
* GGomNBD: Free workspace after it is not used anymore to avoid memory-leak
* `SetDynamicCovariates`: Verify there is no covariate data for nonexistent customers



# CLVTools 0.10.0

### NEW FEATURES
* We add an interface to specify models using a formula notation (`latentAttrition()` and `spending()`)
* New method to plot customer's transaction timings (`plot.clv.data(which='timings')`)
* Draw diagnostic plots of multiple models in single plot (`plot(other.models=list(), label=c())`)
* MUCH faster fitting for the Pareto/NBD with time-varying covariates because we implemented the LL in Rcpp



# CLVTools 0.9.0

### NEW FEATURES
* Three new diagnostic plots for transaction data to analyse frequency, spending and interpurchase time
* New diagnostic plot for fitted transaction models (PMF plot)
* New function to calculate the probability mass function of selected models
* Calculate summary statistics only for the transaction data of selected customers
* Canonical transformation from data.frame/data.table to transaction data object and vice-versa
* Canonical subset for the data stored in the transaction data object
* Pareto/NBD DERT: Improved numerical stability



# CLVTools 0.8.1

### BUG FIXES
* Fix importing issue after package lubridate does no longer use Rcpp



# CLVTools 0.8.0

### NEW FEATURES
* Partially refactor the LL of the extended Pareto/NBD in Rcpp with code kindly donated by Elliot Shin Oblander
* Improved documentation 

### BUG FIXES
* Optimization methods nlm and nlminb can now be used. Thanks to Elliot Shin Oblander for reporting



# CLVTools 0.7.0

### NEW FEATURES
* Refactor the Gamma-Gamma (GG) model to predict mean spending per transaction into an independent model
* The prediction for transaction models can now be combined with separately fit spending models
* Write the unconditional expectation functions in Rcpp for faster plotting (Pareto/NBD and Beta-Geometric/NBD)
* Improved documentation and walkthrough

### BUG FIXES
* Pareto/NBD log-likelihood: For the case Tcal = t.x and for the case alpha == beta
* Static or dynamic covariates with syntactically invalid names (spaces, start with numbers, etc) could not be fit


# CLVTools 0.6.0

### NEW FEATURES
* Beta-Geometric/NBD (BG/NBD) model to predict repeat transactions without and with static covariates
* Gamma-Gompertz (GGompertz) model to predict repeat transactions without and with static covariates
* Predictions are now possible for all periods >= 0 whereas before a minimum of 2 periods was required


# CLVTools 0.5.0
* Initial release of the CLVTools package

### NEW FEATURES
* Pareto/NBD model to predict repeat transactions without and with static or dynamic covariates 
* Gamma-Gamma model to predict average spending
* Predicting CLV and future transactions per customer
* Data class to preprocess transaction data and to provide summary statistics
* Plot of expected repeat transactions as by the fitted model compared against actuals
