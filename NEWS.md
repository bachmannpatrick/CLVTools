# CLVTools 0.7.0

### NEW FEATURES
* Refactor the Gamma-Gamma (GG) model to predict mean spending per transaction into an independent model
* The prediction for transaction models can now be combined with separately fit spending models
* Write the unconditional expectation functions in Rcpp for faster plotting (Pareto/NBD and Beta-Geometric/NBD)
* Improved documentation and walkthrough

### BUG FIXES
* Pareto/NBD LogLikelihood: For the case Tcal = t.x and for the case alpha == beta
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
* Data class to pre-process transaction data and to provide summary statistics
* Plot of expected repeat transactions as by the fitted model compared against actuals
