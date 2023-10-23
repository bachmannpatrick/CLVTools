# Comment from the authors
This is version 0.10.0 of the CLVTools package. It contains the following changes compared to the previous version: 
* We add an interface to specify models using a formula notation
* New method to plot customer's transaction timings
* Draw diagnostic plots of multiple models in single plot
* MUCH faster fitting for the Pareto/NBD with time-varying covariates because we implemented the LL in Rcpp


## Test environments

## Testthat
A total of 16363 tests provide coverage of roughly 91.5 percent (covr) and ensure that all functionalities work correctly for all models in all settings
* Winbuilder devel, release, and old-release 
* macbuilder release (devtools::check_mac_release())

## R CMD check results
0 errors | 0 warnings | 1 note
* sub-directories of 1Mb or more (varying from 4Mb to 12Mb, we are linking against RcppArmadillo and RcppGSL)
