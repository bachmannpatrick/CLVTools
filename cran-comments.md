# Comment from the authors
This is version 0.11.0 of the CLVTools package. It contains a wide range of changes compared to the previous version, the most relevant being: 

* Bootstrapping: Add facilities to estimate parameter uncertainty for all models
* Simplify the formula interfaces `latentAttrition()` and `spending()`
* Implement erratum and new expressions derived by fellow researchers
* Reduced fitting times for all models by using compressed model data as input to the likelihood method
* Ability to predict future transactions of customers with no existing transaction history
* Improved numeric stability of various methods
* lrtest(): Likelihood ratio testing for latent attrition models
* Estimating the Pareto/NBD with time-varying covariates with process correlation was not possible

# Test environments

## Testthat
Tests provide coverage of roughly 91.5 percent (covr) and ensure that all functionalities work correctly for all models in all settings
* Winbuilder devel, release, and old-release 
* macbuilder release

## R CMD check results
0 errors | 0 warnings | 1 note
* sub-directories of 1Mb or more (varying from 4Mb to 16.6Mb, we are linking against RcppArmadillo and the GNU GSL)
