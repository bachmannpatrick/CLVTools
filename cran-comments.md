# Comment from the authors
This is version 0.7 of the CLVTools package. We add general support for probabilistic models that predict mean spending per transaction and add the Gamma-Gamma model as a first. Two bugs were fixed in the log-likelihood function of the Pareto/NBD model and 1 bug when adding covariates to transaction data. Existing methods were refactored using Rcpp to decrease runtime.
Further, documentation and walkthrough were improved in many places.



## Test environments

## Testthat
A total of 16'363 tests provide coverage of roughly 92 percent (covr) and ensure that all functionalities work correctly for all models in all settings
* ubuntu 16.04, R 4.0.2 (on github-actions)
* Winbuilder devel, release, and old-release
* Windows Server 2008 R2 SP1, R-devel, 32/64 bit (rhub check_for_cran)
* Debian Linux, R-devel, GCC  (rhub check_for_cran)
* Ubuntu Linux 16.04 LTS, R-release, GCC (rhub check_for_cran)
* Fedora Linux, R-devel, clang, gfortran (rhub check_for_cran)

## R CMD check results
0 errors | 0 warnings | 1-2 notes
* Possibly mis-spelled words
* sub-directories of 1Mb or more (varying from 4Mb to 12Mb, we are linking against RcppArmadillo and RcppGSL)
