# Comment from the authors
This is version 0.8 of the CLVTools package. We drastically reduce runtime for the extended Pareto/NBD model by refactoring part of the log-likelihood in Rcpp. A bug was fixed which would not allow to use the optimization methods "nlm" and "nlminb" in optimx for our methods
Further, documentation was improved.



## Test environments

## Testthat
A total of 16363 tests provide coverage of roughly 91.5 percent (covr) and ensure that all functionalities work correctly for all models in all settings
* ubuntu 16.04, R 4.0.4 (on github-actions)
* Winbuilder devel, release, and old-release
* Windows Server 2008 R2 SP1, R-devel, 32/64 bit (rhub check_for_cran)
* Debian Linux, R-devel, GCC  (rhub check_for_cran)
* Ubuntu Linux 16.04 LTS, R-release, GCC (rhub check_for_cran)
* Fedora Linux, R-devel, clang, gfortran (rhub check_for_cran)

## R CMD check results
0 errors | 0 warnings | 1-2 notes
* Possibly mis-spelled words
* sub-directories of 1Mb or more (varying from 4Mb to 12Mb, we are linking against RcppArmadillo and RcppGSL)
