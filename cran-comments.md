# Comment from the authors
This is version 0.8.1 of the CLVTools package. It fixes an import issue with Rcpp after package lubridate does no longer use Rcpp. This caused the package to fail tests on CRAN and threatens to remove it from CRAN in the coming days. The changes in this version address this.



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
