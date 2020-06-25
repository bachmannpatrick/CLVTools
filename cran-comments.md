# Comment from the authors
This is the second resubmission of version 0.6 of the CLVTools package after testing times were still too long. 
We have some troubles explaining why testing time did not decrease at all in the previous resubmission although we have cut the numbers of tests from 3100 to 2700 (using skip_on_cran() from package testthat) and we were able to reduce testing time by about 25% locally.

We have now cut another 700 tests. Locally, we are now staying under 70 seconds, while on winbuilder i368 and x64 combined take 303s (release) and 302s (devel). This is about halve the time from the original submission.

We are therefore confident that we can stay within the allowed timeframe on CRAN.

### Novelties compared to previous version
This is version 0.6 of the CLVTools package. We add 2 additional models to predict customer's repeat transactions: The Beta-Geometric/NBD (bgnbd) and the Gamma-Gompertz/NBD (ggomnbd) model. Both can be fit without and with static covariates.
The existing PNBD model with dynamic covariates was improved to generally allow for shorter prediction periods.
Further, internal tests were improved.



## Test environments

## Testthat
A total of 2092 (local: 11'743) tests provide coverage of roughly 95 percent (covr) and ensure that all functionalities work correctly for all models in all settings
* ubuntu 16.04, R 4.0.1 (on github-actions)
* Windows Server 2008 R2 SP1, R-devel, 32/64 bit (rhub check_for_cran)
* Debian Linux, R-devel, GCC  (rhub check_for_cran)
* Ubuntu Linux 16.04 LTS, R-release, GCC (rhub check_for_cran)
* Fedora Linux, R-devel, clang, gfortran (rhub check_for_cran)

## R CMD check results
0 errors | 0 warnings | 1-2 notes
* Possibly mis-spelled words
* sub-directories of 1Mb or more (varying from 4Mb to 12Mb, we are linking against RcppArmdaillo and RcppGSL) 
