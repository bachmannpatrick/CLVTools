# Comment from the authors
This is version 0.11.2 of the CLVTools package. 
The most relevant changes in this version are: 

* `newcustomer.spending()`: Predict average spending per transaction for customers without order history
* Improved optimizer defaults


 
# Test environments

## Testthat
Tests provide coverage of roughly 91.5 percent (covr) and ensure that all functionalities work correctly for all models in all settings
* Winbuilder devel, release, and old-release 
* macbuilder release

## R CMD check results
0 errors | 0 warnings | 1 note
* sub-directories of 1Mb or more (varying from 4Mb to 16.6Mb, we are linking against RcppArmadillo and the GNU GSL)
