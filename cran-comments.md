# Comment from the authors
This is version 0.12.0 of the CLVTools package. 
The most relevant changes in this version are: 

* Add 3 new vignettes covering: Advanced modelling techniques, model intuition, and the internal class system
* Add method `hessian()` to calculate hessian matrix for already fitted models
* Correct significance indicators NA in `summary()`
* Add new parameter to data preparation method
* Renaming prediction output columns
* Fix CRAN notes



 
# Test environments

## Testthat
Tests provide coverage of roughly 91.5 percent (covr) and ensure that all functionalities work correctly for all models in all settings
* Winbuilder devel, release, and old-release 
* macbuilder release

## R CMD check results
0 errors | 0 warnings | 1 note
* sub-directories of 1Mb or more (varying from 4Mb to 16.6Mb, we are linking against RcppArmadillo and the GNU GSL)
