# Comment from the authors
This is the initial release of the CLVTools package. It provides convenient methods to estimate customer lifetime value from transaction data.

This is a re-submission after the following shortcomings were pointed out and have been addressed by us:

Overall checktime 13 min > 10 min: 
More tests were marked for skipping (before:2076 now:1367). The testing time is reduced to 85s (locally), 151s (rhub debian) and 188s (winbuilder). The overall checktime on winbuilder is reduced to 419s. We are optimistic to also stay within 10min on CRAN.

Documentation
Unfortunately, none of these were caught by our local checks.
- Missing rd tag for return value: Added for all missing methods
- Missing rd for arguments: Added for all missing methods
- Unexecutable code in man/pnbd.Rd: Example corrected
- reset of user's options: Example changed as proposed as well as marked with dontrun

DESCRIPTION
- starting phrases: The package description has been reworded
- acronyms/abbreviations: Have been spelled out
- references describing the methods: DOIs have been added


We appreciate your patience and hope that the packge now really requires <10min for checking.


## Test environments

## Testthat
A total of 1367 (local: 5809) tests that provide a coverage of roughly 94 percent (covr)
* ubuntu 16.04, R 3.6 (on github-actions)
* Windows Server 2008 R2 SP1, R-devel, 32/64 bit (rhub check_for_cran)
* Debian Linux, R-devel, GCC  (rhub check_for_cran)
* Ubuntu Linux 16.04 LTS, R-release, GCC (rhub check_for_cran)
* Fedora Linux, R-devel, clang, gfortran (rhub check_for_cran)

## R CMD check results
0 errors | 0 warnings | 1-3 notes
* This is a new release.
* Invalid URL / DOI
* Possibly mis-spelled words
