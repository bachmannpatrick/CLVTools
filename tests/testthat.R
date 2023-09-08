# On cran, all code may only use 2 threads.
# For some reason, threads=2 and threads=1 still results in "Running R code in ‘testthat.R’ had CPU time 3.2 (2.8) times elapsed time"
# Therefore set to single threaded (same as Win) and also set threads for openMP.
# As per the example in https://github.com/Rdatatable/data.table/issues/5658#issuecomment-1741934995, this is done before loading any packages
# check for cran is from testthat:::on_cran()
if(!identical(Sys.getenv("NOT_CRAN"), "true")){
  Sys.setenv("OMP_THREAD_LIMIT" = 1)
  data.table::setDTthreads(threads = 1)
}

library("testthat")
library("lubridate")
library("data.table")
library("CLVTools")


test_check("CLVTools")
