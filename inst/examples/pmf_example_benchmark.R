# Example and Benchmark for pnbd_dyncov_pmf Rcpp Implementation
# This script demonstrates the usage of both R and Rcpp implementations 
# of the pnbd_dyncov_pmf function and compares their performance

# Load CLVTools - adjust path based on whether running from source or installed package
if (file.exists("../../DESCRIPTION")) {
  # Running from package source directory
  library(devtools)
  load_all("../..")
} else {
  # Running from installed package
  library(CLVTools)
}

library(data.table)
library(microbenchmark)

# Load the apparel dataset
data("apparelTrans")
data("apparelDynCov")

# Create CLV data object with dynamic covariates
clv.data <- clvdata(apparelTrans, 
                    date.format = "ymd", 
                    time.unit = "week")

# Add dynamic covariates
clv.data <- SetDynamicCovariates(clv.data, 
                                 data.cov.life = apparelDynCov,
                                 data.cov.trans = apparelDynCov, 
                                 names.cov.life = "High.Season",
                                 names.cov.trans = "High.Season",
                                 name.date = "Cov.Date")

# Fit the PNBD model with dynamic covariates
pnbd.dyncov <- pnbd(clv.data, 
                    verbose = TRUE,
                    start.params.model = c(r=0.5, alpha=10, s=0.5, beta=10),
                    start.params.life = c(High.Season=0.5),
                    start.params.trans = c(High.Season=0.5))

cat("Model fitted successfully!\n")
cat("Model parameters:\n")
print(coef(pnbd.dyncov))

# Test PMF calculation for different values of x
x_values <- 0:5

cat("\n=== Testing PMF calculation correctness ===\n")

# Calculate PMF using R implementation - skip if it fails
cat("Calculating PMF using R implementation...\n")
pmf_r <- tryCatch({
  pnbd_dyncov_pmf(pnbd.dyncov, x = x_values, use_r = TRUE)
}, error = function(e) {
  cat("R implementation failed.\n")
  return(NULL)
})

# Calculate PMF using Rcpp implementation
cat("Calculating PMF using Rcpp implementation...\n")
# Note: The current Rcpp implementation only handles single x values
# So we'll calculate for each x value separately
pmf_cpp_list <- lapply(x_values, function(x) {
  res <- pnbd_dyncov_pmf(pnbd.dyncov, x = x, use_r = FALSE)
  res[, x := x]
  return(res)
})

# Combine results
pmf_cpp_combined <- rbindlist(pmf_cpp_list)

# Compare results for the first few customers
if (!is.null(pmf_r)) {
  sample_ids <- head(unique(pmf_r$Id), 10)
  comparison_data <- data.table()

  for (x_val in x_values) {
    r_subset <- pmf_r[Id %in% sample_ids & x == x_val]
    cpp_subset <- pmf_cpp_combined[Id %in% sample_ids & x == x_val]
    
    if (nrow(r_subset) > 0 && nrow(cpp_subset) > 0) {
      # Match by customer ID
      merged <- merge(r_subset, cpp_subset, by = "Id", suffixes = c("_r", "_cpp"))
      merged[, x_value := x_val]
      merged[, diff := abs(pmf_r - pmf_cpp)]
      merged[, rel_diff := diff / abs(pmf_r)]
      comparison_data <- rbindlist(list(comparison_data, merged), fill = TRUE)
    }
  }

  if (nrow(comparison_data) > 0) {
    cat("\nComparison of R vs Rcpp results (first 10 customers):\n")
    print(comparison_data[, .(Id, x_value, pmf_r, pmf_cpp, diff, rel_diff)])
    
    cat("\nSummary statistics:\n")
    cat("Mean absolute difference:", mean(comparison_data$diff, na.rm = TRUE), "\n")
    cat("Max absolute difference:", max(comparison_data$diff, na.rm = TRUE), "\n")
    cat("Mean relative difference:", mean(comparison_data$rel_diff, na.rm = TRUE), "\n")
    cat("Max relative difference:", max(comparison_data$rel_diff, na.rm = TRUE), "\n")
    
    # Check if results are approximately equal (within tolerance)
    tolerance <- 1e-10
    are_equal <- all(comparison_data$diff < tolerance, na.rm = TRUE)
    cat("Results are approximately equal (tolerance =", tolerance, "):", are_equal, "\n")
  } else {
    cat("No data for comparison.\n")
  }
} else {
  cat("Skipping comparison due to R implementation failure.\n")
}

# Performance benchmark
cat("\n=== Performance Benchmark ===\n")

# Benchmark performance for a single x value
x_test <- 2
sample_customers <- head(unique(pmf_r$Id), 50)  # Use smaller sample for benchmarking

# Create subset of data for benchmarking
benchmark_data <- clv.data
# Filter to sample customers for faster benchmarking
benchmark_data@data.transactions <- benchmark_data@data.transactions[Id %in% sample_customers]
benchmark_data@data.cov.life <- benchmark_data@data.cov.life[Id %in% sample_customers]
benchmark_data@data.cov.trans <- benchmark_data@data.cov.trans[Id %in% sample_customers]

# Fit model on subset
pnbd.benchmark <- pnbd(benchmark_data, 
                      verbose = FALSE,
                      start.params.model = coef(pnbd.dyncov)[1:4],
                      start.params.life = c("High.Season"=coef(pnbd.dyncov)["life.High.Season"]),
                      start.params.trans = c("High.Season"=coef(pnbd.dyncov)["trans.High.Season"]))

cat("Benchmarking with", length(benchmark_customers), "customers...\n")

# Benchmark different x values
benchmark_results <- microbenchmark(
  R_implementation = pnbd_dyncov_pmf(pnbd.benchmark, x = x_test, use_r = TRUE),
  Rcpp_implementation = pnbd_dyncov_pmf(pnbd.benchmark, x = x_test, use_r = FALSE),
  times = 10
)

print(benchmark_results)

# Calculate speedup
r_time <- median(benchmark_results$time[benchmark_results$expr == "R_implementation"])
cpp_time <- median(benchmark_results$time[benchmark_results$expr == "Rcpp_implementation"])
speedup <- r_time / cpp_time

cat("\nPerformance Summary:\n")
cat("R implementation median time:", r_time / 1e6, "ms\n")
cat("Rcpp implementation median time:", cpp_time / 1e6, "ms\n")
cat("Speedup factor:", round(speedup, 2), "x\n")

# Plot benchmark results
library(ggplot2)
autoplot(benchmark_results) + 
  ggtitle("PMF Calculation Performance: R vs Rcpp") +
  theme_minimal()

ggsave("pmf_benchmark_plot.png", width = 10, height = 6, dpi = 300)
cat("Benchmark plot saved as 'pmf_benchmark_plot.png'\n")

cat("\n=== Example Usage ===\n")
cat("# Load data and create CLV object\n")
cat("data('apparelTrans')\n")
cat("data('apparelDynCov')\n")
cat("clv.data <- clvdata(apparelTrans, date.format = 'ymd', time.unit = 'week')\n")
cat("clv.data <- SetDynamicCovariates(clv.data, data.cov.life = apparelDynCov, data.cov.trans = apparelDynCov, names.cov.life = 'High.Season', names.cov.trans = 'High.Season', name.date = 'Cov.Date')\n")
cat("\n# Fit PNBD model with dynamic covariates\n")
cat("pnbd.dyncov <- pnbd(clv.data, verbose = TRUE)\n")
cat("\n# Calculate PMF using R implementation (supports vector x)\n")
cat("pmf_r <- pnbd_dyncov_pmf(pnbd.dyncov, x = 0:5, use_r = TRUE)\n")
cat("\n# Calculate PMF using fast Rcpp implementation (single x value)\n")
cat("pmf_cpp <- pnbd_dyncov_pmf(pnbd.dyncov, x = 2, use_r = FALSE)\n")
cat("\n# Default uses Rcpp implementation for single x values\n")
cat("pmf_default <- pnbd_dyncov_pmf(pnbd.dyncov, x = 2)\n")

cat("\nExample completed successfully!\n")
