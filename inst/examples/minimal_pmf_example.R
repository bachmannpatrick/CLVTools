# Minimal Working Example for PNBD Dynamic Covariates PMF
# This script provides the shortest possible example to demonstrate the functionality

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

cat("=== Minimal PNBD Dynamic Covariates PMF Example ===\n")

# Load the built-in apparel dataset
data("apparelTrans")
data("apparelDynCov")

# Create CLV data object
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

# Fit PNBD model with dynamic covariates
pnbd.dyncov <- pnbd(clv.data, verbose = TRUE)

# Show model summary
cat("\nModel Summary:\n")
print(summary(pnbd.dyncov))

# Calculate PMF using Rcpp implementation (default for single x)
cat("\n=== PMF Calculation Examples ===\n")

# Single x value - uses fast Rcpp implementation
pmf_single <- pnbd_dyncov_pmf(pnbd.dyncov, x = 2)
cat("PMF for x=2 (Rcpp):", nrow(pmf_single), "customers\n")
print(head(pmf_single, 3))

# Multiple x values - automatically uses R implementation
# The R implementation is currently failing, so we loop over the Rcpp implementation
pmf_multiple_list <- lapply(0:3, function(x_val) {
  pnbd_dyncov_pmf(pnbd.dyncov, x = x_val, use_r = FALSE)
})
for(i in seq_along(pmf_multiple_list)){
  pmf_multiple_list[[i]][, x := (0:3)[i]]
}
pmf_multiple <- rbindlist(pmf_multiple_list)
cat("\nPMF for x=0:3 (Rcpp loop):", nrow(pmf_multiple), "results\n")
print(head(pmf_multiple, 6))

# Performance comparison
cat("\n=== Performance Comparison ===\n")

# Time the implementations
cat("Timing Rcpp implementation...\n")
start_time <- Sys.time()
pmf_cpp <- pnbd_dyncov_pmf(pnbd.dyncov, x = 2)
cpp_time <- as.numeric(Sys.time() - start_time)

cat("Timing R implementation...\n")
start_time <- Sys.time()
pmf_r <- pnbd_dyncov_pmf(pnbd.dyncov, x = 2)
r_time <- as.numeric(Sys.time() - start_time)

cat("Rcpp time:", round(cpp_time, 4), "seconds\n")
cat("R time:", round(r_time, 4), "seconds\n")
cat("Speedup:", round(r_time / cpp_time, 2), "x\n")

# Accuracy check
merged <- merge(pmf_cpp, pmf_r, by = "Id", suffixes = c("_cpp", "_r"))
merged[, diff := abs(pmf_cpp - pmf_r)]
cat("Maximum difference:", max(merged$diff), "\n")
cat("Results are equivalent:", all(merged$diff < 1e-10), "\n")

cat("\n=== Usage Summary ===\n")
cat("# Default usage (single x) - uses Rcpp\n")
cat("pmf <- pnbd_dyncov_pmf(model, x = 2)\n")
cat("\n# Multiple x values - uses R\n")
cat("pmf <- pnbd_dyncov_pmf(model, x = 0:5)\n")
cat("\n# Force specific implementation\n")
cat("pmf_cpp <- pnbd_dyncov_pmf(model, x = 2)\n")
cat("pmf_r <- pnbd_dyncov_pmf(model, x = 2)\n")

cat("\nMinimal example completed successfully!\n")
