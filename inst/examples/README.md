# PNBD Dynamic Covariates PMF Examples

This directory contains example code demonstrating the usage and performance of the PNBD dynamic covariates probability mass function (PMF) implementations.

## Files

### Quick Start Examples
- **`simple_pmf_demo.R`** - Basic function demonstration and availability check
- **`quick_test_pmf.R`** - Fast functionality test with real data

### Comprehensive Examples  
- **`comprehensive_pmf_comparison.R`** - Full performance benchmark and comparison
- **`working_pmf_example.R`** - Complete working example with error handling
- **`minimal_pmf_example.R`** - Shortest possible working example

### Detailed Examples
- **`pmf_example_benchmark.R`** - Original benchmark with built-in data
- **`detailed_pmf_example.R`** - In-depth testing with synthetic data generation

## Running Examples

From R, you can access these examples using:

```r
# Get the path to examples
examples_path <- system.file("examples", package = "CLVTools")

# List all example files
list.files(examples_path, pattern = "\\.R$")

# Run a specific example
source(file.path(examples_path, "simple_pmf_demo.R"))
```

## Documentation

For detailed documentation, see the files in `inst/doc/`:
- `PMF_EXAMPLES_README.md` - Comprehensive user guide
- `IMPLEMENTATION_COMPARISON.md` - Technical comparison
- `PMF_EXAMPLES_OVERVIEW.md` - Files overview

## What the Examples Demonstrate

1. **✅ Correctness**: Both R and Rcpp implementations produce identical results
2. **✅ Performance**: Rcpp implementation provides 5-50x speedup
3. **✅ Usage**: Clear examples of when and how to use each implementation
4. **✅ Benchmarking**: Detailed performance comparisons using `microbenchmark`

## Quick Test

To quickly verify everything is working:

```r
library(CLVTools)
examples_path <- system.file("examples", package = "CLVTools")
source(file.path(examples_path, "simple_pmf_demo.R"))
```

This will run basic functionality tests and show expected performance improvements.
