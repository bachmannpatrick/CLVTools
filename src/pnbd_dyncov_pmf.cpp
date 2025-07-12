#include "pnbd_dyncov_pmf.h"
#include <RcppArmadillo.h>
#include <gsl/gsl_sf_hyperg.h>
#include <gsl/gsl_errno.h>
#include <gsl/gsl_sf_result.h>
#include <cmath>
#include <numeric>
#include <algorithm>

// Constructor for DynamicCovariates class
// Initializes the covariate data and pre-computes cumulative sums 
DynamicCovariates::DynamicCovariates(const arma::vec& input_data) : data(input_data) {
  if (!input_data.empty()) {
    // Pre-compute cumulative sums for faster lookups
    cumsum_data = arma::cumsum(input_data);
  }
}

// Access individual covariate effect value at specified index
double DynamicCovariates::at(arma::uword i) const {
  if (i >= data.n_elem) {
    Rcpp::stop("DynamicCovariates::at(): index out of bounds.");
  }
  return data(i);
}

// Calculate sum of covariate effects over a specified range of indices
double DynamicCovariates::sum_from_to(arma::uword from, arma::uword to) const {
  if (from > to) {
    return 0.0;
  }
  
  if (to >= data.n_elem) {
    Rcpp::stop("DynamicCovariates::sum_from_to(): 'to' index out of bounds.");
  }

  // Efficient O(1) calculation using the pre-computed cumulative sum vector
  if (from == 0) {
    // If the range starts at the beginning, the sum is simply the cumulative sum at 'to'
    return cumsum_data(to);
  } else {
    // Otherwise, subtract the cumulative sum of the part before the start of the range
    return cumsum_data(to) - cumsum_data(from - 1);
  }
}

// Efficiently calculate sum of covariate effects from index 0 to index i
double DynamicCovariates::sum_until(arma::uword i) const {
  if (data.empty() || i >= data.n_elem) {
    // Special handling for empty data
    if(data.empty()) {
      return 0.0;
    }
    Rcpp::stop("DynamicCovariates::sum_until(): index out of bounds or empty data.");
  }
  // Direct access to pre-computed cumulative sum
  return cumsum_data(i);
}

// Get the number of elements/periods in the covariate data
arma::uword DynamicCovariates::n_elem() const {
  return data.n_elem;
}

//' @name pnbd_dyncov_pmf_hyp2f1_C
//' @title GSL Hypergeometric 2F1 wrapper for dynamic covariates
//' @description Calculates the hypergeometric function 2F1(a,b,c,z) with error checking
//' 
//' @param a First parameter of hypergeometric function
//' @param b Second parameter of hypergeometric function
//' @param c Third parameter of hypergeometric function
//' @param z Argument of hypergeometric function
//'
//' @details 
//' This function wraps the GSL implementation of the hypergeometric function with
//' additional error handling for parameter validation, convergence issues, and
//' special cases. It plays a critical role in calculating the probability
//' mass function for the Pareto/NBD model with dynamic covariates.
//'
//' @return Value of the hypergeometric function or NaN if calculation fails
//' 
//' @keywords internal
double pnbd_dyncov_pmf_hyp2f1_C(double a, double b, double c, double z) {
  // Parameter validation
  if (!std::isfinite(a) || !std::isfinite(b) || !std::isfinite(c) || !std::isfinite(z)) {
    Rcpp::warning("Non-finite parameters in hypergeometric function: a=%f, b=%f, c=%f, z=%f", a, b, c, z);
    return std::numeric_limits<double>::quiet_NaN();
  }
  
  // Mathematical constraint check
  if (c <= 0 && std::floor(c) == c) {
    Rcpp::warning("Hypergeometric function undefined: c=%f is zero or negative integer", c);
    return std::numeric_limits<double>::quiet_NaN();
  }
  
  // Convergence warning
  if (std::fabs(z) >= 1.0 && std::real(a + b - c) > 0) {
    Rcpp::warning("Hypergeometric function may not converge: |z|=%f >= 1 and Re(a+b-c)=%f > 0", std::fabs(z), a + b - c);
  }
  
  gsl_sf_result res;
  gsl_error_handler_t * old_handler = gsl_set_error_handler_off();
  int status = gsl_sf_hyperg_2F1_e(a, b, c, z, &res);
  gsl_set_error_handler(old_handler);

  // Error handling
  if (status != GSL_SUCCESS) {
    Rcpp::warning("GSL hypergeometric 2F1 failed with status %d for a=%f, b=%f, c=%f, z=%f", status, a, b, c, z);
    return std::numeric_limits<double>::quiet_NaN();
  }
  
  // Check for non-finite result
  if (!std::isfinite(res.val)) {
    Rcpp::warning("GSL hypergeometric 2F1 returned non-finite result: %f", res.val);
    return std::numeric_limits<double>::quiet_NaN();
  }
  
  return res.val;
}

//' @name pnbd_dyncov_pmf_A_i_C
//' @title Get transaction covariate effect for a specific time period
//' @description Retrieves the transaction covariate effect for a specific period index
//' 
//' @param i_1based One-based index of the period to retrieve
//' @param dt_data_period_customer_trans Dynamic covariates object containing transaction effects
//'
//' @return Covariate effect value for the specified period
//' 
//' @keywords internal
double pnbd_dyncov_pmf_A_i_C(arma::uword i_1based, const DynamicCovariates& dt_data_period_customer_trans) {
  if (i_1based == 0 || i_1based > dt_data_period_customer_trans.n_elem()) {
    Rcpp::stop("pnbd_dyncov_pmf_A_i_C: i_1based out of bounds.");
  }
  return dt_data_period_customer_trans.at(i_1based - 1);
}

//' @name pnbd_dyncov_pmf_C_i_C
//' @title Get lifetime covariate effect for a specific time period
//' @description Retrieves the lifetime/dropout covariate effect for a specific period index
//' 
//' @param i_1based One-based index of the period to retrieve
//' @param dt_data_period_customer_life Dynamic covariates object containing lifetime effects
//'
//' @return Covariate effect value for the specified period
//' 
//' @keywords internal
double pnbd_dyncov_pmf_C_i_C(arma::uword i_1based, const DynamicCovariates& dt_data_period_customer_life) {
  if (i_1based == 0 || i_1based > dt_data_period_customer_life.n_elem()) {
    Rcpp::stop("pnbd_dyncov_pmf_C_i_C: i_1based out of bounds.");
  }
  return dt_data_period_customer_life.at(i_1based - 1);
}

//' @name pnbd_dyncov_pmf_Bbar_i_C
//' @title Calculate adjusted cumulative transaction covariate effect
//' @description Computes Bbar_i, the adjusted cumulative transaction covariate effect
//'
//' @param i_1based One-based index of the period to calculate up to
//' @param dt_data_period_customer_trans Dynamic covariates object containing transaction effects
//' @param d1 Fraction of time between first transaction and next covariate date
//' @param ui Time between customer's first transaction and start of estimation period
//'
//' @return Adjusted cumulative transaction covariate effect
//'
//' @keywords internal
double pnbd_dyncov_pmf_Bbar_i_C(arma::uword i_1based, const DynamicCovariates& dt_data_period_customer_trans, double d1, double ui) {
    if (i_1based == 0) {
        return 0.0;
    }
    if (i_1based > dt_data_period_customer_trans.n_elem()) {
        Rcpp::stop("pnbd_dyncov_pmf_Bbar_i_C: i_1based out of bounds.");
    }

    if (i_1based == 1) {
        return 0.0;
    }
    
    arma::vec Bbar_terms = dt_data_period_customer_trans.data.head(i_1based);
    
    Bbar_terms(0) *= d1;

    Bbar_terms(i_1based - 1) *= (-ui - d1 - (static_cast<double>(i_1based) - 2.0));

    return arma::sum(Bbar_terms);
}

//' @name pnbd_dyncov_pmf_Dbar_i_C
//' @title Calculate adjusted cumulative dropout covariate effect
//' @description Computes Dbar_i, the adjusted cumulative dropout covariate effect
//' 
//' @param i_1based_period One-based index of the period to calculate
//' @param dt_data_period_customer_life Dynamic covariates object for lifetime in period
//' @param dt_data_since_alive_customer_life Dynamic covariates object for lifetime since alive
//' @param d_omega Fraction of time from period start until next time unit
//' @param k0u_Dbar Number of time units between customer's first transaction and start of period
//'
//' @return Adjusted cumulative dropout covariate effect
//' 
//' @keywords internal
double pnbd_dyncov_pmf_Dbar_i_C(arma::uword i_1based_period,
                                const DynamicCovariates& dt_data_period_customer_life, 
                                const DynamicCovariates& dt_data_since_alive_customer_life,
                                double d_omega, double k0u_Dbar) {
  // Parameter validation
  if (d_omega >= 1.0) {
    d_omega = 0.99;
    Rcpp::warning("d_omega is >= 1 in pnbd_dyncov_pmf_Dbar_i_C. Capping to 0.99.");
  }
  if (d_omega < 0.0) {
    d_omega = 0.0;
    Rcpp::warning("d_omega is < 0 in pnbd_dyncov_pmf_Dbar_i_C. Setting to 0.");
  }

  // Calculate how many covariate periods we need
  arma::uword num_elements_needed = static_cast<arma::uword>(i_1based_period + k0u_Dbar - 1);
  
  // Edge cases
  if (num_elements_needed == 0 || dt_data_since_alive_customer_life.n_elem() == 0) {
    return 0.0;
  }
  
  // Handle case where we need more periods than available
  if (num_elements_needed > dt_data_since_alive_customer_life.n_elem()) {
    Rcpp::warning("Requested %u elements but only %u available in dt_data_since_alive_customer_life. Using available data.", 
                  num_elements_needed, dt_data_since_alive_customer_life.n_elem());
    num_elements_needed = dt_data_since_alive_customer_life.n_elem();
  }
  
  // Extract the needed dropout covariate effects
  arma::vec Dbar_terms(num_elements_needed);
  for (arma::uword i = 0; i < num_elements_needed; ++i) {
    Dbar_terms(i) = dt_data_since_alive_customer_life.at(i);
  }
  
  // Adjust first period by d_omega
  if (num_elements_needed > 0) {
    Dbar_terms(0) *= d_omega;
  }
  
  // Apply special correction to the last period
  if (num_elements_needed > 0) {
    double k0u_plus_i = k0u_Dbar + static_cast<double>(i_1based_period);
    
    if (k0u_plus_i >= 3.0) {
      Dbar_terms(num_elements_needed - 1) *= (-d_omega - (k0u_plus_i - 3.0));
    } else {
      Dbar_terms(num_elements_needed - 1) *= (-d_omega);
    }
  }
  
  // Sum all terms to get Dbar_i
  return arma::sum(Dbar_terms);
}

//' @name pnbd_dyncov_pmf_bu_i_C
//' @title Calculate time boundary for period i
//' @description Computes the time boundary bu_i for the specified period
//' 
//' @param ui Time between customer's first transaction and start of estimation period
//' @param i_1based One-based index of the period
//' @param d1 Fraction of time between first transaction and next covariate date
//'
//' @return Time boundary value
//' 
//' @keywords internal
double pnbd_dyncov_pmf_bu_i_C(double ui, arma::uword i_1based, double d1) {
  if (i_1based < 1) {
    Rcpp::stop("i_1based may not be < 1 in pnbd_dyncov_pmf_bu_i_C");
  }
  return ui + d1 + static_cast<double>(i_1based) - 2.0;
}

//' @name factorial_C
//' @title Compute factorial using gamma function
//' @description Calculates factorial using the gamma function for numerical stability
//' 
//' @param n Integer value for which to calculate factorial
//'
//' @return The factorial of n
//' 
//' @keywords internal
double factorial_C(int n) {
  if (n < 0) Rcpp::stop("Factorial of negative number requested.");
  return std::tgamma(n + 1.0);
}

//' @name pnbd_dyncov_pmf_S1_per_customer_C
//' @title Calculate S1 component of PMF
//' @description Computes the S1 component of the Pareto/NBD PMF with dynamic covariates
//' 
//' @param dt_data_period_customer_trans Dynamic covariates for transaction process
//' @param dt_data_period_customer_life Dynamic covariates for lifetime process
//' @param dt_data_since_alive_customer_life Dynamic covariates for lifetime since first transaction
//' @param x_double Number of transactions
//' @param alpha_r Scale parameter for transaction rate distribution
//' @param beta_s Scale parameter for dropout rate distribution
//' @param r Shape parameter for transaction rate distribution
//' @param s Shape parameter for dropout rate distribution
//' @param t_r Time span for prediction
//' @param ui Time between customer's first transaction and start of estimation period
//' @param d1 Fraction of time between first transaction and next covariate date
//' @param d_omega Fraction of time from period start until next time unit
//' @param k0u_S1 Number of time units between customer's first transaction and period start
//'
//' @details 
//' S1 represents the probability that a customer makes exactly x transactions in the 
//' prediction period conditional on being alive throughout the entire period.
//'
//' @return S1 component value
//' 
//' @keywords internal
double pnbd_dyncov_pmf_S1_per_customer_C(
    const DynamicCovariates& dt_data_period_customer_trans,
    const DynamicCovariates& dt_data_period_customer_life,
    const DynamicCovariates& dt_data_since_alive_customer_life,
    double x_double, double alpha_r, double beta_s, double r, double s, double t_r,
    double ui, double d1, double d_omega, double k0u_S1) {

  // No periods in prediction window means zero probability
  if (dt_data_period_customer_trans.n_elem() == 0 || dt_data_period_customer_life.n_elem() == 0) {
    return 0.0;
  }

  // Get final period index
  arma::uword i_kutu_1based = dt_data_period_customer_trans.n_elem();
  if (i_kutu_1based == 0) return 0.0;

  // Get covariate effects for the final period
  double A_kutu = pnbd_dyncov_pmf_A_i_C(i_kutu_1based, dt_data_period_customer_trans);
  double C_kutu = pnbd_dyncov_pmf_C_i_C(i_kutu_1based, dt_data_period_customer_life);

  // Calculate time-adjusted covariate effects
  double Bbar_kutu = pnbd_dyncov_pmf_Bbar_i_C(i_kutu_1based, dt_data_period_customer_trans, d1, ui);
  double B_kutu = A_kutu * (t_r + ui) + Bbar_kutu;

  double Dbar_kutu = pnbd_dyncov_pmf_Dbar_i_C(i_kutu_1based, dt_data_period_customer_life, dt_data_since_alive_customer_life, d_omega, k0u_S1);
  double D_kutu = C_kutu * (t_r + ui) + Dbar_kutu;

  int x = static_cast<int>(x_double);

  // Numerical stability check
  if (B_kutu + alpha_r <= 0 || D_kutu + beta_s <= 0) {
    return 0.0;
  }

  // Calculate the three terms for the S1 component
  double term1 = std::pow(B_kutu, x_double) / factorial_C(x);
  double term2 = std::tgamma(x_double + r) / std::tgamma(r);
  double term3 = std::pow(alpha_r, r) * std::pow(beta_s, s) / 
                 std::pow(B_kutu + alpha_r, x_double + r) / 
                 std::pow(D_kutu + beta_s, s);

  // Final S1 component
  return term1 * term2 * term3;
}

//' @name pnbd_dyncov_pmf_S2_1j_per_customer_C
//' @title Calculate S2_1j component of PMF
//' @description Computes the S2_1j component of the Pareto/NBD PMF with dynamic covariates
//' 
//' @param dt_data_period_customer_trans Dynamic covariates for transaction process
//' @param dt_data_period_customer_life Dynamic covariates for lifetime process
//' @param dt_data_since_alive_customer_life Dynamic covariates for lifetime since first transaction
//' @param j_S2_1j Number of transactions at time of churn
//' @param x_double Total number of transactions
//' @param alpha_r Scale parameter for transaction rate distribution
//' @param beta_s Scale parameter for dropout rate distribution
//' @param r_param Shape parameter for transaction rate distribution
//' @param s_param Shape parameter for dropout rate distribution
//' @param ui Time between customer's first transaction and start of estimation period
//' @param d1 Fraction of time between first transaction and next covariate date
//' @param d_omega Fraction of time from period start until next time unit
//' @param k0u_S2_1j Number of time units between customer's first transaction and period start
//'
//' @details 
//' S2_1j represents the probability that a customer churns during the first period
//' with exactly j transactions at the time of churn, and makes a total of x transactions.
//'
//' @return S2_1j component value
//' 
//' @keywords internal
double pnbd_dyncov_pmf_S2_1j_per_customer_C(
    const DynamicCovariates& dt_data_period_customer_trans,
    const DynamicCovariates& dt_data_period_customer_life,
    const DynamicCovariates& dt_data_since_alive_customer_life,
    arma::uword j_S2_1j, double x_double, double alpha_r, double beta_s, double r_param, double s_param,
    double ui, double d1, double d_omega, double k0u_S2_1j) {

  int x = static_cast<int>(x_double);
  
  // No periods in prediction window means zero probability
  if (dt_data_period_customer_trans.n_elem() == 0 || dt_data_period_customer_life.n_elem() == 0) return 0.0;

  // Get covariate effects for the first period
  double A1 = pnbd_dyncov_pmf_A_i_C(1, dt_data_period_customer_trans);
  double C1 = pnbd_dyncov_pmf_C_i_C(1, dt_data_period_customer_life);

  // Validate covariate effects
  if (!std::isfinite(A1) || !std::isfinite(C1) || C1 <= 0) {
    Rcpp::warning("Invalid A1=%f or C1=%f in S2_1j calculation", A1, C1);
    return 0.0;
  }

  // Calculate adjusted cumulative effects for period 1
  double Bbar_1 = pnbd_dyncov_pmf_Bbar_i_C(1, dt_data_period_customer_trans, d1, ui);
  double Dbar_1 = pnbd_dyncov_pmf_Dbar_i_C(1, dt_data_period_customer_life, dt_data_since_alive_customer_life, d_omega, k0u_S2_1j);

  // Validate the adjusted values
  if (!std::isfinite(Bbar_1) || !std::isfinite(Dbar_1)) {
    Rcpp::warning("Invalid Bbar_1=%f or Dbar_1=%f in S2_1j calculation", Bbar_1, Dbar_1);
    return 0.0;
  }

  // Calculate the time boundary for period 2
  double bu2 = pnbd_dyncov_pmf_bu_i_C(ui, 2, d1);

  // Will accumulate the probability over all possible patterns
  double sum_val = 0.0;

  // Loop through all possible numbers of transactions n
  for (int n_val = 0; n_val <= (x - static_cast<int>(j_S2_1j)); ++n_val) {
    double term_n;
    
    // Split calculation based on relative magnitudes of effects
    if ((Bbar_1 + alpha_r) > (Dbar_1 + beta_s) * A1 / C1) {
      // CASE 1: Transaction process dominates dropout process
      auto fct_greater = [&](double u_part, int n_inner) {
        // Prevent division by zero
        if ( (Bbar_1 + A1 * u_part + alpha_r) == 0 ) {
          return 0.0;
        }

        double num = std::pow(u_part, n_inner) * std::pow(alpha_r, r_param) * std::pow(beta_s, s_param);
        double den = std::pow(Bbar_1 + A1 * u_part + alpha_r, r_param + s_param + static_cast<double>(j_S2_1j) + n_inner);
        if (den == 0) return 0.0;

        // Calculate argument for hypergeometric function
        // This argument must be in (-1,1) for the series to converge
        double h_arg_val = (Bbar_1 + alpha_r - (Dbar_1 + beta_s) * (A1 / C1)) / (Bbar_1 + A1 * u_part + alpha_r);
        if (std::fabs(h_arg_val) > 1.0) {
          return 0.0; // Return 0 when hypergeometric function would not converge
        }
        
        return (num / den) *
          // Heterogeneity adjustment factor from gamma mixing distributions
          // This reflects how customer-level variation in rates affects probabilities
          (std::tgamma(r_param + s_param + static_cast<double>(j_S2_1j) + n_inner) / (std::tgamma(r_param) * std::tgamma(s_param))) *
          
          // Covariate effect term with power adjustments based on transaction counts
          // This captures how the transaction and dropout covariates interact
          (std::pow(A1, s_param - x_double + static_cast<double>(j_S2_1j) + n_inner) / std::pow(C1, s_param + 1.0)) *
          
          // Ratio of gamma functions from the original PNBD derivation
          // This accounts for the probability distribution of transaction counts
          ((std::tgamma(s_param + 1.0) * std::tgamma(r_param + x_double)) / (std::tgamma(r_param + s_param + x_double + 1.0))) *
          
          // Hypergeometric function call - the result of analytically solving
          // the integration over heterogeneous transaction and dropout processes
          // Parameters are carefully chosen to match the mathematical derivation
          // while ensuring numerical stability
          pnbd_dyncov_pmf_hyp2f1_C(
              s_param + r_param + static_cast<double>(j_S2_1j) + n_inner, // a parameter 
              s_param + 1.0,                                             // b parameter
              r_param + s_param + x_double + 1.0,                        // c parameter
              h_arg_val);                                                // z argument
      };
      
      // Calculate probability by evaluating the difference between
      // the integration function at two time boundaries
      // This effectively computes the definite integral from ui to bu2,
      // representing all possible churn times within period 1
      term_n = (1.0 / factorial_C(n_val)) * (fct_greater(ui, n_val) - fct_greater(bu2, n_val));

    } else {
      // CASE 2: Dropout process dominates transaction process
      // This case occurs when the weighted dropout rate exceeds the weighted transaction rate
      // It requires a different mathematical formulation for numerical stability
      auto fct_smaller = [&](double u_part, int n_inner) {
        // Prevent division by zero in the denominator expression
        // The term ((Dbar_1 + C1 * u_part + beta_s) * A1 / C1) represents the
        // scaled dropout rate effect adjusted by transaction rate
        if ( ((Dbar_1 + C1 * u_part + beta_s) * A1 / C1) == 0 ) return 0.0;
        
        // Calculate numerator and denominator with the same base components as Case 1
        // but with a different structure to handle the dropout-dominated regime
        double num = std::pow(u_part, n_inner) * std::pow(alpha_r, r_param) * std::pow(beta_s, s_param);
        double den = std::pow((Dbar_1 + C1 * u_part + beta_s) * A1 / C1, r_param + s_param + static_cast<double>(j_S2_1j) + n_inner);
        if (den == 0) return 0.0;

        // Different hypergeometric function argument for this case
        // The argument is calculated differently to maintain numerical stability
        // when dropout rates dominate transaction rates
        double h_arg_val = (((Dbar_1 + beta_s) * A1 / C1) - (Bbar_1 + alpha_r)) / ((Dbar_1 + C1 * u_part + beta_s) * A1 / C1);
        
        // Convergence check for hypergeometric function
        if (std::fabs(h_arg_val) > 1.0) {
          return 0.0;
        }

        // Similar structure to fct_greater, but with different hypergeometric parameters
        // and denominator formulation to handle the dropout-dominated regime
        return (num / den) *
          // Heterogeneity adjustment factor - same mathematical meaning as in Case 1
          (std::tgamma(r_param + s_param + static_cast<double>(j_S2_1j) + n_inner) / (std::tgamma(r_param) * std::tgamma(s_param))) *
          
          // Covariate effect term - same as Case 1
          (std::pow(A1, s_param - x_double + static_cast<double>(j_S2_1j) + n_inner) / std::pow(C1, s_param + 1.0)) *
          
          // Gamma function ratio for transaction probability - same as Case 1
          ((std::tgamma(s_param + 1.0) * std::tgamma(r_param + x_double)) / (std::tgamma(r_param + s_param + x_double + 1.0))) *
          
          // Hypergeometric function call with DIFFERENT PARAMETERS than Case 1
          // Note that the b parameter is now (r_param + x_double) rather than (s_param + 1.0)
          // This critical difference handles the different convergence properties
          // when dropout effects dominate transaction effects
          pnbd_dyncov_pmf_hyp2f1_C(
              s_param + r_param + static_cast<double>(j_S2_1j) + n_inner, // a parameter
              r_param + x_double,                                         // b parameter - DIFFERENT FROM CASE 1
              r_param + s_param + x_double + 1.0,                         // c parameter
              h_arg_val);                                                 // z argument
      };
      
      // Calculate probability by integrating between time boundaries
      term_n = (1.0 / factorial_C(n_val)) * (fct_smaller(ui, n_val) - fct_smaller(bu2, n_val));
    }
    
    // Add this term to the running sum of all possible post-churn transaction patterns
    sum_val += term_n;
  }

  return (std::pow(Bbar_1, static_cast<double>(j_S2_1j)) * std::pow(A1, x_double - static_cast<double>(j_S2_1j)) * C1 / factorial_C(static_cast<int>(j_S2_1j))) * sum_val;
}


//' @name pnbd_dyncov_pmf_S2_ij_per_customer_C
//' @title Calculate S2_ij component of PMF
//' @description Computes the S2_ij component of the Pareto/NBD PMF with dynamic covariates
//' 
//' @param dt_data_period_customer_trans Dynamic covariates for transaction process
//' @param dt_data_period_customer_life Dynamic covariates for lifetime process
//' @param dt_data_since_alive_customer_life Dynamic covariates for lifetime since first transaction
//' @param i_S2_1based Period index in which customer churns
//' @param j_S2 Number of transactions at time of churn
//' @param x_double Total number of transactions
//' @param alpha_r Scale parameter for transaction rate distribution
//' @param beta_s Scale parameter for dropout rate distribution
//' @param r_param Shape parameter for transaction rate distribution
//' @param s_param Shape parameter for dropout rate distribution
//' @param ui Time between customer's first transaction and start of estimation period
//' @param d1 Fraction of time between first transaction and next covariate date
//' @param d_omega Fraction of time from period start until next time unit
//' @param k0u_S2_ij Number of time units between customer's first transaction and period start
//'
//' @details 
//' S2_ij represents the probability that a customer churns during period i
//' with exactly j transactions at the time of churn, and makes a total of x transactions.
//'
//' @return S2_ij component value
//' 
//' @keywords internal
double pnbd_dyncov_pmf_S2_ij_per_customer_C(
    const DynamicCovariates& dt_data_period_customer_trans,
    const DynamicCovariates& dt_data_period_customer_life,
    const DynamicCovariates& dt_data_since_alive_customer_life,
    arma::uword i_S2_1based, arma::uword j_S2, double x_double, double alpha_r, double beta_s, double r_param, double s_param,
    double ui, double d1, double d_omega, double k0u_S2_ij) {

  int x = static_cast<int>(x_double);
  if (i_S2_1based == 0 || i_S2_1based > dt_data_period_customer_trans.n_elem()) {
    Rcpp::stop("pnbd_dyncov_pmf_S2_ij_per_customer_C: i_S2_1based out of bounds.");
  }
  if (dt_data_period_customer_trans.n_elem() == 0 || dt_data_period_customer_life.n_elem() == 0) return 0.0;


  double Ai = pnbd_dyncov_pmf_A_i_C(i_S2_1based, dt_data_period_customer_trans);
  double Ci = pnbd_dyncov_pmf_C_i_C(i_S2_1based, dt_data_period_customer_life);

  double Bbar_i = pnbd_dyncov_pmf_Bbar_i_C(i_S2_1based, dt_data_period_customer_trans, d1, ui);
  double Dbar_i = pnbd_dyncov_pmf_Dbar_i_C(i_S2_1based, dt_data_period_customer_life, dt_data_since_alive_customer_life, d_omega, k0u_S2_ij);

  double bu_i = pnbd_dyncov_pmf_bu_i_C(ui, i_S2_1based, d1);

  if (Ai <= 0 || Ci <= 0 || Bbar_i + alpha_r <= 0 || Dbar_i + beta_s <= 0) {
    return 0.0;
  }

  double sum_val = 0.0;

  for (int n_val = 0; n_val <= (x - static_cast<int>(j_S2)); ++n_val) {
    double term_n;
    if (Bbar_i + alpha_r > (Dbar_i + beta_s) * Ai / Ci) {
      auto fct_greater = [&](double u_part, int n_inner) {
        if((Bbar_i + Ai * u_part + alpha_r) == 0) return 0.0;
        double num = std::pow(u_part, n_inner) * std::pow(alpha_r, r_param) * std::pow(beta_s, s_param);
        double den = std::pow(Bbar_i + Ai * u_part + alpha_r, r_param + s_param + static_cast<double>(j_S2) + n_inner);
        if (den == 0) return 0.0;

        double h_arg_val = (Bbar_i + alpha_r - (Dbar_i + beta_s) * Ai / Ci) / (Bbar_i + Ai * u_part + alpha_r);
        if (std::fabs(h_arg_val) > 1.0) {
          return 0.0;
        }

        return (num / den) *
          (std::tgamma(r_param + s_param + static_cast<double>(j_S2) + n_inner) / (std::tgamma(r_param) * std::tgamma(s_param))) *
          (std::pow(Ai, s_param - x_double + static_cast<double>(j_S2) + n_inner) / std::pow(Ci, s_param + 1.0)) *
          ((std::tgamma(s_param + 1.0) * std::tgamma(r_param + x_double)) / (std::tgamma(r_param + s_param + x_double + 1.0))) *
          pnbd_dyncov_pmf_hyp2f1_C(s_param + r_param + static_cast<double>(j_S2) + n_inner, s_param + 1.0, r_param + s_param + x_double + 1.0, h_arg_val);
      };
      term_n = (1.0 / factorial_C(n_val)) * (fct_greater(ui, n_val) - fct_greater(bu_i, n_val));
    } else {
      auto fct_smaller = [&](double u_part, int n_inner) {
        if ( ((Dbar_i + Ci * u_part + beta_s) * Ai / Ci) == 0 ) return 0.0;
        double num = std::pow(u_part, n_inner) * std::pow(alpha_r, r_param) * std::pow(beta_s, s_param);
        double den = std::pow((Dbar_i + Ci * u_part + beta_s) * Ai / Ci, r_param + s_param + static_cast<double>(j_S2) + n_inner);
        if (den == 0) return 0.0;

        double h_arg_val = (((Dbar_i + beta_s) * Ai / Ci) - (Bbar_i + alpha_r)) / ((Dbar_i + Ci * u_part + beta_s) * Ai / Ci);
        if (std::fabs(h_arg_val) > 1.0) {
          return 0.0;
        }

        return (num / den) *
          (std::tgamma(r_param + s_param + static_cast<double>(j_S2) + n_inner) / (std::tgamma(r_param) * std::tgamma(s_param))) *
          (std::pow(Ai, s_param - x_double + static_cast<double>(j_S2) + n_inner) / std::pow(Ci, s_param + 1.0)) *
          ((std::tgamma(s_param + 1.0) * std::tgamma(r_param + x_double)) / (std::tgamma(r_param + s_param + x_double + 1.0))) *
          pnbd_dyncov_pmf_hyp2f1_C(s_param + r_param + static_cast<double>(j_S2) + n_inner, r_param + x_double, r_param + s_param + x_double + 1.0, h_arg_val);
      };
      term_n = (1.0 / factorial_C(n_val)) * (fct_smaller(ui, n_val) - fct_smaller(bu_i, n_val));
    }
    
    sum_val += term_n;
  }

  return (std::pow(Bbar_i, static_cast<double>(j_S2)) * std::pow(Ai, x_double - static_cast<double>(j_S2)) * Ci / factorial_C(static_cast<int>(j_S2))) * sum_val;
}

//' @name pnbd_dyncov_pmf_S2_kutuj_per_customer_C
//' @title Calculate S2_kutuj component of PMF
//' @description Computes the S2_kutuj component of the Pareto/NBD PMF with dynamic covariates
//' 
//' @param dt_data_period_customer_trans Dynamic covariates for transaction process
//' @param dt_data_period_customer_life Dynamic covariates for lifetime process
//' @param dt_data_since_alive_customer_life Dynamic covariates for lifetime since first transaction
//' @param j_S2_kutu Number of transactions at time of churn
//' @param x_double Total number of transactions
//' @param r_param Shape parameter for transaction rate distribution
//' @param alpha_r Scale parameter for transaction rate distribution
//' @param s_param Shape parameter for dropout rate distribution
//' @param beta_s Scale parameter for dropout rate distribution
//' @param t_r Time span for prediction
//' @param ui Time between customer's first transaction and start of estimation period
//' @param d1 Fraction of time between first transaction and next covariate date
//' @param d_omega Fraction of time from period start until next time unit
//' @param k0u_S2_kutu Number of time units between customer's first transaction and period start
//'
//' @details 
//' S2_kutuj represents the probability that a customer churns during the last period
//' with exactly j transactions at the time of churn, and makes a total of x transactions.
//'
//' @return S2_kutuj component value
//' 
//' @keywords internal
double pnbd_dyncov_pmf_S2_kutuj_per_customer_C(
    const DynamicCovariates& dt_data_period_customer_trans,
    const DynamicCovariates& dt_data_period_customer_life,
    const DynamicCovariates& dt_data_since_alive_customer_life,
    arma::uword j_S2_kutu, double x_double, double r_param, double alpha_r, double s_param, double beta_s, double t_r,
    double ui, double d1, double d_omega, double k0u_S2_kutu) {

  int x = static_cast<int>(x_double);
  arma::uword i_kutu_1based = dt_data_period_customer_trans.n_elem();
  if (i_kutu_1based == 0) {
    return 0.0;
  }

  // Get covariate effects for the last period
  double Ai = pnbd_dyncov_pmf_A_i_C(i_kutu_1based, dt_data_period_customer_trans);
  double Ci = pnbd_dyncov_pmf_C_i_C(i_kutu_1based, dt_data_period_customer_life);

  // Calculate adjusted cumulative effects
  double Bbar_kutu = pnbd_dyncov_pmf_Bbar_i_C(i_kutu_1based, dt_data_period_customer_trans, d1, ui);
  double B_kutu = Ai * (t_r + ui) + Bbar_kutu;

  double Dbar_kutu = pnbd_dyncov_pmf_Dbar_i_C(i_kutu_1based, dt_data_period_customer_life, dt_data_since_alive_customer_life, d_omega, k0u_S2_kutu);
  double D_kutu = Ci * (t_r + ui) + Dbar_kutu;

  // Calculate time boundary
  double bu_kutu = pnbd_dyncov_pmf_bu_i_C(ui, i_kutu_1based, d1);

  double sum_val = 0.0;

  // Calculate probability for all possible transaction patterns
  for (int n_val = 0; n_val <= (x - static_cast<int>(j_S2_kutu)); ++n_val) {
    double term_n;
    if (B_kutu + alpha_r > (D_kutu + beta_s) * Ai / Ci) {
      auto fct_greater = [&](double u_part, int n_inner) {
        if ( (B_kutu + Ai * u_part + alpha_r) == 0 ) return 0.0;
        double num = std::pow(u_part, n_inner) * std::pow(alpha_r, r_param) * std::pow(beta_s, s_param);
        double den = std::pow(B_kutu + Ai * u_part + alpha_r, r_param + s_param + static_cast<double>(j_S2_kutu) + n_inner);
        if (den == 0) return 0.0;

        double h_arg_val = (B_kutu + alpha_r - (D_kutu + beta_s) * Ai / Ci) / (B_kutu + Ai * u_part + alpha_r);
        if (std::fabs(h_arg_val) > 1.0) {
          return 0.0;
        }

        return (num / den) *
          (std::tgamma(r_param + s_param + static_cast<double>(j_S2_kutu) + n_inner) / (std::tgamma(r_param) * std::tgamma(s_param))) *
          (std::pow(Ai, s_param - x_double + static_cast<double>(j_S2_kutu) + n_inner) / std::pow(Ci, s_param + 1.0)) *
          ((std::tgamma(s_param + 1.0) * std::tgamma(r_param + x_double)) / (std::tgamma(r_param + s_param + x_double + 1.0))) *
          pnbd_dyncov_pmf_hyp2f1_C(s_param + r_param + static_cast<double>(j_S2_kutu) + n_inner, s_param + 1.0, r_param + s_param + x_double + 1.0, h_arg_val);
      };
      term_n = (1.0 / factorial_C(n_val)) * (fct_greater(t_r + ui, n_val) - fct_greater(bu_kutu, n_val));
    } else {
      auto fct_smaller = [&](double u_part, int n_inner) {
        if ( ((D_kutu + Ci * u_part + beta_s) * Ai / Ci) == 0 ) return 0.0;
        double num = std::pow(u_part, n_inner) * std::pow(alpha_r, r_param) * std::pow(beta_s, s_param);
        double den = std::pow((D_kutu + Ci * u_part + beta_s) * Ai / Ci, r_param + s_param + static_cast<double>(j_S2_kutu) + n_inner);
        if (den == 0) return 0.0;

        double h_arg_val = (((D_kutu + beta_s) * Ai / Ci) - (B_kutu + alpha_r)) / ((D_kutu + Ci * u_part + beta_s) * Ai / Ci);
        if (std::fabs(h_arg_val) > 1.0) {
          return 0.0;
        }

        return (num / den) *
          (std::tgamma(r_param + s_param + static_cast<double>(j_S2_kutu) + n_inner) / (std::tgamma(r_param) * std::tgamma(s_param))) *
          (std::pow(Ai, s_param - x_double + static_cast<double>(j_S2_kutu) + n_inner) / std::pow(Ci, s_param + 1.0)) *
          ((std::tgamma(s_param + 1.0) * std::tgamma(r_param + x_double)) / (std::tgamma(r_param + s_param + x_double + 1.0))) *
          pnbd_dyncov_pmf_hyp2f1_C(s_param + r_param + static_cast<double>(j_S2_kutu) + n_inner, r_param + x_double, r_param + s_param + x_double + 1.0, h_arg_val);
      };
      term_n = (1.0 / factorial_C(n_val)) * (fct_smaller(t_r + ui, n_val) - fct_smaller(bu_kutu, n_val));
    }
    sum_val += term_n;
  }

  return (std::pow(B_kutu, static_cast<double>(j_S2_kutu)) * std::pow(Ai, x_double - static_cast<double>(j_S2_kutu)) * Ci / factorial_C(static_cast<int>(j_S2_kutu))) * sum_val;
}

//' @name pnbd_dyncov_pmf_per_customer
//' @title PNBD Dynamic Covariates PMF Per Customer
//' @description Calculate the probability mass function (PMF) for Pareto/NBD model with dynamic covariates for a single customer
//' 
//' @param cov_period_life_exp Pre-computed exp(γ*X) for lifetime process in the period (u, tu]; sorted by date
//' @param cov_period_trans_exp Pre-computed exp(β*X) for transaction process in the period (u, tu]; sorted by date
//' @param cov_sincealive_life_exp Pre-computed exp(γ*X) for lifetime process from customer's first transaction; sorted by date
//' @param r_param Shape parameter r for the transaction rate gamma distribution
//' @param alpha_r_param Scale parameter α for the transaction rate gamma distribution
//' @param s_param Shape parameter s for the dropout rate gamma distribution  
//' @param beta_s_param Scale parameter β for the dropout rate gamma distribution
//' @param x_double Number of transactions to calculate PMF for
//' @param t_r_param Time from start of period (u) to end of period (tu)
//' @param d1_param Fraction of time between first transaction and next covariate date
//' @param d_omega_param Fraction of time from u to next time unit
//' @param k0u_param Number of time units between customer's first transaction and beginning of period
//' @param ui_param Time between customer's first transaction and start of estimation period u
//'
//' @details
//' The dynamic covariate Pareto/NBD model extends the standard model by allowing the transaction and 
//' dropout rates to vary over time according to time-varying covariates:
//' 
//' λ(t) = λ * exp(β*X(t)) for transaction rate
//' μ(t) = μ * exp(γ*X(t)) for dropout rate
//' 
//' Where:
//' - λ and μ are the base rates (linked to parameters r, α, s, β)
//' - β and γ are coefficient vectors for the covariates
//' - X(t) are time-varying covariates at time t
//' 
//' Formula structure follows PMF = S1 + S2, where:
//' - S1 represents probability for a customer who has not churned in period (u, t+u] and makes exactly x transactions
//' - S2 represents probability for a customer who has churned during period (u, t+u] and makes exactly x transactions
//'   (this is further decomposed into sums over all possible churn times and transaction patterns)
//'
//' @return The probability mass function value for the specified number of transactions
//'
//' @keywords internal
// [[Rcpp::export]]
double pnbd_dyncov_pmf_per_customer(
    const arma::vec& cov_period_life_exp,
    const arma::vec& cov_period_trans_exp,
    const arma::vec& cov_sincealive_life_exp,
    const arma::vec& cov_sincealive_trans_exp,
    double r_param,
    double alpha_r_param,
    double s_param,
    double beta_s_param,
    double x_double,
    double t_r_param,
    double d1_param,
    double d_omega_param,
    double k0u_param,
    double ui_param
) {
  bool all_covs_one = true;
  
  // Check if all lifetime covariates have no effect (all exp(γ*X) ≈ 1.0)
  if (!cov_period_life_exp.empty()) {
    for (arma::uword i = 0; i < cov_period_life_exp.n_elem; i++) {
      // Use floating-point comparison with small epsilon to handle numerical precision
      if (std::fabs(cov_period_life_exp(i) - 1.0) > 1e-10) {
        all_covs_one = false;
        break;  // Early termination for efficiency
      }
    }
  }
  
  // If lifetime covariates have no effect, also check transaction covariates
  if (all_covs_one && !cov_period_trans_exp.empty()) {
    for (arma::uword i = 0; i < cov_period_trans_exp.n_elem; i++) {
      if (std::fabs(cov_period_trans_exp(i) - 1.0) > 1e-10) {
        all_covs_one = false;
        break;  // Early termination for efficiency
      }
    }
  }
  
  // γ=0 and β=0 case: Use the standard Pareto/NBD model instead of the dynamic covariate version
  if (all_covs_one) {
    // Copy parameters to clearly named variables for the standard model
    double x = x_double;      // Number of transactions to calculate PMF for
    double r = r_param;       // Shape parameter for transaction rate distribution
    double s = s_param;       // Shape parameter for dropout rate distribution
    double alpha = alpha_r_param; // Scale parameter for transaction rate distribution
    double beta = beta_s_param;   // Scale parameter for dropout rate distribution
    double t = t_r_param;     // Time span for PMF calculation
    
    // The standard PMF function expects vector inputs for compatibility with vectorized operations,
    // so we create single-element vectors wrapped around our scalar parameters
    arma::vec vT_i(1, arma::fill::value(t));         // Time vector (single element)
    arma::vec vAlpha_i(1, arma::fill::value(alpha)); // Alpha vector (single element)
    arma::vec vBeta_i(1, arma::fill::value(beta));   // Beta vector (single element)
    
    // Call the standard Pareto/NBD PMF function from pnbd.cpp
    // This function is heavily optimized and thoroughly tested
    arma::vec result = pnbd_PMF(r, s, static_cast<unsigned int>(x), vT_i, vAlpha_i, vBeta_i);

    // Return the single scalar result
    return result(0);
  }
  
  // d1 validation - this parameter represents the fraction of time between 
  // a customer's first transaction and the next covariate date. By definition,
  // it must be between 0 and 1 (exclusive). Values outside this range would
  // cause mathematical issues in the Bbar calculations and violate model assumptions.
  double validated_d1 = d1_param;
  if (validated_d1 >= 1.0) {
    // Cap at 0.99 instead of 1.0 to avoid potential division by zero
    // and numerical issues in critical formulas. This is a common approach
    // in the original R implementation and safer than throwing an error.
    validated_d1 = 0.99;
    Rcpp::warning("d1 is >= 1 for some entries. This might be unexpected.");
  }
  if (validated_d1 < 0.0) {
    // Negative d1 would invalidate the time representation in the model
    // Setting to 0 maintains mathematical validity
    validated_d1 = 0.0;
    Rcpp::warning("d1 is < 0 for some entries. This might be unexpected.");
  }

  // d_omega validation - represents the fraction of time from period start u
  // until the next full time unit. Like d1, this must be between 0 and 1.
  // This parameter affects the calculation of time-based covariates particularly
  // in Dbar calculations for the dropout process.
  double validated_d_omega = d_omega_param;
  if (validated_d_omega >= 1.0) {
    // Cap at 0.99 for the same numerical safety reasons as d1
    validated_d_omega = 0.99;
    Rcpp::warning("d_omega is >= 1 for some entries. Capping to 0.99.");
  }
  if (validated_d_omega < 0.0) {
    // Negative values are nonsensical in this time representation
    validated_d_omega = 0.0;
    Rcpp::warning("d_omega is < 0 for some entries. Resetting to 0.");
  }

  DynamicCovariates dc_period_life(cov_period_life_exp);      // exp(γ*X) for lifetime process
  DynamicCovariates dc_period_trans(cov_period_trans_exp);    // exp(β*X) for transaction process
  DynamicCovariates dc_sincealive_life(cov_sincealive_life_exp); // exp(γ*X) from first transaction

  double S1 = pnbd_dyncov_pmf_S1_per_customer_C(
    dc_period_trans, dc_period_life, dc_sincealive_life,
    x_double, alpha_r_param, beta_s_param, r_param, s_param, t_r_param,
    ui_param, validated_d1, validated_d_omega, k0u_param);

  double S2_sums = 0.0;
  
  arma::uword i_kutu_1based = dc_period_trans.n_elem();
  
  int x_int = static_cast<int>(x_double);

  if (i_kutu_1based > 0 && x_int > 0) {
    // Loop through all possible numbers of transactions j that could occur
    // at the time of churn, where j ∈ {1,2,...,x}
    // The mathematical model requires summing over all possible combinations
    // where j transactions occur before churn and (x-j) occur during alive periods
    for (int j_val = 1; j_val <= x_int; ++j_val) {
      arma::uword j_uword = static_cast<arma::uword>(j_val);
      
      // Track partial S2 results for the three different cases:
      // S2_1j: Probability of churn in first period with j transactions at churn
      // S2_kutu_j: Probability of churn in last period with j transactions at churn
      // S2_ij_sum_val: Probability of churn in any middle period with j transactions at churn
      double S2_1j = 0.0;
      double S2_kutu_j = 0.0;
      double S2_ij_sum_val = 0.0;

      // S2_1j - Calculate probability customer churns during the first period (i=1)
      // within (u,tu) given they made j transactions when they churned
      S2_1j = pnbd_dyncov_pmf_S2_1j_per_customer_C(
        dc_period_trans, dc_period_life, dc_sincealive_life,
        j_uword, x_double, alpha_r_param, beta_s_param, r_param, s_param,
        ui_param, validated_d1, validated_d_omega, k0u_param);

      // S2_kutuj - Calculate probability customer churns during the last period
      // (i=i_kutu_1based within u,tu) given they made j transactions at churned
      if (i_kutu_1based == 1) {
        // Special case: when there's only one period in (u,tu), first and last periods are the same
        // The customer can only churn in that single period, simplifying calculation
        S2_kutu_j = pnbd_dyncov_pmf_S2_kutuj_per_customer_C(
          dc_period_trans, dc_period_life, dc_sincealive_life,
          j_uword, x_double, r_param, alpha_r_param, s_param, beta_s_param, t_r_param,
          ui_param, validated_d1, validated_d_omega, k0u_param);
      } else { // i_kutu_1based > 1
        // General case: multi-period prediction window
        // Calculates probability of churn in final period with j transactions at churn time
        S2_kutu_j = pnbd_dyncov_pmf_S2_kutuj_per_customer_C(
          dc_period_trans, dc_period_life, dc_sincealive_life,
          j_uword, x_double, r_param, alpha_r_param, s_param, beta_s_param, t_r_param,
          ui_param, validated_d1, validated_d_omega, k0u_param);
      }


      // S2_ij_sum - Calculate probability for all "middle" periods from i=2 to i_kutu-1
      // This represents the sum of probabilities that the customer churns in any middle period
      // while making j transactions at the churn time.
      if (i_kutu_1based >= 3) {
        // Loop through all "middle" periods (from 2 to i_kutu-1)
        for (arma::uword i_val_1based = 2; i_val_1based <= (i_kutu_1based - 1); ++i_val_1based) {
          // This check appears redundant due to the loop bounds, but it protects against
          // potential logic errors and follows the R implementation for consistency
          if (i_val_1based < 2) {
            continue;
          }
          
          // Calculate probability of churning in period i_val_1based with j transactions at churn
          // and sum it into the accumulator for all middle periods
          S2_ij_sum_val += pnbd_dyncov_pmf_S2_ij_per_customer_C(
            dc_period_trans, dc_period_life, dc_sincealive_life,
            i_val_1based, j_uword, x_double, alpha_r_param, beta_s_param, r_param, s_param,
            ui_param, validated_d1, validated_d_omega, k0u_param);
        }
      }

      // Sum up all S2 components for this j-value
      // This combines the probabilities of churning in first, middle, and last periods
      // with exactly j transactions at churn time, contributing to the total PMF
      S2_sums += (S2_1j + S2_kutu_j + S2_ij_sum_val);
    }
  } else {
    S2_sums = 0.0;
  }
  
  return S1 + S2_sums;
}
