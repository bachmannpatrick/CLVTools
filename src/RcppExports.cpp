// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <RcppGSL.h>
#include <Rcpp.h>

using namespace Rcpp;

// bgnbd_nocov_CET
arma::vec bgnbd_nocov_CET(const double alpha, const double beta, const double gamma, const double delta, const double nPeriods, const arma::vec& vX, const arma::vec& vT_x, const arma::vec& vN_cal, const arma::vec& vN_star);
RcppExport SEXP _CLVTools_bgnbd_nocov_CET(SEXP alphaSEXP, SEXP betaSEXP, SEXP gammaSEXP, SEXP deltaSEXP, SEXP nPeriodsSEXP, SEXP vXSEXP, SEXP vT_xSEXP, SEXP vN_calSEXP, SEXP vN_starSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< const double >::type beta(betaSEXP);
    Rcpp::traits::input_parameter< const double >::type gamma(gammaSEXP);
    Rcpp::traits::input_parameter< const double >::type delta(deltaSEXP);
    Rcpp::traits::input_parameter< const double >::type nPeriods(nPeriodsSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type vX(vXSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type vT_x(vT_xSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type vN_cal(vN_calSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type vN_star(vN_starSEXP);
    rcpp_result_gen = Rcpp::wrap(bgnbd_nocov_CET(alpha, beta, gamma, delta, nPeriods, vX, vT_x, vN_cal, vN_star));
    return rcpp_result_gen;
END_RCPP
}
// bgbb_nocov_DERT
arma::vec bgbb_nocov_DERT(const arma::vec& vEstimated_params, const double continuous_discount_factor, const arma::vec& vX, const arma::vec& vT_x, const arma::vec& vN_cal);
RcppExport SEXP _CLVTools_bgbb_nocov_DERT(SEXP vEstimated_paramsSEXP, SEXP continuous_discount_factorSEXP, SEXP vXSEXP, SEXP vT_xSEXP, SEXP vN_calSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type vEstimated_params(vEstimated_paramsSEXP);
    Rcpp::traits::input_parameter< const double >::type continuous_discount_factor(continuous_discount_factorSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type vX(vXSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type vT_x(vT_xSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type vN_cal(vN_calSEXP);
    rcpp_result_gen = Rcpp::wrap(bgbb_nocov_DERT(vEstimated_params, continuous_discount_factor, vX, vT_x, vN_cal));
    return rcpp_result_gen;
END_RCPP
}
// bgbb_nocov_LL_ind
arma::vec bgbb_nocov_LL_ind(const arma::vec& vLogparams, const arma::vec& vX, const arma::vec& vT_x, const arma::vec& vN_cal);
RcppExport SEXP _CLVTools_bgbb_nocov_LL_ind(SEXP vLogparamsSEXP, SEXP vXSEXP, SEXP vT_xSEXP, SEXP vN_calSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type vLogparams(vLogparamsSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type vX(vXSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type vT_x(vT_xSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type vN_cal(vN_calSEXP);
    rcpp_result_gen = Rcpp::wrap(bgbb_nocov_LL_ind(vLogparams, vX, vT_x, vN_cal));
    return rcpp_result_gen;
END_RCPP
}
// bgbb_nocov_LL_sum
double bgbb_nocov_LL_sum(const arma::vec& vLogparams, const arma::vec& vX, const arma::vec& vT_x, const arma::vec& vN_cal);
RcppExport SEXP _CLVTools_bgbb_nocov_LL_sum(SEXP vLogparamsSEXP, SEXP vXSEXP, SEXP vT_xSEXP, SEXP vN_calSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type vLogparams(vLogparamsSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type vX(vXSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type vT_x(vT_xSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type vN_cal(vN_calSEXP);
    rcpp_result_gen = Rcpp::wrap(bgbb_nocov_LL_sum(vLogparams, vX, vT_x, vN_cal));
    return rcpp_result_gen;
END_RCPP
}
// bgbb_nocov_PAlive
arma::vec bgbb_nocov_PAlive(const double alpha, const double beta, const double gamma, const double delta, const arma::vec& vX, const arma::vec& vT_x, const arma::vec& vN_cal);
RcppExport SEXP _CLVTools_bgbb_nocov_PAlive(SEXP alphaSEXP, SEXP betaSEXP, SEXP gammaSEXP, SEXP deltaSEXP, SEXP vXSEXP, SEXP vT_xSEXP, SEXP vN_calSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const double >::type alpha(alphaSEXP);
    Rcpp::traits::input_parameter< const double >::type beta(betaSEXP);
    Rcpp::traits::input_parameter< const double >::type gamma(gammaSEXP);
    Rcpp::traits::input_parameter< const double >::type delta(deltaSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type vX(vXSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type vT_x(vT_xSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type vN_cal(vN_calSEXP);
    rcpp_result_gen = Rcpp::wrap(bgbb_nocov_PAlive(alpha, beta, gamma, delta, vX, vT_x, vN_cal));
    return rcpp_result_gen;
END_RCPP
}
// vec_gsl_hyp2f0_e
Rcpp::List vec_gsl_hyp2f0_e(const RcppGSL::Vector& vA, const RcppGSL::Vector& vB, const RcppGSL::Vector& vZ);
RcppExport SEXP _CLVTools_vec_gsl_hyp2f0_e(SEXP vASEXP, SEXP vBSEXP, SEXP vZSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const RcppGSL::Vector& >::type vA(vASEXP);
    Rcpp::traits::input_parameter< const RcppGSL::Vector& >::type vB(vBSEXP);
    Rcpp::traits::input_parameter< const RcppGSL::Vector& >::type vZ(vZSEXP);
    rcpp_result_gen = Rcpp::wrap(vec_gsl_hyp2f0_e(vA, vB, vZ));
    return rcpp_result_gen;
END_RCPP
}
// vec_gsl_hyp2f1_e
Rcpp::List vec_gsl_hyp2f1_e(const RcppGSL::Vector& vA, const RcppGSL::Vector& vB, const RcppGSL::Vector& vC, const RcppGSL::Vector& vZ);
RcppExport SEXP _CLVTools_vec_gsl_hyp2f1_e(SEXP vASEXP, SEXP vBSEXP, SEXP vCSEXP, SEXP vZSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const RcppGSL::Vector& >::type vA(vASEXP);
    Rcpp::traits::input_parameter< const RcppGSL::Vector& >::type vB(vBSEXP);
    Rcpp::traits::input_parameter< const RcppGSL::Vector& >::type vC(vCSEXP);
    Rcpp::traits::input_parameter< const RcppGSL::Vector& >::type vZ(vZSEXP);
    rcpp_result_gen = Rcpp::wrap(vec_gsl_hyp2f1_e(vA, vB, vC, vZ));
    return rcpp_result_gen;
END_RCPP
}
// gg_LL
double gg_LL(const arma::vec& vLogparams, const arma::vec& vX, const arma::vec& vM_x);
RcppExport SEXP _CLVTools_gg_LL(SEXP vLogparamsSEXP, SEXP vXSEXP, SEXP vM_xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type vLogparams(vLogparamsSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type vX(vXSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type vM_x(vM_xSEXP);
    rcpp_result_gen = Rcpp::wrap(gg_LL(vLogparams, vX, vM_x));
    return rcpp_result_gen;
END_RCPP
}
// pnbd_nocov_CET
arma::vec pnbd_nocov_CET(const arma::vec& vEstimated_params, const double dPrediction_period, const arma::vec& vX, const arma::vec& vT_x, const arma::vec& vT_cal);
RcppExport SEXP _CLVTools_pnbd_nocov_CET(SEXP vEstimated_paramsSEXP, SEXP dPrediction_periodSEXP, SEXP vXSEXP, SEXP vT_xSEXP, SEXP vT_calSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type vEstimated_params(vEstimated_paramsSEXP);
    Rcpp::traits::input_parameter< const double >::type dPrediction_period(dPrediction_periodSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type vX(vXSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type vT_x(vT_xSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type vT_cal(vT_calSEXP);
    rcpp_result_gen = Rcpp::wrap(pnbd_nocov_CET(vEstimated_params, dPrediction_period, vX, vT_x, vT_cal));
    return rcpp_result_gen;
END_RCPP
}
// pnbd_staticcov_CET
arma::vec pnbd_staticcov_CET(const arma::vec& vEstimated_params, const double dPrediction_period, const arma::vec& vX, const arma::vec& vT_x, const arma::vec& vT_cal, const arma::vec& vCovParams_trans, const arma::vec& vCovParams_life, const arma::mat& mCov_trans, const arma::mat& mCov_life);
RcppExport SEXP _CLVTools_pnbd_staticcov_CET(SEXP vEstimated_paramsSEXP, SEXP dPrediction_periodSEXP, SEXP vXSEXP, SEXP vT_xSEXP, SEXP vT_calSEXP, SEXP vCovParams_transSEXP, SEXP vCovParams_lifeSEXP, SEXP mCov_transSEXP, SEXP mCov_lifeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type vEstimated_params(vEstimated_paramsSEXP);
    Rcpp::traits::input_parameter< const double >::type dPrediction_period(dPrediction_periodSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type vX(vXSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type vT_x(vT_xSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type vT_cal(vT_calSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type vCovParams_trans(vCovParams_transSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type vCovParams_life(vCovParams_lifeSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type mCov_trans(mCov_transSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type mCov_life(mCov_lifeSEXP);
    rcpp_result_gen = Rcpp::wrap(pnbd_staticcov_CET(vEstimated_params, dPrediction_period, vX, vT_x, vT_cal, vCovParams_trans, vCovParams_life, mCov_trans, mCov_life));
    return rcpp_result_gen;
END_RCPP
}
// pnbd_nocov_DERT
arma::vec pnbd_nocov_DERT(const arma::vec& vEstimated_params, const double continuous_discount_factor, const arma::vec& vX, const arma::vec& vT_x, const arma::vec& vT_cal);
RcppExport SEXP _CLVTools_pnbd_nocov_DERT(SEXP vEstimated_paramsSEXP, SEXP continuous_discount_factorSEXP, SEXP vXSEXP, SEXP vT_xSEXP, SEXP vT_calSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type vEstimated_params(vEstimated_paramsSEXP);
    Rcpp::traits::input_parameter< const double >::type continuous_discount_factor(continuous_discount_factorSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type vX(vXSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type vT_x(vT_xSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type vT_cal(vT_calSEXP);
    rcpp_result_gen = Rcpp::wrap(pnbd_nocov_DERT(vEstimated_params, continuous_discount_factor, vX, vT_x, vT_cal));
    return rcpp_result_gen;
END_RCPP
}
// pnbd_staticcov_DERT
arma::vec pnbd_staticcov_DERT(const arma::vec& vEstimated_params, const double continuous_discount_factor, const arma::vec& vX, const arma::vec& vT_x, const arma::vec& vT_cal, const arma::mat& mCov_life, const arma::mat& mCov_trans, const arma::vec& vCovParams_life, const arma::vec& vCovParams_trans);
RcppExport SEXP _CLVTools_pnbd_staticcov_DERT(SEXP vEstimated_paramsSEXP, SEXP continuous_discount_factorSEXP, SEXP vXSEXP, SEXP vT_xSEXP, SEXP vT_calSEXP, SEXP mCov_lifeSEXP, SEXP mCov_transSEXP, SEXP vCovParams_lifeSEXP, SEXP vCovParams_transSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type vEstimated_params(vEstimated_paramsSEXP);
    Rcpp::traits::input_parameter< const double >::type continuous_discount_factor(continuous_discount_factorSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type vX(vXSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type vT_x(vT_xSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type vT_cal(vT_calSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type mCov_life(mCov_lifeSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type mCov_trans(mCov_transSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type vCovParams_life(vCovParams_lifeSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type vCovParams_trans(vCovParams_transSEXP);
    rcpp_result_gen = Rcpp::wrap(pnbd_staticcov_DERT(vEstimated_params, continuous_discount_factor, vX, vT_x, vT_cal, mCov_life, mCov_trans, vCovParams_life, vCovParams_trans));
    return rcpp_result_gen;
END_RCPP
}
// pnbd_nocov_LL_ind
arma::vec pnbd_nocov_LL_ind(const arma::vec& vLogparams, const arma::vec& vX, const arma::vec& vT_x, const arma::vec& vT_cal);
RcppExport SEXP _CLVTools_pnbd_nocov_LL_ind(SEXP vLogparamsSEXP, SEXP vXSEXP, SEXP vT_xSEXP, SEXP vT_calSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type vLogparams(vLogparamsSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type vX(vXSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type vT_x(vT_xSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type vT_cal(vT_calSEXP);
    rcpp_result_gen = Rcpp::wrap(pnbd_nocov_LL_ind(vLogparams, vX, vT_x, vT_cal));
    return rcpp_result_gen;
END_RCPP
}
// pnbd_nocov_LL_sum
double pnbd_nocov_LL_sum(const arma::vec& vLogparams, const arma::vec& vX, const arma::vec& vT_x, const arma::vec& vT_cal);
RcppExport SEXP _CLVTools_pnbd_nocov_LL_sum(SEXP vLogparamsSEXP, SEXP vXSEXP, SEXP vT_xSEXP, SEXP vT_calSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type vLogparams(vLogparamsSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type vX(vXSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type vT_x(vT_xSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type vT_cal(vT_calSEXP);
    rcpp_result_gen = Rcpp::wrap(pnbd_nocov_LL_sum(vLogparams, vX, vT_x, vT_cal));
    return rcpp_result_gen;
END_RCPP
}
// pnbd_staticcov_LL_ind
arma::vec pnbd_staticcov_LL_ind(const arma::vec& vParams, const arma::vec& vX, const arma::vec& vT_x, const arma::vec& vT_cal, const arma::mat& mCov_life, const arma::mat& mCov_trans);
RcppExport SEXP _CLVTools_pnbd_staticcov_LL_ind(SEXP vParamsSEXP, SEXP vXSEXP, SEXP vT_xSEXP, SEXP vT_calSEXP, SEXP mCov_lifeSEXP, SEXP mCov_transSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type vParams(vParamsSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type vX(vXSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type vT_x(vT_xSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type vT_cal(vT_calSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type mCov_life(mCov_lifeSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type mCov_trans(mCov_transSEXP);
    rcpp_result_gen = Rcpp::wrap(pnbd_staticcov_LL_ind(vParams, vX, vT_x, vT_cal, mCov_life, mCov_trans));
    return rcpp_result_gen;
END_RCPP
}
// pnbd_staticcov_LL_sum
double pnbd_staticcov_LL_sum(const arma::vec& vParams, const arma::vec& vX, const arma::vec& vT_x, const arma::vec& vT_cal, const arma::mat& mCov_life, const arma::mat& mCov_trans);
RcppExport SEXP _CLVTools_pnbd_staticcov_LL_sum(SEXP vParamsSEXP, SEXP vXSEXP, SEXP vT_xSEXP, SEXP vT_calSEXP, SEXP mCov_lifeSEXP, SEXP mCov_transSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type vParams(vParamsSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type vX(vXSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type vT_x(vT_xSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type vT_cal(vT_calSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type mCov_life(mCov_lifeSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type mCov_trans(mCov_transSEXP);
    rcpp_result_gen = Rcpp::wrap(pnbd_staticcov_LL_sum(vParams, vX, vT_x, vT_cal, mCov_life, mCov_trans));
    return rcpp_result_gen;
END_RCPP
}
// pnbd_nocov_PAlive
arma::vec pnbd_nocov_PAlive(const arma::vec& vEstimated_params, const arma::vec& vX, const arma::vec& vT_x, const arma::vec& vT_cal);
RcppExport SEXP _CLVTools_pnbd_nocov_PAlive(SEXP vEstimated_paramsSEXP, SEXP vXSEXP, SEXP vT_xSEXP, SEXP vT_calSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type vEstimated_params(vEstimated_paramsSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type vX(vXSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type vT_x(vT_xSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type vT_cal(vT_calSEXP);
    rcpp_result_gen = Rcpp::wrap(pnbd_nocov_PAlive(vEstimated_params, vX, vT_x, vT_cal));
    return rcpp_result_gen;
END_RCPP
}
// pnbd_staticcov_PAlive
arma::vec pnbd_staticcov_PAlive(const arma::vec& vEstimated_params, const arma::vec& vX, const arma::vec& vT_x, const arma::vec& vT_cal, const arma::vec& vCovParams_trans, const arma::vec& vCovParams_life, const arma::mat& mCov_trans, const arma::mat& mCov_life);
RcppExport SEXP _CLVTools_pnbd_staticcov_PAlive(SEXP vEstimated_paramsSEXP, SEXP vXSEXP, SEXP vT_xSEXP, SEXP vT_calSEXP, SEXP vCovParams_transSEXP, SEXP vCovParams_lifeSEXP, SEXP mCov_transSEXP, SEXP mCov_lifeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::vec& >::type vEstimated_params(vEstimated_paramsSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type vX(vXSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type vT_x(vT_xSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type vT_cal(vT_calSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type vCovParams_trans(vCovParams_transSEXP);
    Rcpp::traits::input_parameter< const arma::vec& >::type vCovParams_life(vCovParams_lifeSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type mCov_trans(mCov_transSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type mCov_life(mCov_lifeSEXP);
    rcpp_result_gen = Rcpp::wrap(pnbd_staticcov_PAlive(vEstimated_params, vX, vT_x, vT_cal, vCovParams_trans, vCovParams_life, mCov_trans, mCov_life));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_CLVTools_bgnbd_nocov_CET", (DL_FUNC) &_CLVTools_bgnbd_nocov_CET, 9},
    {"_CLVTools_bgbb_nocov_DERT", (DL_FUNC) &_CLVTools_bgbb_nocov_DERT, 5},
    {"_CLVTools_bgbb_nocov_LL_ind", (DL_FUNC) &_CLVTools_bgbb_nocov_LL_ind, 4},
    {"_CLVTools_bgbb_nocov_LL_sum", (DL_FUNC) &_CLVTools_bgbb_nocov_LL_sum, 4},
    {"_CLVTools_bgbb_nocov_PAlive", (DL_FUNC) &_CLVTools_bgbb_nocov_PAlive, 7},
    {"_CLVTools_vec_gsl_hyp2f0_e", (DL_FUNC) &_CLVTools_vec_gsl_hyp2f0_e, 3},
    {"_CLVTools_vec_gsl_hyp2f1_e", (DL_FUNC) &_CLVTools_vec_gsl_hyp2f1_e, 4},
    {"_CLVTools_gg_LL", (DL_FUNC) &_CLVTools_gg_LL, 3},
    {"_CLVTools_pnbd_nocov_CET", (DL_FUNC) &_CLVTools_pnbd_nocov_CET, 5},
    {"_CLVTools_pnbd_staticcov_CET", (DL_FUNC) &_CLVTools_pnbd_staticcov_CET, 9},
    {"_CLVTools_pnbd_nocov_DERT", (DL_FUNC) &_CLVTools_pnbd_nocov_DERT, 5},
    {"_CLVTools_pnbd_staticcov_DERT", (DL_FUNC) &_CLVTools_pnbd_staticcov_DERT, 9},
    {"_CLVTools_pnbd_nocov_LL_ind", (DL_FUNC) &_CLVTools_pnbd_nocov_LL_ind, 4},
    {"_CLVTools_pnbd_nocov_LL_sum", (DL_FUNC) &_CLVTools_pnbd_nocov_LL_sum, 4},
    {"_CLVTools_pnbd_staticcov_LL_ind", (DL_FUNC) &_CLVTools_pnbd_staticcov_LL_ind, 6},
    {"_CLVTools_pnbd_staticcov_LL_sum", (DL_FUNC) &_CLVTools_pnbd_staticcov_LL_sum, 6},
    {"_CLVTools_pnbd_nocov_PAlive", (DL_FUNC) &_CLVTools_pnbd_nocov_PAlive, 4},
    {"_CLVTools_pnbd_staticcov_PAlive", (DL_FUNC) &_CLVTools_pnbd_staticcov_PAlive, 8},
    {NULL, NULL, 0}
};

RcppExport void R_init_CLVTools(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
