// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// tree_search_rcpp
Rcpp::List tree_search_rcpp(const Rcpp::NumericMatrix& X, const Rcpp::NumericMatrix& Y, int depth, int split_step, int min_node_size, int reward_type, size_t reward_dim);
RcppExport SEXP _policytree_tree_search_rcpp(SEXP XSEXP, SEXP YSEXP, SEXP depthSEXP, SEXP split_stepSEXP, SEXP min_node_sizeSEXP, SEXP reward_typeSEXP, SEXP reward_dimSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Rcpp::NumericMatrix& >::type X(XSEXP);
    Rcpp::traits::input_parameter< const Rcpp::NumericMatrix& >::type Y(YSEXP);
    Rcpp::traits::input_parameter< int >::type depth(depthSEXP);
    Rcpp::traits::input_parameter< int >::type split_step(split_stepSEXP);
    Rcpp::traits::input_parameter< int >::type min_node_size(min_node_sizeSEXP);
    Rcpp::traits::input_parameter< int >::type reward_type(reward_typeSEXP);
    Rcpp::traits::input_parameter< size_t >::type reward_dim(reward_dimSEXP);
    rcpp_result_gen = Rcpp::wrap(tree_search_rcpp(X, Y, depth, split_step, min_node_size, reward_type, reward_dim));
    return rcpp_result_gen;
END_RCPP
}
// tree_search_rcpp_predict
Rcpp::NumericMatrix tree_search_rcpp_predict(const Rcpp::NumericMatrix& tree_array, const Rcpp::NumericMatrix& X);
RcppExport SEXP _policytree_tree_search_rcpp_predict(SEXP tree_arraySEXP, SEXP XSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Rcpp::NumericMatrix& >::type tree_array(tree_arraySEXP);
    Rcpp::traits::input_parameter< const Rcpp::NumericMatrix& >::type X(XSEXP);
    rcpp_result_gen = Rcpp::wrap(tree_search_rcpp_predict(tree_array, X));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_policytree_tree_search_rcpp", (DL_FUNC) &_policytree_tree_search_rcpp, 7},
    {"_policytree_tree_search_rcpp_predict", (DL_FUNC) &_policytree_tree_search_rcpp_predict, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_policytree(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
