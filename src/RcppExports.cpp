// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// decode_line_cpp
Rcpp::DataFrame decode_line_cpp(std::string encoded);
RcppExport SEXP route_decode_line_cpp(SEXP encodedSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< std::string >::type encoded(encodedSEXP);
    __result = Rcpp::wrap(decode_line_cpp(encoded));
    return __result;
END_RCPP
}
