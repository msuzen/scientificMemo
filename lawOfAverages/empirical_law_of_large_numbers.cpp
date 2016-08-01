#include <Rcpp.h>
#include <stdlib.h>
using namespace Rcpp;

//
// Sum a numeric vector up to an index
//
double sum_nv(NumericVector x, int s) {
  int i=0;
  double ss=0.0;
  for(i=0;i<(s+1);i++) {
   ss=ss+x[i];
  }
  return(ss);
}

//
// Calculate the running difference in a Bernoulli process (binary_vec)
//
// [[Rcpp::export]]
//
NumericVector diff_binary_vec2(NumericVector binary_vec) {
  int ll = binary_vec.size();
  int i  = 0;
  NumericVector diff_vec(ll, 0.0);
  for(i=0;i<ll;i++) {
    diff_vec[i] = std::abs(i+1.0-2.0*sum_nv(binary_vec, i));
  }
  return(diff_vec);
}

//
// Calculate the running ratio in a Bernoulli process (binary_vec)
//
// [[Rcpp::export]]
NumericVector ratio_binary_vec2(NumericVector binary_vec) {
  int ll = binary_vec.size();
  int i  = 0;
  NumericVector ratio_vec(ll, 0.0);
  for(i=0;i<ll;i++) {
    ratio_vec[i] = std::abs(((i+1.0)/sum_nv(binary_vec, i))-1.0);
  }
  return(ratio_vec);
}