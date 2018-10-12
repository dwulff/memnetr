#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
std::vector<bool> triangle_ineq(NumericMatrix x) {
  int n = x.nrow();
  std::vector<bool> ineqs;
  for(int i  = 0; i < n; ++i){
    for(int j = (i + 1); j < n; ++j){
      for(int k = (j + 1); k < n; ++k){
        std::vector<double> triplet(3);
        triplet[0] = x(i,j);
        triplet[1] = x(i,k);
        triplet[2] = x(j,k);
        std::sort(triplet.begin(), triplet.end());
        ineqs.push_back(triplet[2] > triplet[0] + triplet[1]);
        }
      }
    }
  return(ineqs);
  }


