#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
GenericVector split_stringlist(GenericVector sents, GenericVector indices) {
  int n = sents.size();
  GenericVector token_list(n);
  for(int i = 0; i < n; ++i){
    std::string sent = sents[i];
    NumericMatrix inds = indices[i];
    int n_token = inds.nrow();
    CharacterVector tokens(n_token);
    for(int j = 0; j < n_token; ++j){
      std::string token = "";
      int start = inds(j,0) - 1, end = inds(j,1);
      for(int k = start; k < end; ++k){
        token += sent[k];
        }
      tokens[j] = token;
      }
    token_list[i] = tokens;
    }
  return token_list;
  }


