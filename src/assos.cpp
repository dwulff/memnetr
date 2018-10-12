// [[Rcpp::depends(RcppArmadillo)]]
#include <Rcpp.h>
using namespace Rcpp;

// returns map containing indices for each word
// [[Rcpp::export]]
DataFrame count_assos_directed(StringVector cues,
                            StringVector responses){
  int i = 0, n = cues.size();
  std::map<std::pair<std::string, std::string>, int > counts;
  std::string cue, resp;
  std::pair<std::string, std::string> cue_resp;
  for(; i < n; ++i){
    cue = cues[i];
    resp = responses[i];
    cue_resp = std::make_pair(cue, resp);
    counts[cue_resp]++;
    }

  // write into data frame
  int n_counts = counts.size();
  StringVector cues_unique(n_counts), responses_unique(n_counts);
  IntegerVector cnts(n_counts);
  std::map<std::pair<std::string, std::string>, int >::iterator it = counts.begin();
  i = 0;
  for(; it != counts.end(); ++it){
    cue_resp = it->first;
    cues_unique[i] = std::get<0>(cue_resp);
    responses_unique[i] = std::get<1>(cue_resp);
    cnts[i] = it->second;
    i++;
    }
  return DataFrame::create(Named("cue") = cues_unique,
                           Named("response") = responses_unique,
                           Named("count") = cnts);

  }

// returns map containing indices for each word
// [[Rcpp::export]]
DataFrame count_assos_undirected(StringVector cues,
                               StringVector responses){
  int i = 0, n = cues.size();
  std::map<std::pair<std::string, std::string>, int > counts;
  std::string cue, resp;
  std::pair<std::string, std::string> cue_resp;
  for(; i < n; ++i){
    cue = cues[i];
    resp = responses[i];
    if(cue < resp){
      cue_resp = std::make_pair(cue, resp);
    } else {
      cue_resp = std::make_pair(resp, cue);
    }
    counts[cue_resp]++;
  }

  // write into data frame
  int n_counts = counts.size();
  StringVector cues_unique(n_counts), responses_unique(n_counts);
  IntegerVector cnts(n_counts);
  std::map<std::pair<std::string, std::string>, int >::iterator it = counts.begin();
  i = 0;
  for(; it != counts.end(); ++it){
    cue_resp = it->first;
    cues_unique[i] = std::get<0>(cue_resp);
    responses_unique[i] = std::get<1>(cue_resp);
    cnts[i] = it->second;
    i++;
  }
  return DataFrame::create(Named("cue") = cues_unique,
                           Named("response") = responses_unique,
                           Named("count") = cnts);

}




// returns map containing indices for each word
// [[Rcpp::export]]
std::map<std::string, int> determine_indices(std::vector<std::string> words) {
  std::map<std::string, int> inds;
  int ind = 0, i = 0, n = words.size();
  for(; i < n; ++i){
    std::string word = words[i];
    if(inds.find(word) == inds.end()){
      inds[word] = ind;
      ind++;
      }
    }
  return inds;
  }

// // [[Rcpp::export]]
// CharacterMatrix get_indices(std::vector<std::string> words){
//   std::map<std::string, int> inds = determine_indices(words);
//   int ind = 0, n = inds.size();
//   CharacterMatrix inds_mat(n, 2);
//   std::map<std::string, int>::iterator it;
//   for(it = inds.begin(); it != inds.end(); ++it) {
//     inds_mat(ind, 0) = it->first;
//     inds_mat(ind, 1) = std::to_string(it->second);
//     ind++;
//     }
//   return inds_mat;
//   }


// // Get cue x response association matrix matrix
// // [[Rcpp::export]]
// NumericVector get_A_square(std::vector<std::string> cues, std::vector<std::string> responses) {
//   std::vector<std::string> words(cues.size() + responses.size());
//   words.insert(cues.begin(), cues.end(), cues.end());
//   words.insert(responses.begin(), responses.end(), responses.end());
//   std::map<std::string, int> inds = determine_indices(cues);
//   NumericMatrix A(inds.size(), inds.size(), arma::fill::eye);
//   int n = cues.size();
//   for(int ind = 0; ind < n; ++ind){
//     std::string cue_word = cues[ind], res_word = responses[ind];
//     int i = inds[cue_word], j = inds[res_word];
//     A(i, j)++;
//     }
//   return A;
//   }

// Get cue x response association matrix matrix
// [[Rcpp::export]]
NumericVector get_A(std::vector<std::string> cues, std::vector<std::string> responses) {
  std::map<std::string, int> cue_inds = determine_indices(cues);
  std::map<std::string, int> res_inds = determine_indices(responses);
  NumericMatrix A(cue_inds.size(), res_inds.size());
  int n = cues.size();
  for(int ind = 0; ind < n; ++ind){
    std::string cue_word = cues[ind], res_word = responses[ind];
    int i = cue_inds[cue_word], j = res_inds[res_word];
    A(i, j)++;
  }
  return A;
}


//Get row sums
// [[Rcpp::export]]
NumericVector rowsums(NumericMatrix A) {
  int n = A.nrow(), ncol = A.ncol();
  NumericVector sums(n);
  for(int i = 0; i < n; ++i){
    double sum = 0;
    for(int j = 0; j < ncol; ++j){
      sum += A(i, j);
    }
  sums[i] = sum;
  }
  return sums;
}

//Get col sums
// [[Rcpp::export]]
NumericVector colsums(NumericMatrix A) {
  int n = A.nrow(), ncol = A.ncol();
  NumericVector sums(ncol);
  for(int j = 0; j < ncol; ++j){
    double sum = 0;
    for(int i = 0; i < n; ++i){
      sum += A(i, j);
      }
    sums[j] = sum;
  }
  return sums;
}

// Get cue x response association matrix matrix
// [[Rcpp::export]]
NumericMatrix P_from_A(NumericMatrix A) {
  int n = A.nrow(), ncol = A.ncol();
  NumericVector sums = rowsums(A);
  NumericMatrix P(n, ncol);
  for(int i = 0; i < n; ++i){
    for(int j = 0; j < ncol; ++j){
      P(i, j) = A(i, j) / sums[i];
    }
  }
  return P;
}


// Get cue x response association matrix matrix
// [[Rcpp::export]]
NumericMatrix PPMI_from_P(NumericMatrix P) {
  int n = P.nrow(), ncol = P.ncol();
  NumericVector sums = colsums(P);
  for(int i = 0; i < 20; ++i) std::cout << sums[i] << '\n';
  NumericMatrix PPMI(n, ncol);
  for(int i = 0; i < n; ++i){
    for(int j = 0; j < ncol; ++j){
      PPMI(i, j) = std::max(0.0, std::log2((P(i, j) * n) / sums[j]));
    }
  }
  return PPMI;
}


// Get cue x response association matrix matrix
// [[Rcpp::export]]
NumericVector get_P(std::vector<std::string> cues, std::vector<std::string> responses) {
  std::map<std::string, int> cue_inds = determine_indices(cues);
  std::map<std::string, int> res_inds = determine_indices(responses);
  NumericMatrix A(cue_inds.size(), res_inds.size());
  int n = cues.size();
  for(int ind = 0; ind < n; ++ind){
    std::string cue_word = cues[ind], res_word = responses[ind];
    int i = cue_inds[cue_word], j = res_inds[res_word];
    A(i, j)++;
  }
  return P_from_A(A);
}


// Get cue x response association matrix matrix
// [[Rcpp::export]]
NumericVector get_PPMI(std::vector<std::string> cues, std::vector<std::string> responses) {
  std::map<std::string, int> cue_inds = determine_indices(cues);
  std::map<std::string, int> res_inds = determine_indices(responses);
  NumericMatrix A(cue_inds.size(), res_inds.size());
  int n = cues.size();
  for(int ind = 0; ind < n; ++ind){
    std::string cue_word = cues[ind], res_word = responses[ind];
    int i = cue_inds[cue_word], j = res_inds[res_word];
    A(i, j)++;
    }
  return PPMI_from_P(P_from_A(A));
  }



// Get cue x response association matrix matrix
// [[Rcpp::export]]
NumericVector get_cosines(NumericMatrix A) {
  int n_i = A.nrow() - 1, n = A.nrow(), ncol = A.ncol();
  NumericMatrix Cos(n, n);
  for(int i = 0; i < n_i; ++i){
    for(int j = i + 1; j < n; ++j){
      double prod = 0, norm_i = 0, norm_j = 0;
      for(int k = 0; k < ncol; ++k){
        prod = A(i, k) * A(j, k);
        norm_i = A(i, k) * A(i, k);
        norm_j = A(j, k) * A(j, k);
      }
      Cos(i, j) = Cos(j, i) = prod / (std::pow(norm_i, .5) * std::pow(norm_j, .5));
      }
  }
  return Cos;
}

