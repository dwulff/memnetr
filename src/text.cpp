#include <Rcpp.h>
#include <string>
#include <iostream>

using namespace Rcpp;

// [[Rcpp::export]]
bool in_doc(std::string doc,
            std::string term,
            bool space = true){
  if(space)   return doc.find(" " + term + " ") != std::string::npos;
  return doc.find(term) != std::string::npos;
  }

// [[Rcpp::export]]
int count_in_doc(std::string doc,
                 std::string term,
                 bool space = true){
  int occurrences = 0;
  std::string::size_type pos = 0;
  if(space){
    while ((pos = doc.find(" " + term + " ", pos )) != std::string::npos) {
      occurrences++;
      pos += term.length();
      }
    } else {
    while ((pos = doc.find(term, pos )) != std::string::npos) {
      occurrences++;
      pos += term.length();
      }
    }
  return occurrences;
  }


// return term-document matrix with unit weights
// [[Rcpp::export]]
IntegerMatrix td_unit(std::vector<std::string> docs,
                      std::vector<std::string> terms){

  int nrow = terms.size(), ncol = docs.size();
  IntegerMatrix td_unit(nrow, ncol);
  for(int j = 0; j < ncol; ++j){
    std::string doc = docs[j];
    for(int i = 0; i < nrow; ++i){
      std::string term = terms[i];
      if(in_doc(doc, term)) td_unit(i, j) = 1;
    }
  }
  return td_unit;
  }



//
// // return term-document matrix with unit weights
// // [[Rcpp::export]]
// IntegerMatrix td_count(std::vector<std::string> docs,
//                       std::vector<std::string> terms){
//
//   int nrow = terms.size(), ncol = docs.size();
//   IntegerMatrix td_unit(nrow, ncol);
//   for(int j = 0; j < ncol; ++j){
//     std::string doc = docs[j];
//     for(int i = 0; i < nrow; ++i){
//       std::string term = terms[i];
//       if(in_doc(doc, term)) td_unit(i, j) = count_in_doc(doc, term);
//       }
//     }
//   return td_unit;
//   }

// return term-document matrix with count weights
// [[Rcpp::export]]
IntegerMatrix td_count(std::vector<std::string> docs,
                       std::vector<std::string> terms){

  int nrow = terms.size(), ncol = docs.size();
  IntegerMatrix td_unit(nrow, ncol);
  for(int j = 0; j < ncol; ++j){
    std::string doc = docs[j];
    for(int i = 0; i < nrow; ++i){
      std::string term = terms[i];
      td_unit(i, j) = count_in_doc(doc, term);
    }
  }
  return td_unit;
}


// return term-term matrix with unit weights
// [[Rcpp::export]]
IntegerMatrix tt_unit(IntegerMatrix td_mat){
  int nrow = td_mat.nrow();
  IntegerMatrix tt_unit(nrow, nrow);
  for(int i = 0; i < nrow; ++i){
   IntegerVector term_1 = td_mat(i,_);
    tt_unit(i, i) = sum(term_1);
    for(int j = i + 1; j < nrow; ++j){
      IntegerVector term_2 = td_mat(j,_);
      tt_unit(i, j) = tt_unit(j, i) = sum(term_1 * term_2);
      }
    }
  return tt_unit;
  }


// return term-term matrix with unit weights
// [[Rcpp::export]]
IntegerMatrix tt_count(IntegerMatrix td_mat){
  int nrow = td_mat.nrow();
  IntegerMatrix tt_unit(nrow, nrow);
  for(int i = 0; i < nrow; ++i){
    IntegerVector term_1 = td_mat(i,_);
    tt_unit(i, i) = sum(term_1);
    for(int j = i + 1; j < nrow; ++j){
      IntegerVector term_2 = td_mat(j,_);
      int count = 0, ncol = td_mat.ncol();
      for(int k = 0; k < ncol; ++k){
        count += std::min(term_1[k], term_2[k]);
        }
      tt_unit(i, j) = tt_unit(j, i) = count;
      }
    }
  return tt_unit;
  }




bool find_int(std::vector<int> vec, int el){
  return std::find(vec.begin(), vec.end(), el) != vec.end();
  }


// remove rows in numeric matrix
// [[Rcpp::export]]
NumericMatrix remove_rows(NumericMatrix mat, std::vector<int> remove){
  int ind = 0, n = mat.nrow();
  NumericMatrix new_mat(n - remove.size(), mat.ncol());
  for(int i = 0; i < n; ++i){
    if(!find_int(remove, i)){
      new_mat(ind,_) = mat(i,_);
      ind++;
      }
    }
  return new_mat;
  }

// remove rows in numeric matrix
// [[Rcpp::export]]
NumericMatrix combine_rows(NumericMatrix mat,
                           NumericMatrix inds,
                           std::string method = "max"){
  int index_stay, index_add, n = inds.nrow(), ncol = mat.ncol();
  std::vector<int> remove;
  for(int i = 0; i < n; ++i){
    index_stay = inds(i, 0) - 1;
    index_add = inds(i, 1) - 1;
    NumericVector stay = mat(index_stay,_);
    NumericVector add = mat(index_add,_);
    for(int k = 0; k < ncol; ++k){
      if(method.compare("max") == 0){
        if(stay[k] < add[k]){
          stay[k] = add[k];
          }
        } else {
        stay[k] += add[k];
        }
      }
    mat(index_stay,_) = stay;
    remove.push_back(index_add);
    }
  return remove_rows(mat, remove);
  }




