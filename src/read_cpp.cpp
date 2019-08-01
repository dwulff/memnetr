// [[Rcpp::depends(BH)]]
#include <fstream>
#include <sstream>
#include <string>
#include <Rcpp.h>
#include <iostream>
#include <regex>
#include <boost/algorithm/string/replace.hpp>

using namespace Rcpp;



std::string remove_substr(std::string& str, const std::vector<std::string>& patterns){
  std::vector<std::string>::const_iterator it = patterns.begin();
  for(;it != patterns.end(); ++it) boost::replace_all(str, *it, "");
  return str;
}


// [[Rcpp::export]]
StringVector read_texts(CharacterVector paths) {
  int n = paths.size();
  StringVector contents(n);
  for(int i = 0; i < n; ++i){
    std::string path = Rcpp::as<std::string>(paths[i]);
    std::ifstream in(path.c_str());
    std::string content;
    in.seekg(0, std::ios::end);
    content.resize(in.tellg());
    in.seekg(0, std::ios::beg);
    in.read(&content[0], content.size());
    in.close();
    contents[i] = content;
  }

  return contents;
}

// [[Rcpp::export]]
StringVector reads_texts_remove(CharacterVector paths, std::vector<std::string>& patterns) {
  int n = paths.size();
  StringVector contents(n);
  for(int i = 0; i < n; ++i){
    std::string path = Rcpp::as<std::string>(paths[i]);
    std::ifstream in(path.c_str());
    std::string content;
    in.seekg(0, std::ios::end);
    content.resize(in.tellg());
    in.seekg(0, std::ios::beg);
    in.read(&content[0], content.size());
    in.close();
    remove_substr(content, patterns);
    contents[i] = content;
  }
  return contents;
}


// [[Rcpp::export]]
NumericVector upper_mat_(NumericMatrix mat) {
  int nrow = mat.nrow();
  int len = (nrow * nrow)/2 - nrow/2;
  NumericVector res(len);
  int ind = 0;
  for(int i = 0; i < (nrow-1); ++i){
    for(int j = i+1; j < nrow; ++j){
      if(ind == res.size()) {
        std::cout << "shit" << '\n';
        break;
      }
      res[ind] = mat(i,j);
      ind++;
    }
  }
  return res;
}



