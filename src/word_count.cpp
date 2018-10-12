#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
CharacterMatrix cnt_tokens(std::vector<std::string> words) {
  std::map<std::string, int> tab;

  // count
  int n = words.size();
  for(int i = 0; i < n; ++i){
    tab[words[i]]++;
    }

  // transform
  n = tab.size();
  CharacterMatrix table(n,2);
  std::map<std::string, int>::iterator it = tab.begin();
  int i = 0;
  for(;it != tab.end(); ++it){
    table(i,0) = it->first;
    table(i,1) = it->second;
    i++;
    }
  return table;
  }
