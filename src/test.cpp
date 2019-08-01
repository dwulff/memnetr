#include <Rcpp.h>
#include <sstream>
using namespace Rcpp;

// [[Rcpp::export]]
List tokenize(GenericVector docs) {

  // Obtaining namespace of Matrix package
  Environment pkg = Environment::namespace_env("tm");

  // Picking up Matrix() function from Matrix package
  Function f = pkg["Boost_tokenizer"];

  int i = 0, n = docs.size();
  List words(n);
  for(;i < n; ++i){
    StringVector doc = docs[i];
    words[i] = f(doc);
  }

  return words;
}

bool BothAreSpaces(char lhs, char rhs) { return (lhs == rhs) && (lhs == ' '); }


// [[Rcpp::export]]
IntegerMatrix get_td(std::vector<std::string> docs){

  // containers
  std::map<std::string, int> words;
  std::vector<std::map<std::string, int> > doc_counts;
  int index = 0;

  int i = 0, n_docs = docs.size();
  for(; i < n_docs; ++i){

  // get doc
  std::string doc = docs[i];

  // handle new lines and spaces
  doc.erase(std::remove(doc.begin(), doc.end(), '\n'), doc.end());
  doc.erase(std::remove(doc.begin(), doc.end(), '\r'), doc.end());
  // std::replace(doc.begin(), doc.end(), '\r', ' ');
  // std::replace(doc.begin(), doc.end(), '\n', ' ');
  std::string::iterator new_end = std::unique(doc.begin(), doc.end(), BothAreSpaces);
  doc.erase(new_end, doc.end());

  // prepare iterating through doc
  std::stringstream doc_strm(doc);
  std::string word;
  char delim = ' '; // Ddefine the delimiter to split by

  // containers
  std::map<std::string, int> counts;
  while (std::getline(doc_strm, word, delim)) {

    // count
    counts[word]++;

    // word set
    if(words.find(word) == words.end()){
      words[word] = index;
      index++;
      }
    }

  // storee counts
  doc_counts.push_back(counts);
  }

  // create matrix
  int n_words = words.size();
  //arma::sp_mat td(n_words, n_docs);
  IntegerMatrix td(n_words, n_docs);
  for(int j = 0; j < n_docs; ++j){
    std::map<std::string, int> counts = doc_counts[j];
    std::map<std::string, int>::iterator it;
    for(it = counts.begin(); it != counts.end(); ++it){
      int i = words[(*it).first];
      td(i, j) = (*it).second;
    }
  }

  // get names
  std::map<std::string, int>::iterator it;
  StringVector row_names(n_words);
  for(it = words.begin(); it != words.end(); ++it){
    row_names[(*it).second] =  (*it).first;
  }

  // name and out
  rownames(td) = row_names;
  return td;
}



