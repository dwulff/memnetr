#include <Rcpp.h>
#include <random>
#include <ctime>
using namespace Rcpp;

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
// HELPERS
//
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// [[Rcpp::export]]
std::vector<double> normalize(std::vector<double> vec){
  int n = vec.size();
  double norm = 0;
  for(int i = 0; i < n; ++i){
    norm += std::pow(vec[i], 2);
    }
  norm /= std::pow(norm, .5);
  for(int i = 0; i < n; ++i){
    vec[i] /= norm;
    }
  return vec;
  }

// [[Rcpp::export]]
std::vector<double> chunk(const std::vector<double> &a, const std::vector<double> &b){
  int n = a.size();
  std::vector<double> res(n);
  for(int i = 0; i < n; ++i) res[i] = a[i] + b[i];
  return res;
  }

// [[Rcpp::export]]
std::vector<double> chunk_norm(const std::vector<double> &a, const std::vector<double> &b){
  int n = a.size();
  std::vector<double> res(n);
  for(int i = 0; i < n; ++i) res[i] = a[i] + b[i];
  return normalize(res);
  }

// [[Rcpp::export]]
std::vector<double> make_word(int k){
  std::random_device r;
  std::default_random_engine generator(r());
  std::uniform_real_distribution<double> distribution(-1.0,1.0);
  std::vector<double> vec(k);
  for(int i = 0; i < k; ++i) vec[i] = distribution(generator);
  return normalize(vec);
  }

std::vector<double> word_init(int d){
  std::vector<double> vec(d, 0.0);
  return vec;
  }



// [[Rcpp::export]]
GenericVector split_string_list(GenericVector sents, GenericVector indices) {
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


// [[Rcpp::export]]
GenericVector beagle_basis(GenericVector sents, GenericVector indices, std::vector<std::string> targets) {
  int n = sents.size();
  GenericVector token_list(n + 1);
  std::map<std::string, int> table;
  GenericVector includes(n);
  GenericVector positions(n);

  // fill table with targets
  int n_targets = targets.size();
  for(int i = 0; i < n_targets; ++i){
    std::string target = targets[i];
    table[target] = i;
    }

  // iterate through sents
  for(int i = 0; i < n; ++i){
    std::string sent = sents[i];
    NumericMatrix inds = indices[i];
    int n_tokens = inds.nrow();
    IntegerVector tokens(n_tokens);
    std::vector<int> include, position;
    for(int j = 0; j < n_tokens; ++j){
      std::string token = "";
      int start = inds(j,0) - 1, end = inds(j,1);
      for(int k = start; k < end; ++k){
        token += sent[k];
        }
      std::map<std::string, int>::iterator it;
      it = table.find(token);
      int index;
      if(it == table.end()){
        index = table.size();
        table[token] = index;
        tokens[j] = index;
        } else {
        index = it->second;
        tokens[j] = index;
        if(index < n_targets){
          include.push_back(index);
          position.push_back(j);
          };
        }
      }
    includes[i] = include;
    positions[i] = position;
    token_list[i + 1] = tokens;
    }

  // write table to matrix
  int n_tokens = table.size();
  StringVector all_tokens(n_tokens);
  IntegerVector all_indices(n_tokens);
  std::map<std::string, int>::iterator it = table.begin();
  int i = 0;
  for(; it != table.end(); ++it){
    all_tokens[i] = it->first;
    all_indices[i] = it->second;
    i++;
    }
  GenericVector token_map(4);
  token_map[0] = all_tokens;
  token_map[1] = all_indices;
  token_map[2] = includes;
  token_map[3] = positions;
  token_list[0] = token_map;

  // return
  return token_list;
  }


// [[Rcpp::export]]
GenericVector beagle_basis_stops(std::vector<std::string> sents, GenericVector indices, std::vector<std::string> targets, std::vector<std::string> stops) {
  int n = sents.size();
  GenericVector token_list(n + 1);
  std::map<std::string, int> table;
  GenericVector includes(n);
  GenericVector positions(n);

  // fill table with targets
  int n_targets = targets.size();
  for(int i = 0; i < n_targets; ++i){
    std::string target = targets[i];
    table[target] = i;
    }

  // create stopwords set
  std::set<std::string> stopwords;
  stopwords.insert(stops.begin(), stops.end());

  // iterate through sents
  for(int i = 0; i < n; ++i){
    std::string sent = sents[i];
    NumericMatrix inds = indices[i];
    int n_tokens = inds.nrow();
    std::vector<int> tokens, include, position;
    for(int j = 0; j < n_tokens; ++j){
      std::string token = "";
      int start = inds(j,0) - 1, end = inds(j,1);
      for(int k = start; k < end; ++k){
        token += sent[k];
        }
      if(stopwords.find(token) == stopwords.end()){
        std::map<std::string, int>::iterator it;
        it = table.find(token);
        int index;
        if(it == table.end()){
          index = table.size();
          table[token] = index;
          tokens.push_back(index);
        } else {
          index = it->second;
          tokens.push_back(index);
          if(index < n_targets){
            include.push_back(index);
            position.push_back(tokens.size() - 1);
          }
        }
      }
    }
    includes[i] = include;
    positions[i] = position;
    token_list[i + 1] = tokens;
  }

  // write table to matrix
  int n_tokens = table.size();
  StringVector all_tokens(n_tokens);
  IntegerVector all_indices(n_tokens);
  std::map<std::string, int>::iterator tab_it = table.begin();
  int i = 0;
  for(; tab_it != table.end(); ++tab_it){
    all_tokens[i] = tab_it->first;
    all_indices[i] = tab_it->second;
    i++;
  }
  GenericVector token_map(4);
  token_map[0] = all_tokens;
  token_map[1] = all_indices;
  token_map[2] = includes;
  token_map[3] = positions;
  token_list[0] = token_map;

  // return
  return token_list;
}

// [[Rcpp::export]]
NumericMatrix beagle_con(GenericVector sents, GenericVector indices, int n_target, int d = 1000, bool missing = true){
  std::map<int, std::vector<double> > targets;
  std::map<int, std::vector<double> > words;
  int n = sents.size();

  // count
  for(int i = 0; i < n; ++i){

    // extract data
    IntegerVector sent = sents[i];
    std::vector<int> inds = indices[i];

    //get target set
    std::set<int> target_set;
    target_set.insert(inds.begin(), inds.end());
    std::set<int>::iterator target_it = target_set.begin();

    // iterate through targets and make representations
    for(; target_it != target_set.end(); ++target_it){
      int target = *target_it, n_words = sent.size();
      if(targets.find(target) == targets.end()){
        std::vector<double> word_rep = make_word(d);
        targets[target] = word_rep;
        words[target] = word_rep;
        }
      for(int k = 0; k < n_words; ++k){
        int word = sent[k];
        if(words.find(word) == words.end()) words[word] = make_word(d);
        if(word != target) targets[target] = chunk(targets[target], words[word]);
        // std::vector<double> test1 = targets[target];
        // std::vector<double> test2 = words[word];
        // std::cout << target << '\t' << word << '\t' << test1[1] << '\t' << test2[1]  <<'\n';
        }
      }
    }

  NumericMatrix representations(n_target,d);
  for(int i = 0; i< n_target; ++i){
    std::vector<double> rep(d, NA_REAL);;
    if(targets.find(i) != targets.end()){
      //rep = normalize(targets[i]);
      rep = targets[i];
      } else {
      if(missing == false) rep = make_word(d);
      }
    for(int j = 0; j < d; ++j){
      representations(i, j) = rep[j];
      }
    }

  return representations;
}





////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
// COSINE
//
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// [[Rcpp::export]]
double cosine(NumericVector a, NumericVector b){
  int n = a.size();
  double sum_a = 0, sum_b = 0, sum_ab = 0;
  for(int i = 0; i < n; ++i){
    sum_a += a[i] * a[i];
    sum_b += b[i] * b[i];
    sum_ab += a[i] * b[i];
  }
  return sum_ab * std::pow(sum_a * sum_b, -.5);
}

// [[Rcpp::export]]
NumericMatrix cosine_mat(NumericMatrix representation){
  int n = representation.nrow();
  NumericMatrix mat(n,n);
  for(int i = 0; i < n; ++i){
    for(int j = i; j < n; ++j){
      if(i != j) {
        mat(i,j) = mat(j,i) = cosine(representation(i,_),representation(j,_));
      } else {
        mat(i,j) = mat(j,i) = 1;
      }
    }
  }
  return mat;
}


// [[Rcpp::export]]
NumericVector upper_mat(NumericMatrix mat) {
  int nrow = mat.nrow(), ncol = mat.ncol();
  int len = (mat.nrow() * mat.ncol())/2 - mat.nrow();
  NumericVector res(len);
  int ind = 0;
  for(int i = 0; i < nrow; ++i){
    for(int j = i+1; j < ncol; ++j){
      res[ind] = mat(i,j);
      ind++;
    }
  }
  return res;
}



// // [[Rcpp::export]]
// GenericVector remove_stops(GenericVector sents, std::vector<int> stops){
//
//   // create stopwords set
//   std::set<int> stopwords;
//   stopwords.insert(stops.begin(), stops.end());
//   std::set<int>::iterator it = stopwords.begin();
//   for(;it != stopwords.end(); ++it) std::cout << *it << '\n';
//
//   // remove stopwords
//   int n = sents.size();
//   for(int i = 0; i < n; ++i){
//     std::vector<int> sent = sents[i];
//     int n_sent = sent.size(), i_erase = 0;
//     for(int j = n_sent-1; j > -1; --j){
//       std::set<int>::iterator it = stopwords.find(sent[j]);
//       //std::cout << (it != stopwords.end()) << '\n';
//       if(it != stopwords.end()){
//         sent.erase(sent.begin() + j);//(j - i_erase));
//         i_erase++;
//         }
//       }
//     sents[i] = sent;
//     }
//   return sents;
//   }



// // [[Rcpp::export]]
// GenericVector count(GenericVector sents, GenericVector indices, int n_target, int k = 1000){
//   NumericMatrix representation(n_target, k);
//   std::map<int, std::map<int, int> > counts;
//   //std::map<int, std::vector<int> > words;
//   int n = sents.size();
//
//   // count
//   for(int i = 0; i < n; ++i){
//     IntegerVector sent = sents[i];
//     IntegerVector inds = indices[i];
//     int n_ind = inds.size(), n_sent = sent.size();
//     for(int j = 0; j < n_ind; ++j){
//       for(int k = 0; k < n_sent; ++k){
//         if(inds[j] != sent[k]) counts[inds[j]][sent[k]]++;
//         }
//       }
//     }
//
//   GenericVector res(n_target);
//   for(int i = 0; i< n_target; ++i){
//     std::map<int, int> count = counts[i];
//     int n_words = count.size();
//     std::map<int, int>::iterator it = count.begin();
//     int j = 0;
//     IntegerMatrix word_counts(n_words,2);
//     for(; it != count.end(); ++it){
//       word_counts(j,0) = it->first;
//       word_counts(j,1) = it->second;
//       j++;
//       }
//     res[i] = word_counts;
//     }
//   return res;
// }
