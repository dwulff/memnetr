#include <Rcpp.h>
using namespace Rcpp;

// create long matrix from term-term matrix
// [[Rcpp::export]]
IntegerMatrix serialize_tt(IntegerMatrix tt_mat, bool include_diag = true){

  // determine size
  int n = 0, nrow = tt_mat.nrow(), ncol = tt_mat.ncol();
  for(int i = 0; i < nrow; ++i){
    for(int j = 0; j < ncol; ++j){
      if(include_diag){
        n += tt_mat(i, j);
        } else {
        if(i != j) n += tt_mat(i, j);
        }
      }
    }

  // fill serialized matrix
  IntegerMatrix serial_tt(n, 2);
  int ind = 0;
  for(int i = 0; i < nrow; ++i){
    for(int j = 0; j < ncol; ++j){
      n = tt_mat(i, j);
      if(include_diag){
        for(int k = 0; k < n; ++k){
          serial_tt(ind,_) = NumericVector::create(i, j);
          ind++;
          }
        } else {
        if(i != j){
          for(int k = 0; k < n; ++k){
            serial_tt(ind,_) = NumericVector::create(i, j);
            ind++;
            }
          }
        }
      }
    }
  return serial_tt;
  }


// return term-document matrix with unit weights
std::vector<std::pair<std::vector<int>, int> > get_unit_events(IntegerMatrix td_mat,
                                                               bool remove_self = true){

  std::vector<std::pair<std::vector<int>, int> > events;
  int nrow = td_mat.nrow(), ncol = td_mat.ncol();
  for(int j = 0; j < ncol; ++j){
    IntegerVector doc = td_mat(_,j);
    std::vector<int> words;
    for(int i = 0; i < nrow; ++i){
      if(doc[i] > 0) words.push_back(i);
    }
    int nwords = words.size();
    for(int k = 0; k < nwords; ++k){
      std::vector<int> cues = words;
      int outcome = cues[k];
      if(remove_self) cues.erase(cues.begin() + k);
      events.push_back(std::make_pair(cues, outcome));
      }
    }
  return events;
  }

bool find_int(std::vector<int> vec, int el){
  return std::find(vec.begin(), vec.end(), el) != vec.end();
  }

// return term-document matrix with unit weights
// [[Rcpp::export]]
StringMatrix serialize_td(IntegerMatrix td_mat,
                          bool add_context = false){

  // get events
  std::vector<std::pair<std::vector<int>, int> > events = get_unit_events(td_mat);

  int n = events.size();

  StringMatrix event_mat(n, 2);
  for(int e = 0; e < n; ++e){

    // get event
    std::pair<std::vector<int>, int> event = events[e];

    // get cues & out
    std::vector<int> cues = std::get<0>(event);
    int outcome = std::get<1>(event);

    // string cues
    std::string cue_string;
    int ncues = cues.size();
    for(int k = 0; k < ncues; ++k) {
      if(k < 1){
        cue_string += std::to_string(cues[k]);
        } else {
        cue_string += "_" + std::to_string(cues[k]);
        }
    }

    // fill stringmat
    event_mat(e,0) = cue_string;
    event_mat(e, 1) = std::to_string(outcome);


    }

  return event_mat;
  }

// [[Rcpp::export]]
NumericMatrix rescorla_wagner(IntegerMatrix td_mat,
                              bool include_context = true,
                              bool scramble_order = true,
                              double gamma = 0.01,
                              double lambda = 1,
                              double start = 0) {

  // get events
  std::vector<std::pair<std::vector<int>, int> > events = get_unit_events(td_mat);

  // set up ns
  int n = events.size(), n_nodes = td_mat.nrow(), add_context = 0;
  if(include_context) add_context = 1;

  // set up order vector
  IntegerVector order = sample(n,n);

  // set up seen
  // std::vector<int> seen;
  // int n_seen;

  // get weight matrix
  NumericMatrix w(n_nodes + add_context, n_nodes);
  std::fill( w.begin(), w.end(), start );

  // iteratre through events
  for(int e = 0; e < n; ++e){

    // get event
    std::pair<std::vector<int>, int> event = events[e];

    // get cues & out
    std::vector<int> cues = std::get<0>(event);
    int outcome = std::get<1>(event);

    // update seen
    // seen.push_back(outcome);
    // n_seen = seen.size();

    // add context
    if(include_context) cues.push_back(n_nodes);

    // set up predicted outcome
    int ncues = cues.size();
    double outcome_hat = 0;

    // get predicted outcome
    for(int i = 0; i < ncues; ++i){
      outcome_hat += w(cues[i],outcome);
      }

    // sigmoid
    outcome_hat = 1 / (1 + std::exp(outcome_hat));

    Rcout << "\n\n" << e << "\t" << ncues << "\t" << outcome_hat << "\t" << outcome << "\n\n";

    // update weights
    for(int i = 0; i < ncues; ++i){
      for(int j = 0; j < n_nodes; ++j){
        double delta;
        if(j == outcome){
          delta = gamma * (lambda - outcome_hat);
          } else {
          delta = gamma * (0 - outcome_hat);
          }
          Rcout << cues[i] << "\t" << j << "\t" << w(cues[i], j) << "\t" << delta << '\n';
        w(cues[i], j) += delta;
        }
      }
    }
  return w;
  }

