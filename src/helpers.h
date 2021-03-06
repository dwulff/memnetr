#ifndef __UTILITIES__
#define __UTILITIES__


//////////////////////////////////////////////////////////////////////////////
//
//          GENERIC UTILITIES
//
//////////////////////////////////////////////////////////////////////////////

// find integer
inline bool find_int(std::vector<int> vec, int el){
  return std::find(vec.begin(), vec.end(), el) != vec.end();
}

// cumulative sum
inline Rcpp::NumericVector csum(Rcpp::NumericVector x){
  int i, j, n = x.size();
  double cp = 0;
  Rcpp::NumericVector cumx(n);
  for(i = 0; i < n; i++){
    for(j = 0; j <= i; j++){
      cp += double(x[j]);
      }
    cumx[i] = cp;
    cp = 0;
    }
  return cumx;
  }

// create diagonal matrix
// with two types of entries for diagnoal and non-diagnoal entries
inline Rcpp::NumericMatrix diagmat(int g, double pout, double pin){
  int i,j;
  Rcpp::NumericMatrix dm(g,g);
  for(i = 0; i < g; i++){
    for(j = 0; j < g; j++){
      if(i == j){
        dm(i,j) = pin;
        } else {
        dm(i,j) = pout;
        }
      }
    }
  return(dm);
  }


// get rank ????
inline double prbs(Rcpp::NumericVector x, double p){
  int i = 0;
  double rp = x[0], n = double(x.size());
  while(rp <= p){
    rp += double(x[i]);
    i++;
    }
  return double(i) / n;
  }

// dunno what this does
inline double trm(Rcpp::NumericVector x, Rcpp::NumericVector y){
  int i,n,nx = x.size(),ny = y.size();
  double p, d, dmax = 0;
  Rcpp::NumericVector cum, cumx = csum(x), cumy = csum(y);
  if(nx > ny) {
    n = nx;
    cum = cumx;
    } else {
    n = ny;
    cum = cumy;
    }
  for(i = 1; i < n; i++){
    p = cum[i];
    d = std::abs(prbs(cumx,p) - prbs(cumy,p));
    if(d > dmax) dmax = d;
    }
  return dmax;
  }

// random integer
inline int rndint(int n){
  return rand() % n;
  }

// allocate
inline Rcpp::NumericVector allct(int n, int g, int mx){
  int i = 0, grs;
  int total = n - g;
  if(mx < 1) mx = 1;
  Rcpp::NumericVector sizes(g);
  while( i < (g-1) ){
    if(total > 0){
      if(total >= mx) {
        grs = rndint(mx-1);
        sizes[i] = grs + 1;
        } else {
        grs = rndint(total-1);
        sizes[i] = grs + 1;
        }
      total = total - grs;
      } else {
      sizes[i] = 1;
      }
    i++;
    }
  if(total > 0){
    sizes[i] = total + 1;
    } else {
    sizes[i] = 1;
    }
  return sizes;
  }

// draw random integer between 0 and (n-1)
int rint(int n);

// draw random from uniform [0,1]
double runi();

// sampl according to vector of values (interpreted as proportional to probability)
inline int smpl(std::vector<double> ps){
  int k, i = 0, n = ps.size();
  double v, sum = 0, r = double(std::rand()) / RAND_MAX;
  for(k = 0; k < n; k++){
    sum += ps[k];
    }
  v = ps[0] / double(sum);
  while(i < n && v <= r){
    i++;
    v += ps[i] / double(sum);
    }
  return i;
  }

// unique
std::vector<int> unique_int(std::vector<int> v);


//////////////////////////////////////////////////////////////////////////////
//
//          GRAPH UTILITIES
//
//////////////////////////////////////////////////////////////////////////////

// convert integer vector into Character vector
// [[Rcpp::export]]
inline Rcpp::CharacterVector tostring(std::vector<int> items){
  int n = items.size();
  Rcpp::CharacterVector res(n);
  for(int i = 0; i < n; i++) {
    res[i] = std::to_string(items[i]);
    }
  return res;
  }


// join two integer vectors
inline std::vector<int> join_int(std::vector<int> a, std::vector<int> b){
  std::vector<int> ab;
  ab.reserve( a.size() + b.size() ); // preallocate memory
  ab.insert( ab.end(), a.begin(), a.end() );
  ab.insert( ab.end(), b.begin(), b.end() );
  return ab;
  }

// reduce integer vector to unique items
//std::vector<int> unique_int(std::vector<int> v)


//////////////////////////////////////////////////////////////////////////////
//
//          SEARCH UTILITIES
//
//////////////////////////////////////////////////////////////////////////////

// test if element is in set
inline bool inset(int el, std::vector<int> set){
  return std::find(set.begin(), set.end(), el) != set.end();
  }

// test if element is in set
inline bool inset_str(std::string el, std::vector<std::string> set){
  return std::find(set.begin(), set.end(), el) != set.end();
  }

// test if element is in set
inline bool inset_strset(std::string el, std::set<std::string> set){
  return set.find(el) != set.end();
  }

// test if edge is in pair
inline bool inmap_2int(int el, std::map<int, int> m){
  return m.find(el) != m.end();
  }

//////////////////////////////////////////////////////////////////////////////
//
//          GONI UTILITIES
//
//////////////////////////////////////////////////////////////////////////////

// n over k
double noverk(int n, int k);

#endif // __UTILITIES__
