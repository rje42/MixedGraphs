#include <Rcpp.h>
#include <string.h>
#include <iostream>
#include <vector>
#include <queue>
#include <unordered_set>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix add_edges_aM (NumericMatrix aM, IntegerVector e1, IntegerVector e2, int dir) {
  if (e1.size() != e2.size()) stop("index vectors must be the same length");
  
  NumericMatrix aM2 = clone(aM);
  
  for (int i=0; i < e1.size(); i++) {
    if (dir >= 0) aM2(e1[i]-1, e2[i]-1) = 1;
    if (dir <= 0) aM2(e2[i]-1, e1[i]-1) = 1;
  }
  
  return aM2;
}

// [[Rcpp::export]]
List add_edges_aL (List aL, IntegerVector e1, IntegerVector e2, int dir) {
  if (e1.size() != e2.size()) stop("index vectors must be the same length");
  
  List aL2 = clone(aL);
  
  // go through, add each edge in turn
  for (int i=0; i < e1.size(); i++) {
    // copy
    // Rcout << i << ": " << e1[i] << "," << e2[i] << "\n";
    std::vector<int> v1 = aL2[e1[i]-1];
    std::vector<int> v2 = aL2[e2[i]-1];
    
    // add only if not already present
    if (dir >= 0 && std::find(v2.begin(), v2.end(), e1[i]-1) == v2.end()) {
      // if not already in v2, add it
      v2.push_back(e1[i]);
    }
    if (dir <= 0 && std::find(v1.begin(), v1.end(), e2[i]-1) == v1.end()) {
      // if not already in v1, add it
      v1.push_back(e2[i]);
    }
    
    // Rcout << "v1 = "  << v1[v1.size()-1] << "\n";
    // Rcout << "v2 = "  << v2[v2.size()-1] << "\n";
    
    aL2[e1[i]-1] = v1;
    aL2[e2[i]-1] = v2;
    
    // if (dir >= 0) {
    //   vl2.insert(e1[i]-1);
    // }
    // if (dir <= 0) {
    //   vl1.insert(e2[i]-1);
    // }
  }
  
  return aL2;
}

// [[Rcpp::export]]
List rmv_edges_aL (List aL, IntegerVector e1, IntegerVector e2, int dir) {
  if (e1.size() != e2.size()) stop("index vectors must be the same length");
  
  List aL2 = clone(aL);
  
  // go through, remove each edge in turn
  for (int i=0; i < e1.size(); i++) {
    // copy
    // Rcout << i << ": " << e1[i] << "," << e2[i] << "\n";
    std::vector<int> v1 = aL2[e1[i]-1];
    std::vector<int> v2 = aL2[e2[i]-1];
    
    // remove any cases present
    if (dir >= 0) { 
      v2.erase(remove(v2.begin(), v2.end(), e1[i]), v2.end());
    }
    if (dir <= 0) { 
      v1.erase(remove(v1.begin(), v1.end(), e2[i]), v1.end());
    }
    
    aL2[e1[i]-1] = v1;
    aL2[e2[i]-1] = v2;
  }
  
  return aL2;
}

// [[Rcpp::export]]
NumericMatrix rmv_edges_aM (NumericMatrix aM, IntegerVector e1, IntegerVector e2, int dir) {
  NumericMatrix aM2 = clone(aM);
  
  // remove each edge in turn
  for (int i=0; i < e1.size(); i++) {
    if (dir >= 0) aM2(e1[i]-1, e2[i]-1) = 0;
    if (dir <= 0) aM2(e2[i]-1, e1[i]-1) = 0;
  }
  
  return aM2;
}


// [[Rcpp::export]]
List chg_ends_cpp (NumericMatrix m1, NumericMatrix m2, NumericVector v1, NumericVector v2, bool d2) {
  std::vector<int> done;
  // Rcout << v1.size() << ',' << v2.size() << '\n';
  if (!Rf_isMatrix(m1) || !Rf_isMatrix(m2)) stop("'m1' and 'm2' must be matrices");
  else if (m1.ncol() != m1.nrow()) stop("'m1' not an adjacency matrix");
  else if (m2.ncol() != m2.nrow()) stop("'m2' not an adjacency matrix");
  else if (m1.ncol() != m2.nrow()) stop("'m1' and 'm2' have different dimensions");
  else if (v1.size() != v2.size()) stop("'v1' and 'v2' have different lengths");
  
  for (int i : seq_along(v1)) {
    int v = v1[i-1]-1; int w = v2[i-1]-1;
    // Rcout << i << ' ' << v << ' ' << w << '\n';
    
    if (m1(v,w) > 0) {
      m1(v,w) = m1(w,v) = 0;
      m2(v,w) = 1;
      if (!d2) m2(w,v) = 1;
      done.push_back(i);
    }
  }
  
  List out = List::create(Named("m1") = m1, 
                          _["m2"] = m2, 
                          _["done"] = done);
  
  return out;
}
