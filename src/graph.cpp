#include <Rcpp.h>
#include <string.h>
#include <iostream>
#include <vector>
#include <queue>
#include <unordered_set>
using namespace Rcpp;

// [[Rcpp::export]]
List adjMat_to_adjList (NumericMatrix aM) {
  int nv = aM.nrow();
  if (aM.ncol() != nv) stop("Not an adjacency matrix");
  
  List out;
  
  for (int i=0; i < nv; i++) {
    IntegerVector tmp;
    NumericMatrix::Column col = aM.column(i);
    for (int j=0; j < nv; j++) {
      // Rcout << i << ' ' << j << ' '  << aM[j,i] << '\n';
      if (col[j] > 0) tmp.push_back(j+1);
    }
    out.push_back(tmp);
  }
  
  return out;
}

// [[Rcpp::export]]
List rev_adjList_cpp(List adjList) {
  int nv = adjList.length();
  
  std::vector<std::vector<int>> out(nv);
  
  for (int i=0; i < nv; i++) {
    std::vector<int> adj0 = adjList[i];
    for (int j : adj0) out[j-1].push_back(i+1);
  }
  
  List out2;
  
  for (int i=0; i < nv; i++) {
    out2.push_back(out[i]);
  }
  
  return(out2);
}

// [[Rcpp::export]]
List sym_adjList_cpp(List adjList) {
  List radjList = rev_adjList_cpp(adjList);
  List sadjList;
  
  int nv = adjList.length();
  
  for (int i=0; i < nv; i++) {
    std::vector<int> al = adjList[i];
    std::vector<int> ral = radjList[i];
    std::vector<int> out;
    std::unordered_set<int> vals;
    for (auto j : al) {
      // insert entries from original list
      auto nw = vals.insert(j);
      if (nw.second) out.push_back(j);
    }
    for (auto j : ral) {
      // insert entries from reversed list
      auto nw = vals.insert(j);
      if (nw.second) out.push_back(j);
    }
    
    // add this vector to the list
    sadjList.push_back(out);
  }
  
  return(sadjList);
}
