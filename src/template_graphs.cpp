#include <Rcpp.h>
#include <string.h>
#include <iostream>
#include <vector>
#include <queue>
#include <unordered_set>
using namespace Rcpp;

// [[Rcpp::export]]
List complete_mg_cpp(IntegerVector n, IntegerVector m) {
  
  int nv = n[0] + m[0];
  if (nv == 0) {
    List out;
    return out;
  }
  
  IntegerMatrix ud(nv, nv), di(nv, nv), bi(nv, nv);
  
  if (n[0] > 0) {
    // put in undirected edges
    for (int i1 = 1; i1 < n[0]; i1++) for (int i2 = 0; i2 < i1; i2++) {
      ud(i1, i2) = ud(i2, i1) = 1;
    }
  }
  if (m[0] > 0) {
    if (n[0] > 0) {
      // put in directed edges
      for (int i1 = 0; i1 < n[0]; i1++) for (int i2 = n[0]; i2 < nv; i2++) {
        di(i1, i2) = 1;
      }
    } 
   
    // put in bidirected edges
    for (int i1 = n[0]+1; i1 < nv; i1++) for (int i2 = n[0]; i2 < i1; i2++) {
      bi(i1, i2) = bi(i2, i1) = 1;
    }
  }
  
  // set classes appropriately
  ud.attr("class") = "adjMatrix";
  di.attr("class") = "adjMatrix";
  bi.attr("class") = "adjMatrix";
  
  List out = List::create(_["undirected"]=ud, _["directed"]=di, _["bidirected"]=bi);
  out.attr("class") = "edgeList";
  
  return out;
}

// [[Rcpp::export]]
List complete_gr_cpp(IntegerVector n, LogicalVector dir) {
  
  // obtain number of vertices
  int nv = n[0];
  bool dr = dir[0];
  if (nv < 0) Rf_error("Number of vertices must be a non-negative integer");
  
  // list for output
  List out;
  
  for (int i=0; i < nv; i++) {
    IntegerVector tmp;
    if (dr) {
      for (int j=i+2; j < nv+1; j++) {
        // add in all vertices except for the entry being filled
        if (j == i+1) continue;
        tmp.push_back(j);
      }
    }
    else {
      for (int j=1; j < nv+1; j++) {
        // add in all vertices except for the entry being filled
        if (j == i+1) continue;
        tmp.push_back(j);
      }
    }
    out.push_back(tmp);
  }
  
  out.attr("class") = "adjList";
  
  return out;
}

// [[Rcpp::export]]
List chain_gr_cpp(IntegerVector n, LogicalVector dir) {
  
  // obtain number of vertices
  int nv = n[0];
  bool dr = dir[0];
  if (nv < 0) Rf_error("Number of vertices must be a non-negative integer");
  
  // list for output
  List out;
  
  for (int i=0; i < nv-1; i++) {
    IntegerVector tmp;
    if (i > 0 && !dr) {
      // add in all vertices except for the entry being filled
      tmp.push_back(i);
    }
    tmp.push_back(i+2);
    out.push_back(tmp);
  }
  if (nv > 0) {
    IntegerVector tmp;
    if (!dr && nv > 1) {
      tmp.push_back(nv-1);
    }
    out.push_back(tmp);
  }
  
  out.attr("class") = "adjList";
  
  return out;
}

// [[Rcpp::export]]
List cycle_gr_cpp(int n, LogicalVector dir) {
  
  // obtain number of vertices
  bool dr = dir[0];
  if (n < 0) Rf_error("Number of vertices must be a non-negative integer");
  
  // list for output
  List out;
  
  for (int i=0; i < n; i++) {
    IntegerVector tmp;
    // add neibouring vertices for this vertex being filled
    if (!dr & (n > 2)) {
      if (i == 0) tmp.push_back(n);
      else tmp.push_back(i);
    }
    if (n > 1) {
      if (i == n-1) tmp.push_back(1);
      else tmp.push_back(i+2);
    }    
    // add to list
    out.push_back(tmp);
  }

  out.attr("class") = "adjList";
  
  return out;
}

// [[Rcpp::export]]
List bipartite_gr_cpp(int n, int m, LogicalVector dir) {
  int nv = n + m; // Total number of vertices
  
  if (n < 0 || m < 0) Rf_error("Sizes of vertex sets must be non-negative integers");
  
  bool dr = dir[0];
  List out(nv); // List for output
  
  // Create the bipartite graph
  for (int i = 0; i < n; i++) {
    IntegerVector tmp;
    for (int j = n; j < nv; j++) {
      tmp.push_back(j + 1); // Vertices in the second set (1-based index)
    }
    out[i] = tmp;
  }
  
  // for directed edges put in empty vector, otherwise add in list of vertices in first set
  for (int i = n; i < nv; i++) {
    IntegerVector tmp;
    if (!dr) {
      for (int j = 0; j < n; j++) {
        tmp.push_back(j + 1); // Vertices in the first set (1-based index)
      }
    }
    out[i] = tmp;
  }

  
  out.attr("class") = "adjList";
  return out;
}

// [[Rcpp::export]]
List grid_gr_cpp(int n, int m, LogicalVector dir) {
  if (n < 0 || m < 0) Rf_error("Both n and m must be non-negative integers");
  if (n == 0 || m == 0) {
    List out(0);
    out.attr("class") = "adjList";
    return out;
  }
  
  bool dr = dir[0];

  int nv = n * m;
  List out(nv); // List for output

  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < m; ++j) {
      IntegerVector neighbors;

      if (!dr) {
        if (j > 0) neighbors.push_back((j - 1) * n + i + 1);       // (i, j-1)
        if (i > 0) neighbors.push_back(j * n + (i - 1) + 1);        // (i-1, j)
      }
      
      if (i < n - 1) neighbors.push_back(j * n + (i + 1) + 1);    // (i+1, j)
      if (j < m - 1) neighbors.push_back((j + 1) * n + i + 1);    // (i, j+1)
      
      out[i + j * n] = neighbors;
    }
  }

  out.attr("class") = "adjList";
  return out;
} 

// [[Rcpp::export]]
List star_gr_cpp(int n, LogicalVector dir, int cn = -1) {
  if (n == 0) {
    List out(0);
    out.attr("class") = "adjList";
    return out;
  }
  if (n < 0) Rf_error("n must be a non-negative integer");
  
  bool dr = dir[0];
  
  // If cn is not specified or invalid, use the last node as the central node
  if (cn < 1 || cn > n) {
    cn = n;
  }
  
  List out(n); // List for output
  
  for (int i = 0; i < n; ++i) {
    IntegerVector tmp;
    if (i+1 != cn) {
      // for non-central nodes, add central node if appropriate
      if (i+1 < cn) tmp.push_back(cn);
      else if (!dr && i+1 > cn) tmp.push_back(cn);
    }
    else if (i+1 == cn) {
      // for central node add appropriate neighbours
      if (!dr) {
        for (int j=1; j < cn; j++) tmp.push_back(j);
      }
      for (int j=cn+1; j < n+1; j++) tmp.push_back(j);
    }
    
    out[i] = tmp;
  }
  
  out.attr("class") = "adjList";
  return out;
}
