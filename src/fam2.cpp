#include <Rcpp.h>
#include <string.h>
#include <iostream>
#include <vector>
#include <queue>
#include <unordered_set>
using namespace Rcpp;

std::vector<std::string> estr = {"adjList", "adjMatrix", "edgeMatrix", "eList"};

// [[Rcpp::export]]
IntegerVector grp_cpp (List graph, IntegerVector v, int dir) {
  std::unordered_set<int> w;
  std::queue<int> qu;
  int v2;

  // get original number of vertices
  CharacterVector vnms = graph["vnames"];
  int nv = vnms.length();

  // extract list of edges
  List edges = graph["edges"];
  // char vnms[] = graph["vnames"];
  int ne = edges.length();
  char nms = edges.names();
  CharacterVector type[ne];
  
  // Rcout << "Edge types: " << edges.names() << "\n";
  // for (int i=0; i < ne; i++) Rprintf("", nms[0], nms[1]);
  
  // go through list of edges
  for (int i=0; i < ne; i++) {
    // Rprintf("edge list entry %i...", i);

    if (Rf_isMatrix(edges[i])){
      // This is a matrix, so record
      NumericMatrix edg = edges[i];

      // Check class of object
      CharacterVector clsv = edg.attr("class");
      std::string cls = (std::string) clsv[0];
      
      // edgeMatrix not yet supported
      if (cls == estr[2]) {
        stop("edgeMatrix objects are not supported");
      }
      if (edg.nrow() != nv || 
          edg.nrow() != nv) {
        stop("adjacency matrix has wrong number of rows or columns");
      }
      
      // set up problem to include vertices provided
      for (int i : v) {
        qu.push(i-1);
        w.insert(i-1);
      }
      
      // go through until queue is exhausted
      while (!qu.empty()) {     
        v2 = qu.front();
        qu.pop();
        
        // now look at variables adjacent to v2
        NumericMatrix::Column v_col = edg.column(v2);
        for (int i=0; i < nv; i++) {
          if (v_col[i] == 0) continue;
          auto ins = w.insert(i);
          if (ins.second) {
            qu.push(i);
          }
          checkUserInterrupt();
        }
      }
    }
    else {
      // if the entry is a list, record it
      // Rcout << "this is a list...";
      List edg = edges[i];
      type[i] = wrap(edg.attr("class"));
      
      // Rcout << "of class " << type[i] << "\n";
      // for (int i=0; i < nv; i++) {
        // Rcout << "Neighbours of vertex " << i << " are: ";
        // std::vector<int> v_nb2 = edg[i];
        // for (int j=0; j < v_nb2.size(); j++) Rcout << v_nb2[j] << " ";
        // Rcout << "\n";
      // }
      
      // char val[] = "eList";
      if (as<std::string>(edg.attr("class")) == estr[3]) {
        stop("eList objects are not supported");
      }
      
      // set up problem to include vertices provided
      for (int i : v) {
        qu.push(i-1);
        w.insert(i-1);
      }
      
      // go through until queue is exhausted
      while (!qu.empty()) {
        v2 = qu.front();
        qu.pop();

        // now look at variables adjacent to v2
        std::vector<int> v_nb = edg[v2];
        for (int i: v_nb) {
          if (w.count(i-1) == 0) {
            w.insert(i-1);
            qu.push(i-1);
          }
        }
      }
    }
  }
  
  // Return an integer vector
  IntegerVector w2 = wrap(w);
  
  return w2;
}

// // [[Rcpp::export]]
// IntegerVector grp_cpp (List graph, IntegerVector v) {
//   grp_cpp (graph, v, 0L);
// }

// revAdjList <- function(object) {
//   n <- length(object)
//   out <- vector(mode="list", length=n)
//   out[] <- list(integer(0))
//   for (i in seq_len(n)) {
//     out[object[[i]]] <- lapply(out[object[[i]]], function(x) c(x,i))
//   }
//   class(out) <- "adjList"
//   out
// }

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

