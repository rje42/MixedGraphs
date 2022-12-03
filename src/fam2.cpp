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
  // char nms = edges.names();
  CharacterVector type(ne);
  
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


