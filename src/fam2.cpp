#include <Rcpp.h>
#include <string.h>
#include <iostream>
#include <vector>
#include <queue>
#include <unordered_set>
#include "MGgraphs.h"
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
  
  // go through list of types of edge
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
        if (dir <= 0) {
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
        if (dir >= 0) {
          NumericMatrix::Row v_row = edg.row(v2);
          for (int i=0; i < nv; i++) {
            if (v_row[i] == 0) continue;
            auto ins = w.insert(i);
            if (ins.second) {
              qu.push(i);
            }
            checkUserInterrupt();
          }
        }
      }
    }
    else {
      // if the entry is a list, record it
      // Rcout << "this is a list...";
      List edg = edges[i];
      // type[i] = wrap(edg.attr("class"));

      CharacterVector clsv = edg.attr("class");
      std::string cls = (std::string) clsv[0];
      
      // eList not yet supported
      if (cls == estr[3]) {
        stop("eList objects are not supported");
      }
      if (cls != estr[0]) {
        stop("list objects should be 'adjList's");
      }
      
      if (dir <= 0) {
        if (dir == 0) edg = sym_adjList_cpp(edg);
        else if (dir == -1) edg = rev_adjList_cpp(edg);
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

// [[Rcpp::export]]
IntegerVector adj_cpp (List graph, IntegerVector v, int dir) {
  std::unordered_set<int> w;
  std::queue<int> qu;

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
  
  // go through list of types of edge
  for (int i=0; i < ne; i++) {
    // Rprintf("edge list entry %i...", i);
    
    if (Rf_isMatrix(edges[i])) {
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
        
      for (int j=0; j < v.size(); j++) {
        // now look at variables adjacent to jth vertex
        if (dir >= 0) {
          NumericMatrix::Column v_col = edg.column(v[j]-1);
          // Rcout << v_col[0] << ','<< v_col[1] << ','<<  v_col[2] << ','<< v_col[3] << ','<< v_col[4] << '\n';
          for (int i=0; i < nv; i++) {
            if (v_col[i] == 0) continue;
            auto ins = w.insert(i);
            if (ins.second) {
              qu.push(i);
            }
            checkUserInterrupt();
          }
        }
        if (dir <= 0) {
          NumericMatrix::Row v_row = edg.row(v[j]-1);
          // Rcout << v_row[0] << ','<< v_row[1] << ',' <<  v_row[2] << ','<< v_row[3] << ','<< v_row[4] << '\n';
          for (int i=0; i < nv; i++) {
            if (v_row[i] == 0) continue;
            auto ins = w.insert(i);
            if (ins.second) {
              qu.push(i);
            }
            checkUserInterrupt();
          }
        }
      } // for j
    } // Rf_isMatrix
    else {
      List edg = edges[i];
      // type[i] = wrap(edg.attr("class"));
      
      CharacterVector clsv = edg.attr("class");
      std::string cls = (std::string) clsv[0];
      
      // eList not yet supported
      if (cls == estr[3]) {
        stop("eList objects are not supported");
      }
      if (cls != estr[0]) {
        stop("list objects should be 'adjList's");
      }
      
      if (dir <= 0) {
        if (dir == 0) edg = sym_adjList_cpp(edg);
        else if (dir == -1) edg = rev_adjList_cpp(edg);
      }
      
      // go through until queue is exhausted
      for (int j=0; j < v.size(); j++) {
        // now look at variables adjacent to v
        std::vector<int> v_nb = edg[v[j]-1];
        for (int i: v_nb) {
          if (w.count(i-1) == 0) {
            w.insert(i-1);
            qu.push(i-1);
          }
        }
      }
    }
  }  
  IntegerVector out = wrap(w);
  return out;
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


