#include <Rcpp.h>
using namespace Rcpp;

List adjMat_to_adjList (NumericMatrix aM);
List rev_adjList_cpp(List adjList);
List sym_adjList_cpp(List adjList);
