##' Dual Graphs
##' 
##' Constructs the (undirected) dual graph for a mixed graph
##' 
##' @param graph a mixed graph object
##' 
##' @details Uses the skeleton and then inverts the set of edges
##' 
##' @export
dual <- function(graph) {
  n <- length(graph$vnames)
  out <- withAdjMatrix(skeleton(graph))
  out$edges$undirected[] <- (1 - diag(n)) - out$edges$undirected
  
  out
}

