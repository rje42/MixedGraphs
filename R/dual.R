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
  n <- nv(graph)
  out <- withAdjMatrix(skeleton(graph), force=TRUE)
  out$edges$undirected[graph$v, graph$v] <- (1 - diag(n)) - out$edges$undirected[graph$v, graph$v]
  
  out
}

