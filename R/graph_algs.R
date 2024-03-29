##' Obtain a perfect elimination order for an undirected graph
##' 
##' @param graph object of class `mixedgraph`
##' @param v subset of vertices whose undirected edges should be oriented
##' 
##' @details
##' Defaults to the entire graph if `v` is not supplied.
##' 
##' @export
perfect_elim_order <- function (graph, v) {
  if (missing(v)) v <- graph$v
  
  graph <- graph[v]
  if (length(v) <= 2) return(v)
  
  ## intialize order
  ord <- integer(0)
  
  while (length(ord) < length(v)) { 
    ok <- FALSE
    for (i in setdiff(v, ord)) {
      nbs <- nb(graph, i)
      if (is_complete(graph[nbs])) {
        ## neighbours complete, add vertex to ordering
        ord <- c(ord, i)
        graph <- graph[-i]
        ok <- TRUE
      }
    }
    if (!ok) {
      return(NA)
    }
  }
  
  return(ord)
}