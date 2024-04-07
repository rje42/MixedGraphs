##' @describeIn graph_algorithms find a perfect elimination ordering (quadratic)
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

##' Algorithms for common graphical combinatorial problems
##' 
##' @param graph object of class `mixedgraph`
##' @param v subset of vertices
##' 
##' @details
##' Defaults to the entire graph if `v` is not supplied.  This algorithm applies
##' only to undirected edges.
##' 
##' @describeIn graph_algorithms find a perfect elimination ordering
##' 
##' @export
peo <- function (graph, v) {
  if (missing(v)) v <- graph$v
  else graph <- graph[v]
  
  if (length(v) <= 2) return(v)
  
  # lab <- rep(0,length(v))
  # rk <- seq_along(v)
  w <- v
  
  ## intialize order
  ord <- integer(0)
  i <- length(v)
  
  while (i > 0) {
    cv <- w[1]
    w <- w[-1]
    ord[cv] <- i
    i <- i - 1
    
    nbs <- nb(graph, cv)
    if (length(nbs) == 0) next
    # lab[nbs] <- 
    in_nbs <- w %in% nbs
    w <- w[c(which(in_nbs), which(!in_nbs))]
    graph <- mutilate(graph, cv)
  }
  
  ## return elimination ordering
  peo <- order(ord)
  peo <- peo[seq_along(v)]
  
  return(peo)
}

##' @describeIn graph_algorithms number of edges to add for given ordering
##' 
##' @param any logical, if `TRUE` returns `TRUE` if any edge is added, `FALSE` otherwise
##' 
##' @export
n_fill_in <- function (graph, v, any=FALSE) {
  ## perhaps skip if called from peo()
  graph <- graph[etype="undirected"]
  if (missing(v)) v <- graph$v
  else graph <- graph[v]
  
  ## number of edges to add
  n_add <- 0
  
  nv <- length(v)
  if (nv <= 2) return(list())
  
  test <- rep(FALSE, nv)
  for (i in seq_len(nv-1)) {
    nbs_i <- nb(graph, v[i])
    graph <- graph[-v[i]]
    if (length(nbs_i) < 2) next
    cur <- graph[nbs_i]
    to_add <- choose(length(nbs_i), 2) - nedge(cur)
    n_add <- n_add + to_add
    if (any && to_add > 0) return(TRUE)
  }
  
  if (any) return(FALSE)
  return(n_add)
}
