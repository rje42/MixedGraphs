##' Algorithms for common graphical combinatorial problems
##' 
##' @param graph object of class `mixedgraph`
##' @param v subset of vertices
##' 
##' @name graph_algorithms
NULL

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

##' @param po optional partial ordering on elimination order
##' 
##' @details
##' Defaults to the entire graph if `v` is not supplied.  This algorithm applies
##' only to undirected edges.
##' 
##' @describeIn graph_algorithms find a perfect elimination ordering
##' 
##' @export
peo <- function (graph, v, po) {
  if (missing(v)) v <- graph$v
  else graph <- graph[v]
  
  if (missing(po)) {
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
  }
  else {
    if (length(po) != length(v)) stop("Must be an entry in partial order for each vertex")
    if (!all(po > 0)) stop("All entries to 'po' should be positive integers")
    if (length(v) <= 1) return(v)  # nothing to do
    
    ## get position of each vertex in the vector v
    key_v <- match(seq_len(max(v)), v)
    
    ## intialize order
    ord <- integer(0)
    k <- max(po)  # maybe test that other integers are represented?
    w <- v
    i <- length(v)
    
    while (k > 0) {
      w2 <- w[po[key_v[w]] == k]
      
      while (length(w2) > 0) {
        cv <- w2[1]
        w2 <- w2[-1]
        w <- setdiff(w, cv)
        ord[cv] <- i
        i <- i - 1
        
        nbs <- nb(graph, cv)
        if (length(nbs) == 0) next
        
        # reorder later vertices
        in_nbs <- w %in% nbs
        w <- w[c(which(in_nbs), which(!in_nbs))]
        in_nbs2 <- w2 %in% nbs
        w2 <- w2[c(which(in_nbs2), which(!in_nbs2))]
        
        # remove edges adjacent to vertex just added
        graph <- mutilate(graph, cv)
      }
      k <- k - 1
    }
  }

  ## return elimination ordering
  if (any(duplicated(na.omit(ord)))) stop("Vertices duplicated in order")
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
  if (nv <= 2) return(0L)
  
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


##' Bron-Kerbosch Algorithm
##' 
##' @param R,P,X current largest complete set, sets of vertices still to consider, and already considered
##' @param nbs list of neighbouring vertices
##' @param max_len maximum length of output
##' 
BK <- function(R, P, X, nbs, max_len=length(P)) {
  
  ## if nothing else to add, then return R
  if (length(R) == max_len || (length(P) == 0 && length(X) == 0)) {
    # print(R)
    return(list(R))
  }
  
  ## otherwise, make a list
  out <- list()
  
  for (v in P) {
    ## add each vertex in turn
    nb_v <- nbs[[v]]
    out <- c(out, BK(c(R,v), intersect(P, nb_v), intersect(X, nb_v), nbs=nbs, 
                     max_len=max_len))
    P <- setdiff(P, v)
    X <- c(X, v)
  }
  
  # return list of cliques found
  return(out)
}

##' Obtain isomorphisms from one graph to another
##' 
##' List mappings that map one undirected graph onto another
##' 
##' @param graph1,graph2 two graphs to consider
##' @param use_skel logical indicating whether to take the skeleton
##' 
##' @importFrom igraph graph.get.isomorphisms.vf2 degree
##' @importFrom combinat permn
##' 
##' @export
list_isomorphisms <- function (graph1, graph2, use_skel=TRUE) {
  if (use_skel) {
    graph1 <- skeleton(graph1)
    graph2 <- skeleton(graph2)
  }
  
  nund <- edgeTypes()$type[edgeTypes()$type != "undirected"]
  if (nedge(graph1[edges=nund]) > 0 || nedge(graph2[edges=nund]) > 0) {
    stop("All edges should be undirected")
  }
  
  if (requireNamespace("igraph")) {
    g1 <- convert(graph1, format="igraph")
    g2 <- convert(graph2, format="igraph")
    
    out <- igraph::graph.get.isomorphisms.vf2(g1, g2)
    out <- lapply(out, as.numeric)
  }
  else {
    stop("'igraph' must be installed to count isomorphisms")
    # ## check that this works!
    # A <- graph1$edges$undirected
    # 
    # deg <- igraph::degree(graph1)
    # toTry <- combinat::permn(igraph::vcount(graph1))
    # kp <- sapply(toTry, function(x) all(deg == deg[x]))
    # toTry <- toTry[kp]
    # 
    # kp <- logical(length(toTry))
    # 
    # for (i in seq_along(toTry)) {
    #   A2 <- graph2$edges$undirected[toTry[[i]],toTry[[i]]]
    #   kp[i] <- all(A==A2)
    # }
    # 
    # out <- toTry[kp]
  }
  
  return(out)
}
