##' Collapse multiple edge types to a single list or matrix
##' 
##' @param edges list of edges to include
##' @param v1,v2 incoming and outgoing vertices, defaults to all.
##' @param dir direction
##' @param matrix logical indicating whether to force the return of an adjacency matrix
##' 
##' @details returns an edgeMatrix or adjacency matrix for possibly multiple edge types.
##' If any of the edges are specified as an adjacency matrix, then the output will also
##' be an adjacency matrix.
##' 
##' @export collapse
collapse <- function(edges, v1, v2, dir=1, matrix=FALSE) {
  ## repeat direction with warning if necessary
  if (length(edges) == 0 || length(unlist(edges))==0) return(matrix(NA, 2, 0))
  dir <- dir*rep(1L, length(edges))
  
  all1 <- all2 <- FALSE
  
  rmv <- sapply(edges, is.null)
  edges <- edges[!rmv]
  dir <- dir[!rmv]
  
  isAMat <- sapply(edges, is.adjMatrix)
  ## vertices not specified, use all
  if (missing(v1) || missing(v2)) {
    if (any(isAMat)) {
      i <- which(isAMat)[1]
      nv <- nrow(edges[[i]])
    }
    else {
      nv <- max(unlist(edges))
#      if (is.infinite(nv)) return(matrix(NA, 2, 0))
    }
    if(missing(v1)) {
      v1 <- seq_len(nv)
      all1 <- TRUE
    }
    if(missing(v2)) {
      v2 <- seq_len(nv)
      all2 <- TRUE
    }
  }
    
  ## if any representation is an adjacency matrix, or if forced, 
  ## then use this representation
  if (any(isAMat) || matrix) {
    ## could speed this up by not converting edge lists
    jointMat <- matrix(0, length(v1), length(v2))
    for (i in seq_along(edges)) {
      if (!isAMat[i]) edges[[i]] <- adjMatrix(edges[[i]], n=nv, directed=dir[i])
      if (dir[i] >= 0) jointMat <- jointMat + edges[[i]][v1, v2]
      if (dir[i] <= 0) jointMat <- jointMat + t(edges[[i]][v2, v1])
    }
    return(pmin(jointMat, 1))
  }
  ## otherwise use edgeMatrices
  edges <- lapply(edges, edgeMatrix)
  ## reverse edges for direction =0,-1.  
  edges[dir < 0] <- lapply(edges[dir < 0], function(x) x[2:1,,drop=FALSE])
  edges <- c(edges, lapply(edges[dir == 0], function(x) x[2:1,,drop=FALSE]))

  ## shortcut for all vertices, saves time
   if (all1 & all2) {
     jointEM <- do.call(cbind, edges)
     return(jointEM)
   }

  jointEM <- matrix(NA, ncol=0, nrow=2)
  for (i in seq_along(edges)) {
      wh <- (edges[[i]][1,] %in% v1) & (edges[[i]][2,] %in% v2)
      jointEM <- cbind(jointEM, edges[[i]][,wh])
  }
  return(jointEM)
}

##' Find adjacent vertices
##' 
##' Generic function for finding adjacent vertices based on any kind of edge.
##' 
##' @param graph an object of class \code{mixedgraph}
##' @param v vertices to find adjacencies
##' @param etype edge types to consider; defaults to all
##' @param directed for directed edges, indicates which direction to search in:
##' 1: along direction, -1: against direction, 0: both directions.
##' @param inclusive logical indicating whether elements of \code{v} can be 
##' included in output group.
##' @param sort integer:1 for unique but unsorted, 2 for 
##' sorted (0 for possibly repeated and unsorted).  If edges are stored as a matrix
##' then output will always be unique and sorted.
##' @param force logical - should invalid \code{v} be ignored?
##' 
##' @details The argument \code{directed} is recycled for multiple edge types, but 
##' has no effect for edges without a specified direction.  If any \code{v} is 
##' not a vertex of \code{graph}, an error is returned, unless \code{force=TRUE}.
##' 
##' @export adj
##' 
##' @seealso \code{\link{grp}} for paths
##' 
adj <- function(graph, v, etype, dir=0, inclusive=TRUE, sort=1, force=FALSE) {
  ## if no edge type specified, use all available types
  if (!is.mixedgraph(graph)) stop("'graph' should be an object of class 'mixedgraph'")
  
  ## only include vertices in the graph
  if (force) v <- intersect(v, graph$v)
  else if (any(!(v %in% graph$v))) stop("Invalid values of v")
  
 # if (missing(etype)) etype <- edgeTypes()$type
  if (missing(etype)) etype <- names(graph$edges)
  ##' repeat dir() vector with warning if necessary
  if (length(dir) > length(etype)) warning("More directions specified than edge types")
  dir = dir*rep.int(1L, length(etype))
  
  tmp <- pmatch(etype, edgeTypes()$type)
  dir[!edgeTypes()$directed[tmp]] <- 0L
  
  whEdge <- pmatch(etype,names(graph$edges))
  
  edges <- graph$edges[whEdge]
  n <- length(graph$v)
  
  ## special case of one adjacency matrix to save time
  if (length(edges) == 1 && is.adjMatrix(edges[[1]], n)) {
    es <- edges[[1]][v,,drop=FALSE]
    n <- ncol(es)
    if (dir == 0) es <- es + t(edges[[1]][,v,drop=FALSE])
    else if (dir == -1) es <- t(edges[[1]][,v,drop=FALSE])
    return(which(.colSums(es, nrow(es), n) > 0))
  }
  
  es <- collapse(edges, v1=v, dir=dir)
  
  ## if an adjacency matrix
  if (any(es == 0)) {
    d <- dim(es)
    return(which(.colSums(es, d[1], d[2]) > 0))
  }
  ## otherwise take bottom row of es
  if (ncol(es) == 0) return(integer(0))
  out = es[2,]  
  if (sort > 0) out <- unique.default(out)
  if (sort > 1) out <- sort.int(out)
  if (!inclusive) out = setdiff(out, v)
  return(out)

  out
}

##' Group vertices by adjacencies
##' 
##' Generic functions for finding groups based on edge adjacencies, 
##' e.g. neighbourhoods, districts or ancestral sets.
##' 
##' @param graph an object of class \code{mixedgraph}
##' @param v vertices to group
##' @param etype edge types to group by (defaults to all)
##' @param inclusive logical indicating whether \code{v} should be 
##' included in output group.
##' @param dir integer(s) indicating if direction should be 
##' considered: 0 = undirected, 1 = from row to column; -1 = from column to row.
##' @param sort integer: 1 for unique but unsorted, 2 for 
##' sorted (0 for possibly repeated and unsorted).
##' @param force logical - should invalid \code{v} be ignored?
##' 
##' @details \code{grp()} finds all vertices that can be reached from
##' vertices in \code{v} by edges of the specified type, and in the 
##' specified direction.
##' For example, we can find all the ancestors of \code{v} by using 
##' \code{type="directed"} and \code{dir=-1} (i.e.\ go back up the edges).
##' 
##' \code{groups()} finds equivalence classes in a graph based on being 
##' connected by such a path (i.e.\ the connected components with respect 
##' to the specified edge types).
##' 
##' If any \code{v} is 
##' not a vertex of \code{graph}, an error is returned, unless 
##' \code{force=TRUE}.
##' 
##' @export grp
##' @seealso \code{\link{adj}} for single edge adjacencies.
grp <- function(graph, v, etype, inclusive=TRUE, dir=0, sort=1, force=FALSE) {
  if (!is.mixedgraph(graph)) stop("'graph' should be an object of class 'mixedgraph'")
  
  ## only include vertices in the graph
  if (force) v <- intersect(v, graph$v)
  else if (any(!(v %in% graph$v))) stop("Invalid values of v")

  if (length(v) == 0) return(integer(0))
  if (missing(etype)) etype <- edgeTypes()$type
  ##' repeat dir() vector with warning if necessary
  if (length(dir) > length(etype)) warning("More directions specified than edge types")
  dir = dir*rep.int(1L, length(etype))
  
  tmp <- pmatch(etype, edgeTypes()$type)
  dir[!edgeTypes()$directed[tmp]] <- 0L
  
  whEdge <- pmatch(etype,names(graph$edges))
  
  edges <- graph$edges[whEdge]  
  es <- collapse(edges, dir=dir)
  
  #   tmp = lapply(graph$edges[etype], edgeMatrix)
  #   if (!is.null(tmp)) {
  #     es = do.call(cbind, tmp)
  #   }
  #   else es = matrix(NA, ncol=0, nrow=2)
  
  if (inclusive) out = v
  else out = integer(0)
  
  if (any(es == 0)) {
    continue = TRUE
    new = v
    
    while (continue) {
      out2 = which(colSums(es[out, , drop=FALSE]) > 0)
      new = setdiff(out2, out)
      es[out,] = 0L
      out <- c(out, new)
      
      continue = (length(new) > 0)
    }
  }
  else {
    if (isTRUE(ncol(es) > 0)) {
      continue = TRUE
      new = v
      
      while (continue) {
        out2 = es[2, es[1,] %in% new]
        
        new = unique.default(out2[!(out2 %in% out)])
        out = c(out, new)
        continue = (length(new) > 0)
      }
    }
  }
  if (sort > 1) out = sort.int(out)
  
  out
}

##' @export groups
##' @describeIn grp find equivalence classes
groups = function(graph, etype) {
  if (!is.mixedgraph(graph)) stop("'graph' should be an object of class 'mixedgraph'")
  if (missing(etype)) etype <- edgeTypes()$type

  ##' repeat dir() vector with warning if necessary
  whEdge <- pmatch(etype,names(graph$edges))
  edges <- graph$edges[whEdge[!is.na(whEdge)]]
  es <- collapse(edges, dir=0)
  
  grp = rep(0,length(graph$vnames))
  grp[graph$v] = seq_along(graph$v)  
  
  if (any(es == 0)) {
    for (i in seq_len(ncol(es))) {
      g1 = grp[i]
      g2s = grp[which(es[,i] > 0)]
      grp[grp %in% g2s] = g1
    }
  }
  else {
    for (i in seq_len(ncol(es))) {
      g1 = grp[es[1,i]]
      g2 = grp[es[2,i]]
      if (g1 == g2) next
      grp[grp == g2] = g1
    }
  }
  
  out = list()
  for (i in unique.default(grp[grp>0])) {
    out = c(out, list(which(grp==i)))
  }
  
  out
}

##' Familial Mixed Graph Relations
##' 
##' The usual familial relations between vertices in
##' mixed graphs.
##' 
##' @aliases ch
##' @param graph \code{mixedgraph} object
##' @param v collection of vertices
##' @param sort integer:1 for unique but unsorted, 2 for 
##' sorted (0 for possibly repeated and unsorted).  If edges are stored as a matrix
##' then output will always be unique and sorted.
##' 
##' @details \code{pa}, \code{ch}, \code{sp} and \code{nb} find the 
##' parents, children, spouses and neighbours of \code{v} respectively.
##' \code{anc}, \code{dec}, \code{ant}, \code{dis} finds the ancestors
##' descendants, anterior and district of \code{v} respectively.
##' 
##' @export pa
pa = function(graph, v, sort=1) {
  adj(graph, v, etype="directed", dir=-1, inclusive=TRUE, sort=sort)
}

##' @describeIn pa find children of vertices
##' @export ch 
ch = function(graph, v, sort=1) {
  adj(graph, v, etype="directed", dir=1, inclusive=TRUE, sort=sort)
}

##' @describeIn pa find spouses of vertices
##' @export sp
sp = function(graph, v, sort=1) {
  adj(graph, v, etype="bidirected", dir=0, inclusive=TRUE, sort=sort)
}

##' @describeIn pa find undirected neighbours of vertices
##' @export nb
nb = function(graph, v, sort=1) {
  adj(graph, v, etype="undirected", dir=0, inclusive=TRUE, sort=sort)
}

##' @describeIn pa find ancestors of vertices
##' @export anc
anc = function(graph, v, sort=1) {
  grp(graph, v, etype="directed", dir=-1, sort=sort)
}

##' @describeIn pa find descendants of vertices
##' @export dec
dec = function(graph, v, sort=1) {
  grp(graph, v, etype="directed", dir=1, sort=sort)
}

##' @describeIn pa find anterior vertices
##' @export ant
ant = function(graph, v, sort=1) {
  grp(graph, v, etype=c("directed", "undirected"), dir=c(-1,0), sort=sort)
}

##' @describeIn pa find district of vertices
##' @export dis
dis = function(graph, v, sort=1) {
  grp(graph, v, etype="bidirected", dir=0, sort=sort)
}

##' @export districts
districts = function(graph) {
  groups(graph, etype="bidirected")
}

##' @export un
un <- function(graph, sort=1) {
  out <- unlist(graph$edges$undirected)
  if (length(out) == 0) return(integer(0))
  if (sort > 0) out <- unique.default(out)
  if (sort > 1) out <- sort.int(out)
  out
}

##' @export neighbourhoods
neighbourhoods = function(graph) {
  groups(graph[un(graph)], etype="undirected")
}

##' Find Markov blanket
##' 
##' Find the Markov blanket for a vertex in an 
##' ancestral set
##' 
##' @param graph \code{mixedgraph} object
##' @param v a vertex, childless in \code{A}
##' @param A an ancestral collection of vertices
##' @param sort integer:1 for unique but unsorted, 2 for 
##' sorted (0 for possibly repeated and unsorted).  If edges are stored as a matrix
##' then output will always be unique and sorted.
##' 
##' @details Finds the Markov blanket of \code{v} in \code{A}.
##' 
##' @export mb
mb = function(graph, v, A, check=TRUE, sort=1) {
  if (!is.mixedgraph(graph)) stop("'graph' should be an object of class 'mixedgraph'")
  if (missing(A)) A <- graph$v
  
  if (!(v %in% A)) stop("v must be a member of A")
  
  if (check) {
    A2 <- pa(graph, A)
    if (!all(A2 %in% A)) {
      stop("Error: A is not ancestral")
    }
  }
  
  # consider subgraph over A
  graph <- graph[A]
  if (length(ch(graph, v)) > 0) stop("v is not childless in A")

  ## get Markov blanket  
  D <- dis(graph, v)
  out <- c(pa(graph, D), D)
  
  if (sort == 1) {
    out <- unique.default(out)
  }
  else if (sort == 2) {
    out <- sort.default(unique.default(out))
  }
  
  out
}

##' Find barren, sterile, orphaned vertices
##' 
##' @param graph an object of class \code{mixedgraph}
##' @param v set of vertices of \code{graph}
##' 
##' @details Barren vertices (within \code{v}) are those
##' that have no proper descendants also within \code{v}.
##' Sterile (orphaned) vertices in \code{v} have no 
##' children (parents) also within \code{v}.
##' 
##' Need to think about whether thse
##' do the right thing for \code{v} a proper subset of the 
##' vertices
##' 
##' @export barren
barren = function (graph, v = graph$v) {
  if (length(v) == 0) return(integer(0))
  if (length(v) == length(graph$v)) {
    ans = adj(graph, v, etype="directed", dir=-1)
    ans <- setdiff(v, ans)
  }
  else {
    ans <- v
    for (i in seq_along(v)) {
      if (ans[i] == 0) next
      tmp <- setdiff(anc(graph, v[i]), v[i])
      ans[ans %in% tmp] = 0
    }
    ans <- ans[ans > 0]
  }
  
  return(ans)
}

##' @describeIn barren find vertices with no parents
##' @export orphaned
orphaned = function (graph, v = graph$v) {
  if (length(v) == 0) return(integer(0))
  ans = adj(graph, v, etype="directed", dir=1)
  
  return(setdiff(v, ans))
}

##' @export sterile
##' @describeIn barren find vertices with no children in the same set
sterile = function(graph, v=graph$v){
  pas = pa(graph, v)
  sterile = setdiff(v, pas)
  
  sterile
}

##' Graph skeleton
##' 
##' Find undirected skeleton of a mixed graph
##' 
##' @export skeleton
skeleton = function(graph) {
  if (!is.mixedgraph(graph)) stop("'graph' should be an object of class 'mixedgraph'")
  # e = lapply(unlist(graph$edges, recursive=FALSE), sort.int)
  # e = unique(e)
  e = collapse(graph$edges)
  out = mixedgraph(v=graph$v, edges=list(undirected=e), vnames=graph$vnames)
  return(out)
}

##' Find ancestral sets of a graph.
##'
##' @param graph object of class \code{mixedgraph}, should be a summary graph
##' @param topOrder optional topological order of vertices
##' @param sort integer:1 for unique but unsorted, 2 for 
##' sorted (0 for possibly repeated and unsorted). 
##'
##' @details Algorithm:
##' 1. Find a topological order of nodes.
##' 2. Base case: {} is ancestral
##' 3. Induction: (i) Assume we have a list L of all ancestral sets involving Xi-1 in the order.
##' (ii) If an ancestral set S in L contains all parents of Xi, Xi + S is also ancestral.
##' 
##' @author Ilya Shpitser
##' 
##' @export anSets
anSets = function(graph, topOrder) {
  if (length(graph$v) <= 1) return(list(graph$v))
  out = list(integer(0))
  if (missing(topOrder)) topOrder <- topologicalOrder(graph)

  for(node in topOrder){
    parents <- pa(graph, node)
    additions <- list()
    
    for(set in out){
      if(length(parents) == 0 || all(parents %in% set)){
        additions <- c(additions, list(c(set, node)))
      }
    }
    out <- c(out, additions)
  }
  
  out[-1]
}
