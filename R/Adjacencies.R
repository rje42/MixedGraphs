##' Collapse multiple edge types to a single list or matrix
##' 
##' @param edges list of edges to include
##' @param v1,v2 incoming and outgoing vertices, defaults to all.
##' @param dir direction
##' @param matrix logical indicating whether to force the return of an adjacency matrix
##' @param sparse should we use sparse matrices if available?
##' @param sort 1=unique but not sorted, 2=unique and sorted, 0=neither
##' 
##' @details returns an edgeMatrix or adjacency matrix for possibly multiple edge types.
##' If any of the edges are specified as an adjacency matrix, then the output will also
##' be an adjacency matrix, and similarly for an edgeMatrix. 
##' 
##' If \code{v1} or \code{v2} are specified then the first and 
##' second vertices must belong to these respective sets. 
##' 
##' @export collapse
collapse <- function(edges, v1, v2, dir=1, matrix=FALSE, sparse=FALSE, sort=1) {
  ## repeat direction with warning if necessary
  if (length(edges) == 0 || length(unlist(edges))==0) return(matrix(NA, 2, 0))
  dir <- dir*rep(1L, length(edges))
  
  all1 <- all2 <- FALSE
  
  rmv <- sapply(edges, is.null)
  edges <- edges[!rmv]
  dir <- dir[!rmv]
  
  ### first look for objects of class adjMatrix and adjList
  isAMat <- sapply(edges, is.adjMatrix)
  isAList <- sapply(edges, is.adjList, checknm=TRUE)

  ## vertices not specified, use all
  if (missing(v1) || missing(v2)) {
    if (any(isAMat)) {
      i <- which(isAMat)[1]
      nv <- nrow(edges[[i]])
    }
    else if (any(isAList)) {
      i <- which(isAList)[1]
      nv <- length(edges[[i]])
    }
    else {
      ## just look for biggest vertex
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
    if (length(v1) > 0) vr1 <- seq_len(nv)[-v1]
    else vr1 <- seq_len(nv)
    if (length(v2) > 0) vr2 <- seq_len(nv)[-v2]
    else vr2 <- seq_len(nv)
    vr <- intersect(vr1, vr2)
  }
  
  ## if any representation is an adjacency matrix, or if forced, 
  ## then use this adjMatrix
  if (any(isAMat) || matrix) {
    ## could speed this up by not converting edge lists
    if (sparse) {
      jointMat <- Matrix(0, length(v1), length(v2))
    }
    else jointMat <- matrix(0, length(v1), length(v2))
    
    for (i in seq_along(edges)) {
      if (!isAMat[i]) edges[[i]] <- adjMatrix(edges[[i]], n=nv, directed=dir[i], sparse=sparse)
      if (dir[i] >= 0) jointMat <- jointMat + edges[[i]][v1, v2]
      if (dir[i] <= 0) jointMat <- jointMat + t(edges[[i]][v2, v1])
    }
    jointMat <- pmin(jointMat, 1)
    class(jointMat) <- c("adjMatrix", class(jointMat))
    
    return(jointMat)
  }
  
  ## if everything is an adjList, then use this representation
  if (all(isAList)) {
    for (i in seq_along(edges)) {
      if (dir[i] == 1) {
        edges[[i]][vr2] <- vector(mode="list", length=length(vr2))
        edges[[i]] <- lapply(edges[[i]], function(x) intersect(x,v1))
      }
      else if (dir[i] == 0) {
        edges[[i]][vr] <- vector(mode="list", length=length(vr))
        edges[[i]] <- lapply(edges[[i]], function(x) intersect(x,c(v1,v2)))
      }
      else if (dir[i] == -1) {
        edges[[i]][vr1] <- vector(mode="list", length=length(vr1))
        edges[[i]] <- lapply(edges[[i]], function(x) intersect(x,v2))
      }
    }
    out <- unlist(purrr::transpose(edges), recursive = FALSE)
    
    if (sort > 0) {
      out <- lapply(out, unique.default)
      if (sort > 1) out <- lapply(out, sort.int)
    }

    class(out) <- c("adjList", class(out))

    return(out)
  }

  ## otherwise use edgeMatrices
  edges <- lapply(edges, edgeMatrix)
  ## reverse edges for direction =0,-1.  
  edges[dir < 0] <- lapply(edges[dir < 0], function(x) x[2:1,,drop=FALSE])
  edges <- c(edges, lapply(edges[dir == 0], function(x) x[2:1,,drop=FALSE]))

  ## shortcut for all vertices, saves time
   if (all1 & all2) {
     jointEM <- do.call(cbind, edges)
     class(jointEM) <- c("edgeMatrix", class(jointEM))
     return(jointEM)
   }

  jointEM <- matrix(NA, ncol=0, nrow=2)
  for (i in seq_along(edges)) {
      wh <- (edges[[i]][1,] %in% v1) & (edges[[i]][2,] %in% v2)
      jointEM <- cbind(jointEM, edges[[i]][,wh])
  }
  class(jointEM) <- c("edgeMatrix", class(jointEM))
  
  return(jointEM)
}

##' Find adjacent vertices
##' 
##' Generic function for finding adjacent vertices based on any kind of edge.
##' 
##' @param graph an object of class \code{mixedgraph}
##' @param v vertices to find adjacencies
##' @param etype edge types to consider; defaults to all
##' @param dir for directed edges, indicates which direction to search in:
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
  ## repeat dir() vector with warning if necessary
  if (length(dir) > length(etype)) {
    warning("More directions specified than edge types")
    dir <- dir[seq_along(etype)]
  }
  else dir = dir*rep.int(1L, length(etype))
  
  ## if undirected edges are specified as directed 
  ## then change and give a warning
  tmp <- pmatch(etype, edgeTypes()$type)
  if (any(is.na(tmp))) stop("Some edges not matched to a registered type")
  if (any(!edgeTypes()$directed[tmp] & dir[rank(tmp)] != 0)) {
    warning("Undirected edges specified as directed, changing to undirected")
    dir[!edgeTypes()$directed[tmp]] <- 0L
  }
  
  whEdge <- pmatch(etype,names(graph$edges))
  
  edges <- graph$edges[whEdge]
  # n <- length(graph$v)
  
  ## special case of one adjacency matrix to save time
  if (length(edges) == 1 && is.adjMatrix(edges[[1]], length(graph$vnames))) {
    es <- matrix(0, length(v), length(graph$vnames))

    ## add in edges dependent on direction
    if (dir >= 0) es <- es + edges[[1]][v,,drop=FALSE]
    if (dir <= 0) es <- es + t(edges[[1]][,v,drop=FALSE])

    wh <- which(colSums(es) > 0)
    if (!inclusive) wh <- setdiff(wh, v)
    
    return(wh)
  }
  
  es <- collapse(edges, v1=v, dir=dir)
  
  ## select depending on mode of output
  if (is.adjMatrix(es, checknm=TRUE)) {
    ## this is an adjacency matrix
    d <- dim(es)
    wh <- which(.colSums(es, d[1], d[2]) > 0)
    if (!inclusive) wh <- setdiff(wh, v)
    return(wh)
  }
  else if (is.adjList(es, checknm = TRUE)) {
    out <- unlist(es[v])
  }
  else if (is.edgeMatrix(es)) {
    ## this is an edge matrix
    if (ncol(es) == 0) return(integer(0))
    out = es[2,]  
  }
  else stop("Unrecognised edge format")
  
  if (sort > 0) out <- unique.default(out)
  if (sort > 1) out <- sort.int(out)
  if (!inclusive) out = setdiff(out, v)

  return(out)
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
## @param skipChecks guarantees that input is valid
##' @param edges an \code{adjMatrix} or \code{adjList}
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
##' \code{grp2} is a faster version of the function for a single \code{adjMatrix}
##' or \code{adjList}.
##' 
##' @export grp
##' @seealso \code{\link{adj}} for single edge adjacencies.
grp <- function(graph, v, etype, inclusive=TRUE, dir=0, sort=1, force=FALSE) {
  if (!is.mixedgraph(graph)) stop("'graph' should be an object of class 'mixedgraph'")

  if (force) v <- intersect(v, graph$v)
  if (length(v) == 0) return(integer(0))
    
  # if (!skipChecks) {
    ## only include vertices in the graph
    if (!force && any(!(v %in% graph$v))) stop("Invalid values of v")

    if (missing(etype)) etype <- edgeTypes()$type
    ##' repeat dir() vector with warning if necessary
    if (length(dir) > length(etype)) warning("More directions specified than edge types")
    dir = dir*rep.int(1L, length(etype))
    
    tmp <- pmatch(etype, edgeTypes()$type)
    dir[!edgeTypes()$directed[tmp]] <- 0L
  
    whEdge <- pmatch(etype,names(graph$edges))
  
    edges <- graph$edges[whEdge]

    if (length(edges) == 1) {
      if (is.adjList(edges[[1]], checknm=TRUE)) {
        wh <- match(names(graph$edges)[whEdge], edgeTypes()$type)
        if (dir == -1 && edgeTypes()$directed) {
          return(grp2(v, edges[[1]], dir=dir, inclusive=inclusive))
        }
        else if (dir == 0 && !edgeTypes()$directed) {
          return(grp2(v, edges[[1]], dir=dir, inclusive=inclusive))
        }
      }
      else if (is.adjMatrix(edges[[1]], checknm=TRUE)) {
        return(grp2(v, edges[[1]], dir=dir, inclusive=inclusive))
      }
    }
    
    es <- collapse(edges, dir=dir)
    
  # }
  # else {
  #   if (etype %in% names(graph$edges)) {
  #     es <- graph$edges[etype]
  #   }
  #   else return(integer(0))
  # }
  
  #   tmp = lapply(graph$edges[etype], edgeMatrix)
  #   if (!is.null(tmp)) {
  #     es = do.call(cbind, tmp)
  #   }
  #   else es = matrix(NA, ncol=0, nrow=2)
  
  out = v
  
  if (is.adjMatrix(es)) {
    continue = TRUE
    new = v
    
    while (continue) {
      out2 = which(colSums(es[new, , drop=FALSE]) > 0)
      new = setdiff(out2, out)
      es[out,] = 0L
      out <- c(out, new)
      
      continue = (length(new) > 0)
    }
  }
  else if (is.adjList(es)) {
    
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
  if (!inclusive) out = setdiff(out, v)
  
  if (sort > 1) out = sort.int(out)
  
  out
}

##' @describeIn grp Faster routine for single \code{adjMatrix} or \code{adjList}
##' @export
grp2 <- function(v, edges, dir, inclusive, sort=1) {
  new <- v
  out <- v
  
  if (is.adjList(edges)) {
    ## note this only works if dir matches the type of edge (so -1 for directed, 0 for not directed)
    while (length(new) > 0) {
      new <- unlist(edges[new])
      if (sort > 0) new <- setdiff(new, out)
      out <- c(out, new)
    }
  }
  else if (is.adjMatrix(edges)) {
    if (dir == 1) edges <- t(edges)
    else if (dir == 0) edges <- edges + t(edges)
    
    while (length(new) > 0) {
      new <- which(rowSums(edges[,new,drop=FALSE]) > 0)
      if (sort > 0) new <- setdiff(new, out)
      out <- c(out, new)
    }
  }
  # else if (is.eList(edges)) {
  #   
  # }
  # else if (is.edgeMatrix(edges)) {
  #   
  # }
  else stop("Not a valid approach for this function")
  
  if (!inclusive) out <- setdiff(out, v)
  
  if (sort > 1) out <- sort.int(out)
  
  out
}

##' @export groups
##' @describeIn grp find equivalence classes
groups = function(graph, etype, sort=1) {
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


