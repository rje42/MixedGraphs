##' Get adjacency matrix from list of edges
##' 
##' @param edges list or data frame giving edges
##' @param n total number of vertices, defaults to maximal value
##' @param directed logical: if TRUE edges are assumed directed
##' 
##' @export adjMatrix
adjMatrix = function(edges, n, directed=FALSE) {
  if (is.adjMatrix(edges)) {
    return(edges)
  }
  else if (is.list(edges)) {
    if (length(edges) == 0) {
      if (missing(n)) {
        warning("number of vertices not supplied, assumed to be 0")
        n = 0
      }
      return(matrix(0L,n,n))
    }
    
    tmp <- matrix(unlist(edges), nrow=2)
  }  
  else if (is.edgeMatrix(edges)) tmp <- edges
  else if (is.null(edges)) return(NULL)
  
  if (missing(n)) n = max(tmp)
  
  idx <- c(1, n) %*% (tmp-1) + 1
  if (!directed) idx <- c(idx, c(n, 1) %*% (tmp-1) + 1)
  
  out <- rep(0, n*n)
  out[idx] = 1
  
  dim(out) <- c(n,n)
  # if (!missing(vnames)) dimnames(out) <- list(vnames, vnames)

  out
}

##' Get edges from adjacency matrix or list
##' 
##' @param edges
##' @param directed logical: if TRUE edges are assumed directed
##' 
##' @export edgeMatrix
edgeMatrix <- function(edges, directed=FALSE) {
  
  if (length(edges)==0) return(matrix(NA, 2, 0))

  if (is.edgeMatrix(edges)) return(edges)
  if (is.adjMatrix(edges)) {
    rs <- row(edges)[edges > 0]
    cs <- col(edges)[edges > 0]
    if (!directed) {
      wh = (rs < cs)
      rs = rs[wh]; cs = cs[wh]
    }
    out <- matrix(c(rs,cs), nrow=2, byrow=TRUE)
  }
  else if (is.list(edges)) {
    out <- matrix(unlist(edges), nrow=2)
  }
  out
}

##' Edge list
##' 
##' @param edges
##' @param directed logical: if TRUE edges are assumed directed
##' 
##' @export edgeList
edgeList <- function(edges, directed=FALSE) {
  if(is.null(edges)) {
    return(list())
  }
  else if (is.adjMatrix(edges)) {
    rs <- row(edges)[edges > 0]
    cs <- col(edges)[edges > 0]
    if (!directed) {
      wh = (rs < cs)
      rs = rs[wh]; cs = cs[wh]
    }
    out <- mapply(c, rs, cs, SIMPLIFY=FALSE)
  }
  else if (is.edgeMatrix(edges)) {
     out <- mapply(c, edges[1,], edges[2,], SIMPLIFY=FALSE)
  }
  else if (is.list(edges)) out <- edges
  else stop("Not a valid edgeList")
  out
}

##' Adjacency matrix representation
##' 
##' Change edge representation of graph to use adjacency matrices
##' 
##' @param graph an object of class \code{mixedgraph}
##' @param edges character vector of edge types to change, defaults to all
##' 
##' @export withAdjMatrix
withAdjMatrix <- function(graph, edges) {
  if (missing(edges)) idx <- seq_along(graph$edges)
  else idx <- pmatch(edges, names(graph$edges))
  if (length(idx) == 0) return(graph)

  dir <- edgeTypes()$directed[pmatch(names(graph$edges[idx]), edgeTypes()$type)]
  n <- length(graph$vnames)
  graph$edges[idx] <- mapply(adjMatrix, graph$edges[idx], directed=dir, n=n, SIMPLIFY=FALSE)
  graph
}

##' Adjacency matrix representation
##' 
##' Change edge representation of graph to use adjacency matrices
##' 
##' @param graph an object of class \code{mixedgraph}
##' @param edges character vector of edge types to change, defaults to all
##' 
##' @export withEdgeList
withEdgeList <- function(graph, edges) {
  if (missing(edges)) idx <- seq_along(graph$edges)
  else idx <- pmatch(edges, names(graph$edges))
  if (length(idx) == 0) return(graph)
  
  dir <- edgeTypes()$directed[pmatch(names(graph$edges[idx]), edgeTypes()$type)]
  n <- length(graph$vnames)
  graph$edges[idx] <- mapply(edgeList, graph$edges[idx], directed=dir, SIMPLIFY=FALSE)
  class(graph$edges) = "edgeList"
  graph
}

## internal function to count edges 
##
## should it know whether matrix is directed or not?
nedge2 <- function(edges, directed=TRUE) {
  if (is.adjMatrix(edges)) {
    if (directed) return(sum(edges != 0))
    else {
      return(sum(edges[upper.tri(edges)] != 0))
    }
  }
  else if (is.edgeMatrix(edges)) {
    return(ncol(edges))
  }
  else if (is.list(edges)) {
    return(length(edges))
  }
}

##' Give number of edges
##' 
##' @param graph an object of class \code{mixedgraph}
##' @param edges character vector of edge types to include, defaults to all
##' 
##' Uses an internal function \code{nedge2} to count for
##' each type of edge separately.  
##' Do we want this to apply to graphs or edge lists
##' or both?
##' 
##' @export nedge
nedge <- function (graph, edges) {
  if (missing(edges)) idx <- seq_along(graph$edges)
  else idx <- pmatch(edges, names(graph$edges))
  if (length(idx) == 0) return(0L)
  
  dir <- edgeTypes()$directed[pmatch(names(graph$edges[idx]), edgeTypes()$type)]
  sum(mapply(nedge2, graph$edges[idx], dir))
}

