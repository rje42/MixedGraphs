##' Get adjacency matrix from list of edges
##' 
##' @param edges list or matrix giving edges
##' @param n total number of vertices, defaults to maximal value
##' @param directed logical: if `TRUE` edges are assumed directed
##' @param sparse logical: use a sparse matrix?
##' 
##' @export 
adjMatrix <- function(edges, n, directed=FALSE, sparse=FALSE) {
  if (missing(edges)) {
    if (sparse) {
      out <- Matrix(0L,n,n)
    }
    else {
      out <- matrix(0L,n,n)
    }
    class(out) <- c("adjMatrix", class(out))
    return(out)
  }
  if (is.adjMatrix(edges, checknm=TRUE)) {
    return(edges)
  }
  else if (is.adjList(edges, checknm = TRUE)) {
    n <- length(edges)

    if (sparse) {
      out <- Matrix(0L,n,n)
      class(out) <- c("adjMatrix", class(out))
    }
    else {
      out <- matrix(0L,n,n)
      class(out) <- c("adjMatrix", class(out))
    }
    
    for (i in seq_along(edges)) {
      out[i,edges[[i]]] <- 1
    }
    # if (!directed && !isSymmetric(out)) stop("Error in input")
    
    return(out)
  }
  else if (is.eList(edges)) {
    if (length(edges) == 0) {
      if (missing(n)) {
        warning("number of vertices not supplied, assumed to be 0")
        n <- 0
      }
      ## return zero matrix
      if (sparse) {
        out <- Matrix(0L,n,n)
        class(out) <- c("adjMatrix", class(out))
      }
      else {
        out <- matrix(0L,n,n)
        class(out) <- c("adjMatrix", class(out))
      }
      return(out)
    }
    
    tmp <- matrix(unlist(edges), nrow=2)
  }  
  else if (is.edgeMatrix(edges)) tmp <- edges
  else if (is.null(edges)) return(NULL)
  else stop("Failed to identify edge type")
  
  if (missing(n)) n <- max(tmp)
  
  idx <- c(1, n) %*% (tmp-1) + 1
  if (!directed) idx <- c(idx, c(n, 1) %*% (tmp-1) + 1)
  
  if (sparse) out <- Matrix(0, n, n, doDiag = FALSE)
  else out <- matrix(0, n, n)
  out[c(idx)] <- 1
  
#  dim(out) <- c(n,n)
  class(out) <- c("adjMatrix", class(out))
  # if (!missing(vnames)) dimnames(out) <- list(vnames, vnames)

  return(out)
}

##' Get adjacency list of edges
##' 
##' @param edges list or matrix giving edges in the form of an 
##' `eList`, `adjMatrix` or `edgeMatrix`
##' @param n total number of vertices (defaults to maximum value)
##' @param directed logical: if `TRUE` edges are assumed directed
##' @param transpose logical: if `TRUE` we consider children instead of parents
##' 
##' @details Stores adjacencies by one of their vertices.  E.g. directed edges
##' are stored indexed by the child vertex, undirected by both neighbours.
##' 
##' @export
adjList <- function(edges, n, directed=FALSE, transpose=FALSE) {
  if (missing(edges) || length(edges) == 0L) {
    out <- rep(list(integer(0)), n)
    class(out) <- "adjList"
    return(out)
  }
  if (is.adjList(edges, checknm=TRUE)) {
    ## nothing to do, just check everything is an integer
    edges <- lapply(edges, as.integer)
    return(edges)
  }
  else if (is.eList(edges)) {
    ## seems to be an eList
    if (length(edges) == 0) {
      if (missing(n)) {
        warning("number of vertices not supplied, assumed to be 0")
        n <- 0
      }
      class(edges) <- "adjList"
      return(edges)
    }
    else if (missing(n)) n <- max(sapply(edges, max))
    
    out <- vector(mode = "list", length=n)
    
    for (i in seq_along(edges)) {
      ## go through each edge and record in appropriate position on list
      out[[edges[[i]][1]]] <- c(out[[edges[[i]][1]]], edges[[i]][2])
      if (!directed) out[[edges[[i]][2]]] <- c(out[[edges[[i]][2]]], edges[[i]][1])
    }
  }  
  else if (is.edgeMatrix(edges)) {
    ## seems to be an edgeMatrix

    if (transpose) edges <- edges[2:1,,drop=FALSE]
    
    # tmp <- edges
    if(missing(n)) n <- max(edges)
    out <- rep(list(integer(0)), n) #vector(mode="list", length=n)

    for (i in seq_len(n)) {
      if (directed) {
        wh_i <- which(edges[1,] == i)
        out[[i]] <- edges[2,wh_i]
      }
      else {
        wh_i <- which(apply(edges, MARGIN=2, FUN=function(x) any(x==i)))
        wh_j <- apply(edges[,wh_i,drop=FALSE], 2, FUN=function(x) which(x==i))
        out[[i]] <- edges[cbind(3-wh_j,wh_i)]
      }
    }
  }
  else if (is.adjMatrix(edges)) {
    ## seems to be an adjMatrix
    if (!directed) edges <- edges+t(edges)
    out <- unlist(apply(edges, 1, function(x) list(which(x > 0))), recursive=FALSE)
    # if (is.numeric(out)) out <- unlist(lapply(out, list), recursive = FALSE)
  }
  else if (is.null(edges)) return(NULL)
  else stop("Don't know what kind of edge-set this is")
  
  # if (missing(n)) n <- max(tmp)
  
  # idx <- c(1, n) %*% (tmp-1) + 1
  # if (!directed) idx <- c(idx, c(n, 1) %*% (tmp-1) + 1)
  # 
  # out <- rep(0, n*n)
  # out[idx] <- 1

  out <- lapply(out, as.integer)
  
  class(out) <- "adjList"
  # if (!missing(vnames)) dimnames(out) <- list(vnames, vnames)
  
  return(out)
}

##' @describeIn adjList Get reverse directions for edges
##' @export
revAdjList <- function(edges) {
  n <- length(edges)
  out <- vector(mode="list", length=n)
  out[] <- list(integer(0))
  for (i in seq_len(n)) {
    out[edges[[i]]] <- lapply(out[edges[[i]]], function(x) c(x,i))
  }
  class(out) <- "adjList"
  return(out)
}

##' @describeIn adjList Make adjList symmetric
##' @param unq logical: should values be made unique and sorted?
##' @export
symAdjList <- function(edges, unq=TRUE) {
  edges_r <- revAdjList(edges)
  if (unq) out <- mapply(function(x,y) sort.int(unique.default(c(x,y))), edges, edges_r, SIMPLIFY = FALSE)
  else out <- mapply(c, edges, edges_r, SIMPLIFY = FALSE)
  
  class(out) <- "adjList"
  return(out)
}

##' Get edges from adjacency matrix or list
##' 
##' @param edges list or adjacency matrix of edges
##' @param directed logical: if `TRUE` edges are assumed to be directed
##' @param double logical: if `TRUE`, edges are written in both directions
##' 
##' @export 
edgeMatrix <- function(edges, directed=FALSE, double=FALSE) {
  
  if (missing(edges) || length(edges) == 0) {
    out <- matrix(NA, 2, 0)
    class(out) <- "edgeMatrix"
    return(out)
  }

  if (is.edgeMatrix(edges)) {
    out <- edges
    # if (double) {
    #   edges <- cbind(edges, edges[2:1,])
    #   edges <- unique.matrix(edges, MARGIN=2)
    # }
    # return(edges)
  }
  else if (is.adjMatrix(edges)) {
    rs <- row(edges)[edges > 0]
    cs <- col(edges)[edges > 0]
    if (!directed) {
      wh <- (rs < cs)
      rs <- rs[wh]; cs <- cs[wh]
    }
    out <- matrix(c(rs,cs), nrow=2, byrow=TRUE)
  }
  else if (is.adjList(edges, checknm=TRUE)) {
    if (directed) out <- matrix(NA, 2, sum(lengths(edges)))
    else {
      if (sum(lengths(edges)) %% 2 == 1) stop("Should not be an odd number of edge entries for non-directed edge-type")
      out <- matrix(NA, 2, sum(lengths(edges))/2)
    }
    pos <- 0
    for (i in seq_along(edges)) {
      if (directed) {
        out[1,pos + seq_along(edges[[i]])] <- i
        out[2,pos + seq_along(edges[[i]])] <- edges[[i]]
        pos <- pos + length(edges[[i]])
      }
      else {
        out[1,pos + seq_len(sum(edges[[i]] < i))] <- i
        out[2,pos + seq_len(sum(edges[[i]] < i))] <- edges[[i]][edges[[i]] < i]
        pos <- pos + length(edges[[i]][edges[[i]] < i])
      }
    }
  }
  else if (is.eList(edges)) { # && all(lengths(edges) %in% c(0,2))) {
    out <- matrix(unlist(edges), nrow=2)
  }
  else stop("Hyperedges not supported for this format")
  
  if (double) {
    ## requested to double edges, do this
    out <- cbind(out, out[2:1,,drop=FALSE])
    out <- unique.matrix(out, MARGIN=2)
  }
  
  out[] <- as.integer(out)
  class(out) <- "edgeMatrix"
  
  return(out)
}

##' Edge list
##' 
##' Returns an `eList` object from an adjacency matrix
##' or edge matrix object.
##' 
##' @param edges `adjList`, `edgeMatrix`, `adjMatrix`, or a `list` of pairs of edges
##' @param directed logical: if `TRUE` edges are assumed directed
##' 
##' @details The `directed` argument is important; if omitted, 
##' then some edges may be recorded only once, even though they are present 
##' in both directions.
##' 
##' @export
eList <- function(edges, directed=FALSE) {
  if (!is.logical(directed)) stop("argument 'directed' must be logical")
  if (missing(edges) || is.null(edges)) {
    out <- list()
    class(out) <- "eList"
    return(out)
  }
  else if (is.adjMatrix(edges)) {
    rs <- row(edges)[edges > 0]
    cs <- col(edges)[edges > 0]
    if (!directed) {
      wh <- (rs < cs)
      rs <- rs[wh]; cs <- cs[wh]
    }
    out <- mapply(c, rs, cs, SIMPLIFY=FALSE)
  }
  else if (is.adjList(edges, checknm=TRUE)) {
    out <- list()
    for (i in seq_along(edges)) {
      tmp <- edges[[i]]
      if (!directed) tmp <- tmp[tmp < i]
      
      if (length(tmp) > 0) out <- c(out, lapply(tmp, function(x) c(i,x)))
      # if (directed) out <- c(out, lapply(tmp, function(x) c(x,i)))
      # else {
      #   out <- c(out, lapply(tmp, function(x) c(x[x < i],i)))
      # }
    }
    
    # assertthat::are_equal(lengths(out), rep(2L, length(out)))
  }
  else if (is.edgeMatrix(edges)) {
     out <- mapply(c, edges[1,], edges[2,], SIMPLIFY=FALSE)
  }
  else if (is.list(edges)) out <- edges
  else if (is.numeric(edges) && length(edges) == 2) out <- list(edges)
  else stop("Not a valid edgeList")
  
  out <- lapply(out, as.integer)
  class(out) <- "eList"
  
  out
}

##' Deprecated function
##' 
##' @param edges an "eList" object
##' @param directed logical, should edges be interpreted as directed?
##' 
edgeList <- function(edges, directed=FALSE) {
  .Deprecated("eList")
  eList(edges, directed)
}


##' Make an edgeList from a collection of edges
##' 
##' @param ... list of named edge objects
##' 
##' @export
makeEdgeList <- function(...) {
  
  ## get arguments, and return an empty list if necessary
  args <- list(...)
  if (length(args) == 0) {
    class(args) <- "edgeList"
    return(args)
  }
  
  ## otherwise, compare to standard edge types
  etys <- edgeTypes()$type
  if (length(args) > 0) {
    wh <- pmatch(names(args), etys)
    names(args)[!is.na(wh)] <- etys[na.omit(wh)]
    
    if (any(is.na(wh))) {
      warning(paste("edge types", paste(names(args)[is.na(wh)], collapse=", "), "not matched"))
      args <- args[!is.na(wh)]
    }
    
    ## define edges to be an edgeList consisting of entries from ...
    edges <- args
    class(edges) <- "edgeList"
  }
  
  edges
}


##' Check if edge object could be in given format
##' 
##' @param object item of purported type
##' @param n (optionally) number of vertices in graph
##' @param checknm logical: use class of object to determine answer?
##' 
##' @name check_edge_type
##' 
NULL

##' @describeIn check_edge_type check if could be `edgeMatrix`
##' @export
is.edgeMatrix <- function(object, n, checknm=TRUE) {
  if("edgeMatrix" %in% class(object)) return(TRUE)
  else if (checknm || any(c("adjList", "eList", "adjMatrix") %in% class(object))) return(FALSE)
  
  if (!is.matrix(object)) return(FALSE)
  if (!missing(n) && any(object > n)) return(FALSE)
  if (nrow(object) != 2) return(FALSE)
  if (any(object <= 0)) return(FALSE)
  return(TRUE)
}

##' @describeIn check_edge_type check if could be `eList`
##' @export
is.eList <- function(object, n, checknm=TRUE) {
  if ("eList" %in% class(object)) return(TRUE)
  else if (checknm || any(c("adjList", "adjMatrix", "edgeMatrix") %in% class(object))) return(FALSE)
  
  if (!is.list(object)) return(FALSE)
  if (!missing(n) && any(unlist(object) > n)) return(FALSE)
  if (any(unlist(object) <= 0)) return(FALSE)
  return(TRUE)
}


##' @describeIn check_edge_type check if could be `adjMatrix`
##' @export
is.adjMatrix <- function(object, n, checknm=TRUE) {
  if("adjMatrix" %in% class(object)) return(TRUE)
  else if (checknm || any(c("adjList", "eList", "edgeMatrix") %in% class(object))) return(FALSE)
  
  if (!is.matrix(object) && !is(object, "Matrix")) return(FALSE)
  if (!missing(n) && n != ncol(object) && n != nrow(object)) return(FALSE)  
  else if (nrow(object) != ncol(object)) return(FALSE)  ## do we really require n rows and n columns?
  if (any(is.na(object))) return(FALSE)
  if (any(object < 0) || any(object > 1)) return(FALSE)
  if (all(object > 0)) return(FALSE)
  return(TRUE)
}

##' @describeIn check_edge_type check if could be `adjList`
##' @export
is.adjList <- function(object, n, checknm = TRUE) {
  if("adjList" %in% class(object)) return(TRUE)
  else if (checknm || any(c("adjMatrix", "eList", "edgeMatrix") %in% class(object))) return(FALSE)
  
  ## check it is a list
  if (!is.list(object)) return(FALSE)
  if (missing(n)) {
    ## if n is not known, check length makes sense with values
    min_n <- suppressWarnings(max(unlist(object)))
    if (!is.infinite(min_n) && length(object) < min_n) return(FALSE)
  }
  else {
    ## if n is known, check length and values make sense
    if(n != length(object)) return(FALSE)
    if (any(unlist(object) > n)) return(FALSE)
  }
  if (any(unlist(object) <= 0)) return(FALSE)

  return(TRUE)
}

##' Change representation of edges
##' 
##' Change edge representation of (part of) graph to use `adjMatrix`,
##' `adjList`, `eList` or `edgeMatrix` formats.
##' 
##' @param graph an object of class `mixedgraph`
##' @param edges character vector of edge types to change, defaults to all
##' @param sparse logical: should sparse matrices be used?
##' @param force logical: should edge sets be added when named?
##' 
##' @export
withAdjMatrix <- function(graph, edges, sparse=FALSE, force=FALSE) {
  if (missing(edges)) idx <- seq_along(graph$edges)
  else idx <- pmatch(edges, names(graph$edges))
  
  if (any(is.na(idx))) {
    if (force) {
      idx2 <- pmatch(edges, edgeTypes()[,1])
      if (any(is.na(idx2))) stop("Some edge types not valid")
      nms <- c(names(graph$edges), edgeTypes()[idx2[is.na(idx)],1])
      graph$edges <- c(graph$edges, rep(list(adjMatrix(n=length(graph$vnames))), sum(is.na(idx))))
      names(graph$edges) <- nms
    }
    else {
      warning("Some edge types not matched")
      edges <- edges[!is.na(idx)]
      idx <- na.omit(idx)
    }
  }
  
  if (length(idx) == 0) {
    class(graph$edges) <- "edgeList"
    return(graph)
  }

  ## get directed and number of vertices, then transform using adjMatrix()
  dir <- edgeTypes()$directed[pmatch(names(graph$edges[idx]), edgeTypes()$type)]
  n <- length(graph$vnames)
  graph$edges[idx] <- mapply(adjMatrix, graph$edges[idx], directed=dir, n=n, sparse=sparse, SIMPLIFY=FALSE)
  
  class(graph$edges) <- "edgeList"
  
  graph
}

##' @describeIn withAdjMatrix Change to `adjList` format
##' @export
withAdjList <- function(graph, edges, force=FALSE) {
  if (missing(edges)) idx <- seq_along(graph$edges)
  else {
    idx <- pmatch(edges, names(graph$edges))
  }
  
  if (any(is.na(idx))) {
    if (force) {
      idx2 <- pmatch(edges, edgeTypes()[,1])
      if (any(is.na(idx2))) stop("Some edge types not valid or specified ambiguously")
      nms <- c(names(graph$edges), edgeTypes()[idx2[is.na(idx)],1])
      graph$edges <- c(graph$edges, rep(list(adjList(n=length(graph$vnames))), sum(is.na(idx))))
      idx[is.na(idx)] <- length(na.omit(idx)) + seq_len(sum(is.na(idx)))
      names(graph$edges) <- nms
    }
    else {
      warning("Some edge types not matched")
      edges <- edges[!is.na(idx)]
      idx <- na.omit(idx)
    }
  }
  
  if (length(idx) == 0) {
    class(graph$edges) <- "edgeList"
    return(graph)
  }

  ## get directed and number of vertices, then transform using adjList()
  dir <- edgeTypes()$directed[pmatch(names(graph$edges[idx]), edgeTypes()$type)]
  n <- length(graph$vnames)
  graph$edges[idx] <- mapply(adjList, graph$edges[idx], directed=dir, n=n, SIMPLIFY=FALSE)
  
  for (i in idx) class(graph$edges[[i]]) <- "adjList"
  
  class(graph$edges) <- "edgeList"
  
  graph
}

##' @describeIn withAdjMatrix Change to `edgeMatrix` format
##' @export
withEdgeMatrix <- function(graph, edges, force=FALSE) {
  if (missing(edges)) idx <- seq_along(graph$edges)
  else idx <- pmatch(edges, names(graph$edges))
  
  if (any(is.na(idx))) {
    if (force) {
      idx2 <- pmatch(edges, edgeTypes()[,1])
      if (any(is.na(idx2))) stop("Some edge types not valid")
      nms <- c(names(graph$edges), edgeTypes()[idx2[is.na(idx)],1])
      graph$edges <- c(graph$edges, rep(list(edgeMatrix()), sum(is.na(idx))))
      names(graph$edges) <- nms
    }
    else {
      warning("Some edge types not matched")
      edges <- edges[!is.na(idx)]
      idx <- na.omit(idx)
    }
  }
  
  if (length(idx) == 0) {
    class(graph$edges) <- "edgeList"
    return(graph)
  }
  
  ## get directed and number of vertices, then transform using edgeMatrix()
  dir <- edgeTypes()$directed[pmatch(names(graph$edges[idx]), edgeTypes()$type)]
  n <- length(graph$vnames)
  graph$edges[idx] <- mapply(edgeMatrix, graph$edges[idx], directed=dir, SIMPLIFY=FALSE)
  
  class(graph$edges) <- "edgeList"
  
  return(graph)
}

##' @describeIn withAdjMatrix Change to `eList` format
##' @export
withEdgeList <- function(graph, edges, force=FALSE) {
  if (missing(edges)) idx <- seq_along(graph$edges)
  else idx <- pmatch(edges, names(graph$edges))
  
  if (any(is.na(idx))) {
    if (force) {
      idx2 <- pmatch(edges, edgeTypes()[,1])
      if (any(is.na(idx2))) stop("Some edge types not valid")
      nms <- c(names(graph$edges), edgeTypes()[idx2[is.na(idx)],1])
      graph$edges <- c(graph$edges, rep(list(eList()), sum(is.na(idx))))
      names(graph$edges) <- nms
    }
    else {
      warning("Some edge types not matched")
      edges <- edges[!is.na(idx)]
      idx <- na.omit(idx)
    }
  }
  
  if (length(idx) == 0) {
    class(graph$edges) <- "edgeList"
    return(graph)
  }
  
  ## get directed and number of vertices, then transform using eList()
  dir <- edgeTypes()$directed[pmatch(names(graph$edges[idx]), edgeTypes()$type)]
  n <- length(graph$vnames)
  graph$edges[idx] <- mapply(eList, graph$edges[idx], directed=dir, SIMPLIFY=FALSE)
  
  class(graph$edges) <- "edgeList"
  
  return(graph)
}

##' @describeIn graphOps number of edges
##' @export
nedge <- function (graph, edges) {
  if (missing(edges)) idx <- seq_along(graph$edges)
  else {
    idx2 <- pmatch(edges, edgeTypes()$type)
    if (any(is.na(idx2))) stop(paste0("edge type",ifelse(sum(is.na(idx2)>1), "s", 
                                      "")," '", paste(edges[is.na(idx2)], 
                                      collapse="', '"), "' not matched"))
    idx <- na.omit(match(edgeTypes()$type[idx2], names(graph$edges)))
  }
  if (length(idx) == 0) return(0L)
  
  dir <- edgeTypes()$directed[pmatch(names(graph$edges[idx]), edgeTypes()$type)]
  return(sum(mapply(nedge2, graph$edges[idx], dir)))
}

## internal function to count edges 
##
nedge2 <- function(edges, directed=TRUE) {
  if (is.adjMatrix(edges)) {
    if (directed) return(sum(edges != 0))
    else {
      return(sum(edges[upper.tri(edges)] != 0))
    }
  }
  else if (is.adjList(edges, checknm=TRUE)) {
    return(sum(lengths(edges))/(2-directed))
  }
  else if (is.edgeMatrix(edges)) {
    return(ncol(edges))
  }
  else if (is.eList(edges)) {
    return(length(edges))
  }
  else if(is.null(edges)) return(0L)
  else stop("Edge type not recognised")
}

##' @describeIn graphOps Test for existence of particular type of edge
##' @export
has_edge_entry <- function (graph, edges) {
  wh <- pmatch(edges, names(graph$edges))
  return(!is.na(wh))
}
