##' Remove duplicate edges
##' 
##' @param edges something from \code{edgeList} object
##' @param directed are edges directed? (i.e.
##' does the order of vertices matter?)
##' @param sort should vertices in undirected edges
##' be sorted as a side-effect of the function? (list form
##' only)
##' 
##' Check for and remove duplicate edges in a list of
##' edges or edgeMatrix.  Note that this will sort 
##' elements in eList objects.
##' 
remove_duplicate_edges <- function(edges, directed=TRUE, sort=FALSE) {
  if (is.adjMatrix(edges)) {
    # if is an adjacency matrix, ensure no entries exceed 1
    edges[edges > 1] <- 1
    return(edges)
  }
  else if (is.adjList(edges, checknm=TRUE)) {
    edges <- lapply(edges, unique.default)
    class(edges) <- c("adjList", class(edges))
    return(edges)
  }
  else if (is.edgeMatrix(edges)) {
    ## edgeMatrix object
    if (nrow(edges) != 2 || any(edges <= 0)) stop("Object provided is a matrix but 
                                                        doesn't seem to be an edgeMatrix")
    if (ncol(edges) <= 1) return(edges)
    
    ## get a unique number representing each edge
    k = max(edges) + 1L
    char = c(t(c(1L,k)) %*% edges)
    ## if undirected, check both orders and take the smaller
    if (!directed) char = pmin(char, c(t(c(k,1L)) %*% edges))
    dup = duplicated(char)
    
    if (sort) warning("sort = TRUE has no effect for edgeMatrix")
    return(edges[,!dup,drop=FALSE])
  }
  else if (is.list(edges)) {

    if (directed) {
      edges <- unique.default(edges)
      return(edges)
    }
    ## if undirected, sort entries
    out = lapply(edges, sort.int)
    dup = duplicated(out)
    if (sort) {
      out <- out[!dup]
      class(out) <- "eList"
    }
    else {
      out <- edges[!dup]
      class(out) <- "eList"
    }
    
    return(out)
  }
  else stop("Not sure how to handle this, should be list or matrix")
}

##' Add or remove edges
##' 
##' @param graph a \code{mixedgraph} object
##' @param edges list of edges to be added/removed
##' 
##' @details At the moment no effort is made to 
##' detect duplication in addEdges().  To be added later.
##' Currently removeEdges() forces all edges to be
##' represented by adjacency matrices. 
##' 
##' @export addEdges
addEdges <- function(graph, edges) {
  out <- graph
  v <- graph$v
  
  etys = edgeTypes()$type
  
  if (!is.list(edges)) stop("'edges' must be a list named with edge types")
  if (is.null(names(edges))) {
    warning("No edge type given, assuming undirected")
    et = 1
  }
  else et = pmatch(names(edges), etys)
  
  if (any(is.na(et))) stop("Edge types not matched")
  else if (any(duplicated(et))) stop("Repeated edge types matched")
  
  ## Check all edges given as lists to be added are valid and of length 2
  edL <- sapply(edges, is.list)
  if (any(is.na(match(unlist(edges[edL]), v)))) stop("Edges must be between vertices in the graph")
  if (any(sapply(unlist(edges[edL], recursive=FALSE), length) != 2)) stop("Hyper-edges not yet supported")
  
  ## Check all edges given as edge matrices to be added are valid and of length 2
  edE <- sapply(edges, is.edgeMatrix)
  if (any(is.na(match(unlist(edges[edE]), v)))) stop("Edges must be between vertices in the graph")
  if (any(sapply(edges[edE], nrow) != 2)) stop("Hyper-edges not yet supported")

  for (i in seq_along(et)) {
    dir <- edgeTypes()$directed[et[i]]
    
    if (etys[et[i]] %in% names(out$edges)) {
      ## if there are some of this type of edge already
      ## add it in the same format
      A <- out$edges[[etys[et[i]]]]
      
      if (is.list(A)) {
        A = c(A, eList(edges[[i]], directed = dir))
        class(A) <- "eList"
      }
      else if (is.edgeMatrix(A)) {
        A = cbind(A, edgeMatrix(edges[[i]], directed = dir))
      }
      else if (is.adjMatrix(A)) {
        A = A + adjMatrix(edges[[i]], directed = dir)
#        out$edges[[etys[et[i]]]] <- pmin(1, out$edges[[etys[et[i]]]])
       class(A) <- "adjMatrix"
       A[A > 1] <- 1
      }
      else stop("mixedgraph supplied seems invalid")
      
      ## put back edges
      out$edges[[etys[et[i]]]] <- A
    }
    else {
      ## otherwise just add it in
      dimnames(edges[[i]]) <- NULL   # drop dimnames
      out$edges[[etys[et[i]]]] <- edges[[i]]
    }
    if (!is.adjMatrix(out$edges[[etys[et[i]]]])) {
      out$edges[[etys[et[i]]]] = remove_duplicate_edges(out$edges[[etys[et[i]]]], directed=dir)
    }
  }
  
  out
}

##' @describeIn addEdges remove edges
##' @export removeEdges
removeEdges <- function(graph, edges) {
  out <- withAdjMatrix(graph)
  v <- graph$v
  
  etys = edgeTypes()$type
  if (is.null(names(edges))) et = seq_along(edges)
  else et = pmatch(names(edges), etys)
  if (length(et) == 1 && is.na(et)) {
    warning("No edge type given, assuming undirected")
    et = 1
  }
  else if (any(is.na(et))) stop("Edge types not matched")
  else if (any(duplicated(et))) stop("Repeated edge types matched")

  ## Check all edges given as adjacency lists to be added are valid
  adL <- sapply(edges, function(x) is.adjList(x))
  if (any(is.na(match(unlist(edges[adL]), v)))) stop("Edges must be between vertices in the graph")

  ## Check all edges given as lists to be added are valid and of length 2
  edL <- sapply(edges, function(x) is.list(x) & !is.adjList(x))
  if (any(is.na(match(unlist(edges[edL]), v)))) stop("Edges must be between vertices in the graph")
  if (any(sapply(unlist(edges[edL], recursive=FALSE), length) != 2)) stop("Hyper-edges not yet supported")
  
  ## Check all edges given as edge matrices to be added are valid and of length 2
  edE <- sapply(edges, is.edgeMatrix)
  if (any(is.na(match(unlist(edges[edE]), v)))) stop("Edges must be between vertices in the graph")
  if (any(sapply(edges[edE], nrow) != 2)) stop("Hyper-edges not yet supported")
  
  ## Now convert to adjacency matrix anyway
  edges <- mapply(adjMatrix, edges, directed=edgeTypes()$directed[et], n=length(v), SIMPLIFY = FALSE)
  
  for (i in seq_along(et)) {
    if (etys[et[i]] %in% names(out$edges)) {
      ## if these edges are present remove them
      out$edges[[etys[et[i]]]] = out$edges[[etys[et[i]]]] - edges[[i]]
      if (any(out$edges[[etys[et[i]]]] < 0)) stop("Tried to remove edge not present")
    }
    ## else just ignore 
  }
  
  out 
}


##' Delete edges
##' 
##' Remove edges adjacent to set of vertices
##' 
##' @param graph a \code{mixedgraph} object
##' @param A a set of vertices in \code{graph}
##' @param etype which edges to remove
##' @param dir indicates whether only edges of certain orientation are removed
##' 
##' @details  If no edge type is specified, then all edges are removed.
##' If \code{dir=1}, then directed edges out of \code{A} are removed, 
##' but ones into \code{A} are preserved; for \code{dir=-1} the reverse,
##' and for \code{dir=0} (the default), direction is irrelevant.
##' 
##' @export mutilate
mutilate <- function(graph, A, etype, dir=0L) {
  if (!is.mixedgraph(graph)) stop("'graph' should be an object of class 'mixedgraph'")
  if (length(A) == 0) return(graph)
  if (all(graph$v %in% A) && missing(etype)) {
    edg <- graph$edges
    for (i in seq_along(edg)) {
      edg[[i]] <- list()
      class(edg[[i]]) <- "eList"
    }
    out <- mixedgraph(v=graph$v, edges=edg, vnames=graph$vnames)
    return(out)
  }
  
  ## if no edge type specified, use all available types
  if (missing(etype)) {
    whEdge <- seq_along(graph$edges)
    tmp <- pmatch(names(graph$edges), edgeTypes()$type)
  }
  else {
    whEdge <- pmatch(etype, names(graph$edges))
    etype = etype[!is.na(whEdge)]
    whEdge = etype[!is.na(whEdge)]
    if (length(etype) == 0) return(graph)
    tmp <- pmatch(etype, edgeTypes()$type)
    if (any(is.na(tmp))) stop("Some edge types not matched")
  }
  dir[!edgeTypes()$directed[tmp]] <- 0L

  edges <- graph$edges[whEdge]  
  
  for (i in seq_along(edges)) {
    if (is.adjList(edges[[i]], checknm=TRUE)) {
      ## adjList format
      if (dir <= 0) {
        fill <- vector(mode="list", length = length(A))
        edges[[i]][A] <- fill
      }
      if (dir >= 0) {
        for (j in seq_along(edges[[i]])) {
          edges[[j]] <- setdiff(edges[[j]], A)
        }
      }
    }
    else if (is.list(edges[[i]])) {
      ## edge list format
      rm = rep(FALSE, length(edges[[i]]))
      if (dir >= 0) {  # remove outgoing edges
        rm = rm | sapply(edges[[i]], function(x) x[1]) %in% A
      }
      if (dir <= 0) {  # remove incoming edges
        rm = rm | sapply(edges[[i]], function(x) x[2]) %in% A
      }
      edges[[i]] = edges[[i]][!rm]
      class(edges[[i]]) <- "eList"
    }
    else if (is.adjMatrix(edges[[i]])) {
      ## matrix format
      if (dir >= 0) edges[[i]][A,] = 0
      if (dir <= 0) edges[[i]][,A] = 0
    }
    else if (is.edgeMatrix(edges[[i]])) {
      if (dir >= 0) edges[[i]] <- edges[[i]][,!(edges[[i]][,1] %in% A), drop=FALSE]
      if (dir <= 0) edges[[i]] <- edges[[i]][,!(edges[[i]][,2] %in% A), drop=FALSE]
    }
    else stop("Edge type not recognised")
  }
  graph$edges[whEdge] <- edges
  graph
}

##' Add additional nodes to a graph
##' 
##' @param graph a \code{mixedgraph} object
##' @param k the number of nodes to be added
##' @param vnames an optional character vector of names
##' 
##' 
##' @export addNodes
addNodes <- function(graph, k, vnames) {
  n <- length(graph$vnames)
  if (k == 0) return(graph)
  
  ## get new names
  if (missing(vnames) || is.null(vnames)) {
    vnames = paste("x", seq(n+1,n+k), sep="")
    while (any(vnames %in% graph$vnames)) {
      count <- n+k
      vnames <- setdiff(vnames, graph$vnames)
      l <- k - length(vnames)
      vnames = paste("x", count + seq_len(l), sep="")
      count <- count + l
    }
  }

  ## add empty edges if necessary
  edges <- graph$edges
  adjM <- which(sapply(edges, function(x) "adjMatrix" %in% class(x)))
  for (i in adjM) {
    edges[[i]] <- matrix(0, n+k, n+k)
  }
  if (length(adjM) > 0) {
    edges[adjM] <- mapply(function(x,y) {
      x[seq_len(nrow(y)), seq_len(ncol(y))] <- y
      class(x) <- class(y)
      x
    }, edges[adjM], graph$edges[adjM], SIMPLIFY = FALSE)
  }

  ## now return the enlarged graph
  mixedgraph(v=c(graph$v, n+seq_len(k)), edges=edges, vnames=c(graph$vnames, vnames))
}
