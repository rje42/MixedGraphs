##' Remove duplicate edges
##' 
##' @param edgeList \code{edgeList} object
##' @param directed are edges directed? (i.e.
##' does the order of vertices matter?)
##' @param sort should vertices in undirected edges
##' be sorted as a side-effect of the function? (list form
##' only)
##' 
##' Check for and remove duplicate edges in a list of
##' edges or edgeMatrix.  Note that this will sort 
##' elements in 
remove_duplicate_edges <- function(edgeList, directed=TRUE, sort=FALSE) {
  if (is.matrix(edgeList)) {
    ## edgeMatrix object
    if (nrow(edgeList) != 2 || any(edgeList <= 0)) stop("Object provided is a matrix but doesn't seem to be an edgeMatrix")
    if (ncol(edgeList) <= 1) return(edgeList)
    
    ## get a unique number representing each edge
    k = max(edgeList) + 1L
    char = c(t(c(1L,k)) %*% edgeList)
    ## if undirected, check both orders and take the smaller
    if (!directed) char = pmin(char, c(t(c(k,1L)) %*% edgeList))
    dup = duplicated(char)
    
    if (sort) warning("sort = TRUE has no effect for edgeMatrix")
    return(edgeList[,!dup,drop=FALSE])
  }
  else if (is.list(edgeList)) {

    if (directed) return(unique(edgeList))
    ## if undirected, sort entries
    out = lapply(edgeList, sort.int)
    dup = duplicated(out)
    if (sort) return(out[!dup])
    else return(edgeList[!dup])
  }
  else stop("Not sure how to handle this, should be list or matrix")
  
  
}

##' Add or remove edges
##' 
##' @details At the moment no effort is made to 
##' detect duplication in addEdges().  To be added later.
##' Currently removeEdges() forces all edges to be
##' represented by adjacency matrices. 
##' 
##' @param graph a \code{mixedgraph} object
##' @param edges list of edges to be added/removed
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
      if (is.list(out$edges[[etys[et[i]]]])) {
        out$edges[[etys[et[i]]]] = c(out$edges[[etys[et[i]]]], edgeList(edges[[i]], directed = dir))
      }
      else if (is.edgeMatrix(out$edges[[etys[et[i]]]])) {
        out$edges[[etys[et[i]]]] = cbind(out$edges[[etys[et[i]]]], edgeMatrix(edges[[i]], directed = dir))
      }
      else if (is.adjMatrix(out$edges[[etys[et[i]]]])) {
        out$edges[[etys[et[i]]]] = out$edges[[etys[et[i]]]] + adjMatrix(edges[[i]], directed = dir)
      }
      else stop("mixedgraph supplied seems invalid")
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

##' @describeIn addEdges
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

  ## Check all edges given as lists to be added are valid and of length 2
  edL <- sapply(edges, is.list)
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
##' @param directed indicates whether only edges of certain orientation are removed
##' 
##' @details  If no edge type is specified, then all edges are removed.
##' If \code{directed=1}, then directed edges out of \code{A} are removed, 
##' but ones into \code{A} are preserved; for \code{directed=-1} the reverse,
##' and for \code{directed=0} (the default), direction is irrelevant.
##' 
##' @export mutilate
mutilate <- function(graph, A, etype, dir=0L) {
  if (!is.mixedgraph(graph)) stop("'graph' should be an object of class 'mixedgraph'")
  if (length(A) == 0) return(graph)
  
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
    if (is.list(edges[[i]])) {
      ## edge list format
      rm = rep(FALSE, length(edges[[i]]))
      if (dir >= 0) {
        rm = rm | sapply(edges[[i]], function(x) x[1]) %in% A
      }
      if (dir <= 0) {
        rm = rm | sapply(edges[[i]], function(x) x[2]) %in% A
      }
      edges[[i]] = edges[[i]][!rm]
    }
    else {
      ## matrix format
      if (dir >= 0) edges[[i]][A,] = 0
      if (dir <= 0) edges[[i]][,A] = 0
    }
  }
  graph$edges[whEdge] <- edges
  graph
}
