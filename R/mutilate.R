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
  ## if no edge type specified, use all available types
  if (missing(etype)) {
    whEdge <- seq_along(graph$edges)
    tmp <- pmatch(names(graph$edges), edgeTypes()$type)
  }
  else {
    whEdge <- pmatch(etype, names(graph$edges))
    tmp <- pmatch(etype, edgeTypes()$type)
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

##' Test for separation
##' 
##' @param graph a \code{mixedgraph} object
##' @param A,B sets of vertices in \code{graph}
##' @param C proposed separating set of vertices in \code{graph}
##' @param etype which edges to remove
##' 
##' @export separated
separated <- function(graph, A, B, C, etype) {
  if (missing(etype)) etype <- edgeTypes()$type
  if (length(C) > 0) gr2 <- graph[-C]
  else gr2 <- graph
  ad <- grp(gr2, v = A, etype = etype, dir=0, sort=0)
  
  return(!any(B %in% ad))
}

##' Moral graph
##' 
##' Find moral graph of a DAG
##' 
##' @param graph a \code{mixedgraph} object with directed and undirected edges
##' @param A optionally, a set whose ancestors should be moralized (defaults to entire graph)
##' 
##' @details if \code{a -> c <- b} with \code{a,b} not adjacent, then an 
##' undirected edge is added.
##' 
##' @export moralize
moralize <- function(graph, A) {
  add <- list()
  if (missing(A)) A = graph$v
  else A = anc(graph, A)
  
  for (i in A) {
    pas <- pa(graph, i)
    for (p in pas) {
      tmp <- c(p, adj(graph, p, etype=c("undirected", "directed")))
      ## look for other parents not adjacent to p
      miss <- setdiff(pas, tmp)
      add <- lapply(miss, function(x) c(p, x))
      ## add an undirectd edge if no adjacency
      graph$edges$undirected <- c(graph$edges$undirected, add)
    }
  }
  
  return(graph)
}

##' Test for d-separation
##' 
##' @param graph a \code{mixedgraph} object
##' @param A,B sets of vertices in \code{graph}
##' @param C proposed separating set of vertices in \code{graph}
##' 
##' @details Only directed and undirected edges within the ancestral
##' subgraph are considered.  [Would probably make more sense to use
##' anterior instead of ancestors here.]
##' 
##' @export d_separated
d_separated <- function(graph, A, B, C) {
  anGr <- graph[anc(graph, c(A,B,C), sort=1)]
  morGr <- moralize(anGr)
  
  return(separated(morGr, A, B, C))
}
