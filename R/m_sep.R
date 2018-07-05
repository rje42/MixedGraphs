##' Moralize
##' 
##' @param graph an object of class \code{mixedgraph}
moralize <- function(graph) {
  if (length(graph$v) <= 1) return(graph)
  out <- skeleton(graph)
  dists <- districts(graph)
  pa_dists <- lapply(dists, function(x) union(x, pa(graph, x)))
  
  n <- max(graph$v)
  using_am = sapply(graph$edges, is.adjMatrix)
  if (any(using_am)) {
    n <- nrow(graph$edges[[which(using_am)[1]]])
  }
  extra <- matrix(0,n,n)
  
  ## check parents/spouses for each are joined
  for (i in seq_along(dists)) {
    extra[pa_dists[[i]],pa_dists[[i]]] = 1
  }
  diag(extra) = 0
  
  out = addEdges(out, list(undirected=extra))
  return(out)
}

##' Test m-separation
##' 
##' @param graph an object of class \code{mixedgraph}
##' @param A,B,C sets of vertices in \code{graph}
m_sep <- function(graph, A, B, C) {
  if (missing(C)) C = integer(0)
  
  ## deal with trivial cases
  if (length(A) == 0 || length(B) == 0) return(TRUE)
  if (length(intersect(A, B)) > 0) return(FALSE)
  vs <- ant(graph, c(A,B,C))
  graph2 <- withAdjMatrix(graph[vs,drop=TRUE])
  
  ## moralize, remove edges from C
  mg <- moralize(graph2)
  mg <- mutilate(mg, C, "undirected")
  gr <- grp(mg, v=A)

  if (any(B %in% gr)) return(FALSE)
  return(TRUE)
}