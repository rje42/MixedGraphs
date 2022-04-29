##' Moralize
##' 
##' @param graph an object of class \code{mixedgraph}
##' 
##' @export
moralize <- function(graph, check=TRUE) {
  if (check && !is.SG(graph)) stop("Object must be a summary graph of class 'mixedgraph'")
  if (nv(graph) <= 1) return(graph)
  out <- skeleton(graph)
  dists <- districts(graph)
  pa_dists <- lapply(dists, function(x) union(x, pa(graph, x)))
  
  n <- length(graph$vnames)
  using_am <- sapply(graph$edges, is.adjMatrix, checknm=TRUE)
  extra <- adjMatrix(n=n)

  ## check parents/spouses for each are joined
  for (i in seq_along(dists)) {
    extra[pa_dists[[i]],pa_dists[[i]]] = 1
  }
  diag(extra) = 0
  
  out = addEdges(out, undirected=extra)
  return(out)
}

##' Test m-separation
##' 
##' Check if A is m-separated from B by C in a summary graph \code{graph}.
##' 
##' @param graph an object of class \code{mixedgraph}
##' @param A,B,C sets of vertices in \code{graph}
##' 
##' 
##' @export
m_sep <- function(graph, A, B, C) {
  if (!is_SG(graph)) stop("Object must be a summary graph of class 'mixedgraph'")
  if (missing(C)) C <- integer(0)
  
  ## deal with trivial cases
  if (length(A) == 0 || length(B) == 0) return(TRUE)
  if (length(intersect(A, B)) > 0) return(FALSE)
  vs <- ant(graph, c(A,B,C))
  graph2 <- withAdjMatrix(graph[vs])
  
  ## moralize, remove edges from C
  mg <- moralize(graph2, check=FALSE)
  mg <- mutilate(mg, C)
  gr <- grp(mg, v=A)

  if (any(B %in% gr)) return(FALSE)
  return(TRUE)
}
