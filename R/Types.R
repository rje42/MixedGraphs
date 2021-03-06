##' Find topological ordering of vertices
##' 
##' Looks for topological ordering of vertices with respect to directed edges.
##' 
##' @param graph a \code{mixedgraph} object
##' @param warn logical: should a cyclic graph return a warning?
##' 
##' @details \code{isTopological} tests for an ordering being 
##' topological.  If the graph is cyclic, it returns \code{NA}, with or 
##' without a warning depending on the value of \code{warn}.
##' 
##' @export topologicalOrder
topologicalOrder = function(graph, warn=TRUE) {
  n <- length(graph$v)
  if (n <= 1 || length(graph$edges$directed) == 0) return(graph$v)
  
  graph_d <- withAdjMatrix(graph, "directed")
  
  out <- orphaned(graph)
  left <- setdiff(graph$v, out)
  
  while (length(out) < n) {
    rm <- orphaned(graph, left)
    if (length(rm) == 0) {
      if (warn) warning("No topological order found: graph is cyclic")
      return(NA)
    }
    out <- c(out, rm)
    left <- setdiff(left, out)
  }
  
  return(out)
  
  # out1 = orphaned(graph)
  # if (length(out1) == 0) stop("Graph is cyclic")
  # out3 = sterile(graph[-out1])
  # if (length(out3) == 0 && length(out1) < length(graph$v)) stop("Graph is cyclic")
  # 
  # out2 = Recall(graph[-c(out1,out3)])
  # 
  # return(c(out1, out2, out3))
}

##' @describeIn topologicalOrder Check if ordering is topological
##' @param v an ordering of the vertices
##' @export
isTopological = function(graph, v) {
  
  for (i in seq_along(v)) {
    pa_i = pa(graph, v[i])
    if (any(pa_i %in% v[-seq_len(i)])) return(FALSE)
  }
  
  return(TRUE)
}

##' Test cyclicity of graph
##' 
##' Looks for cycles among edges of type 'directed'.  Not tested.
##' Maybe should add option to not test among all directed edges types.
##' 
##' @param graph \code{mixedgraph} object
##' 
##' @export is.cyclic
is.cyclic = function(graph) {
  if (!("mixedgraph" %in% class(graph))) stop("Must be an object of class 'mixedgraph'")
  ord <- topologicalOrder(graph, warn=FALSE)
  if (any(is.na(ord))) return(TRUE)
  
  # out = tryCatch(topologicalOrder(graph), warning = function(e) {
  #   if (e$message == "No topological order found: graph is cyclic") return(TRUE)
  #   else stop(e$message)
  #   })
  # return(any(is.na(out)))
  return(FALSE)
}

##' @describeIn is.DAG test if an ADMG
##' @export is.ADMG
is.ADMG = function(graph) {
  if (!("mixedgraph" %in% class(graph))) stop("Must be an object of class 'mixedgraph'")
  ## should only contain directed and bidirected edges
  if(nedge(graph, setdiff(names(graph$edges), c("directed", "bidirected"))) > 0) return(FALSE)
  
  return(!is.cyclic(graph))
}

##' @describeIn is.DAG test if a summary graph
##' @export is.SG
is.SG = function(graph) {
  if (!("mixedgraph" %in% class(graph))) stop("Must be an object of class 'mixedgraph'")
  ## should only contain undirected, directed and bidirected edges
  if(nedge(graph, setdiff(names(graph$edges), c("undirected", "directed", "bidirected"))) > 0) return(FALSE)
  
  v <- graph$v
  nbs <- nb(graph, v)
  if (length(intersect(ch(graph, v), nbs)) ||
        length(intersect(sp(graph, v), nbs))) return(FALSE)
  
  return(!is.cyclic(graph))
}

##' Test type of graph
##' 
##' @param graph \code{mixedgraph} object
##' 
##' \code{is.UG}, \code{is.DAG}, \code{is.SG}, \code{is.ADMG}
##' test whether a graph is a directed acyclic graph, 
##' summary graph or acyclic directed mixed graph 
##' respectively.
##' 
##' @export is.DAG
is.DAG = function(graph) {
  if (!("mixedgraph" %in% class(graph))) stop("Must be an object of class 'mixedgraph'")
  ## should only contain directed edges
  if(any(lengths(graph$edges[!names(graph$edges)=="directed"]) > 0)) return(FALSE)
  
  return(!is.cyclic(graph))
}

##' @describeIn is.DAG test if an undirected graph
##' @export is.UG
is.UG = function(graph) {
  if (!("mixedgraph" %in% class(graph))) stop("Must be an object of class 'mixedgraph'")
  ## should only contain undirected edges
  if(any(lengths(graph$edges[!names(graph$edges)=="undirected"]) > 0)) return(FALSE)
  
  return(TRUE)
}
