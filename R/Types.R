##' Find topological ordering of vertices
##' 
##' Looks for topological ordering of vertices with respect to directed edges.
##' Not tested
##' 
##' @export topologicalOrder
topologicalOrder = function(graph) {
  if (length(graph$v) <= 1 || length(graph$edges$directed) == 0) return(graph$v)
  
  out1 = orphaned(graph)
  out3 = barren(graph)
  if (length(out1) == 0 || length(out3) == 0) stop("Graph is cyclic")
  out3 = setdiff(out3, out1)
  
  out2 = Recall(graph[-c(out1,out3)])

  return(c(out1, out2, out3))
}

##' Test cyclicity of graph
##' 
##' Looks for cycles among edges of type 'directed'.  Not tested.
##' Maybe should add option to not test among all directed edges types.
##' 
##' @export is.cyclic
is.cyclic = function(graph) {
  if (class(graph) != "mixedgraph") stop("Must be an object of class 'graph'")
  out = tryCatch(topologicalOrder(graph), error = function(e) {
    if (e$message == "Graph is cyclic") return(NA)
    else stop(e$message)
    })
  return(any(is.na(out)))
}

##' @describeIn is.DAG
##' @export is.ADMG
is.ADMG = function(graph) {
  if (class(graph) != "mixedgraph") stop("Must be an object of class 'graph'")
  ## should only contain directed and bidirected edges
  wh <- names(graph$edges) %in% c("directed", "bidirected")
  if(any(sapply(graph$edges[!wh], length) > 0)) return(FALSE)
  
  return(!is.cyclic(graph))
}

##' @describeIn is.DAG
##' @export is.SG
is.SG = function(graph) {
  if (class(graph) != "mixedgraph") stop("Must be an object of class 'graph'")
  ## should only contain undirected, directed and bidirected edges
  wh <- names(graph$edges) %in% c("undirected", "directed", "bidirected")
  if(any(sapply(graph$edges[!wh], length) > 0)) return(FALSE)
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
##' \code{is.DAG}, \code{is.SG}, \code{is.ADMG} 
##' test whether a graph is a directed acyclic graph, 
##' summary graph or acyclic directed mixed graph 
##' respectively.
##' 
##' @export is.DAG
is.DAG = function(graph) {
  if (class(graph) != "mixedgraph") stop("Must be an object of class 'graph'")
  ## should only contain directed edges
  if(any(sapply(graph$edges[!names(graph$edges)=="directed"], length) > 0)) return(FALSE)
  
  return(!is.cyclic(graph))
}