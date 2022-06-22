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
##' @export 
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
##' @export 
is_cyclic = function(graph) {
  if (!is.mixedgraph(graph)) stop("Must be an object of class 'mixedgraph'")
  ord <- topologicalOrder(graph, warn=FALSE)
  if (any(is.na(ord))) return(TRUE)
  
  # out = tryCatch(topologicalOrder(graph), warning = function(e) {
  #   if (e$message == "No topological order found: graph is cyclic") return(TRUE)
  #   else stop(e$message)
  #   })
  # return(any(is.na(out)))
  return(FALSE)
}

##' Deprecated function to test cyclicity of graph
##' 
##' Looks for cycles among edges of type 'directed'.  Not tested.
##' Maybe should add option to not test among all directed edges types.
##' 
##' @param graph \code{mixedgraph} object
##' 
##' @name is.cyclic-deprecated
##' @export
is.cyclic <- function(graph) {
  .Deprecated("is_cyclic")

  if (!is.mixedgraph(graph)) stop("Must be an object of class 'mixedgraph'")
  ord <- topologicalOrder(graph, warn=FALSE)
  if (any(is.na(ord))) return(TRUE)
  
  return(FALSE)
}

##' Test type of graph
##' 
##' @param graph \code{mixedgraph} object
##' 
##' \code{is_UG}, \code{is_DAG}, \code{is_ADMG}, \code{is_SG} respectively
##' test whether a graph is an undirected graph, directed acyclic graph (DAG), 
##' acyclic directed mixed graph (ADMG) or summary graph.
##' 
##' @name test_graph
NULL

##' @describeIn test_graph test if an undirected graph
##' @export 
is_UG = function(graph) {
  if (!is.mixedgraph(graph)) stop("Must be an object of class 'mixedgraph'")
  ## should only contain undirected edges
  if(nedge(graph, setdiff(names(graph$edges), "undirected")) > 0) return(FALSE)
  
  return(TRUE)
}

##' @describeIn test_graph test if a bidirected graph
##' @export 
is_BG = function(graph) {
  if (!is.mixedgraph(graph)) stop("Must be an object of class 'mixedgraph'")
  ## should only contain undirected edges
  if(nedge(graph, setdiff(names(graph$edges), "bidirected")) > 0) return(FALSE)
  
  return(TRUE)
}

##' @describeIn test_graph test if a directed acyclic graph (DAG)
##' @export 
is_DAG = function(graph) {
  if (!is.mixedgraph(graph)) stop("Must be an object of class 'mixedgraph'")
  ## should only contain directed edges
  if(nedge(graph, setdiff(names(graph$edges), "directed")) > 0) return(FALSE)
  
  return(!is_cyclic(graph))
}

##' @describeIn test_graph test if an acyclic directed mixed graph (ADMG)
##' @export 
is_ADMG = function(graph) {
  if (!is.mixedgraph(graph)) stop("Must be an object of class 'mixedgraph'")
  ## should only contain directed and bidirected edges
  if(nedge(graph, setdiff(names(graph$edges), c("directed", "bidirected"))) > 0) return(FALSE)
  
  return(!is_cyclic(graph))
}

##' @describeIn test_graph test if a summary graph
##' @export 
is_SG = function(graph) {
  if (!is.mixedgraph(graph)) stop("Must be an object of class 'mixedgraph'")
  ## should only contain undirected, directed and bidirected edges
  if(nedge(graph, setdiff(names(graph$edges), c("undirected", "directed", "bidirected"))) > 0) return(FALSE)
  
  v <- graph$v
  nbs <- nb(graph, v)
  if (length(intersect(ch(graph, v), nbs)) ||
        length(intersect(sp(graph, v), nbs))) return(FALSE)
  
  return(!is_cyclic(graph))
}

##' Deprecated functions to test type of graph
##' 
##' @param graph \code{mixedgraph} object
##' 
##' \code{is.UG}, \code{is.DAG}, \code{is.ADMG}, \code{is.SG} respectively
##' test whether a graph is an undirected graph, directed acyclic graph (DAG), 
##' acyclic directed mixed graph (ADMG) or summary graph.  These have since 
##' been replaced by \code{is_UG} etc.
##' 
##' @name test_graph-deprecated
NULL

##' @describeIn test_graph-deprecated version of \code{is_UG}
##' @export 
is.UG = function(graph) {
  .Deprecated("is_UG")

  if (!is.mixedgraph(graph)) stop("Must be an object of class 'mixedgraph'")
  ## should only contain undirected edges
  if(any(lengths(graph$edges[!names(graph$edges)=="undirected"]) > 0)) return(FALSE)
  
  return(TRUE)
}

##' @describeIn test_graph-deprecated version of \code{is_DAG}
##' @export
is.DAG = function(graph) {
  .Deprecated("is_DAG")
 
  if (!is.mixedgraph(graph)) stop("Must be an object of class 'mixedgraph'")
  ## should only contain directed edges
  if(any(lengths(graph$edges[!names(graph$edges)=="directed"]) > 0)) return(FALSE)
  
  return(!is_cyclic(graph))
}

##' @describeIn test_graph-deprecated version of \code{is_ADMG}
##' @export
is.ADMG = function(graph) {
  .Deprecated("is_ADMG")

  if (!is.mixedgraph(graph)) stop("Must be an object of class 'mixedgraph'")
  ## should only contain directed and bidirected edges
  if(nedge(graph, setdiff(names(graph$edges), c("directed", "bidirected"))) > 0) return(FALSE)
  
  return(!is_cyclic(graph))
}

##' @describeIn test_graph-deprecated version of \code{is_SG}
##' @export
is.SG = function(graph) {
  .Deprecated("is_SG")

  if (!is.mixedgraph(graph)) stop("Must be an object of class 'mixedgraph'")
  ## should only contain undirected, directed and bidirected edges
  if(nedge(graph, setdiff(names(graph$edges), c("undirected", "directed", "bidirected"))) > 0) return(FALSE)
  
  v <- graph$v
  nbs <- nb(graph, v)
  if (length(intersect(ch(graph, v), nbs)) ||
      length(intersect(sp(graph, v), nbs))) return(FALSE)
  
  return(!is_cyclic(graph))
}
