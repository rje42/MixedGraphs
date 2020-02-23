##' Graph Constructor Functions
##' 
##' @param n number of vertices
##' @param type type of edges to use
##' 
##' @details Provides some construction 
##' functions for common graphs: complete graphs, 
##' chains, cycles and stars.
##' 
##' @export makeGraphComplete
makeGraphComplete = function (n, type = "undirected") {
  if (n < 2) return(mixedgraph(n))
  tmp = combn(n, 2)
  edges = list(list())
  for (i in seq_len(ncol(tmp))) edges[[1]][[i]] = tmp[,i]
  class(edges[[1]]) <- "eList"
  
  etys = edgeTypes()$type
  wh = pmatch(type, etys)
  if (is.na(wh)) stop(paste("Edge type not matched: should be one of ", paste(etys, collapse=", "), sep=""))
  else names(edges) = etys[wh]
  
  out = mixedgraph(n=n, edges=edges)
  return(out)
}

##' @describeIn makeGraphComplete graph with no edges
##' @export makeGraphEmpty
makeGraphEmpty = function(n) {
  edges = list(undirected=list())
  class(edges[[1]]) <- "eList"
  
  out = mixedgraph(n=n, edges=edges)
  return(out)
}

##' @describeIn makeGraphComplete graph with chain of edges
##' @export makeGraphChain
makeGraphChain = function(n, type = "undirected") {
  edges = list(lapply(seq_len(max(n-1,0)), function(x) c(x, x+1)))
  class(edges[[1]]) <- "eList"
  
  etys = edgeTypes()$type
  wh = pmatch(type, etys)
  if (is.na(wh)) stop(paste("Edge type not matched: should be one of ", paste(etys, collapse=", "), sep=""))
  else names(edges) = etys[wh]
  
  out = mixedgraph(n=n, edges=edges)
  return(out)
}

##' @describeIn makeGraphComplete graph with cycle of edges
##' @export makeGraphCycle
makeGraphCycle = function(n, type = "undirected") {
  
  edges = list(lapply(seq_len(max(n-1,0)), function(x) c(x, x+1)))
  if (n > 2) edges[[1]][[n]] = c(n, 1)
  class(edges[[1]]) <- "eList"
  
  etys = edgeTypes()$type
  wh = pmatch(type, etys)
  if (is.na(wh)) stop(paste("Edge type not matched: should be one of ", paste(etys, collapse=", "), sep=""))
  else names(edges) = etys[wh]
  
  out = mixedgraph(n=n, edges=edges)
  return(out)
}

##' @describeIn makeGraphComplete star shaped graph
##' @export makeGraphStar
makeGraphStar = function(n, type = "undirected") {
  
  if (n <= 1) return(makeGraphComplete(n, type))
  edges = list(lapply(seq_len(n-1), function(x) c(x, n)))
  class(edges[[1]]) <- "eList"
  
  etys = edgeTypes()$type
  wh = pmatch(type, etys)
  if (is.na(wh)) stop(paste("Edge type not matched: should be one of ", paste(etys, collapse=", "), sep=""))
  else names(edges) = etys[wh]
  
  out = mixedgraph(n=n, edges=edges)
  return(out)
}
