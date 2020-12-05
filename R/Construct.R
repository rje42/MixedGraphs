##' Graph Constructor Functions
##' 
##' @param n number of vertices (or rows for a grid)
##' @param type type of edges to use
##' 
##' @details Provides some construction 
##' functions for common graphs: complete graphs, 
##' chains, cycles and stars.
##' 
##' @export 
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
##' @export 
makeGraphEmpty = function(n) {
  edges = list(undirected=list())
  class(edges[[1]]) <- "eList"
  
  out = mixedgraph(n=n, edges=edges)
  return(out)
}

##' @describeIn makeGraphComplete graph with chain of edges
##' @export 
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
##' @export 
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
##' @param out should edges be directed out from the centre?
##' @export 
makeGraphStar = function(n, type = "undirected", out=FALSE) {
  
  if (n <= 1) return(makeGraphComplete(n, type))
  if (out) edges = list(lapply(seq_len(n-1)+1, function(x) c(1, x)))
  else edges = list(lapply(seq_len(n-1), function(x) c(x, n)))
  class(edges[[1]]) <- "eList"
  
  etys = edgeTypes()$type
  wh = pmatch(type, etys)
  if (is.na(wh)) stop(paste("Edge type not matched: should be one of ", paste(etys, collapse=", "), sep=""))
  else names(edges) = etys[wh]
  
  out = mixedgraph(n=n, edges=edges)
  return(out)
}

##' @describeIn makeGraphComplete grid of vertices
##' @param m number of columns
##' @export 
makeGraphGrid = function(n, m=n, type="undirected") {
  
  if (n == 0) return(makeGraphEmpty(0))
  if (n == 1) return(makeGraphChain(m, type))

  first <- c(matrix(seq_len(n*m),nrow=n, ncol=m)[-m,,drop=FALSE])
  tmp <- lapply(first, function(x) c(x,x+1))
  out <- eList(c(tmp, lapply(seq_len(n*(m-1)), function(x) c(x,x+n))))
  edges <- list(out)
  
  ## match edge type
  etys = edgeTypes()$type
  wh = pmatch(type, etys)
  if (is.na(wh)) stop(paste("Edge type not matched: should be one of ", paste(etys, collapse=", "), sep=""))
  else names(edges)[[1]] = etys[wh]
  
  vnames = paste0(paste0("x", rep_len(seq_len(n), m*n)), rep(seq_len(m), each=n))
    
  mixedgraph(n*m, edges=edges, vnames=vnames)
}
