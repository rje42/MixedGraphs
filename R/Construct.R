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
##' @param m number of columns or size of second set
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

##' @describeIn makeGraphComplete bipartite graph
##' @export
makeGraphBipartite <- function (n, m, type="undirected") {
  out <- makeGraphComplete(n+m, type=type)
  out <- mutilate(out, seq_len(n), internal=TRUE)
  out <- mutilate(out, n+seq_len(m), internal=TRUE)
  
  return(out)
}

##' Map a graph onto an induced subgraph
##' 
##' @param graph an object of class `mixedgraph`
##' @param ord an ordering to use for the existing vertices
##' @param vnames a character vector of variable names
##' 
##' @details
##' Either `ord` or `vnames` must be specified, and if they are both given then
##' they must have the same length.  `vnames` must contain all the variable names
##' used in `graph`.  
##' 
##' `ord` places the vertices in the order provided.  For example, if the 
##' supplied vector is `c(2, 4, 1, 3)` for a graph with vertices `"x1"` and 
##' `"x2"`, then the output graph will have the vertex order `"x2", "x_4", "x_1", "x_3"`.
##' 
##' @export
to_subgraph <- function (graph, ord, vnames) {

  ## think about what happens if graph has no vertices
  if (missing(ord)) { 
    if (missing(vnames)) stop("Must provide either an ordering or variable names")
    n <- length(vnames)
    loc <- match(graph$vnames[graph$v], vnames)
    if (any(is.na(loc))) stop("Not all names in supplied graph matched")
    match(vnames, graph$vnames, 0L)
  }
  else {
    if (missing(vnames)) {
      n <- length(ord)
      vnames <- character(n)
      wh_v <- match(graph$v, ord)
      vnames[wh_v] <- graph$vnames[graph$v]
      cands <- paste0("x", seq_len(n))
      cands <- cands[-match(graph$vnames[graph$v], cands, nomatch = 0L)]
      vnames[-wh_v] <- cands[seq_len(n-nv(graph))]
    }
  }
  
  out <- addNodes(graph, n-nv(graph))
  out <- out[ord, order=TRUE]
  out$vnames <- vnames
  
  return(out)
}