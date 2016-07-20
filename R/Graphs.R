.onLoad <- function(libname, pkgname) {
  vig_list = tools::vignetteEngine(package = 'knitr')
  vweave <- vig_list[['knitr::knitr']][c('weave')][[1]]
  vtangle <- vig_list[['knitr::knitr']][c('tangle')][[1]]
  tools::vignetteEngine(pkgname, weave = vweave, tangle = vtangle,
                        pattern = "[.]Rmd$", package = pkgname)
  #register_vignette_engines(pkgname)
}

graphOptionsEnv <- new.env()

## List of edge types, which can be expanded.
## type     : character name of edge type
## char     : character representation of edge
## revchar  : if directed, representation of reverse orientation
## directed : does the orientation of this edge have meaning?
## hyper    : is this a hyper edge? [Not yet supported.]
assign("edgeTypesDF", 
       data.frame(type=c("undirected", "directed", "bidirected", "partially directed", "partially undirected", "not directed"),
                  char=c("---","-->","<->", ".->", ".--", ".-."),
                  revchar=c(NA, "<--" ,NA, "<-.", "--.", NA),
                  directed=c(FALSE, TRUE, FALSE, TRUE, TRUE, FALSE),
                  hyper=c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE), stringsAsFactors=FALSE),
       envir=graphOptionsEnv)


## List of graph formats, which can be expanded.
## format    : character name of graph format
## package   : associated package
assign("graphFormatsDF", 
       data.frame(format=c("mixedgraph", "graphNEL", "graphAM", "graphBAM", "igraph", "gAlgo", "ggm"),
                  package=c("MixedGraphs", "graph", "graph", "graph", "igraph", "pcalg", "ggm"), stringsAsFactors=FALSE),
       envir=graphOptionsEnv)

##' @export edgeTypes
graphFormats <- function() {
  get("graphFormatsDF", envir=graphOptionsEnv)
}

## need to add a function to add edge types

## List of vertex types, which can be expanded [not currently supported]
## type     : character name of vertex type
assign("vertexTypesDF", data.frame(type=c("random"),
                                    hidden=FALSE), 
       envir=graphOptionsEnv)

##' See list of edge types
##' 
##' Returns data frame of current edge types.
##' @export edgeTypes
edgeTypes <- function() {
  get("edgeTypesDF", envir=graphOptionsEnv)
}

##' @export vertexTypes
vertexTypes <- function() {
  get("vertexTypesDF", envir=graphOptionsEnv)
}

##' Construct a mixed graph
##' 
##' @param n integer number of vertices in graph
##' @param v integer identifiers of vertices (length \code{n})
##' @param edges named list of edges in graph
##' @param vnames character vector of vertex names (defaults to \code{x1}, \code{x2}, ...)
##' @param vtype optionally, a character vector of vertex types
##' 
##' @details Currently row/column names on adjacency matrices are 
##' dropped.  Might be useful to change this functionality in the future
##' if they match (or can replace) vertex names.
##' 
##' @seealso \code{\link{graphCr}}.
##' 
##' @export mixedgraph
mixedgraph = function(n, v=seq_len(n), edges = list(), vnames, vtype) {

  ## Check vertices are positive integers
  if (missing(n)) {
    n = length(v)
  }
  else if (length(v) != n) stop("Vertex list must have length 'n'")
  else if (any(v < 1)) stop("Vertices must be numbered as positive integers")
  if (!missing(vtype)) {
    vtype <- pmatch(vtype, vertexTypes()$type)
    if (any(is.na(vtype))) stop("Some vertex types not matched")
    if (length(vtype) != n) vtype <- rep.int(vtype, length=n)
  }

  if (missing(vnames) || is.null(vnames)) {
    if (length(v) > 0) vnames = paste("x", seq_len(max(v)), sep="")
    else vnames = character(0)
  }
  else if (length(v) > 0 && length(vnames) < max(v)) {
    stop("Variable names vector must be at least max(v)")
  }
  
  if (length(edges) > 0) {
    ## Check that edge types are matched by global options
    etys = edgeTypes()$type
    if (is.null(names(edges))) et = seq_along(edges)
    else et = pmatch(names(edges), etys)
    if (length(et) == 1 && is.na(et)) {
      warning("No edge type given, assuming undirected")
      et = 1
    }
    else if (any(is.na(et))) stop("Edge types not matched")
    else if (any(duplicated(et))) stop("Repeated edge types matched")
    
    ## Check all edges given as lists are valid and of length 2
    edL <- sapply(edges, is.list)
    if (any(is.na(match(unlist(edges[edL]), v)))) stop("Edges must be between vertices in the graph")
    if (any(sapply(unlist(edges[edL], recursive=FALSE), length) != 2)) stop("Hyper-edges not yet supported")
    
    ## Check all edges given as edge matrices are valid and of length 2
    edE <- sapply(edges, is.edgeMatrix)
    if (any(is.na(match(unlist(edges[edE]), v)))) stop("Edges must be between vertices in the graph")
    if (any(sapply(edges[edE], nrow) != 2)) stop("Hyper-edges not yet supported")
    
    ## Construct edge lists
    edgeList = list()
    for (i in seq_along(et)) {
      dimnames(edges[[i]]) <- NULL   # drop dimnames
      edgeList[[etys[et[i]]]] = edges[[i]]
    }
    class(edgeList) = "edgeList"
    wh = which(names(edgeList) == "")
    if (length(wh) > 0) names(edgeList)[wh] = paste("EdgeType", wh, sep="")
  }
  else {
    edgeList = list()
  }

  out = list(v=v, edges=edgeList, vnames=vnames)
  class(out) = "mixedgraph"
  return(out)
}

##' @export is.mixedgraph
is.mixedgraph <- function(object) {
  "mixedgraph" %in% class(object)
}

##' Check if object could be an edgeMatrix
##' @export is.edgeMatrix
is.edgeMatrix <- function(object, n) {
  if (!is.matrix(object)) return(FALSE)
  if (!missing(n) && any(object > n)) return(FALSE)
  if (nrow(object) != 2) return(FALSE)
  if (any(object <= 0)) return(FALSE)
  return(TRUE)
}

##' Check if object could be an edgeMatrix
##' 
##' Currently assumes that entries must be non-negative
##' @export is.adjMatrix
is.adjMatrix <- function(object, n) {
  if (!is.matrix(object)) return(FALSE)
  if (!missing(n) && n != ncol(object) && n != nrow(object)) return(FALSE)
  else if (nrow(object) != ncol(object)) return(FALSE)
  if (any(object < 0)) return(FALSE)
  return(TRUE)
}

##' @export print.mixedgraph
print.mixedgraph = function(x, ...) {
  n = length(x$v)
  cat("Graph with ", n, ifelse(n == 1, " vertex", " vertices"),
      ifelse(n == 0,"",":  "), sep="")
  cat(x$vnames[x$v], "\n\n", sep="  ")
  
  # get list of edge symbols
  whEdge <- match(names(x$edges), edgeTypes()$type)
  edgeSymb <- edgeTypes()$char
  
  for (i in seq_along(x$edges)) {
    if (is.edgeMatrix(x$edges[[i]])) {
      tmp <- x$edges[[i]]
      for (j in seq_len(ncol(tmp))) {
        cat(x$vnames[tmp[1,j]], edgeSymb[whEdge[i]],
            x$vnames[tmp[2,j]], "\n", sep=" ")
      }      
    }
    else if (is.adjMatrix(x$edges[[i]])) {
      if (all(x$edges[[i]]==0)) next
      tmp <- cbind(row(x$edges[[i]])[x$edges[[i]] > 0], col(x$edges[[i]])[x$edges[[i]] > 0])
      if(!edgeTypes()$directed[whEdge[i]]) tmp = tmp[tmp[,1] < tmp[,2],,drop=FALSE]
      
      for (j in seq_len(nrow(tmp))) {
        cat(x$vnames[tmp[j,1]], edgeSymb[whEdge[i]],
            x$vnames[tmp[j,2]], "\n", sep=" ")
      }
    }
    else if (is.list(x$edges[[i]])) {
      if (!is.null(x$edges[[i]]) && length(x$edges[[i]]) > 0) {
        for (j in seq_along(x$edges[[i]])) {
          cat(x$vnames[x$edges[[i]][[j]][1]], edgeSymb[whEdge[i]],
              x$vnames[x$edges[[i]][[j]][2]], "\n", sep=" ")
        }
        #        cat("\n")
      }
    } 
  }    
  cat("\n")
  
  invisible(x)
}

##' @describeIn subGraph
##' @export [.mixedgraph
`[.mixedgraph` = function(graph, v, ...) {
  if (missing(v)) return(graph)
  v = v[v != 0]  # remove 0s
  if (length(v) > 0 && all(v < 0)) v = setdiff(graph$v, -v)
  subGraph(graph, v)
}

##' Take induced vertex subgraph of mixedgraph
##' 
##' @param graph a \code{mixedgraph} object
##' @param v vertices to keep
##' @param drop force drop removed vertices from representation?
##' 
##' @export subGraph
subGraph = function (graph, v, drop=FALSE) {
#  v = v[v <= graph$n]
  
  if (length(v) == 0) {
    if (drop) out = mixedgraph(n=0)
    else out = mixedgraph(n=0, vnames=graph$vnames)
    return(out)
  }
  v = unique.default(v)
  if (drop) v <- sort.int(v)
  if (!all(v %in% graph$v)) stop("Can only keep vertices which are present")
  if (length(v)==length(graph$v)) return(graph)

  edges = lapply(graph$edges, function(x) {
    if (is.adjMatrix(x)) {
      if (drop) return(x[v,v])
      else {
        x[-v,] = x[,-v] = 0L
        return(x)
      }
    } 
    else if (is.edgeMatrix(x)) {
      tmp <- x[, (x[1,] %in% v) & (x[2,] %in% v), drop=FALSE]
      if (drop) {
        mask <- match(seq_len(max(v)), v)
        tmp <- apply(tmp, 1:2, function(x) mask[x])
        if (any(is.na(tmp))) stop("Something went wrong")
      }
      return(tmp)
    }
    else if (is.list(x)) {
      if (length(x) > 0) {
        tmp <- x[sapply(x, function(y) all(y %in% v))]
        
        if (drop) {
          mask <- match(seq_len(max(v)), v)
          tmp <- lapply(tmp, function(x) mask[x])
          if (any(sapply(tmp, function(x) any(is.na(x))))) stop("Something went wrong")
        }
        return(tmp)
      }
      else return(list())
    }
  })

  if (drop) out = list(v=seq_along(v), edges=edges, vnames=graph$vnames[v])
  else out = list(v=v, edges=edges, vnames=graph$vnames)
  class(out) = "mixedgraph"
  out
}

##' Create graph from character parsing
##' 
##' Convenience function for quickly constructing graphs with character input
##' 
##' @param char string of inputs given by vertex names separated by edges
##' @param ... other strings of further edges
##' @param useMatrices in mixed graph representation, should the output
##' use adjacency matrices?
##' @param representation 
##' 
##' @details Symbols \code{-<>=*|:} are assumed to be part of an edge, so
##' cannot be used in node names for this function.
##' 
##' @examples
##' graphCr("1--->2<-->3<-4","2<->4,4->5")
##' graphCr("1-2-3-4-1", representation="graphNEL")  # requires package 'graph'
##' @export graphCr
graphCr <- function(char, ..., useMatrices=FALSE, representation="mixedgraph") {
  out <- list(char, ...)
  # in future try to allow direct typing
  #out <- unlist(sapply(out, as.character))
  out <- unlist(out)
  out <- unlist(strsplit(out, ","))
  out <- gsub("[-]+", "-", out)
  out <- gsub("[=]+", "=", out)
  out <- gsub("[ ]+", " ", out)
  out <- gsub("([A-Z0-9a-z])([-<>=*.|:])", "\\1 \\2", out)
  out <- gsub("([-<>=*.|:])([A-Z0-9a-z])", "\\1 \\2", out)
  out <- strsplit(out, " ")
  out <- lapply(out, function(x) x[x != ""])
  
  em <- matrix(unlist(lapply(out, function(x) {
      k <- length(x)
      if (k == 1) return(matrix("", 3, 0))
      if (k %% 2 == 0) stop(paste("\"", out, "\" didn't parse correctly", sep=""))
      x[rep(seq(k),times=c(1,rep(c(1,2),length=k-3),1,1))]
    })), nrow=3)
  
  pssedges <- edgeTypes()$char
  mask <- c(seq_along(pssedges), which(edgeTypes()$directed))
  nedgetypes <- length(pssedges)
  pssedges <- c(pssedges, edgeTypes()$revchar[edgeTypes()$directed])
  pssedges <- gsub("[-]+", "-", pssedges)
  pssedges <- gsub("[=]+", "=", pssedges)
  
  etype <- match(em[2,], pssedges)
  if (any(is.na(etype))) {
    stop("Some edge types not matched")
  }
  
  ## unattached vertices
  unattached <- unlist(lapply(out, function(x) {
    k <- length(x)
    if (k != 1) return(character(0))
    else return(x)
  }))
  
  ## first see if numbers were supplied
  ## v0 corresponds to vertices not attached to edges
  v1 <- suppressWarnings(as.integer(em[1,]))
  v2 <- suppressWarnings(as.integer(em[3,]))
  v0 <- suppressWarnings(as.integer(unattached))
  
  ## at least one name wasn't a number, so use supplied names
  if (any(is.na(v1)) || any(is.na(v2)) || any(is.na(v0))) {
    vs_tmp <- factor(c(t(em[c(1,3),,drop=FALSE]), unattached))
    v1 <- as.integer(vs_tmp[seq_len(ncol(em))])
    v2 <- as.integer(vs_tmp[seq_len(ncol(em))+ncol(em)])
    v0 <- as.integer(vs_tmp[2*seq_len(ncol(em))+seq_along(unattached)])
    vnames <- levels(vs_tmp)
  }
  else vnames = NULL
  
  tmp <- v1[etype > nedgetypes]
  v1[etype > nedgetypes] <- v2[etype > nedgetypes]
  v2[etype > nedgetypes] <- tmp
  etype = mask[etype]
  edges <- list()
  etys <- edgeTypes()$type
  for (i in seq_along(v1)) {
    edges[[etys[etype[i]]]] = c(edges[[etys[etype[i]]]], list(c(v1[i], v2[i])))
  }
  n <- max(c(v1, v2, v0))
  
  out <- mixedgraph(n, edges=edges, vnames=vnames)
  if (representation != "mixedgraph") {
    out <- convert(out, format=representation)
  }
  else if (useMatrices) out <- withAdjMatrix(out)
  
  out
}

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

  etys = edgeTypes()$type
  wh = pmatch(type, etys)
  if (is.na(wh)) stop(paste("Edge type not matched: should be one of ", paste(etys, collapse=", "), sep=""))
  else names(edges) = etys[wh]

  out = mixedgraph(n=n, edges=edges)
  return(out)
}

##' @describeIn makeGraphComplete
##' @export makeGraphChain
makeGraphChain = function(n, type = "undirected") {
  edges = list(lapply(seq_len(max(n-1,0)), function(x) c(x, x+1)))

  etys = edgeTypes()$type
  wh = pmatch(type, etys)
  if (is.na(wh)) stop(paste("Edge type not matched: should be one of ", paste(etys, collapse=", "), sep=""))
  else names(edges) = etys[wh]
  
  out = mixedgraph(n=n, edges=edges)
  return(out)
}

##' @describeIn makeGraphComplete
##' @export makeGraphCycle
makeGraphCycle = function(n, type = "undirected") {

  edges = list(lapply(seq_len(max(n-1,0)), function(x) c(x, x+1)))
  if (n > 2) edges[[1]][[n]] = c(n, 1)

  etys = edgeTypes()$type
  wh = pmatch(type, etys)
  if (is.na(wh)) stop(paste("Edge type not matched: should be one of ", paste(etys, collapse=", "), sep=""))
  else names(edges) = etys[wh]
  
  out = mixedgraph(n=n, edges=edges)
  return(out)
}

##' @describeIn makeGraphComplete
##' @export makeGraphStar
makeGraphStar = function(n, type = "undirected") {
  
  if (n <= 1) return(makeGraphComplete(n, type))
  edges = list(lapply(seq_len(n-1), function(x) c(x, n)))
  
  etys = edgeTypes()$type
  wh = pmatch(type, etys)
  if (is.na(wh)) stop(paste("Edge type not matched: should be one of ", paste(etys, collapse=", "), sep=""))
  else names(edges) = etys[wh]
  
  out = mixedgraph(n=n, edges=edges)
  return(out)
}


# distail(gr, 3)
# anc(gr, 3)
# 
# grv = graph(4, e=list(d=list(1:2,2:3,3:4),b=list(c(2,4))))
# 
# makeGraphChain(10)
# 
# graph(4)
# graph(4, e=list(list(1:2)))
# #gr = graph(6, e=list(list(1:2, 3:4), list(c(2,3)), list(4:5), dashed=list(c(1,6))))
# gr = graph(6, e=list(list(1:2, 3:4), list(c(2,3)), list(4:5)))
# 
# gr[1:3]
# gr[c(2,4,5)]
# graph(v=c(), edges=list(), vnames=c("x1","x2"))$vnames
# gr[c()]$vnames
# 
# ####### 
