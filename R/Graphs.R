graphOptionsEnv <- new.env()

## List of edge types, which can be expanded.
## type     : character name of edge type
## char     : character representation of edge
## revchar  : if directed, representation of reverse orientation
## directed : does the orientation of this edge have meaning?
## hyper    : is this a hyper edge? [Not yet supported.]
assign("edgeTypesDF", 
       data.frame(type=c("undirected", "directed", "bidirected", "partially directed", "partially undirected", "not directed"),
                  char=c("---","-->","<->", "o->", "o--", "o-o"),
                  revchar=c(NA, "<--" ,NA, "<-o", "--o", NA),
                  directed=c(FALSE, TRUE, FALSE, TRUE, TRUE, FALSE),
                  hyper=c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
                  abbrv=c("un", "dir", "bi", "pdir", "pun", "non"), stringsAsFactors=FALSE),
       envir=graphOptionsEnv)


## List of graph formats, which can be expanded.
## format    : character name of graph format
## package   : associated package
assign("graphFormatsDF", 
       data.frame(format=c("mixedgraph", "ADMG", "graphNEL", "graphAM", "graphBAM", "igraph", "gAlgo", "ggm", "bn"),
                  package=c("MixedGraphs", "ADMGs", "graph", "graph", "graph", "igraph", "pcalg", "ggm", "bnlearn"), stringsAsFactors=FALSE),
       envir=graphOptionsEnv)

graphFormats <- function() {
  get("graphFormatsDF", envir=graphOptionsEnv)
}

## need to add a function to add edge types

## List of vertex types, which can be expanded [not currently supported]
## type     : character name of vertex type
assign("vertexTypesDF", data.frame(type=c("random"),
                                   hidden=FALSE), 
       envir=graphOptionsEnv)

##' See list of vertex and edge types
##' 
##' Returns data frame of current edge types.
##' 
##' @export
edgeTypes <- function() {
  get("edgeTypesDF", envir=graphOptionsEnv)
}

##' @describeIn edgeTypes See vertex types
##' @export
vertexTypes <- function() {
  get("vertexTypesDF", envir=graphOptionsEnv)
}

##' Graph Operations
##' 
##' @param graph an object of class `mixedgraph`
##' @param edges character vector of edge types to include, defaults to all
##' 
##' @details `nedge` uses an internal function `nedge2` to count for
##' each type of edge separately. Do we want this to apply to graphs or edge lists
##' or both?
##' 
##' @name graphOps
NULL

##' @describeIn graphOps names of vertices for `mixedgraph` object
##' @export
vnames <- function(graph) {
  graph$vnames
}

##' @describeIn graphOps number of vertices for `mixedgraph` object
##' @export
nv <- function(graph) {
  length(graph$v)
}

##' Construct a mixed graph
##' 
##' @param n integer number of vertices in graph
##' @param v integer identifiers of vertices (length `n`)
##' @param edges named list of types of edge in graph
##' @param vnames character vector of vertex names (defaults to `x1`, `x2`, ...)
##' @param vtype optionally, a character vector of vertex types
##' 
##' @details Currently row/column names on adjacency matrices are 
##' dropped.  Might be useful to change this functionality in the future
##' if they match (or can replace) vertex names.
##' 
##' @seealso \code{\link{graphCr}}.
##' 
##' @export
mixedgraph <- function(n, v=seq_len(n), edges = list(), vnames, vtype) {
  
  ## Check vertices are positive integers
  if (missing(n)) {
    n <- length(v)
  }
  else if (length(v) != n) stop("Vertex list must have length 'n'")
  else if (any(v < 1)) stop("Vertices must be numbered as positive integers")
  if (!missing(vtype)) {
    vtype <- pmatch(vtype, vertexTypes()$type)
    if (any(is.na(vtype))) stop("Some vertex types not matched")
    if (length(vtype) != n) vtype <- rep(vtype, length=n)
  }
  
  ## use x1, x2, as vertex names if not supplied
  if (missing(vnames) || is.null(vnames)) {
    if (length(v) > 0) vnames <- paste0("x", seq_len(max(v)))
    else vnames <- character(0)
  }
  else if (length(v) > 0 && length(vnames) < max(v)) {
    stop("Variable names vector must have length at least max(v)")
  }

  ## remove edge types with no edges in
  edges <- edges[lengths(edges) > 0]
    
  ## now consider the edges
  if (length(edges) > 0) {

    ## Check that edge types are matched by global options
    etys <- edgeTypes()$type
    if (is.null(names(edges))) et <- seq_along(edges)
    else et <- pmatch(names(edges), etys)
    if (length(et) == 1 && is.na(et)) {
      warning("No edge type given, assuming undirected")
      et <- 1
    }
    else if (any(is.na(et))) stop("Edge types not matched")
    else if (any(duplicated(et))) stop("Repeated edge types matched")
    
    ## Check all edges given as lists are valid and of length 2
    edAL <- sapply(edges, is.adjList, checknm=TRUE)
    for (i in which(edAL)) if (edAL[i]) class(edges[[i]]) <- "adjList"
    if (any(is.na(match(unlist(edges[edAL]), v)))) stop("Edges must be between vertices in the graph")

    ## Check all edges given as edge matrices are valid and of length 2
    edE <- sapply(edges, is.edgeMatrix, checknm=TRUE)
    for (i in which(edE)) class(edges[[i]]) <- "edgeMatrix"
    if (any(is.na(match(unlist(edges[edE]), v)))) stop("Edges must be between vertices in the graph")
    if (any(sapply(edges[edE], nrow) != 2)) stop("Hyper-edges not yet supported")
    
    ## Check all edges given as adjacency matrices are valid
    edA <- sapply(edges, is.adjMatrix, checknm=TRUE)
    for (i in which(edA)) {
      if ("adjMatrix" %in% class(edges[[i]])) next
      class(edges[[i]]) <- c("adjMatrix", class(edges[[i]]))
    }
    
    ## Check all edges given as edgeLists are valid and of length 2
    edL <- sapply(edges, is.eList, checknm=FALSE)
    for (i in which(edL)) if (edL[i]) class(edges[[i]]) <- "eList"
    if (any(is.na(match(unlist(edges[edL]), v)))) stop("Edges must be between vertices in the graph")
    if (any(sapply(unlist(edges[edL], recursive=FALSE), length) != 2)) stop("Hyper-edges not yet supported")
    
    
    ## refuse to continue if edge types not unambiguously specified
    if (!all(edAL | edL | edE | edA)) stop("Edge types not all given")
    
    ## Construct edge lists
    edgeList <- list()
    for (i in seq_along(et)) {
      dimnames(edges[[i]]) <- NULL   # drop dimnames
      edgeList[[etys[et[i]]]] <- edges[[i]]
    }
    class(edgeList) <- "edgeList"
    wh <- which(names(edgeList) == "")
    if (length(wh) > 0) names(edgeList)[wh] <- paste("EdgeType", wh, sep="")
  }
  else {
    edgeList <- list()
    class(edgeList) <- "edgeList"
  }
  
  out <- list(v=v, edges=edgeList, vnames=vnames)
  class(out) <- "mixedgraph"
  return(out)
}

##' Test for `mixedgraph`
##' 
##' Is this object a `mixedgraph`?
##' 
##' @param object Object to be tested
##' 
##' @details Returns `TRUE` or `FALSE`
##' 
##' @export
is.mixedgraph <- function(object) {
  "mixedgraph" %in% class(object)
}


##' Print a mixedgraph object
##' 
##' Prints a `mixedgraph` object to the standard output.
##' 
##' @param x an object of class `mixedgraph`
##' @param ... other arguments to `print`
##' 
##' @export 
print.mixedgraph <- function(x, ...) {
  n <- length(x$v)
  cat("Graph with ", n, ifelse(n == 1, " vertex", " vertices"),
      ifelse(n == 0,"",":  "), sep="")
  cat(x$vnames[x$v], "\n\n", sep="  ")
  
  print.edgeList(x$edges, x$vnames)
  
  invisible(x)
}

##' @export
print.edgeList <- function(x, vnames, ...) {
  
  if (length(x) == 0) return(invisible(x))
  
  # get list of edge symbols
  whEdge <- match(names(x), edgeTypes()$type)
  edgeSymb <- edgeTypes()$char
  
  if (missing(vnames)) {
    if (!is.null(attr(x, "vnames"))) {
      vnames <- attr(x, "vnames")
      n_v <- length(vnames)
    }
    else {
      adjM <- sapply(x, is.adjMatrix)
      if (any(adjM)) n_v <- nrow(x[[which(adjM)[1]]])
      else {
        adjL <- sapply(x, is.adjList)
        if (any(adjL)) n_v <- length(x[[which(adjL)[1]]])
        else {
          oth <- sapply(x, function(y) is.edgeMatrix(y) || is.eList(y))
          if (any(oth)) n_v <- max(unlist(x[oth]))
          else stop("No valid edgetypes")
        }
      }
      vnames <- paste0("x", seq_len(n_v))
    }
  }
  else n_v <- length(vnames)
  
  for (i in seq_along(x)) {
    if (is.edgeMatrix(x[[i]])) {
      tmp <- x[[i]]
      for (j in seq_len(ncol(tmp))) {
        cat(vnames[tmp[1,j]], edgeSymb[whEdge[i]],
            vnames[tmp[2,j]], "\n", sep=" ")
      }      
    }
    else if (is.adjMatrix(x[[i]])) {
      if (all(x[[i]]==0)) next
      tmp <- cbind(row(x[[i]])[x[[i]] > 0], col(x[[i]])[x[[i]] > 0])
      if(!edgeTypes()$directed[whEdge[i]]) tmp <- tmp[tmp[,1] < tmp[,2],,drop=FALSE]
      
      for (j in seq_len(nrow(tmp))) {
        cat(vnames[tmp[j,1]], edgeSymb[whEdge[i]],
            vnames[tmp[j,2]], "\n", sep=" ")
      }
    }
    else if (is.adjList(x[[i]], checknm=TRUE)) {
      tmp <- cbind(rep(seq_len(n_v), times=lengths(x[[i]][seq_len(n_v)])), unlist(x[[i]]))
      if (!edgeTypes()$directed[whEdge[i]]) tmp <- tmp[tmp[,1] < tmp[,2],,drop=FALSE]

      for (j in seq_len(nrow(tmp))) {
        cat(vnames[tmp[j,1]], edgeSymb[whEdge[i]],
            vnames[tmp[j,2]], "\n", sep=" ")
      }
    }
    else if (is.eList(x[[i]])) {
      if (!is.null(x[[i]]) && length(x[[i]]) > 0) {
        for (j in seq_along(x[[i]])) {
          cat(vnames[x[[i]][[j]][1]], edgeSymb[whEdge[i]],
              vnames[x[[i]][[j]][2]], "\n", sep=" ")
        }
        #        cat("\n")
      }
    }
    else stop("Not a recognised edge type")
  }    
  cat("\n")
  
  return(invisible(x))
}

##' @describeIn subGraph bracket notation for subgraphs
##' @param ... other arguments
##' @export
`[.mixedgraph` <- function(graph, v, w, ..., drop=FALSE, etype, order=FALSE) {
  if (missing(v)) {
    v <- graph$v
  }
  else if (is.logical(v)) v <- which(v)
  
  v <- v[v != 0]  # remove 0s
  if (length(v) > 0 && all(v < 0)) v <- setdiff(graph$v, -v)
  subGraph(graph, v, w, drop=drop, etype=etype, order=order)
}

##' Subgraphs for `mixedgraph` objects
##' 
##' Take induced vertex subgraph or bipartite subgraph of mixedgraph
##' 
##' @param graph a `mixedgraph` object
##' @param v vertices to keep (or restore)
##' @param w other set of vertices for bipartite subgraph
##' @param drop force removed vertices to be dropped from representation in adjacency matrices and lists?
##' @param etype edge types to keep (defaults to all)
##' @param order logical: force graph to follow new order implied by `v`?
##' 
##' @details If `order` is `TRUE`, then `edgeMatrix` and `eList` 
##' formats are converted to `edgeList`s (`adjMatrix` format is preserved).
##' 
##' Note that `order` will have no effect if `w` is specified.
##' 
##' 
##' @export 
subGraph <- function (graph, v, w, drop=FALSE, etype, order=FALSE) {
  
  ## set up vertices to take subgraph over
  if (missing(v)) {
    if (missing(etype)) return(graph)
    v <- graph$v
  }
  else if (is.logical(v)) v <- which(v)
  else v <- unique.default(v)
  ## and other set if supplied
  if (!missing(w)) {
    if (is.logical(w)) w <- which(w)
    else w <- unique.default(w)
  }
  
  
  if (length(v) == nv(graph) && drop) drop <- FALSE
  else if (drop && order)  stop("Cannot both drop vertices and reorder the remainder, use two separate calls")
  if (!order) {
    v <- sort.int(v)
    if (!missing(w)) {
      w <- sort.int(w)
      vw <- sort.int(union(v,w))
    }
  }

  #  v <- v[v <= graph$n]

  ## ensure unwanted edge types are dropped  
  if (!missing(etype)) {
    etype <- intersect(etype, names(graph$edges))
    graph$edges <- graph$edges[etype]
  }
  
  ## deal with cases of no vertices in either v or w
  if (length(v) == 0 && missing(w)) {
    if (drop) out <- mixedgraph(n=0)
    else out <- mixedgraph(n=0, vnames=graph$vnames)
    return(out)
  }
  else if (!missing(w)) {
    if (length(v) == 0) graph[w, drop=drop, etype=character(0)]
    else if (length(w) == 0) graph[v, drop=drop, etype=character(0)]
  }
  # v = unique.default(v)
  # if (drop) v <- sort.int(v)
  if (!all(v %in% graph$v) || (!missing(w) && !all(w %in% graph$v))) stop("Can only keep vertices that are present")
  if (order && missing(w)) {
    if (nedge(graph) > 0) {
      waM <- sapply(graph$edges, is.adjMatrix, checknm=TRUE)
      waL <- sapply(graph$edges, is.adjList, checknm=TRUE)
      not_aLaM <- names(graph$edges)[!(waL | waM)]
      for (i in seq_along(not_aLaM)) {
        graph$edges[[not_aLaM[i]]] <- adjList(graph$edges[[not_aLaM[i]]],
                                              n=length(vnames(graph)), 
                                              directed=edgeTypes()$directed[edgeTypes()$type==not_aLaM[i]])
      }
      waL <- sapply(graph$edges, is.adjList, checknm=TRUE)
      # graph <- withAdjMatrix(graph)
      
      graph$edges[waM] <- lapply(graph$edges[waM], function(x) `[`(x,v,v,drop=FALSE))
      graph$edges[waM] <- lapply(graph$edges[waM], function(x) `class<-`(x,"adjMatrix"))
      
      graph$edges[waL] <- lapply(graph$edges[waL], function(x) `[`(x,v))
      graph$edges[waL] <- lapply(graph$edges[waL], function(x) lapply(x, intersect, y=v))
      graph$edges[waL] <- lapply(graph$edges[waL], function(x) lapply(x, match, v))
      graph$edges[waL] <- lapply(graph$edges[waL], function(x) `class<-`(x,"adjList"))
    }

    if (length(v) < length(graph$vnames)) graph$vnames[] <- c(graph$vnames[v], graph$vnames[-v])
    else graph$vnames <- graph$vnames[v]
    
    return(graph)
  }
  
  noW <- missing(w)
  
  ## now 
  edges <- lapply(graph$edges, function(x) {
    if (is.adjMatrix(x, checknm=TRUE)) {
      if (noW) {
        if (drop) {
          out <- x[v,v,drop=FALSE]  # note a different 'drop' argument!
          class(out) <- "adjMatrix"
          return(out)
        }
        else {
          x[-v,] <- x[,-v] <- 0L
          return(x)
        }
      } 
      else {
        if (drop) {
          x[w,-v] <- x[-v,w] <- 0L
          x[-w,v] <- x[v,-w] <- 0L
          x[-c(v,w),-c(v,w)] <- 0L
          x <- x[vw,vw,drop=FALSE]  # note a different 'drop' argument!
          # out <- x[c(v,w),c(v,w),drop=FALSE]  
          class(x) <- "adjMatrix"
          return(x)
        }
        else {
          x[w,-v] <- x[-v,w] <- 0L
          x[-w,v] <- x[v,-w] <- 0L
          x[-c(v,w),-c(v,w)] <- 0L
          return(x)
        }
      }
    } 
    else if (is.edgeMatrix(x)) {
      if (noW) {
        tmp <- x[, (x[1,] %in% v) & (x[2,] %in% v), drop=FALSE]
        if (drop) {
          mask <- match(seq_len(max(v)), v)
          tmp <- apply(tmp, 1:2, function(x) mask[x])
          if (any(is.na(tmp))) stop("Something went wrong with the mask (edgeMatrix)")
        }
      }
      else {
        tmp <- x[, ((x[1,] %in% v) & (x[2,] %in% w)) | 
                   ((x[2,] %in% v) & (x[1,] %in% w)), drop=FALSE]
        if (drop) {
          mask <- match(seq_len(max(v)), vw)
          tmp <- apply(tmp, 1:2, function(x) mask[x])
          if (any(is.na(tmp))) stop("Something went wrong with the mask (edgeMatrix)")
        }
      }
      class(tmp) <- "edgeMatrix"
      return(tmp)
    }
    else if (is.adjList(x, checknm=TRUE)) {
      if (noW) {
        if (drop) {
          x <- x[v]
          mask <- match(graph$v, v)
          x <- lapply(x, function(w) mask[intersect(w, v)])
          if (any(is.na(unlist(x)))) stop("Something went wrong with the mask (adjList)")
        }
        else {
          x[-v] <- vector(mode="list", length=length(x)-length(v))
          x <- lapply(x, function(w) intersect(w, v))
        }
      } 
      else {
        vw <- intersect(v,w)
        v_w <- setdiff(v, vw)
        w_v <- setdiff(w, vw)
        
        x[vw] <- lapply(x[vw], intersect, y=c(v,w))
        x[v_w] <- lapply(x[v_w], intersect, y=w)
        x[w_v] <- lapply(x[w_v], intersect, y=v)
        x[-c(v,w)] <- list(integer(0))

        if (drop) {
          x <- x[vw]
        }
        else {
          x[-vw] <- list(integer(0))
        }
      }
      class(x) <- "adjList"
      return(x)
    }
    else if (is.eList(x)) {
      if (length(x) > 0) {
        if (noW) {
          tmp <- x[sapply(x, function(y) all(y %in% v))]
          
          if (drop) {
            mask <- match(seq_len(max(v)), v)
            tmp <- lapply(tmp, function(x) mask[x])
            if (any(sapply(tmp, function(x) any(is.na(x))))) stop("Something went wrong with the mask")
          }
        }
        else {
          tmp <- x[sapply(x, function(y) (y[1] %in% v && y[2] %in% w) ||
                            (y[2] %in% v && y[1] %in% w))]
        }
        class(tmp) <- "eList"
        return(tmp)
      }
      else {
        tmp <- list()
        class(tmp) <- "eList"
        return(tmp)
      }
    }
    else stop("Edge type not identified")
  })

  class(edges) <- "edgeList"
  
  if (noW) {
    if (drop) out <- list(v=seq_along(v), edges=edges, vnames=graph$vnames[v])
    else out <- list(v=v, edges=edges, vnames=graph$vnames)
  }
  else {
    if (drop) out <- list(v=seq_along(vw), edges=edges, vnames=graph$vnames[vw])
    else out <- list(v=vw, edges=edges, vnames=graph$vnames)
  }
  class(out) <- "mixedgraph"
  out
}

##' @describeIn subGraph Add back in vertices removed by taking a subgraph
##' @details
##' `restore_nodes` will add back in nodes whose names are still present, but
##' which were removed by a `subGraph` command.
##' 
##' @export
restore_nodes <- function (graph, v) {
  n <- length(graph$vnames)
  
  if (!missing(v)) {
    if (any(v > n) || any(v < 1))  stop("Invalid vertex values given")
    v <- sort.int(union(v, graph$v))
  }
  else v <- seq_len(n)
  
  ## insert vertices back in
  graph$v <- v
  
  return(graph)
}

##' Force graphs to have vertices 1,\dots,k
##'
##' @param graph `mixedgraph` object
##' 
##' Designed to make comparison of graphs easier
##' 
## @export
standardizeVertices <- function(graph) {
  #stop("FUNCTION NOT FINISHED")
  
  v <- graph$v
  k <- length(v)

  if (isTRUE(all.equal(v, seq_len(k)))) return(graph)
  
  edges <- lapply(graph$edges, function(x) {
    if (is.adjMatrix(x)) {
      return(x[v,v,drop=FALSE])
    } 
    else if (is.edgeMatrix(x)) {
      tmp <- x[, (x[1,] %in% v) & (x[2,] %in% v), drop=FALSE]
      mask <- match(seq_len(max(v)), v)
      tmp <- apply(tmp, 1:2, function(x) mask[x])
      if (any(is.na(tmp))) stop("Something went wrong with the mask")
      return(tmp)
    }
    else if (is.list(x)) {
      if (length(x) > 0) {
        tmp <- x[sapply(x, function(y) all(y %in% v))]
        
        mask <- match(seq_len(max(v)), v)
        tmp <- lapply(tmp, function(x) mask[x])
        if (any(sapply(tmp, function(x) any(is.na(x))))) stop("Something went wrong with the mask")
        
        return(tmp)
      }
      else return(list())
    }
  })
  
  graph$edges <- edges
  graph$vnames <- graph$vnames[graph$v]
  graph$v <- seq_len(k)
  
  graph
}

##' Write graphs in standard format
##' 
##' @param graph `mixedgraph` object
##' 
##' Designed to make comparison of graphs easier
##' 
##' @export
standardizeEdges <- function(graph) {

  kp <- rep(TRUE, length(graph$edges))
  for (i in seq_along(graph$edges)) {
    if (nedge(graph, names(graph$edges)[i]) == 0) kp[i] <- FALSE
  }
  graph$edges <- graph$edges[kp]
  
  ## standard order for edges
  #stop("FUNCTION NOT FINISHED")
  et <- na.omit(match(edgeTypes()[,1], names(graph$edges)))
  
  graph$edges <- graph$edges[et]
  graph <- withEdgeList(graph)
  k <- max(graph$v)

  nms <- names(graph$edges)
  et <- sort(et)

  et2 <- match(names(graph$edges), edgeTypes()[,1])
    
  ## order edges numerically
  for (i in seq_along(graph$edges)) {
    
    ## order edges by first vertex
    if (edgeTypes()$directed[et2[i]]) {
      ord <- order(sapply(graph$edges[[i]], function(x) k*x[1]+x[2]))
    }
    else{
      ## make smallest vertex first for undirected edges
      wh <- sapply(graph$edges[[i]], function(x) x[1] > x[2])
      graph$edges[[i]][wh] <- lapply(graph$edges[[i]][wh], rev)
      ord <- order(sapply(graph$edges[[i]], function(x) k*x[1]+x[2]))
    }
    
    graph$edges[[i]] <- graph$edges[[i]][ord]
    class(graph$edges[[i]]) <- "eList"
  }
  class(graph$edges) <- "edgeList"
  
  graph
}

##' Test graphs are equal
##' 
##' @param g1,g2 two `mixedgraph` objects
##' 
##' NOT TESTED
##' @export
graph_equal <- function(g1, g2) {
  g1 <- standardizeEdges(g1)
  g2 <- standardizeEdges(g2)
  return(isTRUE(all.equal(g1, g2)))
}

##' Create graph from character parsing
##' 
##' Convenience function for quickly constructing graphs with character input
##' 
##' @param char string of inputs given by vertex names separated by edges
##' @param ... other strings of further edges
##' @param mode format for edges in graph
##' @param useMatrices deprecated argument: in `mixedgraph` representation, should the output
##' use adjacency matrices?
##' @param format type of graph format to use, options are `mixedgraph` 
##' (the default), `graphNEL` (and `graphAM`, `graphBAM`), 
##' `igraph`, `ggm`, `bn`, `PAG`.
##' @param vnames list of variable names for use in `edgeCr`
##' 
##' @details Symbols `-<>=*|:` are assumed to be part of an edge, so
##' cannot be used in node names using this function.  Note that if we want
##' an edge with an `o` on the end (e.g. `o->`) then we must 
##' leave a space between the `o` and the variable name.
##' 
##' The `edgeCr()` creates just an `edgeList` object, and is helpful
##' for adding edges to existing graphs.  The `vnames` argument can be used if
##' the graph has non-standard vertex labels.
##' 
##' @examples
##' graphCr("1--->2<-->3<-4","2<->4,4->5")
##' graphCr("1-2-3-4-1", representation="graphNEL")  # requires package 'graph'
##' @export
graphCr <- function(char, ..., mode="adjList", useMatrices=FALSE, format="mixedgraph") {
  if (useMatrices) {
    warning("useMatrices argument is deprecated")
    mode <- "adjMatrix"
  }
  out <- list(char, ...)
  # in future try to allow direct typing
  #out <- unlist(sapply(out, as.character))
  out <- unlist(out)
  out <- unlist(strsplit(out, ","))
  out <- gsub("[-]+", "-", out)
  out <- gsub("[=]+", "=", out)
  out <- gsub("[ ]+", " ", out)
  # out <- gsub("([:alnum:]*)([-<>=*.|:]+)", "\\1 \\2", out)
  out <- gsub("([:alnum:]*[A-Za-np-z0-9])([o]{0,1})([-<>=*.|:]+)([o]{0,1})", "\\1 \\2\\3\\4", out)
  out <- gsub("([o]{0,1})([-<>=*.|:]+)([o]{0,1})([A-Za-np-z0-9][:alnum:]*)", "\\1\\2\\3 \\4", out)
  out <- strsplit(out, " ")
  out <- lapply(out, function(x) x[x != ""])
  out <- out[lengths(lapply(out, nchar)) > 0]

  if (length(out) == 0 ) return(mixedgraph(0))
  
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
    v0 <- as.integer(vs_tmp[2*ncol(em)+seq_along(unattached)])
    vnames <- levels(vs_tmp)
  }
  else vnames <- NULL
  
  tmp <- v1[etype > nedgetypes]
  v1[etype > nedgetypes] <- v2[etype > nedgetypes]
  v2[etype > nedgetypes] <- tmp
  etype <- mask[etype]
  
  n <- max(c(v1, v2, v0))
  
  etys <- edgeTypes()$type

  ## determine output based on value of 'mode'
  if (mode == "adjList") {
    len <- length(unique(etys[etype]))
    edges <- lapply(seq_len(len), function(x) adjList(n=n))
    names(edges) <- unique(etys[etype])
    
    for (i in seq_along(v2)) {
      edges[[etys[etype[i]]]][[v1[i]]] <- c(edges[[etys[etype[i]]]][[v1[i]]], v2[i])
      if (!edgeTypes()$directed[etype[i]]) edges[[etys[etype[i]]]][[v2[i]]] <- c(edges[[etys[etype[i]]]][[v2[i]]], v1[i])
    }
    edges <- lapply(edges, function(x) {
      class(x) <- "adjList"
      x
    })
    # names(edges2) <- names(edges)
    # edges <- edges2
  }
  else if (mode == "eList") {
    edges <- list()
    
    for (i in seq_along(v1)) {
      edges[[etys[etype[i]]]] <- c(edges[[etys[etype[i]]]], list(c(v1[i], v2[i])))
    }
    edges <- lapply(edges, function(x) {
      class(x) <- "eList"
      x
    })
  }
  else if (mode == "adjMatrix") {
    len <- length(unique(etys[etype]))
    edges <- lapply(seq_len(len), function(x) matrix(0,n,n))
    names(edges) <- unique(etys[etype])
    
    for (i in seq_along(v1)) {
      edges[[etys[etype[i]]]][v1[i], v2[i]] <- 1
      if (!edgeTypes()$directed[etype[i]]) edges[[etys[etype[i]]]][v2[i], v1[i]] <- 1
    }
    edges <- lapply(edges, function(x) {
      class(x) <- c("adjMatrix", class(x))
      x
    })
    # names(edges2) <- names(edges)
    # edges <- edges2
  }
  else if (mode == "edgeMatrix") {
    tab <- table(etype)
    # typ_nms <- unique(etys[etype])
    # len <- length(typs)
    # 
    # edges <- rbind(v1,v2)
    
    edges <- lapply(tab, function(x) matrix(NA, nrow=2, ncol=x))
    typs <- as.numeric(names(tab))
    names(edges) <- edgeTypes()$type[typs]
    
    for (i in seq_along(tab)) {
      edges[[i]][1,] <- v1[etype == typs[i]]
      edges[[i]][2,] <- v2[etype == typs[i]]
    }
    
    edges <- lapply(edges, function(x) {
      class(x) <- "edgeMatrix"
      x
    })
  }
  else stop("Mode not recognised")
  
  class(edges) <- "edgeList"
  
  ## output a mixedgraph, and convert if necessary
  out <- mixedgraph(n, edges=edges, vnames=vnames)
  if (format != "mixedgraph") {
    out <- convert(out, format=format)
  }

  out
}

##' @describeIn graphCr Create edge list by hand
##' @export
edgeCr <- function(char, mode="eList", vnames) {
  gr <- graphCr(char, mode=mode)
  out <- gr$edges
  if (missing(vnames)) attr(out, "vnames") <- gr$vnames
  else attr(out, "vnames") <- vnames
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
