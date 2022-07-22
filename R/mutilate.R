##' Remove duplicate edges
##' 
##' @param edges something from \code{edgeList} object
##' @param directed are edges directed? (i.e.
##' does the order of vertices matter?)
##' @param sort should vertices in undirected edges
##' be sorted as a side-effect of the function? (list form
##' only)
##' 
##' Check for and remove duplicate edges in a list of
##' edges or edgeMatrix.  Note that this will sort 
##' elements in eList objects.
##' 
remove_duplicate_edges <- function(edges, directed=TRUE, sort=FALSE) {
  if (is.adjMatrix(edges)) {
    # if is an adjacency matrix, ensure no entries exceed 1
    edges[edges > 1] <- 1
    return(edges)
  }
  else if (is.adjList(edges, checknm=TRUE)) {
    edges <- lapply(edges, unique.default)
    class(edges) <- c("adjList", class(edges))
    return(edges)
  }
  else if (is.edgeMatrix(edges)) {
    ## edgeMatrix object
    # if (nrow(edges) != 2 || any(edges <= 0)) stop("Object provided is a matrix but
    #                                                     doesn't seem to be an edgeMatrix or adjMatrix")
    if (ncol(edges) <= 1) return(edges)
    
    ## get a unique number representing each edge
    k = max(edges) + 1L
    char = c(t(c(1L,k)) %*% edges)
    ## if undirected, check both orders and take the smaller
    if (!directed) char = pmin(char, c(t(c(k,1L)) %*% edges))
    dup = duplicated(char)
    
    if (sort) warning("sort = TRUE has no effect for edgeMatrix")
    edges <- edges[,!dup,drop=FALSE]
    class(edges) <- "edgeMatrix"
    return(edges)
  }
  else if (is.eList(edges) || is.list(edges)) {
    edges <- lapply(edges, as.integer)
    if (directed) {
      edges <- unique.default(edges)
      class(edges) <- "eList"
      return(edges)
    }
    ## if undirected, sort entries
    out = lapply(edges, sort.int)
    dup = duplicated(out)
    if (sort) {
      out <- out[!dup]
    }
    else {
      out <- edges[!dup]
    }

    class(out) <- "eList"
        
    return(out)
  }
  else stop("Not sure how to handle this, should be list or matrix")
}

##' Function to match variables using their names
##' 
##' @param graph an object of class \code{mixedgraph}
##' @param edges an \code{edgeList} object created by \code{edgeCr}
##' 
match_vnames <- function (graph, edges) {
  ## check for variable names equivalence
  mtch <- match(attr(edges, "vnames"), graph$vnames)
  if (any(is.na(mtch))) stop("Some variable names do not match")
  
  if (isTRUE(all.equal(mtch, seq_along(graph$vnames))) || length(edges) == 0) {
    attr(edges, "vnames") <- NULL
    return(edges)
  }
  
  ## could speed this up by only checking for length of added vertices for 
  ## eList and edgeMatrix objects
  if (is.eList(edges[[1]])) {
    edges <- rapply(edges, function(x) mtch[x], how = "replace")
    edges <- lapply(edges, function(x) `class<-`(x, "eList"))
  }
  else if (is.adjMatrix(edges[[1]])) {
    edges2 <- rep(list(adjMatrix(n=length(vnames(graph)))), length(edges))
    for (i in seq_along(edges)) edges2[[i]][mtch, mtch] <- edges[[i]]
    
    names(edges2) <- names(edges)
    edges <- edges2
  }
  else if (is.adjList(edges[[1]])) {
    edges2 <- rep(list(adjList(n=length(vnames(graph)))), length(edges))
    for (i in seq_along(edges)) for (j in seq_along(mtch)) if (!is.null(edges[[i]][[j]])) edges2[[i]][[mtch[j]]] <- mtch[edges[[i]][[j]]]
    
    names(edges2) <- names(edges)
    edges <- edges2
  } 
  else if (is.edgeMatrix(edges[[1]])) {
    edges <- lapply(edges, function(x) matrix(mtch[x], nrow=2))
    edges <- lapply(edges, function(x) `class<-`(x, "edgeMatrix"))
  }
  else stop("'edges' should be a valid edge list object")
  
  class(edges) <- "edgeList"
  
  return(edges)
}

##' Add or remove edges
##' 
##' @param graph a \code{mixedgraph} object
##' @param edges list of edges to be added/removed
##' @param ... edges to be added with arguments matching names of edge types
##' @param remDup logical: should we check for duplicate edges?
##' 
##' @details The \code{remDup} argument is set by default to
##' remove duplicate edges. Currently \code{removeEdges()} forces 
##' all edges to be represented by adjacency matrices. 
##' 
##' The \code{fast} argument for \code{removeEdges} requires that 
##' the edge to be removed is given as a single vector of length 2.
##' 
##' @export
addEdges <- function(graph, edges, ..., remDup = TRUE
                     ## add in code to put edges in more directly
                     ) {
  out <- graph
  v <- graph$v
  
  ## if edges provided, ensure that edgeCr has correct vertex numbers
  if (!missing(edges)) if (!is.null(attr(edges, "vnames"))) {
    edges <- match_vnames(graph, edges)
  }
  
  args <- list(...)
  if (length(args) > 0) edges <- do.call(makeEdgeList, args)
  etys = edgeTypes()$type
  
  if (!is.list(edges)) stop("'edges' must be a list named with edge types")
  if (is.null(names(edges))) {
    warning("No edge type given, assuming undirected")
    et = 1
  }
  else et = pmatch(names(edges), etys)
  
  if (any(is.na(et))) stop("Edge types not matched")
  else if (any(duplicated(et))) stop("Repeated edge types matched")
  
  ## make sure edges have integer values
  
  
  ## Check all edges given as lists to be added are valid and of length 2
  edL <- sapply(edges, is.eList, checknm=TRUE)
  if (any(is.na(match(unlist(edges[edL]), v)))) stop("Edges must be between vertices in the graph")
  if (any(sapply(unlist(edges[edL], recursive=FALSE), length) != 2)) stop("Hyper-edges not yet supported")
  
  ## Check all edges given as edge matrices to be added are valid and of length 2
  edE <- !edL & sapply(edges, is.edgeMatrix)
  if (any(is.na(match(unlist(edges[edE]), v)))) stop("Edges must be between vertices in the graph")
  if (any(sapply(edges[edE], nrow) != 2)) stop("Hyper-edges not yet supported")

  ## Check all edges given as edge matrices to be added are valid and of length 2
  edAL <- !edL & !edE & sapply(edges, is.adjList)
  if (any(lengths(edges[edAL]) != length(graph$vnames))) stop("Must be entry in an adjList for each vertex")

  ## Check all edges given as edge matrices to be added are valid and of length 2
  edAM <- !edL & !edE & !edAL & sapply(edges, is.adjMatrix)
  if (any(sapply(edges[edAM], nrow) != length(graph$vnames)) ||
      any(sapply(edges[edAM], ncol) != length(graph$vnames))) stop("Adjacency matrix must have entries for all vertices")

  ## Check for other list objects
  # oth <- !edL & !edE & sapply(edges, is.list)
  oth <- !edL & !edE & !edAM & !edAL & sapply(edges, is.list)
  if (any(oth)) stop("Not a valid edgeList member object")
  
  for (i in seq_along(et)) {
    dir <- edgeTypes()$directed[et[i]]
    
    if (etys[et[i]] %in% names(out$edges)) {
      ## if there are some of this type of edge already
      ## add it in the same format
      A <- out$edges[[etys[et[i]]]]
      
      if (is.eList(A)) {
        A = c(A, eList(edges[[i]], directed = dir))
        class(A) <- "eList"
      }
      else if (is.edgeMatrix(A)) {
        A = cbind(A, edgeMatrix(edges[[i]], directed = dir))
        class(A) <- "edgeMatrix"
      }
      else if (is.adjMatrix(A)) {
        if (is.adjMatrix(edges[[i]]) && nrow(edges[[i]]) == nv(graph)) {
          A[v,v] = A[v,v] + adjMatrix(edges[[i]], n=nrow(A), directed = dir)
        }
        else {
          A = A + adjMatrix(edges[[i]], n=nrow(A), directed = dir)
        }
#        out$edges[[etys[et[i]]]] <- pmin(1, out$edges[[etys[et[i]]]])
        A[A > 1] <- 1
        class(A) <- "adjMatrix"
      }
      else if (is.adjList(A)) {
        nv_orig <- length(A)
        if (length(edges[[i]]) == nv(graph)) {
          A[v] <- mapply(function(x,y) union(x,y), A, adjList(edges[[i]], n=nv_orig, directed=dir))[v]
        }
        else {
          A <- mapply(function(x,y) union(x,y), A, adjList(edges[[i]], n=nv_orig, directed=dir))
        }
        
        # A <- mapply(function(x,y) union(x,y), A, adjList(edges[[i]], directed=dir))
        class(A) <- "adjList"
      }
      else stop("mixedgraph supplied seems invalid")
      
      ## put back edges
      out$edges[[etys[et[i]]]] <- A
    }
    else {
      ## otherwise just add it in
      dimnames(edges[[i]]) <- NULL   # drop dimnames
      out$edges[[etys[et[i]]]] <- edges[[i]]
    }
    if (remDup && !is.adjMatrix(out$edges[[etys[et[i]]]])) {
      out$edges[[etys[et[i]]]] = remove_duplicate_edges(out$edges[[etys[et[i]]]], directed=dir)
    }
  }
  
  out
}

## NEED TO SORT OUT ALL ARGUMENT
##' @describeIn addEdges remove edges
##' @param force should we just ignore edges not actually present?
##' @param fast fast version for when graph already uses adjacency matrices
##' 
##' @export
removeEdges <- function(graph, edges, ..., force=FALSE, fast=FALSE) {
  if (!fast) out <- withAdjMatrix(graph)
  else {
    i <- edges[1]; j <- edges[2]
    graph$edges <- lapply(graph$edges, function(x) x[i,j] <- x[j,i] <- 0)
    return(graph)
  }
  v <- graph$v
  
  # if ("all" %in% names(args)) {
  #   all <- args$all
  #   args <- args[names(args != "all")]
  # }

  ## if edges provided, ensure that edgeCr has correct vertex numbers
  if (!missing(edges)) if (!is.null(attr(edges, "vnames"))) {
    edges <- match_vnames(graph, edges)
  }

  args <- makeEdgeList(...)
  if (length(args) > 0) edges <- args

  if (!("edgeList" %in% class(edges))) class(edges) <- "edgeList"

  ## now have an edgeList
  etys <- edgeTypes()$type
  if (is.null(names(edges))) et = seq_along(edges)
  else et = pmatch(names(edges), etys)
  if (length(et) == 1 && is.na(et)) {
    warning("No edge type given, assuming undirected")
    et = 1
  }
  else if (any(is.na(et))) stop("Edge types not matched")
  else if (any(duplicated(et))) stop("Repeated edge types matched")

  ## Check all edges given as adjacency lists to be removed are valid
  adL <- sapply(edges, function(x) is.adjList(x))
  if (nv(graph) < length(graph$vnames)) for (i in seq_along(edges)[adL]) {
    nv_orig <- length(graph$vnames)
    rmvd <- seq_len(nv_orig)[-v]
    if (any(lengths(edges[[i]])[rmvd] > 0)) stop("Edges must be between vertices in the graph")
  }
  if (any(is.na(match(unlist(edges[adL]), v)))) stop("Edges must be between vertices in the graph")

  ## Check all edges given as lists to be removed are valid and of length 2
  edL <- sapply(edges, function(x) is.eList(x))
  if (any(is.na(match(unlist(edges[edL]), v)))) stop("Edges must be between vertices in the graph")
  if (any(sapply(unlist(edges[edL], recursive=FALSE), length) != 2)) stop("Hyper-edges not yet supported")

  ## Check all edges given as edge matrices to be removed are valid and of length 2
  edE <- sapply(edges, is.edgeMatrix)
  if (any(is.na(match(unlist(edges[edE]), v)))) stop("Edges must be between vertices in the graph")
  if (any(sapply(edges[edE], nrow) != 2)) stop("Hyper-edges not yet supported")

  adM <- sapply(edges, is.adjMatrix)
  chk <- rowSums(cbind(adM, adL, edL, edE)) == 1
  if (!all(chk)) {
    wh <- names(edges)[chk != 1]
    stop(paste("Edge formats for ", paste(wh, collapse=", "), " are not valid", collapse=""))
  }

  ## Now convert to adjacency matrix anyway
  edges <- mapply(adjMatrix, edges, directed=edgeTypes()$directed[et], n=length(graph$vnames), SIMPLIFY = FALSE)
  
  for (i in seq_along(et)) {
    if (etys[et[i]] %in% names(out$edges)) {
      ## if these edges are present remove them
      out$edges[[etys[et[i]]]][graph$v, graph$v] = out$edges[[etys[et[i]]]][graph$v, graph$v] - edges[[i]][graph$v, graph$v]
      if (any(out$edges[[etys[et[i]]]] < 0)) stop("Tried to remove edge not present")
    }
    ## else just ignore 
  }
  
  out 
}

##' Delete edges
##' 
##' Remove edges adjacent to set of vertices
##' 
##' @param graph a \code{mixedgraph} object
##' @param A,B sets of vertices in \code{graph}
##' @param etype which edges to remove
##' @param dir indicates whether only edges of certain orientation are removed
##' 
##' @details  If no edge type is specified, then all edges are removed.
##' If \code{dir=1}, then directed edges out of \code{A} are removed, 
##' but ones into \code{A} are preserved; for \code{dir=-1} the reverse,
##' and for \code{dir=0} (the default), direction is irrelevant.
##' If a second set \code{B} is specified, then all edges between \code{A}
##' and \code{B} are removed.
##' 
##' @export 
mutilate <- function(graph, A, B, etype, dir=0L) {
  if (!is.mixedgraph(graph)) stop("'graph' should be an object of class 'mixedgraph'")
  if (length(A) == 0) return(graph)
  if (!missing(B) && length(B) == 0) return(graph)
  if (missing(B) && all(graph$v %in% A) && missing(etype)) {
    edg <- graph$edges
    for (i in seq_along(edg)) {
      edg[[i]] <- list()
      class(edg[[i]]) <- "eList"
    }
    out <- mixedgraph(v=graph$v, edges=edg, vnames=graph$vnames)
    return(out)
  }
  
  ## if no edge type specified, use all available types
  if (missing(etype)) {
    whEdge <- seq_along(graph$edges)
    tmp <- pmatch(names(graph$edges), edgeTypes()$type)
    dir <- rep_len(dir, length(tmp))
  }
  else {
    whEdge <- pmatch(etype, names(graph$edges))
    etype = etype[!is.na(whEdge)]
    whEdge = etype[!is.na(whEdge)]
    if (length(etype) == 0) return(graph)
    tmp <- pmatch(etype, edgeTypes()$type)
    if (any(is.na(tmp))) stop("Some edge types not matched")
  }
  dir[!edgeTypes()$directed[tmp]] <- 0L

  edges <- graph$edges[whEdge]  
  
  for (i in seq_along(edges)) {
    if (is.adjList(edges[[i]], checknm=TRUE)) {
      ## adjList format
      if (missing(B)) {
        if (dir[i] <= 0) {
          fill <- vector(mode="list", length = length(A))
          edges[[i]][A] <- fill
        }
        if (dir[i] >= 0) {
          edges[[i]] <- lapply(edges[[i]], function(x) setdiff(x, A))
        }
      }
      else {
        if (dir[i] <= 0) {
          edges[[i]][B] <- lapply(edges[[i]][B], function(x) setdiff(x, A))
        }
        if (dir[i] >= 0) {
          edges[[i]][A] <- lapply(edges[[i]][A], function(x) setdiff(x, B))
        }
      }
      class(edges[[i]]) <- "adjList"
    }
    else if (is.eList(edges[[i]])) {
      ## edge list format
      rm = rep(FALSE, length(edges[[i]]))
      if (missing(B)) {
        if (dir[i] >= 0) {  # remove outgoing edges
          rm = rm | (sapply(edges[[i]], function(x) x[1]) %in% A)
        }
        if (dir[i] <= 0) {  # remove incoming edges
          rm = rm | (sapply(edges[[i]], function(x) x[2]) %in% A)
        }
      }
      else {
        if (dir[i] >= 0) {  # remove outgoing edges
          rm = rm | ((sapply(edges[[i]], function(x) x[1]) %in% A) & 
                       (sapply(edges[[i]], function(x) x[2]) %in% B))
        }
        if (dir[i] <= 0) {  # remove incoming edges
          rm = rm | ((sapply(edges[[i]], function(x) x[2]) %in% A) & 
                       (sapply(edges[[i]], function(x) x[1]) %in% B))
        }
      }
      edges[[i]] = edges[[i]][!rm]
      class(edges[[i]]) <- "eList"
    }
    else if (is.adjMatrix(edges[[i]])) {
      ## matrix format
      if (missing(B)) {
        if (dir[i] >= 0) edges[[i]][A,] = 0
        if (dir[i] <= 0) edges[[i]][,A] = 0
      }
      else {
        if (dir[i] >= 0) edges[[i]][A,B] = 0
        if (dir[i] <= 0) edges[[i]][B,A] = 0
      }
    }
    else if (is.edgeMatrix(edges[[i]])) {
      if (missing(B)) {
        if (dir[i] >= 0) edges[[i]] <- edges[[i]][,!(edges[[i]][1,] %in% A), drop=FALSE]
        if (dir[i] <= 0) edges[[i]] <- edges[[i]][,!(edges[[i]][2,] %in% A), drop=FALSE]
      }
      else {
        if (dir[i] >= 0) edges[[i]] <- edges[[i]][,!(edges[[i]][1,] %in% A & edges[[i]][2,] %in% B), drop=FALSE]
        if (dir[i] <= 0) edges[[i]] <- edges[[i]][,!(edges[[i]][2,] %in% A & edges[[i]][1,] %in% B), drop=FALSE]
      }
      class(edges[[i]]) <- "edgeMatrix"
    }
    else stop("Edge type not recognised")
  }
  graph$edges[whEdge] <- edges
  graph
}



##' Add additional nodes to a graph
##' 
##' @param graph a \code{mixedgraph} object
##' @param k the number of nodes to be added
##' @param vnames an optional character vector of names
##' 
##' 
##' @export addNodes
addNodes <- function(graph, k, vnames) {
  n <- length(graph$vnames)
  if (k == 0) return(graph)
  
  ## get new names
  if (missing(vnames) || is.null(vnames)) {
    vnames = paste("x", seq(n+1,n+k), sep="")
    while (any(vnames %in% graph$vnames)) {
      count <- n+k
      vnames <- setdiff(vnames, graph$vnames)
      l <- k - length(vnames)
      vnames = paste("x", count + seq_len(l), sep="")
      count <- count + l
    }
  }

  ## add empty edges if necessary
  edges <- graph$edges
  if (length(edges) > 0) {
    adjM <- which(sapply(edges, is.adjMatrix))
    adjL <- which(sapply(edges, is.adjList))
  }
  else adjM <- adjL <- integer(0)
  # for (i in adjM) {
  #   edges[[i]] <- matrix(0, n+k, n+k)
  # }
  
  if (length(adjL) > 0) {
    for (i in adjL) {
      lens <- lengths(graph$edges[[adjL[i]]])
      graph$edges[[adjL[i]]][lens == 0] <- list(integer(0))
      
      edges[[i]] <- vector("list", length=n+k)
      edges[[i]][seq_len(n)] <- graph$edges[[i]]
      # for (j in seq_len(n)) edges[[i]][[j]] <- graph$edges[[i]][[j]]
      edges[[i]][n + seq_len(k)] <- list(integer(0))
      class(edges[[i]]) <- "adjList"
    }
  }
  
  if (length(adjM) > 0) {
    edges[adjM] <- list(matrix(0, n+k, n+k))
    
    edges[adjM] <- mapply(function(x,y) {
      x[seq_len(nrow(y)), seq_len(ncol(y))] <- y
      class(x) <- class(y)
      x
    }, edges[adjM], graph$edges[adjM], SIMPLIFY = FALSE)
  }

  ## now return the enlarged graph
  mixedgraph(v=c(graph$v, n+seq_len(k)), edges=edges, vnames=c(graph$vnames, vnames))
}

##' Transform edges to different type
##' 
##' @param graph \code{mixedgraph} object
##' @param from character vector of edges to transform (default is all)
##' @param to character string of new edge type
##' 
##' @details \code{to} must be a single entry
##' 
##' @export
morphEdges <- function(graph, from, to) {
  if (missing(from)) from = names(graph$edges)
  if (missing(to)) to = "undirected"
  
  graph <- withEdgeMatrix(graph)
  
  ## partial matching of edge types
  wh_edge <- pmatch(to, edgeTypes()$type)
  if (is.na(wh_edge)) {
    wh_edge <- pmatch(to, edgeTypes()$abbrv)
    if (is.na(wh_edge)) stop("'to' edge type not matched")
  }
  to <- edgeTypes()$type[wh_edge]

  wh_edge <- pmatch(from, edgeTypes()$type)
  if (any(is.na(wh_edge))) {
    wh_edge[is.na(wh_edge)] <- pmatch(from[is.na(wh_edge)], edgeTypes()$abbrv)
    if (is.na(wh_edge)) stop(paste0("'from' edge type ", from[is.na(wh_edge)]," not matched"))
  }
  from <- edgeTypes()$type[wh_edge]
  
  ## add in target edge type if missing
  if (is.null(graph$edges[[to]])) {
    graph$edges[[to]] <- adjMatrix(n=length(graph$vnames))
  }
  else {
    graph$edges[[to]] <- adjMatrix(graph$edges[[to]], n=length(graph$vnames), directed=)
  }
  
  ## convert any edge lists and matrices:
  to_add <- collapse(graph$edge[from], dir=edgeTypes()[wh_edge,"directed"])
  new_edges <- list(to_add)
  class(new_edges) <- "edgeList"
  names(new_edges) = to

  ## edit the graph  
  graph <- graph[etype=setdiff(names(graph$edges), from)]
  graph <- addEdges(graph, edges = new_edges)
  
  graph
}