##' Collapse multiple edge types to a single list or matrix
##' 
##' @param edges list of edges to include
##' @param v1,v2 incoming and outgoing vertices, defaults to all.
##' @param dir direction
##' @param matrix logical indicating whether to force the return of an adjacency matrix
##' @param nv number of vertices for \code{adjMatrix}
##' @param sparse should we use sparse matrices if available?
##' @param sort 1=unique but not sorted, 2=unique and sorted, 0=neither
##' @param rev logical: should directed \code{adjList}s have the direction inverted if \code{dir=1}?
##' @param double_up logical: should edges with \code{dir=0} be repeated in both directions for an edgeMatrix?
##' 
##' @details returns an edgeMatrix or adjacency matrix for possibly multiple edge types.
##' If any of the edges are specified as an adjacency matrix, then the output will also
##' be an adjacency matrix, and similarly for an edgeMatrix. 
##' 
##' If \code{nv} is not supplied, then it is inferred from the input.
##' 
##' If \code{v1} or \code{v2} are specified then the first and 
##' second vertices must belong to these respective sets. 
##' 
##' @export collapse
collapse <- function(edges, v1, v2, dir=1, matrix=FALSE, nv, sparse=FALSE, sort=1, 
                     rev=FALSE, double_up=FALSE) {
  ## repeat direction with warning if necessary
  if (length(edges) == 0 || length(unlist(edges))==0) return(edgeMatrix())
  dir <- dir*rep(1L, length(edges))
  
  all1 <- all2 <- FALSE
  
  rmv <- sapply(edges, is.null)
  edges <- edges[!rmv]
  dir <- dir[!rmv]
  
  ### first look for objects of class adjMatrix and adjList
  isAMat <- sapply(edges, is.adjMatrix)
  isAList <- sapply(edges, is.adjList)
  
  if (missing(nv)) {
    nv <- 0
    if (any(isAMat)) nv <- max(nv, sapply(edges[isAMat], dim))
    if (any(isAList)) nv <- max(nv, lengths(edges[isAList]))
    if (any(!isAMat & !isAList)) nv <- max(nv, unlist(edges[!isAMat & !isAList]))
  }
  
  ## if vertices not specified, use all
  if (missing(v1) || missing(v2)) {
    #     if (any(isAMat)) {
    #       i <- which(isAMat)[1]
    #       nv <- nrow(edges[[i]])
    #     }
    #     else if (any(isAList)) {
    #       i <- which(isAList)[1]
    #       nv <- length(edges[[i]])
    #     }
    #     else {
    #       ## just look for biggest vertex
    #       nv <- max(unlist(edges))
    # #      if (is.infinite(nv)) return(matrix(NA, 2, 0))
    #     }
    ## if vertices not specified, use all
    if(missing(v1)) {
      v1 <- seq_len(nv)
      all1 <- TRUE
    }
    if(missing(v2)) {
      v2 <- seq_len(nv)
      all2 <- TRUE
    }
    if (length(v1) > 0) vr1 <- seq_len(nv)[-v1]
    else vr1 <- seq_len(nv)
    if (length(v2) > 0) vr2 <- seq_len(nv)[-v2]
    else vr2 <- seq_len(nv)
    vr <- intersect(vr1, vr2)
  }
  
  ## if any representation is an adjacency matrix, or if forced, 
  ## then use this adjMatrix
  if (any(isAMat) || matrix) {
    ## could speed this up by not converting edge lists
    if (sparse) {
      jointMat <- Matrix(0, length(v1), length(v2))
    }
    else jointMat <- matrix(0, length(v1), length(v2))
    
    for (i in seq_along(edges)) {
      if (!isAMat[i]) edges[[i]] <- adjMatrix(edges[[i]], n=nv, directed=dir[i], sparse=sparse)
      if (dir[i] >= 0) jointMat <- jointMat + edges[[i]][v1, v2]
      if (dir[i] <= 0) jointMat <- jointMat + t(edges[[i]][v2, v1])
    }
    jointMat <- pmin(jointMat, 1)
    class(jointMat) <- c("adjMatrix", class(jointMat))
    
    return(jointMat)
  }
  
  ## if everything is an adjList, then use this representation
  if (all(isAList)) {
    for (i in seq_along(edges)) {
      if (dir[i] == 1) {
        if (rev) {
          edges[[i]] <- revAdjList(edges[[i]])
          tmp <- vr1
          vr1 <- vr2
          vr2 <- tmp
          tmp <- v1
          v1 <- v2
          v2 <- tmp
        }
        edges[[i]][vr2] <- vector(mode="list", length=length(vr2))
        edges[[i]] <- lapply(edges[[i]], function(x) intersect(x,v1))
      }
      else if (dir[i] == 0) {
        edges[[i]] <- symAdjList(edges[[i]])
        edges[[i]][vr] <- vector(mode="list", length=length(vr))
        edges[[i]] <- lapply(edges[[i]], function(x) intersect(x,c(v1,v2)))
      }
      else if (dir[i] == -1) {
        edges[[i]][vr1] <- vector(mode="list", length=length(vr1))
        edges[[i]] <- lapply(edges[[i]], function(x) intersect(x,v2))
      }
    }
    names(edges) <- NULL
    out <- lapply(purrr::transpose(edges), unlist)
    
    if (sort > 0) {
      out <- lapply(out, unique.default)
      if (sort > 1) out <- lapply(out, sort.int)
    }
    
    class(out) <- "adjList"
    
    return(out)
  }
  
  ## otherwise use edgeMatrices
  if (double_up) {
    edges <- mapply(edgeMatrix, edges, double=(dir==0), SIMPLIFY = FALSE)
  }
  else edges <- lapply(edges, edgeMatrix)
  ## reverse edges for direction =-1.
  edges[dir < 0] <- lapply(edges[dir < 0], function(x) x[2:1,,drop=FALSE])
  
  ## shortcut for all vertices, saves time
  if (all1 && all2) {
    jointEM <- do.call(cbind, edges)
    class(jointEM) <- "edgeMatrix"
    return(jointEM)
  }
  
  # ## need to duplicate undirected edges
  # edges <- c(edges, lapply(edges[dir == 0], function(x) x[2:1,,drop=FALSE]))
  
  ## get the edgeMatrix to return
  jointEM <- matrix(NA, ncol=0, nrow=2)
  for (i in seq_along(edges)) {
    wh <- (edges[[i]][1,] %in% v1) & (edges[[i]][2,] %in% v2)
    jointEM <- cbind(jointEM, edges[[i]][,wh])
  }
  class(jointEM) <- "edgeMatrix"
  
  return(jointEM)
}

# ## wrapper for collapse to deal with directionality problem
# collapse2 <- function (edges, v1, v2, dir=1, matrix=FALSE, sparse=FALSE, sort=1, rev=FALSE) {
#   isAMat <- sapply(edges, is.adjMatrix)
#   isAList <- sapply(edges, is.adjList)
#   
#   if (any(isAMat) || all(isAList) || matrix) collapse(edges, v1, v2, dir=dir, matrix=matrix, sparse=sparse, sort=sort, rev=rev)
#   else {
#     edges2 <- lapply(edges, edgeMatrix, directed=FALSE)
#     collapse(edges2, v1, v2, dir=dir, matrix=FALSE, sparse=sparse, sort=sort)
#   }
# }

##' Find adjacent vertices
##' 
##' Generic function for finding adjacent vertices based on any kind of edge.
##' 
##' @param graph an object of class \code{mixedgraph}
##' @param v vertices to find adjacencies
##' @param etype edge types to consider; defaults to all
##' @param dir for directed edges, indicates which direction to search in:
##' 1: along direction, -1: against direction, 0: both directions.
##' @param inclusive logical indicating whether elements of \code{v} can be 
##' included in output group.
##' @param sort integer:1 for unique but unsorted, 2 for 
##' sorted (0 for possibly repeated and unsorted).  If edges are stored as a matrix
##' then output will always be unique and sorted.
##' @param force logical - should invalid \code{v} be ignored?
##' 
##' @details The argument \code{directed} is recycled for multiple edge types, but 
##' has no effect for edges without a specified direction.  If any \code{v} is 
##' not a vertex of \code{graph}, an error is returned, unless \code{force=TRUE}.
##' 
##' The function \code{adjacent} tests for a particular adjacency between
##' two vertices.  
##' 
##' @examples
##' adj(gr1, v=1, etype="directed")
##' adj(gr1, v=1, etype="directed", dir=-1)
##' adjacent(gr1, 1, 3, etype="directed", dir=1)
##' 
##' @seealso \code{\link{grp}} for paths
##' 
##' @export
adj <- function(graph, v, etype, dir=0, inclusive=TRUE, sort=1, force=FALSE) {
  ## if no edge type specified, use all available types
  if (!is.mixedgraph(graph)) stop("'graph' should be an object of class 'mixedgraph'")
  
  ## only include vertices in the graph
  if (force) v <- intersect(v, graph$v)
  else if (any(!(v %in% graph$v))) stop("Invalid values of v")
  
 # if (missing(etype)) etype <- edgeTypes()$type
  if (missing(etype)) etype <- names(graph$edges)
  ## repeat dir() vector with warning if necessary
  if (length(dir) > length(etype)) {
    warning("More directions specified than edge types")
    dir <- dir[seq_along(etype)]
  }
  else dir = dir*rep.int(1L, length(etype))
  
  ## if undirected edges are specified as directed 
  ## then change and give a warning
  tmp <- pmatch(etype, edgeTypes()$type)
  if (any(is.na(tmp))) stop("Some edges not matched to a registered type")
  if (any(!edgeTypes()$directed[tmp] & dir[rank(tmp)] != 0)) {
    warning("Undirected edges specified as directed, changing to undirected")
    dir[!edgeTypes()$directed[tmp]] <- 0L
  }
  
  ## get edge types
  whEdge <- na.omit(pmatch(etype,names(graph$edges)))
  edges <- graph$edges[whEdge]
  # n <- length(graph$v)
  
  ## special case of one adjacency matrix to save time
  if (length(edges) == 1 && is.adjMatrix(edges[[1]], length(graph$vnames), checknm=TRUE)) {
    es <- matrix(0, length(v), length(graph$vnames))

    ## add in edges dependent on direction
    if (dir >= 0) es <- es + edges[[1]][v,,drop=FALSE]
    if (dir <= 0) es <- es + t(edges[[1]][,v,drop=FALSE])

    wh <- which(colSums(es) > 0)
    if (!inclusive) wh <- setdiff(wh, v)
    
    return(wh)
  }
  
  ## if no edges then return an empty vector
  if (length(edges) == 0) return(integer(0))
  
  ## collapse edges into one representation
  es <- collapse(edges, v1=v, dir=dir, rev=TRUE, double_up=TRUE)
  
  ## select depending on mode of output
  if (is.adjMatrix(es)) {
    ## this is an adjacency matrix
    d <- dim(es)
    wh <- which(.colSums(es, d[1], d[2]) > 0)
    if (!inclusive) wh <- setdiff(wh, v)
    return(wh)
  }
  else if (is.adjList(es)) {
    out <- unlist(es[v])
    # if (dir <= 0) out <- unlist(es[v])
    # else out <- which(sapply(es, function(x) any(v %in% x)))
  }
  else if (is.edgeMatrix(es)) {
    ## this is an edge matrix
    if (ncol(es) == 0) return(integer(0))
    out = es[2,]
  }
  else if (is.eList(es)) stop("Shouldn't be an eList object")
  else stop("Not a valid edgeList element")
  
  if (sort > 0) out <- unique.default(out)
  if (sort > 1) out <- sort.int(out)
  if (!inclusive) out = setdiff(out, v)
  
  out <- as.integer(out)

  return(out)
}

##' @describeIn adj Test for adjacency
##' @param v1,v2 vertices between which to test for adjacency
##' @export 
adjacent <- function(graph, v1, v2, etype, dir=0) {
  v2 %in% adj(graph, v1, etype=etype, dir=dir)
}

##' Group vertices by adjacencies
##' 
##' Generic functions for finding groups based on edge adjacencies, 
##' e.g. neighbourhoods, districts or ancestral sets.
##' 
##' @param graph an object of class \code{mixedgraph}
##' @param v vertices to group
##' @param etype edge types to group by (defaults to all)
##' @param inclusive logical indicating whether \code{v} should be 
##' included in output group.
##' @param dir integer(s) indicating if direction should be 
##' considered: 0 = undirected, 1 = from row to column; -1 = from column to row.
##' @param sort integer: 1 for unique but unsorted, 2 for 
##' sorted (0 for possibly repeated and unsorted).
##' @param force logical - should invalid \code{v} be ignored?
## @param skipChecks guarantees that input is valid
##' @param edges an \code{adjMatrix} or \code{adjList}
##' 
##' @details \code{grp()} finds all vertices that can be reached from
##' vertices in \code{v} by edges of the specified type, and in the 
##' specified direction.
##' For example, we can find all the ancestors of \code{v} by using 
##' \code{type="directed"} and \code{dir=-1} (i.e.\ go back up the edges).
##' 
##' \code{groups()} finds equivalence classes in a graph based on being 
##' connected by such a path (i.e.\ the connected components with respect 
##' to the specified edge types).
##' 
##' If any \code{v} is 
##' not a vertex of \code{graph}, an error is returned, unless 
##' \code{force=TRUE}.
##' 
##' \code{grp2} is a faster version of the function for a single \code{adjMatrix}
##' or \code{adjList}.
##' 
##' @export grp
##' @seealso \code{\link{adj}} for single edge adjacencies.
grp <- function(graph, v, etype, inclusive=TRUE, dir=0, sort=1, force=FALSE) {
  if (!is.mixedgraph(graph)) stop("'graph' should be an object of class 'mixedgraph'")

  # if (!skipChecks) {
  ## only include vertices in the graph unless force=TRUE
  if (force) v <- intersect(v, graph$v)
  else if (any(!(v %in% graph$v))) stop("Invalid values of v")
  if (length(v) == 0) return(integer(0))
  
  
  if (missing(etype)) etype <- edgeTypes()$type
  ##' repeat dir() vector with warning if necessary
  if (length(dir) > length(etype)) warning("More directions specified than edge types")
  dir = dir*rep.int(1L, length(etype))
  
  tmp <- pmatch(etype, edgeTypes()$type)
  dir[!edgeTypes()$directed[tmp]] <- 0L
  
  whEdge <- pmatch(etype,names(graph$edges))
  
  edges <- graph$edges[whEdge]
  
  if (length(edges) == 1) {
    if (is.adjList(edges[[1]], checknm=TRUE)) {
      wh <- match(names(graph$edges)[whEdge], edgeTypes()$type)
      if (dir == -1 && edgeTypes()$directed[wh]) {
        return(grp2(v, edges[[1]], dir=dir, inclusive=inclusive, sort=sort))
      }
      else if (dir == 0 && !edgeTypes()$directed[wh]) {
        return(grp2(v, edges[[1]], dir=dir, inclusive=inclusive, sort=sort))
      }
      # else stop("This won't work")
    }
    else if (is.adjMatrix(edges[[1]], checknm=TRUE)) {
      return(grp2(v, edges[[1]], dir=dir, inclusive=inclusive, sort=sort))
    }
  }

  es <- collapse(edges, dir=dir, rev=TRUE, double_up=TRUE)

  # }
  # else {
  #   if (etype %in% names(graph$edges)) {
  #     es <- graph$edges[etype]
  #   }
  #   else return(integer(0))
  # }

  #   tmp = lapply(graph$edges[etype], edgeMatrix)
  #   if (!is.null(tmp)) {
  #     es = do.call(cbind, tmp)
  #   }
  #   else es = matrix(NA, ncol=0, nrow=2)

  out = v

  if (is.adjList(es)) {
    continue = TRUE
    new = v
    
    while (continue) {
      out2 <- unlist(es[new])
      new <- setdiff(out2, out)
      out <- c(out, new)
      
      continue = (length(new) > 0)
    }
  }
  else if (is.adjMatrix(es)) {
    continue = TRUE
    new = v
    
    while (continue) {
      out2 = which(colSums(es[new, , drop=FALSE]) > 0)
      new = setdiff(out2, out)
      es[out,] = 0L
      out <- c(out, new)
      
      continue = (length(new) > 0)
    }
  }
  else if (is.edgeMatrix(es)) {
    if (isTRUE(ncol(es) > 0)) {
      continue = TRUE
      new = v
      
      while (continue) {
        out2 = es[2, es[1,] %in% new]
        out2 = es[2, es[1,] %in% new]
        
        new = unique.default(out2[!(out2 %in% out)])
        out = c(out, new)
        continue = (length(new) > 0)
      }
    }
  }
  else if (is.eList(es)) stop("Shouldn't be an eList object")
  else stop("Not a valid edgeList element")
  
  ## if not inclusive, then remove elements of v
  if (!inclusive) out = setdiff(out, v)
  
  ## sort if requested
  if (sort > 1) out = sort.int(out)
  out <- as.integer(out)
  
  out
}

##' @describeIn grp Faster routine for single \code{adjMatrix} or \code{adjList}
##' @export
grp2 <- function(v, edges, dir, inclusive, sort=1) {
  new <- v
  out <- v
  
  if (is.adjList(edges)) {
    ## note this only works if dir matches the type of edge (so -1 for directed, 0 for not directed)
    while (length(new) > 0) {
      new <- unlist(edges[new])
      if (sort > 0) new <- setdiff(new, out)
      out <- c(out, new)
    }
  }
  else if (is.adjMatrix(edges)) {
    if (dir == 1) edges <- t(edges)
    else if (dir == 0) edges <- edges + t(edges)
    
    while (length(new) > 0) {
      new <- which(rowSums(edges[,new,drop=FALSE]) > 0)
      if (sort > 0) new <- setdiff(new, out)
      out <- c(out, new)
    }
  }
  # else if (is.eList(edges)) {
  #   
  # }
  # else if (is.edgeMatrix(edges)) {
  #   
  # }
  else stop("Not a valid approach for this function")
  
  if (!inclusive) out <- setdiff(out, v)
  
  if (sort > 1) out <- sort.int(out)
  out <- as.integer(out)
  
  out
}

##' @export groups
##' @describeIn grp find connected components
groups = function(graph, etype, sort=1) {
  if (!is.mixedgraph(graph)) stop("'graph' should be an object of class 'mixedgraph'")
  if (missing(etype)) etype <- edgeTypes()$type

  ##' repeat dir() vector with warning if necessary
  whEdge <- pmatch(etype,names(graph$edges))
  edges <- graph$edges[whEdge[!is.na(whEdge)]]
  es <- collapse(edges, dir=0, double_up = TRUE)
  
  grp = rep(0, length(vnames(graph)))
  grp[graph$v] = seq_along(graph$v)  
  
  if (is.adjList(es, n=nv(graph))) {
    grp0 <- grp
    for (i in seq_along(es)[graph$v]) {
      if (grp[i] < grp0[i]) next
      
      vs <- c(i)
      new <- es[[i]]
      while (length(new) > 0) {
        vs <- c(vs, new)
        new <- setdiff(unlist(es[new]), vs)
      }
      grp[vs] <- min(grp[vs])
    }
    
    # for (i in seq_along(es)[graph$v]) {
    #   if (length(es[[i]]) > 0) grp[c(i, es[[i]])] <- min(grp[c(i, es[[i]])])  ### NEED TO CHECK THIS!
    #   if (grp[i] == 0) stop("Should be in a group")
    # }
  }
  else if (is.adjMatrix(es)) {
    for (i in seq_len(ncol(es))) {
      g1 = grp[i]
      g2s = grp[which(es[,i] > 0)]
      grp[grp %in% g2s] = g1
    }
  }
  else if (is.edgeMatrix(es)) {
    for (i in seq_len(ncol(es))) {
      g1 = grp[es[1,i]]
      g2 = grp[es[2,i]]
      if (g1 == g2) next
      grp[grp == g2] = g1
    }
  }
  else stop("eLists assumed not possible from collapse()")
  
  out = list()
  for (i in unique.default(grp[grp>0])) {
    out = c(out, list(which(grp==i)))
  }
  
  out
}

##' Find vertices in subset connected to vertex
##' 
##' Function for finding nodes in a set connected to a vertex via paths in the 
##' original graph
##' 
##' @param graph an object of class \code{mixedgraph}
##' @param v vertex to check
##' @param D set to look for paths to
##' @param etype edge types to use
##' @param dir integer vector of directions
##' @param verbose logical: should additional information be provided?
##' 
##' @details This function will look for paths in \code{graph} from \code{v} 
##' that only use the edge types in \code{etype} and the directions specified,
##' and stop whenever a path hits something in \code{D}.  It then outputs the 
##' subset of elements of \code{D} that it hits.
##' 
##' Note that for directed edges, \code{dir} defaults to 1, and so only 
##' follows the canonical direction of the edge.  Set to 0 if all directions are
##' valid.
##' 
##' @export
pathConnected <- function(graph, v, D, etype, dir, verbose=FALSE) {
  if (length(D) == 0) return(integer(0))
  
  ## check inputs
  if (!is.mixedgraph(graph)) stop("'graph' should be an object of class 'mixedgraph'")
  if (missing(etype)) etype <- names(graph$edges)
  else {
    ## match names and get correct descriptions
    chk <- pmatch(etype, edgeTypes()$type)
    chk_a <- pmatch(etype, edgeTypes()$abbrv)
    if (any(is.na(chk) & is.na(chk_a))) stop("some edgeTypes not matched")
    if (any(na.omit(chk != chk_a))) stop("conflicting values for edgeTypes")
    chk[is.na(chk)] <- chk_a[is.na(chk)]
    etype <- edgeTypes()$type[chk]
  }
  if (missing(dir)) dir <- 1*edgeTypes()$directed[match(etype, edgeTypes()$type)]
  else {
    if (length(dir) > length(etype)) stop("Must be fewer directions than edge types")
    dir <- dir*rep.int(1L, length(etype))
  }
  
  graph <- withAdjList(graph[etype=etype])
  edges <- graph$edges
  if (length(edges) == 0) return(integer(0))
  
  ## get adjLists into correct orientation
  if (any(dir != 1*edgeTypes()$directed[match(etype, edgeTypes()$type)])) {
    for (i in seq_along(etype)) {
      if (!edgeTypes()$directed[match(etype, edgeTypes()$type)][i]) next
      if (dir[i] == 0) {
        edges[[i]] <- symAdjList(edges[[i]])
      }
      else if (dir[i]) {
        edges[[i]] <- revAdjList(edges[[i]])
      }
    }
  }
  
  ## combine adjLists
  if (length(edges) > 1) {
    edges <- lapply(purrr::transpose(edges), function(x) unique.default(unlist(x)))
  }
  else edges <- edges[[1]]
  
  ## now start search
  new <- TRUE
  active <- v
  seen <- integer(0)
  while (new) {
    new_vs <- setdiff(unlist(edges[active]), seen)
    
    seen <- c(seen, new_vs)
    active <- setdiff(new_vs, D)
    new <- length(active) > 0
    
    if (verbose && new) {
      cat("new vertices: ", paste(new_vs, collapse=", "), "\n", sep="")
      cat("  to check: ", paste(active, collapse=", "), "\n", sep="")
    }
  }
  
  return(intersect(seen, D))
}
