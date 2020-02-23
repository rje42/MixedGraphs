## functions for converting between graph formats
## add checks to these
conv_ggm_mixedgraph <- function(graph) {
  nv <- nrow(graph)
  if (ncol(graph) != nv) stop("ggm adjacency matrix must be square")
  edges <- list()
  
  di <- graph %% 2
  if (any(di > 0)) {
    edges$directed <- di
    graph <- graph - di
  }
  graph <- graph/10
  
  ud <- graph %% 2
  if (any(ud > 0)) {
    edges$undirected <- ud
    graph <- graph - ud
  }
  graph <- graph/10
  
  bi <- graph %% 2
  if (any(bi > 0)) {
    edges$bidirected <- bi
    graph <- graph - bi
  }
  
  if (any(graph != 0)) stop("Not a valid ggm object")
  
  mixedgraph(nv, edges = edges, vnames = colnames(graph))
}

conv_graphNEL_mixedgraph <- function(graph) {
  requireNamespace("graph", warn.conflicts = FALSE, quietly = TRUE)
  vnames <- graph::nodes(graph)
  
  if (graph::edgemode(graph) == "directed") 
    edgeList <- mapply(function(x,y) lapply(y$edges, function(z) c(x,z)), 
                     seq_along(vnames), graph@edgeL)
  else 
    edgeList <- mapply(function(x,y) lapply(y$edges[y$edges > x], function(z) c(x,z)), 
                       seq_along(vnames), graph@edgeL)
  
  if (is.list(edgeList[[1]])) edgeList <- list(do.call(c, edgeList))
  else edgeList <- list(edgeList)
  names(edgeList) <- graph::edgemode(graph)
  
  mixedgraph(n=length(vnames), vnames=vnames, edges=edgeList)
}

conv_graphAM_mixedgraph <- function(graph) {
  requireNamespace("graph", warn.conflicts = FALSE, quietly = TRUE)
  vnames <- colnames(graph@adjMat)
  edgeMat <- list(graph@adjMat)
  # class(edgeMat$undirected) = "adjMatrix"
  # class(edgeMat$directed) = "adjMatrix"
  
  names(edgeMat) <- graph::edgemode(graph)
  mixedgraph(n=length(vnames), vnames=vnames, edges=edgeMat)
}

conv_ADMG_mixedgraph <- function(graph) {
  edges = list(undirected=graph$ud.edges, 
               directed=graph$d.edges, 
               bidirected=graph$bi.edges)
  edges <- edges[!sapply(edges, is.null)]
  n <- graph$n
  
  mixedgraph(n, v=seq_len(n), edges = edges, vnames=graph$vnames)
}

conv_igraph_mixedgraph <- function(graph) {
  requireNamespace("igraph", quietly = TRUE)

  am_sp <- graph[]
  if (nrow(am_sp) > 500) warning("Large graph, might be inefficient to use non-sparse adjacency matrix")
  am <- as.matrix(am_sp)  # get adjacency matrix
  ud <- am*(am == t(am))
  dir <- am - ud
  if (igraph::is_named(graph)) {
    vnames <- igraph::V(graph)$name
  }
  else vnames <- NULL

  mixedgraph(n=length(igraph::V(graph)), vnames=vnames, edges=list(undirected=ud, directed=dir))
}

conv_bn_mixedgraph <- function(graph) {
  vnames <- names(graph$nodes)
  un <- sapply(graph$nodes, FUN = function(x) match(setdiff(x$nb, c(x$children, x$parents)), vnames))
  names(un) <- NULL
  un <- mapply(function(x,y) x[x > y], un, seq_along(un))
  un <- mapply(function(x,y) lapply(x, function(z) c(y,z)), un, seq_along(un))
  un <- do.call(c, un)
  
  dir <- sapply(graph$nodes, FUN = function(x) match(x$children, vnames))
  names(dir) <- NULL
  dir <- mapply(function(x,y) lapply(x, function(z) c(y,z)), dir, seq_along(dir))
  dir <- do.call(c, dir)
  
  edgeList <- list(undirected=un, directed=dir)
  
  mixedgraph(n = length(vnames), vnames=vnames, edges = edgeList)
}

conv_mixedgraph_ADMG <- function(graph) {
  requireNamespace("ADMGs")
  ud.edges <- eList(graph$edges$undirected)
  d.edges <- eList(graph$edges$directed)
  bi.edges <- eList(graph$edges$bidirected)
  nv <- length(graph$vnames)

  ADMGs::makeGraph(nv,
                   ud.edges = ud.edges,
                   d.edges = d.edges,
                   bi.edges = bi.edges,
                   vnames = graph$vnames)
}

conv_mixedgraph_ggm <- function(graph) {
  nv <- length(graph$vnames)
  out <- matrix(0, nv, nv, dimnames=list(graph$vnames, graph$vnames))
  
  dir <- ("directed" %in% names(graph$edges))
  un <- ("undirected" %in% names(graph$edges))
  bi <- ("bidirected" %in% names(graph$edges))
  
  if (dir) out <- out + adjMatrix(graph$edges$directed, nv, directed = TRUE)
  if (un) out <- out + 10*adjMatrix(graph$edges$undirected, nv)
  if (bi) out <- out + 100*adjMatrix(graph$edges$bidirected, nv)
  out
}

conv_mixedgraph_graphNEL <- function(graph) {
  # is graph directed or undirected?
  requireNamespace("graph", warn.conflicts = FALSE, quietly = TRUE)
  mode <- "undirected"
  if (!is.null(graph$edges$directed) && length(graph$edges$directed) > 0) {
    if (!is.null(graph$edges$undirected) && length(graph$edges$undirected) > 0) {
      stop("Both directed and undirected edges, unclear how to proceed")
    }
    mode <- "directed"
    edL <- lapply(graph$v, ch, graph=graph)
  }
  else {
    edL <- lapply(graph$v, nb, graph=graph)
  }
  edL <- lapply(edL, function(x) list(edges=x)) 
  names(edL) <- graph$vnames[graph$v]
  
  graph::graphNEL(nodes=graph$vnames[graph$v], edgeL=edL, edgemode=mode)
}

conv_mixedgraph_graphAM <- function(graph) {
  # is graph directed or undirected?
  requireNamespace("graph", warn.conflicts = FALSE, quietly = TRUE)
  mode <- "undirected"
  if (!is.null(graph$edges$directed) && length(graph$edges$directed) > 0) {
    if (!is.null(graph$edges$undirected) && length(graph$edges$undirected) > 0) {
      stop("Both directed and undirected edges, unclear how to proceed")
    }
    mode <- "directed"
    amat <- collapse(graph$edges["directed"], dir=1, matrix=TRUE)
  }
  else {
    amat <- collapse(graph$edges["undirected"], dir=0, matrix=TRUE)
  }
  colnames(amat) <- graph$vnames[graph$v]

  graph::graphAM(amat, edgemode = mode)
}

conv_mixedgraph_igraph <- function(graph) {
  requireNamespace("igraph")
  igraph::igraph.from.graphNEL(conv_mixedgraph_graphNEL(graph))
}

conv_mixedgraph_bn <- function(graph) {
  requireNamespace("bnlearn")
  arcs1 <- edgeMatrix(graph$edges$directed)
  ## undirected edges are just directed both ways
  arcs2 <- edgeMatrix(graph$edges$undirected)
  arcs2 <- cbind(arcs2, arcs2[2:1,])
  ## make this into matrix of the form used
  ## by bnlearn
  edges <- rbind(t(arcs1), t(arcs2))
  edges[] <- graph$vnames[edges]
  colnames(edges) <- c("from", "to")

  ## make a new bn object
  out <- bnlearn::empty.graph(graph$vnames[graph$v])
  out$arcs <- edges
  # out$nodes <- bnlearn:::cache.structure(graph$vnames[graph$v], edges)
  out
}



## Convert PAG to mixedgraph
conv_PAG_mixedgraph <- function(graph) {
  if (!is.matrix(graph)) graph <- graph@amat

  edges <- list()
  rg <- row(graph); cg <- col(graph)

  tmp <- which((graph == 1) & (t(graph) == 1) & lower.tri(graph))
  if (length(tmp) > 0) edges$`not directed` <- mapply(c, cg[tmp], rg[tmp], SIMPLIFY = FALSE)

  tmp <- which((graph == 1) & (t(graph) == 2))
  if (length(tmp) > 0) edges$`partially directed` <- mapply(c, cg[tmp], rg[tmp], SIMPLIFY = FALSE)

  tmp <- which((graph == 1) & (t(graph) == 3))
  if (length(tmp) > 0) edges$`partially undirected` <- mapply(c, cg[tmp], rg[tmp], SIMPLIFY = FALSE)

  tmp <- which((graph == 2) & (t(graph) == 2) & lower.tri(graph))
  if (length(tmp) > 0) edges$`bidirected` <- mapply(c, cg[tmp], rg[tmp], SIMPLIFY = FALSE)

  tmp <- which((graph == 3) & (t(graph) == 2))
  if (length(tmp) > 0) edges$`directed` <- mapply(c, cg[tmp], rg[tmp], SIMPLIFY = FALSE)

  tmp <- which((graph == 3) & (t(graph) == 3) & lower.tri(graph))
  if (length(tmp) > 0) edges$`undirected` <- mapply(c, cg[tmp], rg[tmp], SIMPLIFY = FALSE)

  mixedgraph(n = ncol(graph), vnames = colnames(graph), edges=edges)
}

## Convert mixedgraph to PAG
conv_mixedgraph_PAG <- function(graph) {
  # requireNamespace("methods")

  n <- length(graph$v)
  graph <- withEdgeList(graph)

  out <- matrix(0, n, n)

  ##
  if (!is.null(graph$edges$directed)) {
    tmp <- graph$edges$directed
    for (i in seq_along(tmp)) {
      out[tmp[[i]][1], tmp[[i]][2]] = 2
      out[tmp[[i]][2], tmp[[i]][1]] = 3
    }
  }
  if (!is.null(graph$edges$undirected)) {
    tmp <- graph$edges$undirected
    for (i in seq_along(tmp)) {
      out[tmp[[i]][1], tmp[[i]][2]] = 3
      out[tmp[[i]][2], tmp[[i]][1]] = 3
    }
  }
  if (!is.null(graph$edges$bidirected)) {
    tmp <- graph$edges$bidirected
    for (i in seq_along(tmp)) {
      out[tmp[[i]][1], tmp[[i]][2]] = 2
      out[tmp[[i]][2], tmp[[i]][1]] = 2
    }
  }
  if (!is.null(graph$edges$`partially undirected`)) {
    tmp <- graph$edges$`partially undirected`
    for (i in seq_along(tmp)) {
      out[tmp[[i]][1], tmp[[i]][2]] = 3
      out[tmp[[i]][2], tmp[[i]][1]] = 1
    }
  }
  if (!is.null(graph$edges$`partially directed`)) {
    tmp <- graph$edges$`partially directed`
    for (i in seq_along(tmp)) {
      out[tmp[[i]][1], tmp[[i]][2]] = 2
      out[tmp[[i]][2], tmp[[i]][1]] = 1
    }
  }
  if (!is.null(graph$edges$`not directed`)) {
    tmp <- graph$edges$`not directed`
    for (i in seq_along(tmp)) {
      out[tmp[[i]][1], tmp[[i]][2]] = 1
      out[tmp[[i]][2], tmp[[i]][1]] = 1
    }
  }

  dimnames(out) = list(graph$vnames, graph$vnames)

  ## bit of a hack, stores as FCI algorithm output
  methods::new("fciAlgo", amat=out)
}
