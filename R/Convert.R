## functions for using graph, igraph, gR, ...

##' Convert graph to format associated with specific package
##' 
##' @param graph graphical object, of one of classes listed below
##' @param format character string giving new format to convert to
##' @param cur_format character string of current format; can be deduced 
##' from the object in many cases
##' @param ...
##' 
##' @details Currently limited functionality, only
##' converts 'ADMGs' and 'ggm' to 'mixedgraph' objects.
##' 
##' Implemented thus far:
##' mixedgraph <-> ggm
##' mixedgraph <-> ADMG
##' mixedgraph <-> graphNEL
##' mixedgraph <-> graphAM
##' mixedgraph <-- PAG
##' 
##' \code{ggm} entries must be specified by \code{cur_format = "ggm"}.
##' 
##' @export convert
convert <- function(graph, format="mixedgraph", cur_format, ...) {
  
  if (missing(cur_format)) {
    if (class(graph) == "igraph") cur_format <- "igraph"
    else if (class(graph) == "mixedgraph") cur_format <- "mixedgraph"
    else if (class(graph) == "grain") cur_format <- "grain"
    else if ("graphNEL" %in% class(graph)) cur_format <- "graphNEL"
    else if ("graphAM" %in% class(graph)) cur_format <- "graphAM"
    else if (class(graph) == "graph") {
      cur_format <- "ADMG"
    }
    else {
      stop("Format not supported")
    }
  }
  
  if (cur_format == "ADMG") {
    if(format == "mixedgraph") {
      edges = list(undirected=graph$ud.edges, 
                   directed=graph$d.edges, 
                   bidirected=graph$bi.edges)
      edges <- edges[!sapply(edges, is.null)]
      out = mixedgraph(n, v=seq_len(n), edges = edges, vnames=graph$vnames)
    } 
    else if (cur_format %in% c("ggm")) {
      Recall(Recall(graph, "mixedgraph", cur_format=cur_format), format=format, "mixedgraph")
    }
    else {
      stop("Method not currently supported, but check back soon...")
    }
  }
  else if (cur_format == "ggm") {
    if (format == "mixedgraph") {
      nv <- nrow(graph)
      if (ncol(graph) != nv) stop("ggm adjacency matrix must be square")
      edges <- list()

      ud <- graph %% 2
      if (any(ud > 0)) {
        edges$undirected <- ud
        graph <- graph - ud
      }
      graph <- graph/10
      
      di <- graph %% 2
      if (any(di > 0)) {
        edges$directed <- di
        graph <- graph - di
      }
      graph <- graph/10
      
      bi <- graph %% 2
      if (any(bi > 0)) {
        edges$bidirected <- bi
        graph <- graph - bi
      }
      
      if (any(graph != 0)) stop("Not a valid ggm object")
      
      out <- mixedgraph(nv, edges = edges, vnames = colnames(graph))
    }    
    else if (cur_format %in% c("ADMG")) {
      Recall(Recall(graph, "mixedgraph", cur_format=cur_format), format=format, "mixedgraph")
    }
    else {
      stop("Method not currently supported, but check back soon...")
    }
  }
  else if (cur_format == "graphNEL") {
    if (format == "mixedgraph") {
      require(graph)
      vnames <- nodes(graph)
      edgeList <- mapply(function(x,y) lapply(y$edges, function(z) c(x,z)), 
                         seq_along(vnames), graph@edgeL)
      edgeList <- list(do.call(c, edgeList))
      names(edgeList) <- edgemode(graph)
      out <- mixedgraph(n=length(vnames), vnames=vnames, edges=edgeList)
    }
    else if (cur_format %in% c("ggm", "ADMG")) {
      Recall(Recall(graph, "mixedgraph", cur_format=cur_format), format=format, "mixedgraph")
    }
    else {
      stop("Method not currently supported, but check back soon...")
    }
  }
  else if (cur_format == "graphAM") {
    if (format == "mixedgraph") {
      require(graph)
      vnames <- colnames(graph@adjMat)
      edgeMat <- list(graph@adjMat)
      names(edgeMat) <- edgemode(graph)
      out <- mixedgraph(n=length(vnames), vnames=vnames, edges=edgeMat)
    }
    else if (cur_format %in% c("ggm", "ADMG")) {
      Recall(Recall(graph, "mixedgraph", cur_format=cur_format), format=format, "mixedgraph")
    }
    else {
      stop("Method not currently supported, but check back soon...")
    }
  }
  else if (cur_format == "PAG") {
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
    
    out <- mixedgraph(n = ncol(graph), vnames = colnames(graph), edges=edges)
    
    if (format != "mixedgraph") out <- Recall(out, format=format)
  }
  else if (cur_format == "mixedgraph") {
    if (format == "ADMG") {
      require(ADMGs)
      ud.edges <- edgeList(graph$edges$undirected)
      d.edges <- edgeList(graph$edges$directed)
      bi.edges <- edgeList(graph$edges$bidirected)
      nv <- length(graph$vnames)
      out = ADMGs::makeGraph(nv, 
                             ud.edges = ud.edges, 
                             d.edges = d.edges, 
                             bi.edges = bi.edges, 
                             vnames = graph$vnames)
    } 
    else if (format == "ggm") {
      nv <- length(graph$vnames)
      out <- matrix(0, nv, nv, dimnames=list(graph$vnames, graph$vnames))
      
      dir <- ("directed" %in% names(graph$edges))
      un <- ("undirected" %in% names(graph$edges))
      bi <- ("bidirected" %in% names(graph$edges))

      if (un) out <- out + adjMatrix(graph$edges$undirected, nv)
      if (dir) out <- out + 10*adjMatrix(graph$edges$directed, nv, directed = TRUE)
      if (bi) out <- out + 100*adjMatrix(graph$edges$bidirected, nv)
    }
    else if (format == "graphNEL") {
      # is graph directed or undirected?
      require(graph)
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
      
      out <- graphNEL(nodes=graph$vnames[graph$v], edgeL=edL, edgemode=mode)
    }
    else if (format == "graphAM") {
      # is graph directed or undirected?
      require(graph)
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
      
      out <- graphAM(amat, edgemode = mode)
    }
    else {
      stop("Method not currently supported, but check back soon...")
    }
  }
  else {
    stop("Method not currently supported, but check back soon...")
  }
  
  out
}
