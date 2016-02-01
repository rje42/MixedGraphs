## functions for using graph, igraph, gR, ...

##' Convert graph to format associated with specific package
##' 
##' @param graph graphical object, of one of classes listed below
##' @param format character string giving new format to convert to
##' 
##' @details Currently limited functionality, only
##' converts 'ADMGs' to 'mixedgraph' objects.
convert <- function(graph, format="mixedgraph", cur_format) {
  
  if (missing(cur_format)) {
    if (class(graph) == "igraph") cur_format <- "igraph"
    else if (class(graph) == "mixedgraph") cur_format <- "mixedgraph"
    else if (class(graph) == "grain") cur_format <- "grain"
    else if (class(graph) == "graphNEL") cur_format <- "graph"
    else if (class(graph) == "graphAM") cur_format <- "graph"
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
    else {
      stop("Method not currently supported, but check back soon...")
    }
  }
  else if (cur_format == "mixedgraph") {
    if(format == "ADMG") {
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
      
      dir <- (length(graph$edges["directed"]) > 0)
      un <- (length(graph$edges["undirected"]) > 0)
      bi <- (length(graph$edges["bidirected"]) > 0)

      if (un) out <- out + adjMatrix(graph$edges$undirected, nv)
      if (dir) out <- out + 10*adjMatrix(graph$edges$directed, nv, directed = TRUE)
      if (dir) out <- out + 100*adjMatrix(graph$edges$bidirected, nv)
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
