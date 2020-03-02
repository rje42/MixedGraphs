## functions for using graph, igraph, PAG, ggm, igraph, bn, ADMGs
## Really need to tidy this up


##' Convert graph to format associated with specific package
##' 
##' @param graph graphical object, of one of classes listed below
##' @param format character string giving new format to convert to
##' @param cur_format character string of current format; can be deduced 
##' from the object in many cases
##' @param ... other arguments
##' 
##' @details Possible formats
##' are 
##' \itemize{
##' \item \code{\link{mixedgraph}}
##' \item \code{ggm}, an adjacency matrix as specified in the \code{ggm} package; 
##' \item \code{graphNEL}, \code{graphAM}, \code{graphBAM} from the \code{graph} package;
##' \item \code{igraph};
##' \item \code{PAG}: that is, the output of \code{pc()} or \code{fci()} functions in the \code{pcalg} package;
##' \item \code{bn} from the \code{bnlearn} package;
##' \item \code{ADMG}, from the \code{ADMGs} package.
##' }
##' The function can recognise the class of objects other than
##' "ggm", which is just an adjacency matrix.
##' 
##' Implemented natively thus far:
##' \itemize{
##' \item mixedgraph <-> ggm
##' \item mixedgraph <-> ADMG
##' \item mixedgraph <-> graphNEL
##' \item mixedgraph <-> graphAM
##' \item mixedgraph <-> graphBAM
##' \item mixedgraph <-> PAG
##' \item mixedgraph <-> bn
##' \item mixedgraph <-> igraph (goes mixedgraph -> graphNEL -> igraph)
##' \item graphNEL <-> igraph (using functions in the \code{igraph} package)
##' \item graphNEL, graphAM <-> bn (using functions in the \code{bnlearn} package)
##' }
##' 
##' \code{ggm} entries must be specified by \code{cur_format = "ggm"}.  
##' \code{PAG} objects are those whose class inherits from \code{gAlgo}, 
##' or an adjacency matrix if \code{cur_format = "PAG"} is specified.
##' 
##' @export convert
convert <- function(graph, format="mixedgraph", cur_format, ...) {
  
  ## formats we can do 'to' and 'from' mixedgraph directly.
  via_mixed_graph <- c("ggm", "ADMG", "graphNEL", "graphAM", "bn")
  
  if (missing(cur_format)) {
    if (class(graph) == "igraph") cur_format <- "igraph"
    else if (class(graph) == "mixedgraph") cur_format <- "mixedgraph"
# else if (class(graph) == "grain") cur_format <- "grain"
    else if ("graphNEL" %in% class(graph)) cur_format <- "graphNEL"
    else if ("graphAM" %in% class(graph)) cur_format <- "graphAM"
    else if ("graphBAM" %in% class(graph)) cur_format <- "graphBAM"
    else if ("gAlgo" %in% is(graph)) cur_format <- "PAG"
    else if (class(graph) == "graph") {
      cur_format <- "ADMG"
    }
    else if (class(graph) == "bn") cur_format <- "bn"
    else if (class(graph) == "matrix") {
      warning("Matrix input, assuming ggm format")
      cur_format <- "ggm"
    }
    else {
      stop("Format not supported")
    }
  }
  
  if (format == cur_format) return(graph)
  
  if (cur_format == "ADMG") {
    if(format == "mixedgraph") {
      out = conv_ADMG_mixedgraph(graph)
    } 
    else if (cur_format %in% via_mixed_graph) {
      ## go 'via' a mixedgraph
      ## this is safe since ADMGs are less general than mixedgraphs
      out <- Recall(Recall(graph, "mixedgraph", cur_format=cur_format), format=format, "mixedgraph")
    }
    else {
      stop("Method not currently supported, but check back soon...")
    }
  }
  else if (cur_format == "ggm") {
    if (format == "mixedgraph") {
      out <- conv_ggm_mixedgraph(graph)
    }    
    else if (cur_format %in% via_mixed_graph) {
      ## go 'via' a mixedgraph.  This _should_ be safe.
      out <- Recall(Recall(graph, "mixedgraph", cur_format=cur_format), format=format, "mixedgraph")
    }
    else {
      stop("Method not currently supported, but check back soon...")
    }
  }
  else if (cur_format == "graphNEL") {
    if (format == "mixedgraph") {
      out = conv_graphNEL_mixedgraph(graph)
    }
    else if (format == "igraph") {
      requireNamespace("igraph", quietly = TRUE)
      out = igraph::igraph.from.graphNEL(graph)
    }
    else if (cur_format %in% via_mixed_graph) {
      ## go 'via' a mixedgraph
      out <- Recall(Recall(graph, "mixedgraph", cur_format=cur_format), format=format, "mixedgraph")
    }
    else {
      stop("Method not currently supported, but check back soon...")
    }
  }
  else if (cur_format == "graphAM") {
    if (format == "mixedgraph") {
      out <- conv_graphAM_mixedgraph(graph)
    }
    else if (cur_format %in% c("ggm", "ADMG")) {
      out <- Recall(Recall(graph, "mixedgraph", cur_format=cur_format), format=format, "mixedgraph")
    }
    else {
      stop("Method not currently supported, but check back soon...")
    }
  }
  else if (cur_format == "graphBAM") {
    if (format == "mixedgraph") {
      requireNamespace("graph", warn.conflicts = FALSE, quietly = TRUE)
      A <- graph::adjacencyMatrix(graph)
      vnames <- colnames(A)
      edgeMat <- list(A)
      names(edgeMat) <- graph::edgemode(graph)
      out <- mixedgraph(n=length(vnames), vnames=vnames, edges=edgeMat)
    }
    else if (cur_format %in% c("ggm", "ADMG")) {
      out <- Recall(Recall(graph, "mixedgraph", cur_format=cur_format), format=format, "mixedgraph")
    }    
    else {
      stop("Method not currently supported, but check back soon...")
    }
  }
  else if (cur_format == "PAG") {
    out <- conv_PAG_mixedgraph(graph)
    if (format != "mixedgraph") out <- Recall(out, format=format)
  }
  else if (cur_format == "igraph") {
    
    if (format == "graphNEL") {
      requireNamespace("igraph", quietly = TRUE)
      out = igraph::igraph.to.graphNEL(graph)
    }
    else {
      out <- conv_igraph_mixedgraph(graph)
      if (format != "mixedgraph") out <- Recall(out, format=format)
    }
  }
  else if (cur_format == "bn") {
    if (format == "graphNEL") {
      requireNamespace("bnlearn", quietly = TRUE)
      return(bnlearn::as.graphNEL(graph))
    }
    else if (format == "graphAM") {
      requireNamespace("bnlearn", quietly = TRUE)
      return(bnlearn::as.graphAM(graph))
    }
    
    out <- conv_bn_mixedgraph(graph)
    if (format != "mixedgraph") out <- Recall(out, format=format)
  }
  else if (cur_format == "mixedgraph") {
    if (format == "ADMG") {
      out = conv_mixedgraph_ADMG(graph)
    } 
    else if (format == "ggm") {
      out = conv_mixedgraph_ggm(graph)
    }
    else if (format == "graphNEL") {
      out = conv_mixedgraph_graphNEL(graph)
    }
    else if (format == "graphAM") {
      out = conv_mixedgraph_graphAM(graph)
    }
    else if (format == "graphBAM") {
      out = as(conv_mixedgraph_graphNEL(graph), "graphBAM")
    }
    else if (format == "bn") {
      out = conv_mixedgraph_bn(graph)
    }
    else if (format == "igraph") {
      out = conv_mixedgraph_igraph(graph)
    }
    else if (format == "PAG") {
      out = conv_mixedgraph_PAG(graph)
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



##' Automatically convert and apply graph function
##' 
##' Experimental automatic conversion function
##' 
##' @param graph a graph object that can be handled
##' by \code{MixedGraphs}
##' @param .f right-hand side of a pipe to be evaluated
##' 
##' This is a version of the pipe `%>%` from \code{magrittr}
##' that converts the left-hand side into a graph of suitable
##' format to be used by the right-hand side.
##' 
##' @export %G%
`%G%` <- function (graph, .f) 
{
  requireNamespace("magrittr", quietly = TRUE)
  subf = substitute(.f)
  package = determinePackage(subf)
  
  wh = match(package, graphFormats()$package)
  if (is.na(wh)) {
    if (package %in% c("gRbase", "gRain")) {
      mode = "graphNEL"
    }
    else mode = package
  }
  else mode = graphFormats()$format[wh]
  
  eval(substitute(graph %>% .f, 
                  list(graph=convert(graph, mode), .f=subf)
                  # list(graph=convert(graph, mode), .f=substitute(.f, env=))
                  ))
}

##' Determine which graph package evaluation will
##' occur in.
##' 
##' @param x quoted expression
##' 
##' @details Fails if functions match distinct
##' packages.  Might need to change this in future if
##' different packages work with the same format.
determinePackage <- function(x) {
  
  if(is.name(x)) {
    ## if RHS is just a single function, find out 
    ## which package it's in
    env = environment(eval(x))
    if (!is.null(env)) package = packageName(env)
    else package = "base"
  }
  else if (is.call(x)) {
    ## otherwise need to do something more clever
    ## add a recursion
    tmp <- as.list(x)
    
    if (x[[1]] == quote(`::`)) return(as.character(x[[2]]))
    
    package = vapply(x, determinePackage, character(1))
    package = intersect(package, graphFormats()$package)
    if (length(package) > 1) stop("Multiple graph packages in code")
    else if (length(package) == 0) package = ""
    # package = packageName(enclosing_env(pairlist(subf)[[1]][[1]]))
  }
  else return("")
  
  return(package)
}

# grswit <- function(name) {
#   if (!is.na(match(name, ls("package:MixedGraphs")))) {
#     return("mixedgraph")
#   }
#   
#   requireNamespace("graph", quietly = TRUE)
#   if (!is.na(match(name, ls("package:graph")))) {
#     return("graphNEL")
#   }
#   
#   requireNamespace("igraph", quietly = TRUE)
#   if (!is.na(match(name, ls("package:igraph")))) {
#     return("igraph")
#   } 
#   
#   requireNamespace("ggm", quietly = TRUE)
#   if (!is.na(match(name, ls("package:ggm")))) {
#     return("ggm")
#   } 
#   
#   NA_character_
# }

