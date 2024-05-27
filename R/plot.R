##' @exportS3Method
plot.mixedgraph <- function (graph) {
  if (!require(pcalg) || !require(Rgraphviz)) stop("Must have 'pcalg' and 'Rgraphviz' packages to plot")
  
  plot_fci <- selectMethod("plot", signature = "fciAlgo")@.Data
  
  amat <- convert(graph, "PAG")
  amat <- as(amat, "matrix")

  ## try to trick pcalg package into plotting for us
  fci_obj <- new("fciAlgo", amat=amat)
  
  plot_fci(fci_obj)
}
