##' @importFrom graphics plot
##' @importFrom methods new selectMethod
##' @exportS3Method
plot.mixedgraph <- function (x, ...) {
  if (!requireNamespace("pcalg") || !requireNamespace("Rgraphviz")) stop("Must have 'pcalg' and 'Rgraphviz' packages installed for plotting")
  
  plot_fci <- selectMethod("plot", signature = "fciAlgo")@.Data
  
  amat <- convert(x, "PAG")
  amat <- as(amat, "matrix")

  ## try to trick pcalg package into plotting for us
  fci_obj <- new("fciAlgo", amat=amat)
  
  plot_fci(fci_obj)
}
