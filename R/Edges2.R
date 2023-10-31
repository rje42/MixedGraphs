##' Put edges in correct format
##' 
##' @param x object containing edge information
##' @param valid logical indicating whether format is already correct
##' 
##' @details
##' If `valid = TRUE` then entry should be a valid adjacency matrix, and checks
##' are not performed.
##' 
##' @export
as.adjMatrix <- function (x, valid=FALSE) {
  
  if (!valid) {
    if (!is.matrix(x)) {
      if (is.list(x)) {
        if (is.adjList(x) || is.eList(x)) {
        }
        else if (is.adjList(x, checknm = FALSE)) {
          class(x) <- c("adjList", class(x))
        }
        else if (is.eList(x, checknm = FALSE)) {
          class(x) <- c("eList", class(x))
        }
        else stop("Format of list not recognized")
        
        ## return correct object      
        return(adjMatrix(x))
      }
      else stop("Input should be a square matrix or a list")
    } 
    else if (nrow(x) != ncol(x)) {
      stop("Adjacency matrix must be square")
    }
    else if (any(diag(x) != 0)) {
      stop("Diagonal should be zero")
    }
    else if (any(x < 0)) {
      stop("Entries should be non-negative")
    }
  }
  
  class(x) <- c("adjMatrix", class(x))
  return(x)
}
