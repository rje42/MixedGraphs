##' Put edges in correct format
##' 
##' Functions to place adjacency object in a specific format.
##' 
##' @param x object containing edge information
##' @param valid logical indicating whether format is already correct
##' @param n number of vertices
##' 
##' @details
##' If `valid = TRUE` then for `as.adjMatrix` the argument `x` should be a 
##' valid adjacency matrix, and no checks are performed.  Similarly for `as.adjList`
##' the object should be a valid adjacency list.
##' 
##' @name as_adjacency
NULL

##' @describeIn as_adjacency Convert to an `adjMatrix` object
##' @export
as.adjMatrix <- function (x, valid=FALSE, n) {
  
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
        return(adjMatrix(x, n = n))
      }
      else stop("Input should be a matrix or a list")
    }
    else if (is.edgeMatrix(x) || is.adjMatrix(x)) {
      return(adjMatrix(x, n = n))
    }
    else if (is.edgeMatrix(x, checknm = FALSE)) {
      class(x) <- c("edgeMatrix", class(x))
      return(adjMatrix(x, n = n))
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
  
  if (!("adjMatrix" %in% class(x))) class(x) <- c("adjMatrix", class(x))
  return(x)
}

##' @describeIn as_adjacency Convert to an `adjList` object
##' @export
as.adjList <- function (x, valid=FALSE, n) {
  
  if (!valid) {
    ## put code in to support sparse adjMatrix objects
    if (!is.matrix(x)) {
      if (is.list(x)) {
        if (is.adjList(x)) {
          return(x)
        }
        if (is.eList(x)) {
        }
        else if (is.adjList(x, checknm = FALSE)) {
          class(x) <- c("adjList", class(x))
          return(x)
        }
        else if (is.eList(x, checknm = FALSE)) {
          class(x) <- c("eList", class(x))
        }
        else stop("Format of list not recognized")
        
        ## return correct object      
        return(adjList(x))
      }
      else stop("Input should be a square (dense) matrix or a list")
    } 
    else if (is.edgeMatrix(x) || is.adjMatrix(x)) {
      return(adjList(x, n = n))
    }
    else if (is.edgeMatrix(x, checknm = FALSE)) {
      class(x) <- c("edgeMatrix", class(x))
      return(adjList(x, n = n))
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
    
    if (!("adjMatrix" %in% class(x))) class(x) <- c("adjMatrix", class(x))
    return(adjList(x, n = n))
  }
  
  if (!("adjList" %in% class(x))) class(x) <- c("adjList", class(x))
  return(x)
}
