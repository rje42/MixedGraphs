##' Familial Mixed Graph Relations
##' 
##' The usual familial relations between vertices in
##' mixed graphs.
##' 
##' @aliases ch
##' @param graph \code{mixedgraph} object
##' @param v collection of vertices
##' @param sort integer:1 for unique but unsorted, 2 for 
##' sorted (0 for possibly repeated and unsorted).  If edges are stored as a matrix
##' then output will always be unique and sorted.
##' 
##' @details \code{pa}, \code{ch}, \code{sp} and \code{nb} find the 
##' parents, children, spouses and neighbours of \code{v} respectively.
##' \code{anc}, \code{dec}, \code{ant}, \code{dis} finds the ancestors
##' descendants, anterior and district of \code{v} respectively.
##' 
##' @export pa
pa = function(graph, v, sort=1) {
  adj(graph, v, etype="directed", dir=-1, inclusive=TRUE, sort=sort)
}

##' @describeIn pa find children of vertices
##' @export ch 
ch = function(graph, v, sort=1) {
  adj(graph, v, etype="directed", dir=1, inclusive=TRUE, sort=sort)
}

##' @describeIn pa find spouses (siblings) of vertices
##' @export sp
sp = function(graph, v, sort=1) {
  adj(graph, v, etype="bidirected", dir=0, inclusive=TRUE, sort=sort)
}

##' @describeIn pa find siblings (spouses) of vertices
##' @export sib
sib = function(graph, v, sort=1) {
  adj(graph, v, etype="bidirected", dir=0, inclusive=TRUE, sort=sort)
}

##' @describeIn pa find undirected neighbours of vertices
##' @export nb
nb = function(graph, v, sort=1) {
  adj(graph, v, etype="undirected", dir=0, inclusive=TRUE, sort=sort)
}

##' @describeIn pa find ancestors of vertices
##' @export anc
anc = function(graph, v, sort=1) {
  grp(graph, v, etype="directed", dir=-1, sort=sort)
}

##' @describeIn pa find descendants of vertices
##' @export dec
dec = function(graph, v, sort=1) {
  grp(graph, v, etype="directed", dir=1, sort=sort)
}

##' @describeIn pa find anterior vertices
##' @export ant
ant = function(graph, v, sort=1) {
  grp(graph, v, etype=c("directed", "undirected"), dir=c(-1,0), sort=sort)
}

##' @describeIn pa find district of vertices
##' @export dis
dis = function(graph, v, sort=1) {
  grp(graph, v, etype="bidirected", dir=0, sort=sort)
}

##' Familial Mixed Graph Groups
##' 
##' The usual familial relations between vertices in
##' mixed graphs.
##' 
##' @aliases neighbourhoods, un
##' @param graph \code{mixedgraph} object
##' 
##' @details \code{districts} and \code{neighbourhoods} find the
##' bidirected-connected and undirected-connected components of \code{graph}.
##' \code{un} finds the undirected part of \code{graph}.
##' 
##' @export districts
districts = function(graph) {
  groups(graph, etype="bidirected")
}

##' @describeIn districts Obtain undirected component
##' @param sort should vertices be sorted?
##' @export un
un <- function(graph, sort=1) {
  out <- unlist(graph$edges$undirected)
  if (length(out) == 0) return(integer(0))
  if (sort > 0) out <- unique.default(out)
  if (sort > 1) out <- sort.int(out)
  out
}

##' @describeIn districts Obtain neighbourhoods
##' @export neighbourhoods
neighbourhoods = function(graph) {
  groups(graph[un(graph)], etype="undirected")
}

##' Find Markov blanket
##' 
##' Find the Markov blanket for a vertex in an 
##' ancestral set
##' 
##' @param graph \code{mixedgraph} object
##' @param v a vertex, childless in \code{A}
##' @param A an ancestral collection of vertices
##' @param check logical: check \code{A} is ancestral?
##' @param sort integer:1 for unique but unsorted, 2 for 
##' sorted (0 for possibly repeated and unsorted).  If edges are stored as a matrix
##' then output will always be unique and sorted.
##' 
##' @details Finds the Markov blanket of \code{v} in \code{A}.
##' 
##' @export mb
mb = function(graph, v, A, check=TRUE, sort=1) {
  if (!is.mixedgraph(graph)) stop("'graph' should be an object of class 'mixedgraph'")
  if (missing(A)) A <- graph$v
  
  if (!(v %in% A)) stop("v must be a member of A")
  
  if (check) {
    A2 <- pa(graph, A)
    if (!all(A2 %in% A)) {
      stop("Error: A is not ancestral")
    }
  }
  
  # consider subgraph over A
  graph <- graph[A]
  if (length(ch(graph, v)) > 0) stop("v is not childless in A")
  
  ## get Markov blanket  
  D <- dis(graph, v)
  out <- c(pa(graph, D), D)
  
  if (sort == 1) {
    out <- unique.default(out)
  }
  else if (sort == 2) {
    out <- sort.default(unique.default(out))
  }
  
  out
}

##' Find barren, sterile, orphaned vertices
##' 
##' @param graph an object of class \code{mixedgraph}
##' @param v set of vertices of \code{graph}
##' 
##' @details Barren vertices (within \code{v}) are those
##' that have no proper descendants also within \code{v}.
##' Sterile (orphaned) vertices in \code{v} have no 
##' children (parents) also within \code{v}.
##' 
##' 
##' @export barren
barren = function (graph, v = graph$v) {
  if (length(v) == 0) return(integer(0))
  if (length(v) == length(graph$v)) {
    ans = adj(graph, v, etype="directed", dir=-1)
    ans <- setdiff(v, ans)
  }
  else {
    ancs <- list()
    for (i in seq_along(v)) {
      if (v[i] %in% unlist(ancs)) next
      else ancs[[i]] <- grp(graph, v[i], etype="directed", dir=-1, inclusive = FALSE)
    }
    
    ans <- setdiff(v, unlist(ancs))
  }
  
  return(ans)
}

##' @describeIn barren find vertices with no parents
##' @export orphaned
orphaned = function (graph, v = graph$v) {
  if (length(v) == 0) return(integer(0))
  ans = adj(graph, v, etype="directed", dir=1)
  
  return(setdiff(v, ans))
}

##' @export sterile
##' @describeIn barren find vertices with no children in the same set
sterile = function(graph, v=graph$v){
  pas = pa(graph, v)
  sterile = setdiff(v, pas)
  
  sterile
}

##' Graph skeleton
##' 
##' Find undirected skeleton of a mixed graph
##' 
##' @param graph a \code{mixedgraph} object
##' 
##' @export skeleton
skeleton = function(graph) {
  if (!is.mixedgraph(graph)) stop("'graph' should be an object of class 'mixedgraph'")
  # e = lapply(unlist(graph$edges, recursive=FALSE), sort.int)
  # e = unique(e)
  e = collapse(graph$edges)
  out = mixedgraph(v=graph$v, edges=list(undirected=e), vnames=graph$vnames)
  return(out)
}

##' Find ancestral sets of a graph.
##'
##' @param graph object of class \code{mixedgraph}, should be a summary graph
##' @param topOrder optional topological order of vertices
##'
##' @details Algorithm:
##' 1. Find a topological order of nodes.
##' 2. Base case: {} is ancestral
##' 3. Induction: (i) Assume we have a list L of all ancestral sets involving Xi-1 in the order.
##' (ii) If an ancestral set S in L contains all parents of Xi, Xi + S is also ancestral.
##' 
##' The function \code{anSets2} proceeds by adding a new barren vertex to the 
##' set, which is not a descendant of any existing vertices.  It consequently 
##' provides the option \code{maxbarren} to cap this at a fixed value.
##' 
##' @author Ilya Shpitser
##' 
##' @export anSets
anSets = function(graph, topOrder, sort=1) {
  if (length(graph$v) <= 1) return(list(graph$v))
  out = list(integer(0))
  if (missing(topOrder)) topOrder <- topologicalOrder(graph)
  
  for(node in topOrder) {
    parents <- pa(graph, node)
    additions <- list()
    
    for(set in out){
      if(length(parents) == 0 || all(parents %in% set)){
        additions <- c(additions, list(c(set, node)))
      }
    }
    out <- c(out, additions)
  }
  
  if (sort > 1) out <- lapply(out, sort.int)
  
  out[-1]
}

##' @param maxbarren maximum size of barren subsets
##' @param same_dist logical, should barren vertices be in the same district?
##' @describeIn anSets Uses different algorithm
##' @export anSets2
anSets2 = function(graph, topOrder, maxbarren, same_dist=FALSE, sort=1) {
  
  if (missing(maxbarren)) maxbarren <- length(graph$v)
  if (maxbarren < 1) return(list())
  
  # children <- lapply(graph$v, function(x) ch(graph, x))
  parents <- list()
  parents[graph$v] <- lapply(graph$v, function(x) pa(graph, x))
  
  if (missing(topOrder)) topOrder <- topologicalOrder(graph)
  ancs <- list()
  
  for (i in topOrder) {
    ancs[[i]] <- c(i, unique.default(unlist(ancs[parents[[i]]])))
  }
  
  out <- tmp <- ancs[!sapply(ancs, is.null)]
  bar <- unlist(lapply(graph$v, list), recursive = FALSE)
  b <- 2
  
  while (b <= maxbarren) {
    if (b > 2) {
      tmp <- tmp2
      bar <- bar2
    }
    tmp2 <- bar2 <- list()
    
    for (i in seq_along(tmp)) {
      if (same_dist) {
        set <- setdiff(dis(graph, tmp[[i]][1]), seq_len(max(tmp[[i]])))
      }
      else set <- setdiff(graph$v, seq_len(max(tmp[[i]])))
      for (j in set) {
        if (!any(bar[[i]] %in% ancs[[j]])) {
          tmp2 <- c(tmp2, list(unique.default(c(tmp[[i]], ancs[[j]]))))
          bar2 <- c(bar2, list(c(bar[[i]], j)))
        }
      }
    }
    out <- c(out, tmp2)
    b <- b+1
  }
  
  if (sort > 1) out <- lapply(out, sort.int)
  
  out
}
