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
##' @param fast optionally opt for a faster metod with adjacency matrices or lists
##' @export dis
dis = function(graph, v, sort=1, fast=FALSE) {
  if (!fast) return(grp(graph, v, etype="bidirected", dir=0, sort=sort))
  
  whEdge <- match("bidirected",names(graph$edges))
  bi_edges <- graph$edges[[whEdge]]  

  if ("adjList" %in% class(bi_edges)) {
    out <- add <- v
    while (length(add) > 0) {
      add <- unlist(bi_edges[add])
      add <- setdiff(add, out)
      out <- c(out, add)
    }
  } 
  else if ("adjMatrix" %in% class(bi_edges)) {
    out <- add <- v
    while (length(add) > 0) {
      add <- c(which(bi_edges[add,] > 0,arr.ind = TRUE))
      add <- setdiff(add, out)
      out <- c(out, add)
    }
  } 
  else return(grp(graph, v, etype="bidirected", dir=0, sort=sort))

  if (sort > 0) out <- unique.default(out)
  if (sort > 1) out <- sort.int(out)
    
  return(out)
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
##' \code{cliques} uses the Bron-Kirbosch algorithm to find 
##' maximal fully-connected subsets.
##' 
##' @export districts
districts = function(graph) {
  groups(graph, etype="bidirected")
}

##' @describeIn districts Obtain undirected component
##' @param sort should vertices be sorted?
##' @export un
un <- function(graph, sort=1) {
  adj(graph, v=graph$v, etype="undirected", dir=0, inclusive=TRUE, sort=sort)
}

##' @describeIn districts Obtain neighbourhoods
##' @param undirected_only logical: should vertices not adjacent to an 
##' undirected edge be ignored?
##' @export neighbourhoods
neighbourhoods = function(graph, undirected_only=TRUE) {
  if (undirected_only) groups(graph[un(graph)], etype="undirected")
  else groups(graph, etype="undirected")
}


##' @param sort should output be sorted?  sort=3 will also sort cliques
##' @param max_len maximum size of clique to consider
##' @describeIn districts Obtain maximal complete undirected subsets
##' @export cliques
cliques = function(graph, sort=1, max_len) {

  # ## could do this by neighbourhood
  # neigh <- neighbourhoods(graph[un(graph)])
  
  if (missing(max_len)) max_len <- length(graph$v)

  ## restrict to undirected part of the graph
  if (is.UG(graph)) gr_u <- graph
  else gr_u <- graph[un(graph)]

  ## get list of neighbours  
  n <- length(graph$vnames)
  nbs <- vector(mode="list", length=n)
  nbs[gr_u$v] <- lapply(gr_u$v, function(x) nb(gr_u, x))

  ## call Bron-Kirbosch algorithm
  out <- BK(R=integer(0), P=gr_u$v, X=integer(0), nbs, max_len=max_len)

  if (sort > 1) out <- lapply(out, sort.int)
  if (sort > 2) {
    out <- out[order(sapply(out, function(x) sum(2^x)))]
  }
  
  out
}

## Bron-Kerbosch Algorithm
BK <- function(R, P, X, nbs, max_len) {
  ## if nothing else to add, then return R
  if (length(R) == max_len || (length(P) == 0 && length(X) == 0)) {
    # print(R)
    return(list(R))
  }
  
  ## otherwise, make a list
  out <- list()
  
  for (v in P) {
    ## add each vertex in turn
    nb_v <- nbs[[v]]
    out <- c(out, BK(c(R,v), intersect(P, nb_v), intersect(X, nb_v), nbs, max_len))
    P <- setdiff(P, v)
    X <- c(X, v)
  }
  
  # return list of cliques found
  out
}

# findCliques <- function(graph) {
#   vs <- graph$v
#   
#   for (v in vs) {
#     nb(graph, v)
#   }
# }

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
  if (setequal(v, graph$v)) {
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
  
  out <- setdiff(v, ans)
  
  return(out)
}

##' @export sterile
##' @describeIn barren find vertices with no children in the same set
sterile = function(graph, v=graph$v){
  if (length(v) == 0) return(integer(0))
  pas = adj(graph, v, etype="directed", dir=-1)
  
  out <- setdiff(v, pas)
  
  out
}

##' Find Claudius of a (bidirected-connected) set
##' 
##' Find the Claudius of a (presumably) bidirected-connected set.
##' 
##' @param graph a \code{mixedgraph} object
##' @param v set of vertices to consider
##' 
##' @details Drops strict spouses of \code{v} and any of their 
##' descendants.
##' 
##' @examples 
##' data(gr1)
##' claudius(gr1, 1)
##' claudius(gr1, 4)
##' 
##' @export claudius
claudius <- function(graph, v) {
  sibs <- adj(graph, v, etype="bidirected", dir=0, inclusive=FALSE)
  
  setdiff(graph$v, dec(graph, sibs))
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
  e = collapse(graph$edges, dir=0)
  out = mixedgraph(v=graph$v, edges=list(undirected=e), vnames=graph$vnames)
  return(out)
}

##' Find ancestral sets of a graph.
##'
##' @param graph object of class \code{mixedgraph}, should be a summary graph
##' @param topOrder optional topological order of vertices
##' @param sort integer:1 for unique but unsorted, 2 for 
##' sorted.
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
  if (sort > 2) {
    ord <- order(sapply(out, function(x) sum(2^x)))
    out <- out[ord]
  }
  
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
  
  bar <- barrenSets(graph, max_size = maxbarren, same_dist = same_dist, 
                    sort=sort, return_anc_sets = TRUE)
  ancs <- attr(bar, "anSets")

  barSet <- list()
    
  for (b in seq_along(bar)) {
    tmp <- powerSet(bar[[b]], m = maxbarren)[-1]
    sm <- setmatch(barSet, tmp, nomatch = 0)
    if (any(sm > 0)) tmp <- tmp[-sm]
    barSet <- c(barSet, tmp)
  }
  
  ancs <- lapply(barSet, function(x) unlist(ancs[x]))
  
  if (sort > 0) {
    ancs <- lapply(ancs, unique.default)
    if (sort > 1) ancs <- lapply(ancs, sort.int)
  }

    
# 
#   out <- tmp <- ancs[!sapply(ancs, is.null)]
#   bar <- barSet # unlist(lapply(graph$v, list), recursive = FALSE)
#   b <- 2
# 
#   while (b <= maxbarren) {
#     if (b > 2) {
#       tmp <- tmp2
#       bar <- bar2
#     }
#     tmp2 <- bar2 <- list()
# 
#     for (i in seq_along(tmp)) {
#       if (same_dist) {
#         ## look for larger variables not already in
#         set <- setdiff(dis(graph, tmp[[i]][1]), seq_len(max(bar[[i]])))
#       }
#       else set <- setdiff(graph$v, seq_len(max(bar[[i]])))
# 
#       ## go through and add in any non-ancestors
#       for (j in set) {
#         if (!all(ancs[[j]] %in% tmp[[i]]) && !any(bar[[i]] %in% ancs[[j]])) {
#           tmp2 <- c(tmp2, list(unique.default(c(tmp[[i]], ancs[[j]]))))
#           bar2 <- c(bar2, list(c(bar[[i]], j)))
#         }
#       }
#     }
# 
#     ## if nothing new to add, then break out
#     if (length(tmp2) == 0) break
# 
#     out <- c(out, tmp2)
#     b <- b+1
#   }
# 
#   if (sort > 1) out <- lapply(out, sort.int)
  
  # out
  ancs
}

##' Get barren subsets
##' 
##' Return list of barren subsets up to specified size
##' 
##' @param graph object of class \code{mixedgraph}
##' @param topOrder optionally, a topological order
##' @param max_size integer giving maximum size to consider
##' @param same_dist logical: should barren sets be in the same district?
##' @param sort integer:1 for unique but unsorted, 2 for 
##' sorted.
##' @param return_anc_sets logical: return ancestral sets for each vertex as an attribute?
##' 
##' @details Uses \code{clique} algorithm on a suitable undirected graph.
##' 
##' \strong{Warning:} Doesn't work for cyclic graphs.
##' 
##' @export barrenSets
barrenSets <- function(graph, topOrder, max_size, same_dist=FALSE, 
                      sort=1, return_anc_sets=FALSE) {
  
  if (missing(max_size)) max_size <- length(graph$v)
  if (max_size < 1) return(list())
  
  # children <- lapply(graph$v, function(x) ch(graph, x))
  parents <- list()
  parents[graph$v] <- lapply(graph$v, function(x) pa(graph, x))
  
  if (missing(topOrder)) topOrder <- topologicalOrder(graph)
  ancs <- vector(mode="list", length = length(parents))
  
  ## create a new undirected graph where edge v -- w exists if 
  ## and only if v and w are incomparable in graph
  graph2 <- mixedgraph(v=graph$v, edges=list(undirected=eList()), vnames=graph$vnames) # mutilate(graph, graph$v)
  # class(graph2$edges$undirected) <- "eList"

  for (i in topOrder) {
    ancs[[i]] <- c(i, unique.default(unlist(ancs[parents[[i]]])))
    graph2 <- addEdges(graph2, list(un = eList(lapply(ancs[[i]][-1], function(x) c(x,i)))))
    # graph2$edges$undirected <- c(graph2$edges$undirected, lapply(ancs[[i]][-1], function(x) c(x,i)))
  }
  
  ### perhaps speed this up by using sparse matrices when same_dist = TRUE
  graph2 <- withAdjMatrix(graph2, sparse = FALSE)
  graph2$edges$undirected <- 1 - graph2$edges$undirected - diag(nrow=nrow(graph2$edges$undirected))
  
  out <- list()
  
  if (requireNamespace("igraph")) {
    if (same_dist) {
      dis <- districts(graph)
      
      for (d in seq_along(dis)) {
        gr_u <- convert(graph2[dis[[d]]], "igraph")
        tmp <- igraph::max_cliques(gr_u)
        tmp <- lapply(tmp, function(x) match(names(x), graph$vnames))
        out <- c(out, tmp)
      }
    }
    else {
      gr_u <- convert(graph2, "igraph")
      tmp <- igraph::max_cliques(gr_u)
      tmp <- lapply(tmp, function(x) match(names(x), graph$vnames))
      out <- cliques(graph2, max_len = max_size)
    }
  }
  else {
    if (same_dist) {
      dis <- districts(graph)
      
      for (d in seq_along(dis)) {
        out <- c(out, cliques(graph2[dis[[d]]], max_len = max_size))
      }
    }
    else {
      out <- cliques(graph2, max_len = max_size)
    }
  }
  
  ## now check if any exceed the maximum size allowed
  lens <- lengths(out)
  if (max_size < max(lens)) {
    for (i in which(lens > max_size)) out <- c(out, combn(out[[i]], max_size, simplify = FALSE))
    
    # remove overly long sets
    out <- out[-which(lens > max_size)]

    # remove any duplicates
    f <- function(n) sum(2^n)
    chk <- sapply(out, f)
    out <- out[!duplicated(chk)]
  }
  
  if (sort > 1) out <- lapply(out, sort.int)

  if (return_anc_sets) attr(out, which = "anSets") <- ancs
  else attr(out, which = "anSets") <- NULL
  
  out
}
