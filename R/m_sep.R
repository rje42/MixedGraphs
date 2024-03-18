##' Moralize a mixed graph
##' 
##' Moralizes a mixed graph, possibly conditional on a subset of vertices.
##' 
##' @param graph an object of class `mixedgraph`
##' @param C optional set of vertices to condition upon
##' @param check logical: should we check graph is a summary graph?
##'
##' @details This works for any summary graph, though currently needs the 
##' graph to be the original one (and not any subgraph) in order to be 
##' guaranteed to run correctly.
##' 
##' 
##' @export
# moralize <- function(graph, C, check=TRUE) {
#   
#   if (check && !is_SG(graph)) stop("Object must be a summary graph of class 'mixedgraph'")
#   
#   if (!missing(C)) {
#     if (length(C) == 0) return(graph)
#     
#     ## work in progress!  FIX THIS
#     An <- anc(graph, C)
#     Pa <- pa(graph, C)
#     Sib <- sib(graph, C)
#     Sibs <- setdiff(sib(graph, An), An) ## edges An -> Sibs must become directed
#     part <- Recall(graph[c(An,Sibs)])
#     part2 <- morphEdges(part[An, Sibs], to="directed", topOrd = c(An,Sibs))
#     part <- mutilate(part, An, Sibs, etype="undirected")
#     part <- addEdges(part, part2$edges)
#     
#     # part <- addNodes(part, length(Sibs), vnames = graph$vnames[Sibs])
#     
#     out <- mutilate(graph, An, internal = TRUE)    
#     
#     out <- mutilate(out, An, Sibs, etype="bidirected")
#     out <- addEdges(out, edges=part[c(An, Sibs)]$edges)
#     # out <- addEdges(out, edges=mid$edges)
#     
#     # ## add edges from parents to siblings
#     # new_dir <- mapply(c, Pa, rep(setdiff(Sibs, Pa), each=length(Pa)), SIMPLIFY = TRUE)
#     # if (length(new_dir) > 0 && ncol(new_dir) > 0) {
#     #   new_dir <- apply(new_dir[, new_dir[1,] != new_dir[2,],drop=FALSE], 2, c, simplify = FALSE)
#     #   out <- addEdges(out, edges=list(directed=eList(new_dir)))
#     # }
#     # 
#     # ## add edges from siblings to siblings
#     # new_dir <- mapply(c, Pa, rep(setdiff(Sibs, Pa), each=length(Pa)), SIMPLIFY = TRUE)
#     # if (length(new_dir) > 0 && ncol(new_dir) > 0) {
#     #   new_dir <- apply(new_dir[, new_dir[1,] != new_dir[2,],drop=FALSE], 2, c, simplify = FALSE)
#     #   out <- addEdges(out, edges=list(directed=eList(new_dir)))
#     # }
#     
#     return(out)
#   }
#   
#   if (nv(graph) <= 1) return(graph)
#   out <- skeleton(graph)
#   dists <- districts(graph)
#   pa_dists <- lapply(dists, function(x) union(x, pa(graph, x)))
#   
#   n <- length(graph$vnames)
#   using_am <- sapply(graph$edges, is.adjMatrix, checknm=TRUE)
#   extra <- adjMatrix(n=n)
# 
#   ## check parents/spouses for each are joined
#   for (i in seq_along(dists)) {
#     extra[pa_dists[[i]],pa_dists[[i]]] = 1
#   }
#   diag(extra) = 0
#   
#   out = addEdges(out, undirected=extra)
#   return(out)
# }
# 
moralize <- function (graph, C, check=TRUE) {

  if (check && !is_SG(graph)) stop("Object must be a summary graph of class 'mixedgraph'")
  
  ## obtain sets to moralize
  if (missing(C)) {
    A <- graph$v
    S <- integer(0)
  }
  else {
    C <- barren(graph, C)
    A <- anc(graph, C)
    S <- setdiff(sib(graph, A), A)
  }
  
  # out <- graph
  # out <- mutilate(out, A=A, internal=TRUE)
  gr_d <- mutilate(graph[c(A,S)], A=S, internal=TRUE)
  
  ## get the districts and their parents in the graph that will need to be moralized
  dists <- districts(gr_d)
  pa_dists <- lapply(dists, function(x) union(x, pa(gr_d, intersect(x, A))))
  
  gr_d <- morphEdges(gr_d, to="directed", A=S, B=A, topOrd=c(A,S))
  gr_d <- morphEdges(gr_d, to="undirected", A=A)
  
  
  graph <- mutilate(graph, A, internal = TRUE)
  graph <- mutilate(graph, A, S)
  
  for (i in seq_along(dists)) {
    ## moralize this district
    D <- pa_dists[[i]]
    if (length(D) <= 1) next
    Ad <- intersect(A, D)
    Sd <- intersect(S, D)
    assertthat::are_equal(length(c(Ad,Sd)), length(D))
    
    
    tmp <- complete_mg(length(Ad), length(Sd))
    ord <- order(c(Ad, Sd, setdiff(seq_along(graph$vnames),c(Ad,Sd))))
    gr_d <- addEdges(gr_d, to_subgraph(tmp, ord = ord)$edges)
    
    # ## get list of bidirected edges to add back
    # SG <- makeGraphComplete(length(Sd), "bidirected")
    # SG <- addNodes(SG, nv(graph)-nv(SG))   # sort this out for subgraphs! 
    # SG <- SG[order(c(Sd,graph$v[-Sd])), order=TRUE]
    # 
    # ## get list of undirected edges to add back
    # AG <- makeGraphComplete(length(Ad), "undirected")
    # AG <- addNodes(AG, nv(graph)-nv(AG))   # sort this out for subgraphs! 
    # AG <- AG[order(c(Ad,graph$v[-Ad])), order=TRUE]
    # 
    # ## get list of directed edges to add back
    # DG <- makeGraphBipartite(length(Ad),length(Sd), "directed")
    # DG <- addNodes(DG, nv(graph)-length(Ad)-length(Sd))   # sort this out for subgraphs! 
    # DG <- DG[order(c(Ad,Sd,graph$v[-c(Sd,Ad)])), order=TRUE]
    # 
    # gr_d <- addEdges(gr_d, edges=list(bidirected=SG$edges$bidirected,
    #                                     undirected=AG$edges$undirected,
    #                                     directed=DG$edges$directed))
    graph <- addEdges(graph, gr_d$edges)
  }
  graph <- addEdges(graph, gr_d$edges)
  
  return(graph)
}

##' Construct complete graph 
##' 
##' Make a complete graph with nodes having only tails in subset of size `nU`, 
##' and only arrow heads in remaining subset of size `nB`
##' 
##' @param nU,nB number of nodes in each component
##' @param ord optional reordering of the vertices
##' 
##' @details
##' The default ordering is topological, placing the vertices with only 
##' tails first.
##' 
##' @export
complete_mg <- function (nU, nB, ord) {
  
  if (missing(ord)) ord <- seq_len(nU+nB)
  else if (length(ord) != nU+nB) stop("Length of order must be the same as the number of nodes")
  
  # Ucomp <- match(seq_len(nU), ord)
  # if (nU > 0) Bcomp <- setdiff(ord, Ucomp)
  # else {
  #   return(makeGraphComplete(nB, "bidirected"))
  # }
  
  if (nB == 0) return(makeGraphComplete(nU))
  else if (nU == 0) return(makeGraphComplete(nB, "bidirected"))

  ## get list of undirected edges
  UG <- makeGraphComplete(nU, "undirected")
  UG <- addNodes(UG, nB)   
  
  ## get list of directed edges
  DG <- makeGraphBipartite(nU,nB, "directed")

  ## get list of bidirected edges
  BG <- makeGraphComplete(nB, "bidirected")
  BG <- addNodes(BG, nU)
  BG <- BG[order(c(nU+seq_len(nB), seq_len(nU))), order=TRUE]
  
  ## create graph
  out <- mixedgraph(nU+nB, edges=list(bidirected=BG$edges$bidirected,
                                      undirected=UG$edges$undirected,
                                      directed=DG$edges$directed))
  ## now reorder, if necessary
  out <- out[order(ord), order=TRUE]
  out$vnames <- paste0("x", seq_len(nU+nB))
  
  return(out)
}

##' Test m-separation
##' 
##' Check if A is m-separated from B by C in a summary graph `graph`.
##' 
##' @param graph an object of class `mixedgraph`
##' @param A,B,C sets of vertices in `graph`
##' 
##' 
##' @export
m_sep <- function(graph, A, B, C) {
  if (!is_SG(graph)) stop("Object must be a summary graph of class 'mixedgraph'")
  if (missing(C)) C <- integer(0)
  
  ## deal with trivial cases
  if (length(A) == 0 || length(B) == 0) return(TRUE)
  if (length(intersect(A, B)) > 0) return(FALSE)
  vs <- ant(graph, c(A,B,C))
  graph2 <- withAdjMatrix(graph[vs])
  
  ## moralize, remove edges from C
  mg <- moralize(graph2, check=FALSE)
  mg <- mutilate(mg, C)
  gr <- grp(mg, v=A)

  if (any(B %in% gr)) return(FALSE)
  return(TRUE)
}
