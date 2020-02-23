# ##' Change random vertices in CADMG
# ##' 
# ##' @param graph object of class mixedgraph corresponding to (C)ADMG
# ##' @param v vertices to keep as random
# ##' @export cadmg
# cadmg = function(graph, v) {
#   w = setdiff(pa(graph, v), v)
#   vs = sort.int(c(v,w))
# 
#   out = withAdjMatrix(graph[vs])
#   bis <- out$edges$bidirected
#   dis <- out$edges$directed
#   bis[,-v] = 0
#   bis[-v,] = 0
#   dis[,-v] = 0
#   out$edges$bidirected = bis
#   out$edges$directed = dis
#   
#   return(out)
# }

##' List Intrinsic Sets of ADMG
##' 
##' List Intrinsic Sets of an ADMG, possibly by district
##' 
##' @param graph object of class \code{mixedgraph}, must be an ADMG.
##' @param r logical, should recursive head definition be used? Defaults to \code{TRUE}
##' @param by_district logical, should intrinsic sets be grouped by district? Defaults to \code{FALSE}
##' @param sort should output be sorted/unique?
##' @param recall logical: is this a recalling of the function (internal use only)
##' 
##' @details \code{intrinsicSets} returns a list of integer vectors, 
##' each being an intrinsic set (or if \code{by_district = TRUE} 
##' a list of lists, each containing the intrinsic sets in a single 
##' district). If \code{r = FALSE} the intrinsic sets are the heads 
##' and their dis-tails.
##' 
##' \code{intrinsicClosure} returns an integer vector containing the closure of set.
##' 
##' @export intrinsicSets
intrinsicSets <- function(graph, r = TRUE, by_district = FALSE, sort=2, recall=FALSE) {

  ## on first run, check graph is a summary graph and remove undirected part
  if(!recall) {
    if(!is.SG(graph)) stop("Graph appears not to be a summary graph or ADMG")
    
    un_g <- un(graph)
    if (length(un_g) > 0) {
      clq <- cliques(graph[un_g])
      graph <- graph[-un_g]
    }
  }
  if (recall || length(un_g) == 0) clq <- list()

  districts <- districts(graph)
  out <- list()

  ## go along each district and find intrinsic sets
  for(i in seq_along(districts)){
    if(by_district) {
      out[[i]] <- list()
    }
    dis <- districts[[i]]
    
    if(r) {
      # if (length(dis) <= 1) {
      if(by_district) {
        out[[i]] <- c(out[[i]], list(dis))
      } 
      else {
        out <- c(out, list(dis))
      }
      
      if (length(dis) <= 1) next
      # }
      
      dis_subgraph <- graph[dis]
      ster <- sterile(dis_subgraph)

      for(s in ster) {
        an_set <- setdiff(dis, s)

        an_set_subgraph <- dis_subgraph[an_set]
        d <- districts(an_set_subgraph)
        
        recursed_sets <- Recall(an_set_subgraph, r, FALSE, recall=TRUE)
        
        ## add in new sets
        if(by_district){
          out[[i]] <- c(out[[i]], recursed_sets)
          if (length(d) == 1) out[[i]] <- c(out[[i]], list(an_set))
        } else {
          out <- c(out, recursed_sets)
          if (length(d) == 1) out <- c(out, list(an_set))
        }
      }
    } # if (r)
    else {
      subs = powerSet(dis)[-1]
      int = logical(length(subs))

      # for each subset C of a district, check if it's intrinsic
      for (j in seq_along(subs)) {
        # ang is G_{an C}, subgraph formed by ancestors of subs[[j]]
        ang = graph[anc(graph, subs[[j]])]
        # district of C in ang.
        subdis = dis(ang, subs[[j]])
        
        # set C is intrinsic if it is <-> connected in G_{an C}
        if (length(subdis) > length(subs[[j]])) int[j] = FALSE # not maximal
        else if(!all(subs[[j]] %in% dis(ang, subs[[j]][1]))) int[j] = FALSE # not connected
        else int[j] = TRUE # OK
      }
      
      if(by_district){
        out[[i]] <- subs[int]
      }
      else out = c(out, subs[int])
    }
  }
  
  ## clean up and finish
  if(by_district){
    out <- c(list(clq), out)
    
    out <- rapply(out, sort.int, how = "list")
    out <- lapply(out, unique.default)
    out
  } 
  else {
    out <- c(clq, out)
    
    out <- lapply(out, sort.int)
    unique.default(out)
  }
}

#' @param maxbarren Maximum number of barren nodes (i.e. head size) to consider
#' @describeIn intrinsicSets Alternative method for non-recursive heads only
#' @export intrinsicSets2
intrinsicSets2 <- function(graph, r = TRUE, by_district = FALSE, maxbarren, sort=1) {
  districts <- districts(graph)
  
  if (missing(maxbarren)) maxbarren = max(graph$v)
  out <- list()
  
  if(!is.ADMG(graph)) stop("Graph appears not to be an ADMG") # could extend to MEGs
  
  subs <- anSets2(graph, maxbarren = maxbarren, same_dist = TRUE)
  if (by_district) d <- sapply(subs, function(x) subsetmatch(x[1], districts))
  
  if(r) {
    # stop("function does not work for recursive heads")

    out <- lapply(graph$v, function(set) intrinsicClosure(graph, set, r=TRUE))
    n <- length(out)
    vnam <- sapply(out, paste, collapse="")
    liv <- rep(TRUE, n)
    liv[lengths(liv) >= maxbarren] <- FALSE

    bid <- matrix(0, n, n)
    for (i in seq_len(n)[-1]) for (j in seq_len(i-1)) bid[i,j] <- bid[j,i] <- 1*any(sib(graph, out[[i]]) %in% out[[j]])
    sub <- matrix(0, n, n)
    for (i in seq_len(n)[-1]) for (j in seq_len(i-1)) sub[j,i] <- 1*is.subset(out[[j]],out[[i]])

    edges <- list(directed=sub, bidirected=bid)

    graph2 <- mixedgraph(v=seq_len(n), edges=edges, vnames=vnam)
    new <- any(bidi > 0)

    while (new) {
      new <- FALSE
      bidi <- which(graph2$edges$bidirected > 0 & upper.tri(graph2$edges$bidirected))
      for (i in seq_len(nrow(bidi))) {
        if (liv[bidi[i,1]] && liv[bidi[i,2]]) {
          out[[n+1]] <- intrinsicClosure(graph, c(out[[bidi[i,1]]], out[[bidi[i,2]]]))
          graph2 <- addNodes(graph2, 1, vnames=paste(out[[n+1]], collapse=""))
          n <- n+1

          for (j in seq_len(n-1)) {
            graph2$edges$directed[j,n] <- 1*is.subset(out[[j]],out[[n]])
            graph2$edges$directed[n,j] <- 1*is.subset(out[[n]],out[[j]])
          }

          new <- TRUE
        }
        
        if (any(liv[dec(graph2, out[[bidi[i,1]]])]) && 
            any(liv[dec(graph2, out[[bidi[i,2]]])])) {
          
          for (j in dec(graph2, out[[bidi[i,1]]])) for (k in dec(graph2, out[[bidi[i,2]]])) {

            
            if (j == 1 && k == 1) next
            
            out[[n+1]] <- intrinsicClosure(graph, c(out[[j]], out[[k]]))
            graph2 <- addNodes(graph2, 1, vnames=paste(out[[n+1]], collapse=""))
            n <- n+1
            liv[n] <- TRUE
            
            for (j in seq_len(n-1)) {
              graph2$edges$directed[j,n] <- 1*is.subset(out[[j]], out[[n]])
              graph2$edges$directed[n,j] <- 1*is.subset(out[[n]], out[[j]])
            }
            
            new <- TRUE
            
          }
        }
        
        graph2 <- removeEdges(graph2, list(bidirected=list(bidi[i,])))
      }
    }
          
    stop("function does not work for recursive heads")
    
    ### insert work here
    
  } # if (r)
  else {
    # subs = powerSet(dis, maxbarren)[-1]
    int = logical(length(subs))
    
    # FOR EACH SUBSET C OF A DISTRICT, CHECK IF IT'S 'INTRINSIC'
    for (j in seq_along(subs)) {
      # ang IS G_{an C}, SUBGRAPH FORMED BY ANCESTORS OF subs[[j]]
      bar <- barren(graph, subs[[j]])
      dist <- dis(graph[subs[[j]]], bar[1])
      dist2 <- dis(graph[subs[[j]]], bar)
      if (setequal(dist2, dist)) {
        out <- c(out, list(dist))
        int[j] = TRUE
      }
      else int[j] = FALSE
      
      # 
      # ang = graph[anc(graph, subs[[j]])]
      # # DISTRICT OF C in ang.
      # subdis = dis(ang, subs[[j]])
      # 
      # # SET C IS INTRINSIC IF IS MAXIMALLY <-> CONNECTED IN G_{an C}
      # if (length(subdis) > length(subs[[j]])) int[j] = FALSE # NOT MAXIMAL
      # else if(!all(subs[[j]] %in% dis(ang, subs[[j]][1]))) int[j] = FALSE # NOT CONNECTED
      # else int[j] = TRUE # OK
    }
  }
  
  # out <- subs[int]
  
  if(by_district){
    out <- tapply(out, INDEX = d[int], FUN=list)
    names(out) <- NULL
    
    if (sort > 1) out <- rapply(out, sort.int, how="list")
  } 
  else if (sort > 1) {
    out <- lapply(out, sort.int)
  }

  out
}

##' @describeIn intrinsicSets Get the intrinsic closure of a set
##' @param set set to find intrinsic closure of
##' @export intrinsicClosure
intrinsicClosure <- function(graph, set, r=TRUE, sort=1) {
  
  if (length(set) == 0) return(integer(0))
  
  subgraph <- graph
  new = TRUE
  ans <- graph$v
  
  if (!r) {
    ancs <- anc(graph, set)
    S <- dis(graph[ancs], set, sort=sort)
    if (length(districts(graph[S])) <= 1) return(S)
    else stop("Not contained in a single intrinsic set")
  }

  while (new) {
    new = FALSE
    dists <- districts(subgraph)
    
    wh <- subsetmatch(list(set[1]), dists)
    if (all(set %in% dists[[wh]])) {
      subgraph <- subgraph[dists[[wh]]]
    }
    else stop("Not contained in a single intrinsic set")
    
    ans <- anc(subgraph, set)
      
    if (setequal(ans, subgraph$v)) break
    else {
      subgraph <- subgraph[ans]
      new = TRUE
    }
  }
  
  return(subgraph$v)
  # stop("Error in intrinsicClosure - perhaps 'set' not contained in a single district?")
}


##' Get list of heads and tails
##' 
##' @param graph a \code{mixedgraph} object
##' @param r logical, should recursive heads be used?
##' @param by_district logical, should these be grouped by district?
##' @param set_list list of intrinsic sets
##' @param max_head largest head size to consider (\code{r=FALSE} only)
##' 
##' @details Returns a list of heads and their corresponding tails.
##' 
##' @export headsTails
headsTails <- function (graph, r = TRUE, by_district = FALSE, set_list, max_head) 
{
  if(missing(set_list)) {
    if (missing(max_head)) set_list <- intrinsicSets(graph, r = r, by_district = by_district)
    else {
      if (r) stop("Function does not support maximizing the size of recursive heads")
      set_list <- intrinsicSets2(graph, r = r, by_district = by_district, maxbarren = max_head)
    }
  }
  
  if (by_district) {
    head.list = tail.list = list()
    for (i in seq_along(set_list)) {
      if (r) {
        tail.list[[i]] = lapply(set_list[[i]], function(v) pa(graph, v))
        head.list[[i]] <- mapply(setdiff, set_list[[i]], tail.list[[i]], SIMPLIFY=FALSE)
      }
      else {
        head.list[[i]] = lapply(set_list[[i]], function(v) barren(graph, v))
        tail.list[[i]] = list()
        for (j in seq_along(head.list[[i]]))  tail.list[[i]][[j]] = setdiff(union(set_list[[i]][[j]], pa(graph, set_list[[i]][[j]])), head.list[[i]][[j]])
      }
    }
  }
  else {
    if (r) {
      tail.list = lapply(set_list, function(v) pa(graph, v))
      head.list <- mapply(setdiff, set_list, tail.list, SIMPLIFY=FALSE)
    }
    else {
      head.list = lapply(set_list, function(v) barren(graph, v))
      tail.list = list()
      for (j in seq_along(head.list))  tail.list[[j]] = setdiff(union(set_list[[j]], pa(graph, set_list[[j]])), head.list[[j]])
    }
  }
  out <- list(heads = head.list, tails = tail.list)
  out
}

##' Gives partition/factorization
##' 
##' Uses order for speed
##' returns integer value of heads from provided list
##' 
##' @param graph object of class \code{mixedgraph}
##' @param heads list of heads 
##' @param v set of vertices to partition
##' @param r logical: should recursive parameterization be used?
##' @param head.order numeric vector in same order as heads
##' 
##' @export partition0
partition0 = function(graph, heads, v = seq_len(graph$n), r=TRUE, head.order) {
  
  wh = rep.int(TRUE, length(heads))
  out = numeric(0)
  
  while (length(v) > 0) {
    wh2 = fsapply(heads[wh], function(x) is.subset(x,v))
    wh[wh] = wh[wh] & wh2
    maxval = max(head.order[wh])
    new = which(wh & head.order==maxval)
    out = c(out, new)
    v = setdiff(v, unlist(heads[new]))
  }
  
  return(out)
}

##' @describeIn partition0 Give factorization of heads and tails
##' @param ht list of heads and tails
##' @export factorize0
factorize0 = function (graph, v = seq_len(n), r = TRUE, ht, head.order) {
  n = graph$n
  if (length(v) == 0) {
    return(numeric(0))
  }
  else {
    out <- partition0(graph, ht$heads, v, r = r, head.order=head.order)
  }
  
  return(out)
}
