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
intrinsicSets <- function(graph, r = TRUE, by_district = FALSE, sort=1) {
  print(graph)
  out <- list()
  districts <- districts(graph)
  
  if(!is.ADMG(graph)) stop("Graph appears not to be an ADMG") # could extend to MEGs
  
  for(i in seq_along(districts)){
    if(by_district) {
      out[[i]] <- list()
    }
    dis <- districts[[i]]
    
    if(r) {
      dis_subgraph <- graph[dis]
      an_sets <- anSets(dis_subgraph)

      for(an_set in an_sets){
        an_set_subgraph <- dis_subgraph[an_set]
        d <- districts(an_set_subgraph)
        
        if(length(d) == 1){
          # ans <- mapV(an_set, dis_subgraph, graph)
          if(by_district) {
            out[[i]] <- c(out[[i]], list(an_set))
          } 
          else {
            out <- c(out, list(an_set))
          }
        }
        
        if(length(d) > 1){
          recursed_sets <- Recall(an_set_subgraph, r, FALSE)

          if(by_district){
            out[[i]] <- c(out[[i]], recursed_sets)
          } else {
            out <- c(out, recursed_sets)
          }
        }
      }
    } # if (r)
    else {
      subs = powerSet(dis)[-1]
      int = logical(length(subs))
      
      # FOR EACH SUBSET C OF A DISTRICT, CHECK IF IT'S 'INTRINSIC'
      for (j in seq_along(subs)) {
        # ang IS G_{an C}, SUBGRAPH FORMED BY ANCESTORS OF subs[[j]]
        ang = graph[anc(graph, subs[[j]])]
        # DISTRICT OF C in ang.
        subdis = dis(ang, subs[[j]])
        
        # SET C IS INTRINSIC IF IS MAXIMALLY <-> CONNECTED IN G_{an C}
        if (length(subdis) > length(subs[[j]])) int[j] = FALSE # NOT MAXIMAL
        else if(!all(subs[[j]] %in% dis(ang, subs[[j]][1]))) int[j] = FALSE # NOT CONNECTED
        else int[j] = TRUE # OK
      }
      
      if(by_district){
        out[[i]] <- subs[int]
      }
      else out = c(out, subs[int])
    }
    
  }
  
  if(by_district){
    out <- lapply(out, unique.default)
    out
  } 
  else {
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
    stop("function does not work for recursive heads")
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
  else if (sort > 1) out <- lapply(out, sort.int)

  out
}

##' @describeIn intrinsicSets Get the intrinsic closure of a set
##' @param set set to find intrinsic closure of
##' @export intrinsicClosure
intrinsicClosure <- function(graph, set, r=TRUE) {
  
  districts <- districts(graph)
  
  for (district in districts) {
    if (all(set %in% district)) {
      subgraph <- graph[district]
      ans <- anc(subgraph, set)
      
      if (setequal(ans, district)) return(district)
      else {
        an_subgraph <- subgraph[ans]
        closure <- Recall(an_subgraph, set)
        return(closure)
      }
    }
  }
  stop("Error in intrinsicClosure")
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
