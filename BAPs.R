library(MixedGraphs)
library(e1071)
library(purrr)

rmv_redundant = function(x) {
  n = nrow(x[[1]])
  perms = permutations(n)
  
  ## include some checks here
  keep = c(TRUE, rep(FALSE, n-1))
  degs = sapply(x, function(y) sum(y)/2)
  
  ## go through each one
  for (i in seq_along(x)[-1]) {
    keep[i] = TRUE
  
    for (j in setdiff(which(keep), i)) {
      if (degs[j] != degs[i]) next
      for (p in seq_len(nrow(perms))) {
        targ = x[[j]][perms[p,],perms[p,]]
        
        if (sum(abs(x[[i]] - targ)) == 0) {
          keep[i] = FALSE
          break
        }
        ## if we get to the end, this graph is unique
      }
      if (!keep[i]) break
    }
  }
  x[keep]
}

## List undirected skeletons of up to size n
## up to permutations
getSkeletons = function(n, max_deg=n-1) {
  out = list(matrix(0,n,n))
  
  for (i in seq_len(min(max_deg,n-1))) {
    ## get graphs for vertex 1 has degree i
    ## assuming degrees non-increasing
    nbrs = combn(n-1,i,simplify=FALSE)
    subgs = Recall(n-1,i)
    tmp = mapply(function(x, M) {
      n = nrow(M)+1
      M2 = matrix(0, n, n)
      M2[-1,-1] = M
      M2[1,x+1] = M2[x+1,1] = 1
      M2
    }, nbrs, subgs[rep(seq_along(subgs), each=length(nbrs))], SIMPLIFY = FALSE)
    tmp = rmv_redundant(tmp)
    out = c(out, tmp)
    #if (n==3 && i==2) stop()
  }
  out = rmv_redundant(out)
  out
}

getADMGs = function(skel, bows=FALSE) {
  if (bows) stop("We don't do bows")
  rcs = cbind(row(skel)[upper.tri(skel) & skel==1], col(skel)[upper.tri(skel) & skel==1])
  p <- nrow(rcs)
  n <- nrow(skel)
  
  subs <- combinations(rep(3,p))
  out = list()
  for (i in seq_len(nrow(subs))) {
    dir = t(rbind(rcs[subs[i,]==0,], rcs[subs[i,]==1,2:1]))
    bid = t(rcs[subs[i,]==2,,drop=FALSE])
    ed = list(directed=dir, bidirected=bid)
    gr = mixedgraph(n, edges = ed)
    if (is.cyclic(gr)) next
    out = c(out, list(gr))
  }
  out
}

arrows_in <- function(graph, v) {
  sort.int(union(MixedGraphs::pa(graph, v), MixedGraphs::sp(graph, v)))
}

##' Test for equivalence by Chris's criterion
collider_equiv = function(g1, g2) {
  if (!MixedGraphs:::graph_equal(skeleton(g1),skeleton(g2))) return(FALSE)

  k = length(g1$v)  
  for (i in seq_len(k)) {
    a1 = arrows_in(g1,i)
    a2 = arrows_in(g2,i)
    if (length(a1) > 1 || length(a2) > 1) {
      if (!isTRUE(all.equal(a1,a2))) return(FALSE)
    }
  }
  return(TRUE)  
}

is_vstructure <- function(graph, vs) {
  if (missing(vs)) vs = graph$v
  if (length(vs) != 3) stop("Supply three vertices")
  graph = MixedGraphs:::withEdgeList(graph[vs])
  
  ## check skeleton matches
  skel = skeleton(graph)
  
  if (nedge(skel) != 2) return(FALSE)

  ## edge structure correct, now work out middle 
  tmp = (graph$v %>% 
    map_int(~ length(adj(graph, .))))
  mid = graph$v[which(tmp == 2)]
  
  ## parents and spouses:
  ps <- c(mid,adj(graph, v=mid, etype = c("directed", "bidirected"), dir = c(-1,0)))
  if (!setequal(ps, vs)) return(FALSE)
  oth <- adj(graph, v=mid, etype = c("directed", "undirected"), dir = c(1,0))
  if (length(oth) > 0) return(FALSE)
  
  return(TRUE)
}

same_SEM_model <- function(g1, g2) {
  v <- g1$v
  if (any(v != g2$v)) return(FALSE)
  n <- length(v)
  if (n < 2) return(TRUE)
  
  ## check skeletons are the same
  s1 <- MixedGraphs:::withAdjMatrix(skeleton(g1))
  s2 <- MixedGraphs:::withAdjMatrix(skeleton(g2))

  if (collider_equiv(g1, g2)) return(TRUE)
  
  ## if skeletons different return FALSE, otherwise if 
  ## all edges present or only two vertices return TRUE
  if (any(s1$edges$undirected != s2$edges$undirected)) return(FALSE)
  else if (n < 3) return(TRUE)
  else if (all(s1$edges$undirected[upper.tri(s1$edges$undirected)] == 1)) return(TRUE)
  
  cs <- combn(v, 3, simplify = FALSE)
  
  ## check v-structures are the same
  for (i in seq_along(cs)) {
    tmp1 <- g1[cs[[i]]]
    tmp2 <- g2[cs[[i]]]
    if (is_vstructure(tmp1) != is_vstructure(tmp2)) return(FALSE)
  }
  
  ## check m-separations are the same: if not models
  ## are different; if so and if there is one m-separation 
  ## for every missing edge then models are the same.
  m1 <- list_m_seps(g1, first_only = TRUE)
  m2 <- list_m_seps(g2, first_only = TRUE)
  if (!isTRUE(all.equal(m1, m2))) return(FALSE)
  else if (length(m1) == choose(n, 2) - nedge(s1)) return(TRUE)
  
  return("ambiguous")
}

same_SEM_model0 <- function(g1, g2) {
  v <- g1$v
  if (any(v != g2$v)) return(FALSE)
  n <- length(v)
  ## assume skeletons and v-structures the same
  if (n < 4) return(TRUE)
  
  if (collider_equiv(g1, g2)) return(TRUE)
  
  ## if skeletons different return FALSE, otherwise if 
  ## all edges present or only two vertices return TRUE
  if (nedge(g1) == choose(n,2)) return(TRUE)
  

  ## check m-separations are the same: if not models
  ## are different; if so and if there is one m-separation 
  ## for every missing edge then models are the same.
  m1 <- list_m_seps(g1, first_only = TRUE)
  m2 <- list_m_seps(g2, first_only = TRUE)
  if (!isTRUE(all.equal(m1, m2))) return(FALSE)
  else if (length(m1) == choose(n, 2) - nedge(s1)) return(TRUE)
  
  return("ambiguous")
}

list_m_seps <- function(graph, first_only = FALSE) {
  require(ggm)
  gr <- convert(graph, format="ggm")
  n <- nrow(gr)
  vnms <- rownames(gr)
  out = list()
  
  ## go through each pair of vertices
  for (i in seq_len(n)[-1]) {
    si <- seq_len(i-1)
    not_adj <- which(gr[si,i] == 0 & gr[i,si] == 0)
    
    for (j in not_adj) {
      C = seq_len(n)[-c(i,j)]
      Cs = powerSet(C)
      ## check whether each subset separates them
      for (c in seq_along(Cs)) {
        if (msep(gr, vnms[i], vnms[j], vnms[Cs[[c]]])) {
          out = c(out, list(list(j,i,Cs[[c]])))
          if (first_only) break
        }
      }
    }
  }
  
  out
}

list_v_structs <- function(graph) {
  v <- graph$v
  n <- length(v)
  if (n < 2) return(list())
  out <- list()
  
  cs <- combn(v, 3, simplify = FALSE)
  
  ## check v-structures are the same
  for (i in seq_along(cs)) {
    tmp <- graph[cs[[i]]]
    if (is_vstructure(tmp)) {
      out = c(out, list(as.integer(cs[[i]])))
    }
  }
  out
}

## Given a list of graphs, remove
## any that are permutations of others
rmv_perms <- function(x) {
  require(purrr)
  ## ggm matrix format is convenient for this
  require(ggm)
  grs <- lapply(x, function(y) convert(y, "ggm"))
  
  sum_stat1 <- map_dbl(grs, sum)
  nv <- map_int(grs, nrow)
  
  rmv <- rep(FALSE, length(grs))
  for (i in seq_along(grs)[-1]) {
    if (i == 2 || nv[i] != nv[i-1]) perms <- permutations(nv[i])
  
    for (j in seq_len(i-1)) {
      if (rmv[j] || 
          nv[i] != nv[j] ||
          sum_stat1[i] != sum_stat1[j]) next
      
      ## go through permutations
      for (p in seq_len(nrow(perms))) {
        prm = perms[p,]
        if (all(grs[[i]][prm, prm] == grs[[j]])) {
          rmv[i] = TRUE
          break
        }
      }
      if (rmv[i]) break
    }
    printPercentage(i, length(x))
  }
  
  rmv
}


k = 4
skel <- getSkeletons(k)
wh_skel = 10
graphs <- getADMGs(skel[[wh_skel]])
nedge_skeleton <- nedge(graphs[[1]])
#graphs <- graphs[!rmv_perms(graphs)]
graphs[[1]]
tmp = sapply(graphs, function(x) length(x$edges$bidirected))
graphs = graphs[tmp > 0]
length(graphs)

keep = logical(length(graphs))
mseps = character(length(graphs))

## Just look for graphs which are deficient in a CI
for (i in seq_along(graphs)) {
  tmp = list_m_seps(graphs[[i]], first_only = TRUE)
  keep[i] = (length(tmp) < choose(k,2)-nedge_skeleton)
  tmp = c(tmp, list_v_structs(graphs[[i]]))
  mseps[i] = digest(tmp)
  
  printPercentage(i,length(graphs))
}

graphs = graphs[keep]
mseps = mseps[keep]

wh = seq_along(graphs)
Rprof()
for (i in seq_along(graphs)[-1]) {
  js <- setdiff(which(mseps == mseps[i]), i)
  for (j in js) {
    # if (mseps[i] != mseps[j]) next
    if (same_SEM_model(graphs[[i]], graphs[[j]]) == "ambiguous") {
      # dput(list(graphs[[i]], graphs[[j]]), file = paste("~/graphs",wh_skel,".out",sep=""))
      print(c(i,j))
    }
    
    # if (wh[j] < j) next
    # if (isTRUE(same_SEM_model(graphs[[i]], graphs[[j]]))) {
    #   wh[i] = j
    #   break
    # }
  
    # printPercentage(i^2+2*j,length(graphs)^2, dp = 1, prev=i^2+2*(j-1))
  }
}
Rprof(NULL)
summaryRprof()

####
# 
# graphs = graphs[unique.default(wh)]
# 
# k = length(graphs)
# tmp = matrix(0,k,k)
# 
# for (i in seq_along(graphs)[-1]) {
#   for (j in seq_len(i-1)) {
#     if ((same_SEM_model(graphs[[i]], graphs[[j]])=="ambiguous")) {
#       tmp[i,j] = tmp[j,i] = 1
#     }
#   }
#   printPercentage(i,length(graphs))
# }
# 
# any(tmp > 0)
# 
# is_vstructure(grs[[8]])
# 
# same_SEM_model(grs[[8]], grs[[9]])
# 
# tmp = graphs %>% map_int(~ length(list_m_seps(.)))
# 
# i = 3
# grs <- getSkeletons(3) %>% 
#   `[[`(i) %>% 
#   getADMGs()
# 
# sort_vs <- function(x) {
#   
# }
# 
# g1 = graphCr("1 <- 2 -> 3 -> 4 <- 2")
# g2 = graphCr("1 <- 2 -> 3 -> 4 <-> 2")
# collider_equiv(g1,g2)
# 
# 
# getSkeletons(4)
# 
# permute.vertices()
# 
# ## skeletons
# tmp <- graphCr("1 -- 2 -- 3 -- 4", format="igraph") %G% 
#   permute.vertices(c(2,1,3,4)) %G%
#   convert 
# tmp$edges$undirected
# 
# 
# Rprof()
# x = lapply(graphs[1:1000], function(y) convert(y, "ggm"))
# Rprof(NULL)
# summaryRprof()
