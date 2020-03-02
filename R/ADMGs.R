# # ##' Change random vertices in CADMG
# # ##' 
# # ##' @param graph object of class mixedgraph corresponding to (C)ADMG
# # ##' @param v vertices to keep as random
# # ##' @export cadmg
# # cadmg = function(graph, v) {
# #   w = setdiff(pa(graph, v), v)
# #   vs = sort.int(c(v,w))
# # 
# #   out = withAdjMatrix(graph[vs])
# #   bis <- out$edges$bidirected
# #   dis <- out$edges$directed
# #   bis[,-v] = 0
# #   bis[-v,] = 0
# #   dis[,-v] = 0
# #   out$edges$bidirected = bis
# #   out$edges$directed = dis
# #   
# #   return(out)
# # }
# 
# ##' List Intrinsic Sets of ADMG
# ##' 
# ##' List Intrinsic Sets of an ADMG, possibly by district
# ##' 
# ##' @param graph object of class \code{mixedgraph}, must be an ADMG.
# ##' @param r logical, should recursive head definition be used? Defaults to \code{TRUE}
# ##' @param by_district logical, should intrinsic sets be grouped by district? Defaults to \code{FALSE}
# ##' @param sort should output be sorted/unique?
# ##' @param recall logical: is this a recalling of the function (internal use only)
# ##' 
# ##' @details \code{intrinsicSets} returns a list of integer vectors, 
# ##' each being an intrinsic set (or if \code{by_district = TRUE} 
# ##' a list of lists, each containing the intrinsic sets in a single 
# ##' district). If \code{r = FALSE} the intrinsic sets are the heads 
# ##' and their dis-tails.
# ##' 
# ##' \code{intrinsicClosure} returns an integer vector containing the closure of set.
# ##' 
# ##' @export intrinsicSets
# intrinsicSets <- function(graph, r = TRUE, by_district = FALSE, sort=2, recall=FALSE) {
# 
#   ## on first run, check graph is a summary graph and remove undirected part
#   if(!recall) {
#     if(!is.SG(graph)) stop("Graph appears not to be a summary graph or ADMG")
#     
#     un_g <- un(graph)
#     if (length(un_g) > 0) {
#       clq <- cliques(graph[un_g])
#       graph <- graph[-un_g]
#     }
#   }
#   if (recall || length(un_g) == 0) clq <- list()
# 
#   districts <- districts(graph)
#   out <- list()
# 
#   ## go along each district and find intrinsic sets
#   for(i in seq_along(districts)){
#     if(by_district) {
#       out[[i]] <- list()
#     }
#     dis <- districts[[i]]
#     
#     if(r) {
#       # if (length(dis) <= 1) {
#       if(by_district) {
#         out[[i]] <- c(out[[i]], list(dis))
#       } 
#       else {
#         out <- c(out, list(dis))
#       }
#       
#       if (length(dis) <= 1) next
#       # }
#       
#       dis_subgraph <- graph[dis]
#       ster <- sterile(dis_subgraph)
# 
#       for(s in ster) {
#         an_set <- setdiff(dis, s)
# 
#         an_set_subgraph <- dis_subgraph[an_set]
#         d <- districts(an_set_subgraph)
#         
#         recursed_sets <- Recall(an_set_subgraph, r, FALSE, recall=TRUE)
#         
#         ## add in new sets
#         if(by_district){
#           out[[i]] <- c(out[[i]], recursed_sets)
#           if (length(d) == 1) out[[i]] <- c(out[[i]], list(an_set))
#         } else {
#           out <- c(out, recursed_sets)
#           if (length(d) == 1) out <- c(out, list(an_set))
#         }
#       }
#     } # if (r)
#     else {
#       subs = powerSet(dis)[-1]
#       int = logical(length(subs))
# 
#       # for each subset C of a district, check if it's intrinsic
#       for (j in seq_along(subs)) {
#         # ang is G_{an C}, subgraph formed by ancestors of subs[[j]]
#         ang = graph[anc(graph, subs[[j]])]
#         # district of C in ang.
#         subdis = dis(ang, subs[[j]])
#         
#         # set C is intrinsic if it is <-> connected in G_{an C}
#         if (length(subdis) > length(subs[[j]])) int[j] = FALSE # not maximal
#         else if(!all(subs[[j]] %in% dis(ang, subs[[j]][1]))) int[j] = FALSE # not connected
#         else int[j] = TRUE # OK
#       }
#       
#       if(by_district){
#         out[[i]] <- subs[int]
#       }
#       else out = c(out, subs[int])
#     }
#   }
#   
#   ## clean up and finish
#   if(by_district){
#     out <- c(list(clq), out)
#     
#     out <- rapply(out, sort.int, how = "list")
#     out <- lapply(out, unique.default)
#     out
#   } 
#   else {
#     out <- c(clq, out)
#     
#     out <- lapply(out, sort.int)
#     unique.default(out)
#   }
# }
# 
# 
# 
