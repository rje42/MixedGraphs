dag1 <- graphCr("1 -> 4 <- 2 -> 3 -> 5 <- 4")
dists <- districts(dag1)
dag1a <- mixedgraph(5, edges=list(dir=adjMatrix(dag1$edges$directed,dir=TRUE)))
dag2 <- dag1[c(2,3,5)]
grv <- graphCr("1->2->3->4<->2", mode="adjList")
grv_a <- graphCr("1->2->3->4<->2", mode="adjMatrix")
grv_b <- graphCr("1 -> 2 -> 3 -> 4 <-> 2", mode="edgeMatrix")
grv_c <- graphCr("1 -> 2 -> 3 -> 4 <-> 2", mode="eList")
grv2 <- graphCr("z -> x -> y <- u -> x")

mag1 <- graphCr("1 -> 3 <-> 2 <-> 4 <- 1")
mag1a <- graphCr("1 -> 3 <-> 2 <-> 4 <- 1", mode = "adjMatrix")
mag1b <- graphCr("1 -> 3 <-> 2 <-> 4 <- 1", mode = "edgeMatrix")
mag1c <- graphCr("1 -> 3 <-> 2 <-> 4 <- 1", mode = "eList")

grv_a0 <- withAdjList(grv_a)
grv_b0 <- withAdjList(grv_b, "bidirected")
grv_c0 <- withAdjList(grv_c)

testthat::test_that("withAdjList works sensibly", {
  expect_equal(standardizeEdges(grv), standardizeEdges(grv_a0))
  expect_equal(standardizeEdges(grv), standardizeEdges(grv_b0))
  expect_equal(standardizeEdges(grv), standardizeEdges(grv_c0))
})
