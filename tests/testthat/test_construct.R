dag1 <- graphCr("1 -> 4 <- 2 -> 3 -> 5 <- 4")
dists <- districts(dag1)
dag1a <- mixedgraph(5, edges=list(dir=adjMatrix(dag1$edges$directed,dir=TRUE)))
dag2 <- dag1[c(2,3,5)]
grv <- graphCr("1->2->3->4<->2", mode="adjList")
grv_a <- graphCr("1->2->3->4<->2", mode="adjMatrix")
grv_b <- graphCr("1 -> 2 -> 3 -> 4 <-> 2", mode="edgeMatrix")
grv_c <- graphCr("1 -> 2 -> 3 -> 4 <-> 2", mode="eList")
grv2 <- graphCr("z -> x -> y <- u -> x")
gr0 <- graphCr("")
gr0a <- graphCr("   ")

mag1 <- graphCr("1 -> 3 <-> 2 <-> 4 <- 1")
mag1a <- graphCr("1 -> 3 <-> 2 <-> 4 <- 1", mode = "adjMatrix")
mag1b <- graphCr("1 -> 3 <-> 2 <-> 4 <- 1", mode = "edgeMatrix")
mag1c <- graphCr("1 -> 3 <-> 2 <-> 4 <- 1", mode = "eList")

test_that("empty graphs can be constructed", {
  expect_equal(gr0, gr0a)
})

test_that("DAG operations correct for list of edges", {
  expect_equal(sort(unique(pa(dag1, 4, sort=0))), c(1,2))
  expect_equal(sort(pa(dag1, 4, sort=1)), c(1,2))
  expect_equal(pa(dag1, 4, sort=2), c(1,2))
  expect_equal(pa(dag1, 1), integer(0))  

  expect_equal(ch(dag1, 2, sort=2), c(3,4))
  expect_equal(dec(dag1, 2, sort=2), c(2,3,4,5))

  expect_equal(length(dists), 5)
})

test_that("DAG operations correct for adjMatrix", {
  expect_equal(sort(unique(pa(dag1a, 4, sort=0))), c(1,2))
  expect_equal(sort(pa(dag1a, 4, sort=1)), c(1,2))
  expect_equal(pa(dag1a, 4, sort=2), c(1,2))
  expect_equal(pa(dag1a, 1), integer(0))  
  
  expect_equal(ch(dag1a, 2, sort=2), c(3,4))
  expect_equal(dec(dag1a, 2, sort=2), c(2,3,4,5))
})

test_that("MAG operations correct for list of edges", {
  expect_equal(sort(unique(sp(mag1, 2, sort=0))), c(3,4))
  expect_equal(sort(unique(sp(mag1a, 2, sort=0))), c(3,4))
  expect_equal(sort(unique(sp(mag1b, 2, sort=0))), c(3,4))
  expect_equal(sort(unique(sp(mag1c, 2, sort=0))), c(3,4))
  
  expect_equal(pa(mag1, 4, sort=2), c(1))
  expect_equal(pa(mag1a, 4, sort=2), c(1))
  expect_equal(pa(mag1b, 4, sort=2), c(1))
  expect_equal(pa(mag1c, 4, sort=2), c(1))
  
  expect_equal(dec(mag1, 1, sort=2), c(1,3,4))
  expect_equal(dec(mag1a, 1, sort=2), c(1,3,4))
  expect_equal(dec(mag1b, 1, sort=2), c(1,3,4))
  expect_equal(dec(mag1c, 1, sort=2), c(1,3,4))
})

dVerm <- districts(grv)

test_that("Basic operations on ADMG work", {
  expect_equal(sort.int(sapply(dVerm, sum)), c(1L, 3L, 6L))  # this is dangerous
  expect_equal(anc(grv, 3, sort=2), c(1L, 2L, 3L))  # this is dangerous  
})

test_that("subgraphs OK", {
  expect_equal(dag2$v, c(2,3,5))  
  expect_equal(dag1[c(2,3,5)], dag1[c(FALSE, TRUE, TRUE, FALSE, TRUE)])
  expect_equal(anc(dag2, 5, sort=2), c(2,3,5))
  
  # should throw an error
  expect_error(anc(dag2, 4, sort=2))
})

# test_that("subgraphs OK with different order", {
#   expect_equal(dag1[c(3,2,5)][c(2,3,5)], dag1[c(2,3,5)])
# })
