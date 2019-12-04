dag1 <- graphCr("1->4<-2->3->5<-4")
dists <- districts(dag1)
dag1a <- mixedgraph(5, edges=list(dir=adjMatrix(dag1$edges$directed,dir=TRUE)))
dag2 <- dag1[c(2,3,5)]
grv <- graphCr("1->2->3->4<->2")
grv2 <- graphCr("z -> x -> y <- u -> x")

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
