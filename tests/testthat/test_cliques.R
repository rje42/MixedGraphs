set.seed(123)
n <- 10
u10 <- makeGraphComplete(n)
u10 <- removeEdges(u10, list(un=combn(n,2, simplify = FALSE)[sample(choose(n,2), n)]))
u10 <- withEdgeList(u10)

ui10 <- convert(u10, "igraph")

test_that("cliques of up to size 2 are correct", {
  expect_true(setequal(cliques(u10, max_len=2), u10$edges$undirected))
  expect_true(setequal(cliques(u10, max_len=8), cliques(u10)))
})

test_that("barrenSets are correct", {
  expect_true(setequal(barrenSets(gr1), 
                       list(c(1L,2L), c(1L,4L), 
                            c(2L,3L), c(3L,4L))))
})
