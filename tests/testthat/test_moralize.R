grV <- standardizeEdges(grVerma)
grVm <- standardizeEdges(moralize(grVerma))
grVma <- standardizeEdges(makeGraphComplete(4))
grVm2 <- standardizeEdges(moralize(grVerma, 2))
grVm2a <- standardizeEdges(graphCr("4 <- 1 -- 2 --> 3 --> 4 <- 2"))
grVm3 <- standardizeEdges(moralize(grVerma, 3))
grVm3a <- standardizeEdges(graphCr("4 <- 1 -- 2 -- 3 --> 4 <- 2"))

## example graph
gr3h <- structure(list(v = 1:6, edges = structure(list(bidirected = structure(list(
  2:4, c(1L, 3L, 5L), c(2L, 1L, 6L), 1L, 2L, 3L), class = "adjList"), 
  directed = structure(list(5L, 6L, 4L, integer(0), integer(0), 
                            integer(0)), class = "adjList")), class = "edgeList"), 
  vnames = c("x1", "x2", "x3", "x4", "x5", "x6")), class = "mixedgraph")

test_that("moralization works for ADMGs", {
  expect_equal(standardizeEdges(moralize(gr2)), 
               standardizeEdges(makeGraphComplete(5, "undirected")))
})

test_that("moralization works for Verma graph", {
  expect_equal(grVm, grVma)
  expect_equal(grVm2, grVm2a)
  expect_equal(grVm3, grVm3a)
  expect_true(m_sep(moralize(gr3h, c(1,2,6)), 1, 6, 2))
})


# mg1 <- graphCr("1 -> 4 <- 2 -> 3 -> 5 <- 4 -- 1 -- 3")
# mg1a_o <- standardizeEdges(graphCr("1 -> 4 <- 2 -> 3 -> 5, 4 -- 1 -- 3"))
# mg1a <- standardizeEdges(graphCr("2 -> 3 -> 5 <- 4 -- 1 -- 3"))
# mg2 <- standardizeEdges(graphCr("1 -> 4 <- 2 -> 3 -> 5 <- 4 -- 1 -- 3 -- 5"))
# mg3 <- standardizeEdges(graphCr("1 -> 4 <- 2 -> 3 -> 5 <- 4 -- 1 -- 3 -- 4"))

