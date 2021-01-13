mg1 <- graphCr("1 -> 4 <- 2 -> 3 -> 5 <- 4 -- 1 -- 3")
mg1a <- graphCr("2 -> 3 -> 5 <- 4 -- 1 -- 3")
mg2 <- graphCr("1 -> 4 <- 2 -> 3 -> 5 <- 4 -- 1 -- 3 -- 5")
mg3 <- graphCr("1 -> 4 <- 2 -> 3 -> 5 <- 4 -- 1 -- 3 -- 4")

test_that("mutilate for incoming edges works", {
  expect_equal(mutilate(mg1, 4, "directed", dir=-1), mg1a)
})

test_that("addEdges works", {
  expect_equivalent(standardizeEdges(addEdges(mg1, undirected=eList(c(3,5)))), 
                    standardizeEdges(mg2))
  expect_equivalent(standardizeEdges(addEdges(mg1, makeEdgeList(undirected=eList(c(3,5))))), 
                    standardizeEdges(mg2))
  expect_equivalent(standardizeEdges(addEdges(mg1, undirected=eList(c(3,4)))), 
                    standardizeEdges(mg3))
})

test_that("removeEdges works", {
  mg3 <- removeEdges(mg2, un=eList(c(5,3)))
  expect_equal(mg3, withAdjMatrix(mg1))
})

test_that("m-separation test works", {
  expect_equal(m_sep(mg1a, 1, 2), FALSE)
  expect_equal(m_sep(mg1a, 1, 2, 3), TRUE)
  expect_equal(m_sep(mg1a, 1, integer(0)), TRUE)
})

mg1 <- graphCr("1 -> 4 <- 2 -> 3 -> 5 <- 4 -- 1 -- 3", mode = "eList")
mg1a <- graphCr("2 -> 3 -> 5 <- 4 -- 1 -- 3", mode = "eList")
mg2 <- graphCr("1 -> 4 <- 2 -> 3 -> 5 <- 4 -- 1 -- 3 -- 5", mode = "eList")
gr3 <- graphCr("1 -> 3 <- 4 -> 2 <- 1, 3 -> 2")

test_that("mutilate for incoming edges works", {
  expect_equal(mutilate(mg1, 4, "directed", dir=-1), mg1a)
})

test_that("addEdges works", {
  expect_equal(addEdges(mg1, edges=makeEdgeList(un=eList(c(3,5)))), mg2)
})

test_that("removeEdges works", {
  mg3 <- removeEdges(mg2, un=eList(c(5,3)))
  expect_equal(mg3, withAdjMatrix(mg1))
})

test_that("m-separation test works", {
  expect_equal(m_sep(mg1a, 1, 2), FALSE)
  expect_equal(m_sep(mg1a, 1, 2, 3), TRUE)
  expect_equal(m_sep(mg1a, 1, integer(0)), TRUE)
  expect_false(m_sep(gr3,1,3,4))
})
