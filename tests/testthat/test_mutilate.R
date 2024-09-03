mg1 <- graphCr("1 -> 4 <- 2 -> 3 -> 5 <- 4 -- 1 -- 3")
mg1_am <- graphCr("1 -> 4 <- 2 -> 3 -> 5 <- 4 -- 1 -- 3", mode="adjMatrix")
mg1_el <- graphCr("1 -> 4 <- 2 -> 3 -> 5 <- 4 -- 1 -- 3", mode="eList")
mg1_em <- graphCr("1 -> 4 <- 2 -> 3 -> 5 <- 4 -- 1 -- 3", mode="edgeMatrix")
mg1a_o <- standardizeEdges(graphCr("1 -> 4 <- 2 -> 3 -> 5, 4 -- 1 -- 3"))
mg1a <- standardizeEdges(graphCr("2 -> 3 -> 5 <- 4 -- 1 -- 3"))
mg2 <- standardizeEdges(graphCr("1 -> 4 <- 2 -> 3 -> 5 <- 4 -- 1 -- 3 -- 5"))
mg3 <- standardizeEdges(graphCr("1 -> 4 <- 2 -> 3 -> 5 <- 4 -- 1 -- 3 -- 4"))

mg1_4 <- standardizeEdges(mutilate(mg1, 4, etype="directed", dir=-1))
mg1_am_4 <- standardizeEdges(mutilate(mg1_am, 4, etype="directed", dir=-1))
mg1_em_4 <- standardizeEdges(mutilate(mg1_em, 4, etype="directed", dir=-1))
mg1_el_4 <- standardizeEdges(mutilate(mg1_el, 4, etype="directed", dir=-1))

mg1_4o <- standardizeEdges(mutilate(mg1, 4, etype="directed", dir=1))
mg1_am_4o <- standardizeEdges(mutilate(mg1_am, 4, etype="directed", dir=1))
mg1_em_4o <- standardizeEdges(mutilate(mg1_em, 4, etype="directed", dir=1))
mg1_el_4o <- standardizeEdges(mutilate(mg1_el, 4, etype="directed", dir=1))

test_that("mutilate for incoming edges works", {
  expect_equal(mg1_4, mg1a)
  expect_equal(mg1_am_4, mg1a)
  expect_equal(mg1_em_4, mg1a)
  expect_equal(mg1_el_4, mg1a)
})

test_that("mutilate for outgoing edges works", {
  expect_equal(mg1_4o, mg1a_o)
  expect_equal(mg1_am_4o, mg1a_o)
  expect_equal(mg1_em_4o, mg1a_o)
  expect_equal(mg1_el_4o, mg1a_o)
})

test_that("addEdges works", {
  expect_equivalent(standardizeEdges(addEdges(mg1, undirected=eList(c(3,5)))), 
                    standardizeEdges(mg2))
  expect_equivalent(standardizeEdges(addEdges(mg1, makeEdgeList(undirected=eList(c(3,5))))), 
                    standardizeEdges(mg2))
  expect_equivalent(standardizeEdges(addEdges(mg1, undirected=eList(c(3,4)))), 
                    standardizeEdges(mg3))
})

test_that("addEdges works with edgeCr()", {
  expect_equivalent(standardizeEdges(addEdges(mg1, edgeCr("3 -- 4", mode="eList"))), 
                    standardizeEdges(mg3))
  expect_equivalent(standardizeEdges(addEdges(mg1, edgeCr("3 -- 4", mode="edgeMatrix"))), 
                    standardizeEdges(mg3))
  expect_equivalent(standardizeEdges(addEdges(mg1, edgeCr("3 -- 4", mode="adjMatrix"))), 
                    standardizeEdges(mg3))
  expect_equivalent(standardizeEdges(addEdges(mg1, edgeCr("3 -- 4", mode="adjList"))), 
                    standardizeEdges(mg3))
})

test_that("removeEdges works", {
  mg3 <- standardizeEdges(removeEdges(mg2, un=eList(c(5,3))))
  expect_equal(mg3, standardizeEdges(mg1))
})

mg1 <- graphCr("1 -> 4 <- 2 -> 3 -> 5 <- 4 -- 1 -- 3", mode = "eList")
mg1a <- graphCr("2 -> 3 -> 5 <- 4 -- 1 -- 3", mode = "eList")
mg2 <- graphCr("1 -> 4 <- 2 -> 3 -> 5 <- 4 -- 1 -- 3 -- 5", mode = "eList")
gr3 <- graphCr("1 -> 3 <- 4 -> 2 <- 1, 3 -> 2")

test_that("mutilate for incoming edges works", {
  expect_equal(mutilate(mg1, 4, etype="directed", dir=-1), mg1a)
})


gr1a <- standardizeEdges(graphCr("2 <-> 3, 1 <-> 4"))

test_that("mutilate removes edge class correctly", {
  gr1t <- standardizeEdges(mutilate(gr1, etype="dir"))
  expect_equal(gr1t, gr1a)
})


test_that("addEdges works", {
  expect_equal(addEdges(mg1, edges=makeEdgeList(un=eList(c(3,5)))), mg2)
})

test_that("removeEdges works", {
  mg3 <- standardizeEdges(removeEdges(mg2, un=eList(c(5,3))))
  expect_equal(mg3, standardizeEdges(mg1))
})

