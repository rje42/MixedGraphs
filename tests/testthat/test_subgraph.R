gr1al <- graphCr("1 <-> 2 <-> 3 <-> 4")
gr1am <- graphCr("1 <-> 2 <-> 3 <-> 4", mode="adjMatrix")
gr1el <- graphCr("1 <-> 2 <-> 3 <-> 4", mode="eList")
gr1em <- graphCr("1 <-> 2 <-> 3 <-> 4", mode="edgeMatrix")

gr2al <- gr1al[c(1,3,2,4), order=TRUE]
gr2am <- gr1am[c(1,3,2,4), order=TRUE]
gr2el <- gr1el[c(1,3,2,4), order=TRUE]
gr2em <- gr1em[c(1,3,2,4), order=TRUE]

gr2 <- mixedgraph(4, edges=makeEdgeList(bidirected=eList(list(c(1,3), c(2,3), c(2,4)))), vnames=paste0("x",c(1,3,2,4)))

testthat::test_that("re-ordering works", {
  expect_equal(standardizeEdges(gr2al), standardizeEdges(gr2))
  expect_equal(standardizeEdges(gr2am), standardizeEdges(gr2))
  expect_equal(standardizeEdges(gr2el), standardizeEdges(gr2))
  expect_equal(standardizeEdges(gr2em), standardizeEdges(gr2))
})

gr3 <- mixedgraph(3, edges=makeEdgeList(bidirected=eList(list(c(2,3)))), vnames=paste0("x",c(1,3,4)))

gr3al <- gr2al[c(1,2,4), drop=TRUE]
gr3am <- gr2am[c(1,2,4), drop=TRUE]
gr3el <- gr2el[c(1,2,4), drop=TRUE]
gr3em <- gr2em[c(1,2,4), drop=TRUE]

testthat::test_that("dropping vertices works", {
  expect_equal(standardizeEdges(gr3al), standardizeEdges(gr3))
  expect_equal(standardizeEdges(gr3am), standardizeEdges(gr3))
  expect_equal(standardizeEdges(gr3el), standardizeEdges(gr3))
  expect_equal(standardizeEdges(gr3em), standardizeEdges(gr3))
})

testthat::test_that("dropping and reordering fails", {
  expect_error(gr2[c(2,1), order=TRUE, drop=TRUE])
})

## Now test bipartite subgraphs
gr1_b <- graphCr("2 <-> 3, 4")
gr1_b <- gr1_b[-1]
gr1al_b <- gr1al[2,4:3]
gr1am_b <- gr1am[2,4:3]
gr1el_b <- gr1el[2,4:3]
gr1em_b <- gr1em[2,4:3]

testthat::test_that("dropping vertices works", {
  expect_equal(standardizeEdges(gr1al_b), standardizeEdges(gr1_b))
  expect_equal(standardizeEdges(gr1am_b), standardizeEdges(gr1_b))
  expect_equal(standardizeEdges(gr1el_b), standardizeEdges(gr1_b))
  expect_equal(standardizeEdges(gr1em_b), standardizeEdges(gr1_b))
})

