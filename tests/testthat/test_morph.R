gr1a1 <- graphCr("1 -- 3 -- 2 -- 4 -- 1", mode = "edgeMatrix")
gr1a2 <- graphCr("1 -> 3 -- 2 -> 4 -- 1", mode = "edgeMatrix")
gr1b1 <- morphEdges(gr1, to="undirected")
gr1b2 <- morphEdges(gr1, from="bidirected", to="undirected")
gr1c1 <- graphCr("1 -> 3 <-- 2 --> 4 <-- 1", mode = "edgeMatrix")
gr1c2 <- graphCr("1 <- 3 -> 2 <-- 4 --> 1", mode = "edgeMatrix")
gr1d1 <- morphEdges(gr1, to="directed")
gr1d2 <- morphEdges(gr1, to="directed", topOrd = 4:1)


gr1a1 <- standardizeEdges(gr1a1)
gr1a2 <- standardizeEdges(gr1a2)
gr1b1 <- standardizeEdges(gr1b1)
gr1b2 <- standardizeEdges(gr1b2)
gr1c1 <- standardizeEdges(gr1c1)
gr1c2 <- standardizeEdges(gr1c2)
gr1d1 <- standardizeEdges(gr1d1)
gr1d2 <- standardizeEdges(gr1d2)

test_that("morphEdges() works", {
  expect_equal(gr1a1, gr1b1)
  expect_equal(gr1a2, gr1b2)
})

test_that("morphEdges() works with direction", {
  expect_equal(gr1c1, gr1d1)
  expect_equal(gr1c2, gr1d2)
})

