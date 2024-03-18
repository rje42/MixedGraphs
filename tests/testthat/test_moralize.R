grV <- standardizeEdges(grVerma)
grVm <- standardizeEdges(moralize(grVerma))
grVma <- standardizeEdges(makeGraphComplete(4))
grVm2 <- standardizeEdges(moralize(grVerma, 2))
grVm2a <- standardizeEdges(graphCr("4 <- 1 -- 2 --> 3 --> 4 <- 2"))
grVm3 <- standardizeEdges(moralize(grVerma, 3))
grVm3a <- standardizeEdges(graphCr("4 <- 1 -- 2 -- 3 --> 4 <- 2"))

test_that("moralization works for Verma graph", {
  expect_equal(grVm, grVma)
  expect_equal(grVm2, grVm2a)
  expect_equal(grVm3, grVm3a)
})


# mg1 <- graphCr("1 -> 4 <- 2 -> 3 -> 5 <- 4 -- 1 -- 3")
# mg1a_o <- standardizeEdges(graphCr("1 -> 4 <- 2 -> 3 -> 5, 4 -- 1 -- 3"))
# mg1a <- standardizeEdges(graphCr("2 -> 3 -> 5 <- 4 -- 1 -- 3"))
# mg2 <- standardizeEdges(graphCr("1 -> 4 <- 2 -> 3 -> 5 <- 4 -- 1 -- 3 -- 5"))
# mg3 <- standardizeEdges(graphCr("1 -> 4 <- 2 -> 3 -> 5 <- 4 -- 1 -- 3 -- 4"))

