gr1 <- graphCr("1 -- 2 -> 3 <-> 4 <-- 2, 1 -- 4")

test_that("conversion between ggm and mixedgraph works", {
  gr1_ggm <- convert(gr1, "ggm")
  gr1_ggm_mg <- standardizeEdges(convert(gr1_ggm, cur_format="ggm"))
  expect_equal(gr1_ggm_mg, standardizeEdges(withAdjMatrix(gr1)))
})

test_that("conversion between PAG and mixedgraph works", {
  require(pcalg)
  gr1_pag <- convert(gr1, "PAG")
  gr1_pag_mg <- convert(gr1_pag, cur_format="PAG")
  gr1_pag_mg <- standardizeEdges(gr1_pag_mg)
  expect_equal(gr1_pag_mg, gr1)
})
