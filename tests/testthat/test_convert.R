gr1 <- graphCr("1 -- 2 -> 3 <-> 4 <-- 2, 1 -- 4")

test_that("conversion between ggm and mixedgraph works", {
  gr1_ggm <- convert(gr1, "ggm")
  gr1_ggm_mg <- convert(gr1_ggm, cur_format="ggm")
  expect_equal(gr1_ggm_mg, withAdjMatrix(gr1))
})

