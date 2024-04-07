svgr <- skeleton(grVerma)
peo_svgr <- match(c(2,1,3), peo(svgr))
peo_svgr_124 <- match(c(2,1,4), peo(svgr, c(1,2,4)))

test_that("peo works", {
  expect_lt(min(peo_svgr[2:3]), peo_svgr[1])
  expect_lt(min(peo_svgr_124[2:3]), peo_svgr_124[1])
})
