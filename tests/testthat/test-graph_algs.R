ud4 <- makeGraphCycle(4)
bi32 <- makeGraphBipartite(3,2)
mis51 <- removeEdges(makeGraphComplete(5), undirected = eList(list(c(2,3))))
ord_mis51 <- perfect_elim_order(mis51)

test_that("PEO works", {
  expect_equal(perfect_elim_order(ud4), NA)
  expect_equal(perfect_elim_order(bi32), NA)
  expect_equal(sort.int(ord_mis51), 1:5)
  expect_true(ord_mis51[1] %in% 2:3)
})

## newer version
svgr <- skeleton(grVerma)
peo_svgr <- match(c(2,1,3), peo(svgr))
peo_svgr_124 <- match(c(2,1,4), peo(svgr, c(1,2,4)))

test_that("peo works", {
  expect_lt(min(peo_svgr[2:3]), peo_svgr[1])
  expect_lt(min(peo_svgr_124[2:3]), peo_svgr_124[1])
})


test_that("peo with po works", {
  expect_equal(peo(svgr), peo(svgr, po=c(1,1,1,1)))
  expect_equal(peo(svgr, po=c(1,2,1,1)), c(4,3,1,2))
})
