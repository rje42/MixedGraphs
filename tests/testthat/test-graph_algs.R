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
