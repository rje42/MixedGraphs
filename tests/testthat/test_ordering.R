mg1 <- graphCr("1 -> 2 <-> 3 <-> 4 <- 2", mode = "adjMatrix")

test_that("DAG topology is plausible", {
  expect_true(isTopological(mg1, topologicalOrder(mg1)))
  expect_true(isTopological(mg1, 1:4))
  expect_false(isTopological(mg1, c(2,1,3,4)))
})
