dgr <- graphCr("1 -> 2, 3")
dgr_a <- graphCr("1 -> 2, 3",mode="edgeMatrix")
dgr_b <- graphCr("1 -> 2, 3",mode="adjMatrix")
dgr_c <- graphCr("1 -> 2, 3",mode="eList")

test_that("adjacent works as expected", {
  expect_true(adjacent(dgr,1,2))
  expect_false(adjacent(dgr,1,3))
  expect_true(adjacent(dgr_a,1,2))
  expect_false(adjacent(dgr_a,1,3))
  expect_true(adjacent(dgr_b,1,2))
  expect_false(adjacent(dgr_b,1,3))
  expect_true(adjacent(dgr_c,1,2))
  expect_false(adjacent(dgr_c,1,3))
})