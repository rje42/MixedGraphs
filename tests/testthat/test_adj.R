dgr <- graphCr("1 -> 2, 3", mode="adjList")
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

grph1 <- graphCr("1 -> 2 -> 3 <- 4 -> 5", mode="adjList")
grph1a <- graphCr("1 -> 2 -> 3 <- 4 -> 5", mode="edgeMatrix")
grph1b <- graphCr("1 -> 2 -> 3 <- 4 -> 5", mode="adjMatrix")
grph1c <- graphCr("1 -> 2 -> 3 <- 4 -> 5", mode="eList")

test_that("pathConnected works as expected", {
  expect_equal(pathConnected(grph1[-3],1,4), integer(0))
  expect_equal(pathConnected(grph1,1,4:5, dir=0), 4L)
})
