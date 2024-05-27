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

gr_cmp3 <- makeGraphComplete(3)
gr_chn3 <- makeGraphChain(3)
gr_cmp3_b <- makeGraphComplete(3, type = "bidirected")
gr_chn3_b <- makeGraphChain(3, type = "bidirected")

test_that("is_complete works as expected", {
  expect_true(is_complete(gr_cmp3))
  expect_true(is_complete(gr_cmp3_b))
  expect_false(is_complete(gr_chn3))
  expect_false(is_complete(gr_chn3_b))
  expect_true(is_complete(gr_chn3, v=1:2))
  expect_true(is_complete(gr_chn3_b, v=1:2))
  expect_true(is_complete(gr_chn3_b[2]))
  expect_true(is_complete(gr_chn3_b[integer(0)]))
})
