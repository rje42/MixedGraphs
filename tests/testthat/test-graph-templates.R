## bipartite_gr_cpp
test_that("bipartite_gr_cpp works for an undirected bipartite graph", {
  result <- bipartite_gr_cpp(2, 2, FALSE)
  expect_equal(length(result), 4)
  
  expect_setequal(result[[1]], c(3L, 4L))
  expect_setequal(result[[2]], c(3L, 4L))
  expect_setequal(result[[3]], c(1L, 2L))
  expect_setequal(result[[4]], c(1L, 2L))
})

test_that("bipartite_gr_cpp works for a directed bipartite graph", {
  result <- bipartite_gr_cpp(2, 2, TRUE)
  expect_equal(length(result), 4)
  
  expect_setequal(result[[1]], c(3L, 4L))
  expect_setequal(result[[2]], c(3L, 4L))
  expect_setequal(result[[3]], integer(0))
  expect_setequal(result[[4]], integer(0))
})

test_that("bipartite_gr_cpp returns correct class", {
  result <- bipartite_gr_cpp(2, 3, FALSE)
  expect_s3_class(result, "adjList")
})

test_that("bipartite_gr_cpp handles invalid input", {
  expect_error(bipartite_gr_cpp(-1, 3))
  expect_error(bipartite_gr_cpp(3, -1))
})

test_that("bipartite_gr_cpp handles n = 0", {
  result <- bipartite_gr_cpp(0, 2, FALSE)
  expect_equal(length(result), 0)
  expect_s3_class(result, "adjList")
})

test_that("bipartite_gr_cpp handles m = 0", {
  result <- bipartite_gr_cpp(2, 0, FALSE)
  expect_equal(length(result), 0)
  expect_s3_class(result, "adjList")
})

test_that("bipartite_gr_cpp handles n = 0 and m = 0", {
  result <- bipartite_gr_cpp(0, 0, FALSE)
  expect_equal(length(result), 0)
  expect_s3_class(result, "adjList")
})


## grid_gr_cpp
test_that("grid_gr_cpp works for a 2x2 undirected grid", {
  result <- grid_gr_cpp(2, 2, FALSE)
  expect_equal(length(result), 4)
  
  expect_setequal(result[[1]], c(2L, 3L))
  expect_setequal(result[[2]], c(1L, 4L))
  expect_setequal(result[[3]], c(1L, 4L))
  expect_setequal(result[[4]], c(2L, 3L))
})

test_that("grid_gr_cpp works for a 2x2 directed grid", {
  result <- grid_gr_cpp(2, 2, TRUE)
  expect_equal(length(result), 4)
  
  expect_setequal(result[[1]], c(2L, 3L))
  expect_setequal(result[[2]], c(4L))
  expect_setequal(result[[3]], c(4L))
  expect_setequal(result[[4]], integer(0))
})

test_that("grid_gr_cpp works for a 3x3 undirected grid", {
  result <- grid_gr_cpp(3, 3, FALSE)
  expect_equal(length(result), 9)
  
  expect_setequal(result[[1]], c(2L, 4L))
  expect_setequal(result[[2]], c(1L, 3L, 5L))
  expect_setequal(result[[3]], c(2L, 6L))
  expect_setequal(result[[4]], c(1L, 5L, 7L))
  expect_setequal(result[[5]], c(2L, 4L, 6L, 8L))
  expect_setequal(result[[6]], c(3L, 5L, 9L))
  expect_setequal(result[[7]], c(4L, 8L))
  expect_setequal(result[[8]], c(5L, 7L, 9L))
  expect_setequal(result[[9]], c(6L, 8L))
})

test_that("grid_gr_cpp works for a 3x3 directed grid", {
  result <- grid_gr_cpp(3, 3, TRUE)
  expect_equal(length(result), 9)
  
  expect_setequal(result[[1]], c(2L, 4L))
  expect_setequal(result[[2]], c(3L, 5L))
  expect_setequal(result[[3]], c(6L))
  expect_setequal(result[[4]], c(5L, 7L))
  expect_setequal(result[[5]], c(6L, 8L))
  expect_setequal(result[[6]], c(9L))
  expect_setequal(result[[7]], c(8L))
  expect_setequal(result[[8]], c(9L))
  expect_setequal(result[[9]], integer(0))
})

test_that("grid_gr_cpp returns correct class", {
  result <- grid_gr_cpp(2, 3, FALSE)
  expect_s3_class(result, "adjList")
})

test_that("grid_gr_cpp handles invalid input", {
  expect_error(grid_gr_cpp(-1, 3))
  expect_error(grid_gr_cpp(3, -1))
})

test_that("grid_gr_cpp handles n = 0", {
  result <- grid_gr_cpp(0, 2, FALSE)
  expect_equal(length(result), 0)
  expect_s3_class(result, "adjList")
})

test_that("grid_gr_cpp handles m = 0", {
  result <- grid_gr_cpp(2, 0, FALSE)
  expect_equal(length(result), 0)
  expect_s3_class(result, "adjList")
})

test_that("grid_gr_cpp handles n = 0 and m = 0", {
  result <- grid_gr_cpp(0, 0, FALSE)
  expect_equal(length(result), 0)
  expect_s3_class(result, "adjList")
})