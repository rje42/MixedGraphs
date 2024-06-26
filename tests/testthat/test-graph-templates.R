test_that("bipartite_gr_cpp works with non-zero n and m", {
  result <- bipartite_gr_cpp(c(3L), c(2L))
  expect_equal(length(result), 5)
  
  expect_setequal(result[[1]], c(4L, 5L))
  expect_setequal(result[[2]], c(4L, 5L))
  expect_setequal(result[[3]], c(4L, 5L))
  expect_setequal(result[[4]], c(1L, 2L, 3L))
  expect_setequal(result[[5]], c(1L, 2L, 3L))
})

test_that("bipartite_gr_cpp works with n = 0", {
  result <- bipartite_gr_cpp(c(0L), c(3L))
  expect_equal(length(result), 3)
  
  expect_equal(result[[1]], integer(0))
  expect_equal(result[[2]], integer(0))
  expect_equal(result[[3]], integer(0))
})

test_that("bipartite_gr_cpp works with m = 0", {
  result <- bipartite_gr_cpp(c(3L), c(0L))
  expect_equal(length(result), 3)
  
  expect_equal(result[[1]], integer(0))
  expect_equal(result[[2]], integer(0))
  expect_equal(result[[3]], integer(0))
})

test_that("bipartite_gr_cpp works with n = 0 and m = 0", {
  result <- bipartite_gr_cpp(0L, 0L)
  expect_equal(length(result), 0L)
})

test_that("bipartite_gr_cpp returns correct class", {
  result <- bipartite_gr_cpp(c(2L), c(2L))
  expect_s3_class(result, "adjList")
})


## grid_graph_cpp
test_that("grid_graph_cpp works for a 2x2 grid", {
  result <- grid_graph_cpp(2, 2)
  expect_equal(length(result), 4)
  
  expect_equal(result[[1]], c(3L, 2L))
  expect_equal(result[[2]], c(4L, 1L))
  expect_equal(result[[3]], c(1L, 4L))
  expect_equal(result[[4]], c(2L, 3L))
})

test_that("grid_graph_cpp works for a 3x3 grid", {
  result <- grid_graph_cpp(3, 3)
  expect_equal(length(result), 9)
  
  expect_setequal(result[[1]], c(4L, 2L))
  expect_setequal(result[[2]], c(5L, 3L, 1L))
  expect_setequal(result[[3]], c(6L, 2L))
  expect_setequal(result[[4]], c(1L, 7L, 5L))
  expect_setequal(result[[5]], c(2L, 8L, 6L, 4L))
  expect_setequal(result[[6]], c(3L, 9L, 5L))
  expect_setequal(result[[7]], c(4L, 8L))
  expect_setequal(result[[8]], c(5L, 9L, 7L))
  expect_setequal(result[[9]], c(6L, 8L))
})

test_that("grid_graph_cpp returns correct class", {
  result <- grid_graph_cpp(2, 3)
  expect_s3_class(result, "adjList")
})

test_that("grid_graph_cpp handles invalid input", {
  expect_error(grid_graph_cpp(-1, 3))
  expect_error(grid_graph_cpp(3, -1))
})

test_that("grid_graph_cpp works for a 1x3 grid", {
  result <- grid_graph_cpp(1, 3)
  expect_equal(length(result), 3)
  
  expect_setequal(result[[1]], c(2L))
  expect_setequal(result[[2]], c(3L, 1L))
  expect_setequal(result[[3]], c(2L))
})

test_that("grid_graph_cpp works for a 3x1 grid", {
  result <- grid_graph_cpp(3, 1)
  expect_equal(length(result), 3)
  
  expect_setequal(result[[1]], c(2L))
  expect_setequal(result[[2]], c(3L, 1L))
  expect_setequal(result[[3]], c(2L))
})

test_that("grid_graph_cpp handles n = 0", {
  result <- grid_graph_cpp(0, 2)
  expect_equal(length(result), 0)
  expect_s3_class(result, "adjList")
})

test_that("grid_graph_cpp handles m = 0", {
  result <- grid_graph_cpp(2, 0)
  expect_equal(length(result), 0)
  expect_s3_class(result, "adjList")
})

test_that("grid_graph_cpp handles n = 0 and m = 0", {
  result <- grid_graph_cpp(0, 0, dir=FALSE)
  expect_equal(length(result), 0)
  expect_s3_class(result, "adjList")
})

test_that("grid_graph_cpp works for a 1x3 directed grid", {
  result <- grid_graph_cpp(1, 3, dir=TRUE)
  expect_equal(length(result), 3)
  
  expect_setequal(result[[1]], c(2L))
  expect_setequal(result[[2]], c(3L))
  expect_setequal(result[[3]], integer(0))
})
