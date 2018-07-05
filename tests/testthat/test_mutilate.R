dag1 <- graphCr("1->4<-2->3->5<-4--1--3")
dag1a <- graphCr("2->3->5<-4--1--3")
dag2 <- graphCr("1->4<-2->3->5<-4--1--3--5")

test_that("mutilate for incoming edges works", {
  expect_equal(mutilate(dag1, 4, "directed", dir=-1), dag1a)
})

test_that("addEdges works", {
  expect_equal(addEdges(dag1, list(un=list(c(3,5)))), dag2)
})

test_that("removeEdges works", {
  dag3 <- removeEdges(dag2, list(un=list(c(5,3))))
  expect_equal(dag3, withAdjMatrix(dag1))
})

test_that("m-separation test works", {
  expect_equal(m_sep(dag1a, 1, 2), FALSE)
  expect_equal(m_sep(dag1a, 1, 2, 3), TRUE)
  expect_equal(m_sep(dag1a, 1, integer(0)), TRUE)
})
