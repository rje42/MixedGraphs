dag1 <- graphCr("1->4<-2->3->5<-4--1--3")
dag1a <- graphCr("2->3->5<-4--1--3")

test_that("mutilate for incoming edges works", {
  expect_equal(mutilate(dag1, 4, "directed", dir=-1), dag1a)
})

