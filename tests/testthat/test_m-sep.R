mg1 <- graphCr("1 -> 4 <- 2 -> 3 -> 5 <- 4 <-- 1 --> 3 <--> 5, 1 -- 6 -- 2")

test_that("m-separation test works", {
  expect_equal(m_sep(mg1, 1, 2), FALSE)
  expect_equal(m_sep(mg1, 1, 2, 6), TRUE)
  # expect_equal(m_sep(mg1, 1, integer(0)), TRUE)
})

# test_that("m-separation test works", {
#   expect_equal(m_sep(mg1a, 1, 2), FALSE)
#   expect_equal(m_sep(mg1a, 1, 2, 3), TRUE)
#   expect_equal(m_sep(mg1a, 1, integer(0)), TRUE)
#   expect_false(m_sep(gr3,1,3,4))
# })
