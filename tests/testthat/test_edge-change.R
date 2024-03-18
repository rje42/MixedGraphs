A <- matrix(0,4,4)

Am <- as.adjMatrix(A)

testthat::test_that("as.adjMatrix() works as expected",
                    expect_true("adjMatrix" %in% class(Am))
)
