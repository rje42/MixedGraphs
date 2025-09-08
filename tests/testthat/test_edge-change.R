A <- matrix(0,4,4)
aL <- list(2, 1, integer(0), 2)
eL <- list(c(2, 1), c(3, 2))
E <- matrix(c(2,1,3,2), nrow=2)

Am <- as.adjMatrix(A)
aLl <- as.adjList(aL)
eLl <- as.eList(eL)
eLl2 <- as.eList(c(1,2))
Em <- as.edgeMatrix(E)

testthat::test_that("as.adjMatrix() works as expected",
                    expect_true("adjMatrix" %in% class(Am))
)

testthat::test_that("as.adjList() works as expected",
                    expect_true("adjList" %in% class(aLl))
)

testthat::test_that("as.edgeMatrix() works as expected",
                    expect_true("edgeMatrix" %in% class(Em))
)

testthat::test_that("as.eList() works as expected", {
                    expect_true("eList" %in% class(eLl))
                    expect_true("eList" %in% class(eLl2))
})
