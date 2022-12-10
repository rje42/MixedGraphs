grAL <- graphCr("1 -- 2 -> 3 <- 4 -- 5 -> 6 -> 4")
grAM <- graphCr("1 -- 2 -> 3 <- 4 -- 5 -> 6 -> 4", mode="adjMatrix")

## start with adj_cpp
adjR <- adj(grAL, 2, "undirected", dir=0)
adjLCpp <- adj_cpp(grAL[etype="undirected"], 2, dir=0)
adjMCpp <- adj_cpp(grAM[etype="undirected"], 2, dir=0)


test_that("adj_cpp with dir=0 works", {
  expect_equal(adjR, adjLCpp + 1)
  expect_equal(adjR, adjMCpp + 1)
})


adjR <- adj(grAL, 3, "directed", dir=1, sort=2)
adjLCpp <- sort.int(adj(grAL[etype="directed"], 3, dir=1))
adjMCpp <- sort.int(adj(grAM[etype="directed"], 3, dir=1))


test_that("adj_cpp with dir=1 works", {
  expect_equal(adjR, adjLCpp + 1)
  expect_equal(adjR, adjMCpp + 1)
})

## now look at grp_cpp
grpMCpp <- sort.int(grp_cpp(grAM[etype="directed"], 2, dir=1))
grpLCpp <- sort.int(grp_cpp(grAL[etype="directed"], 2, dir=1))
grpR <- grp(grAL, etype="directed", 2, dir=1, sort=2)


test_that("grp_cpp with dir=1 works", {
  expect_equal(grpR, grpLCpp + 1)
  expect_equal(grpR, grpMCpp + 1)
})
