gr1 <- graphCr("1 -- 2 -> 3 <-> 4 <-- 2, 1 --> 4", mode="adjList")
gr1a <- graphCr("1 -- 2 -> 3 <-> 4 <-- 2, 1 --> 4", mode="adjMatrix")
gr1b <- graphCr("1 -- 2 -> 3 <-> 4 <-- 2, 1 --> 4", mode="eList")
gr1c <- graphCr("1 -- 2 -> 3 <-> 4 <-- 2, 1 --> 4", mode="edgeMatrix")

gr2 <- graphCr("1 <--> 2 -> 3 <-> 4 <-- 2, 1 --> 4 --> 6, 2 <-> 5", mode="adjList")
gr2a <- graphCr("1 <--> 2 -> 3 <-> 4 <-- 2, 1 --> 4 --> 6, 2 <-> 5", mode="adjMatrix")
gr2b <- graphCr("1 <--> 2 -> 3 <-> 4 <-- 2, 1 --> 4 --> 6, 2 <-> 5", mode="eList")
gr2c <- graphCr("1 <--> 2 -> 3 <-> 4 <-- 2, 1 --> 4 --> 6, 2 <-> 5", mode="edgeMatrix")


test_that("is.SG and is.ADMG work", {
  expect_true(is.SG(gr1))
  expect_true(is.SG(gr1a))
  expect_true(is.SG(gr1b))
  expect_true(is.SG(gr1c))
  expect_false(is.ADMG(gr1))
  expect_false(is.ADMG(gr1a))
  expect_false(is.ADMG(gr1b))
  expect_false(is.ADMG(gr1c))
})

test_that("pa, ch, sib, dis, anc, dec, consistent", {
  expect_equal(pa(gr1, 4, sort=2), pa(gr1a, 4, sort=2))
  expect_equal(pa(gr1, 4, sort=2), pa(gr1b, 4, sort=2))
  expect_equal(pa(gr1, 4, sort=2), pa(gr1c, 4, sort=2))
  
  expect_equal(ch(gr1, 2, sort=2), ch(gr1a, 2, sort=2))
  expect_equal(ch(gr1, 2, sort=2), ch(gr1b, 2, sort=2))
  expect_equal(ch(gr1, 2, sort=2), ch(gr1c, 2, sort=2))

  expect_equal(sib(gr2, 2, sort=2), sib(gr2a, 2, sort=2))
  expect_equal(sib(gr2, 2, sort=2), sib(gr2b, 2, sort=2))
  expect_equal(sib(gr2, 2, sort=2), sib(gr2c, 2, sort=2))

  expect_equal(dis(gr2, 2, sort=2), dis(gr2a, 2, sort=2))
  expect_equal(dis(gr2, 2, sort=2), dis(gr2b, 2, sort=2))
  expect_equal(dis(gr2, 2, sort=2), dis(gr2c, 2, sort=2))
  
  expect_equal(anc(gr2, 2, sort=2), anc(gr2a, 2, sort=2))
  expect_equal(anc(gr2, 2, sort=2), anc(gr2b, 2, sort=2))
  expect_equal(anc(gr2, 2, sort=2), anc(gr2c, 2, sort=2))

  expect_equal(dec(gr2, 2, sort=2), dec(gr2a, 2, sort=2))
  expect_equal(dec(gr2, 2, sort=2), dec(gr2b, 2, sort=2))
  expect_equal(dec(gr2, 2, sort=2), dec(gr2c, 2, sort=2))
})

