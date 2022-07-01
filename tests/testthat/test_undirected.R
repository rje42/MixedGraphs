sk1 <- graphCr("1 -- 2 -- 3 -- 4 -- 2")

gr1_al <- graphCr("1 -- 2 <-> 3 <- 4 -> 2", mode="adjList")
gr1_am <- graphCr("1 -- 2 <-> 3 <- 4 -> 2", mode="adjMatrix")
gr1_el <- graphCr("1 -- 2 <-> 3 <- 4 -> 2", mode="eList")
gr1_em <- graphCr("1 -- 2 <-> 3 <- 4 -> 2", mode="edgeMatrix")

sk1_al <- skeleton(gr1_al)
sk1_am <- skeleton(gr1_am)
sk1_el <- skeleton(gr1_el)
sk1_em <- skeleton(gr1_em)

test_that("skeleton works", {
  expect_equal(standardizeEdges(sk1), standardizeEdges(sk1_al))
  expect_equal(standardizeEdges(sk1), standardizeEdges(sk1_am))
  expect_equal(standardizeEdges(sk1), standardizeEdges(sk1_el))
  expect_equal(standardizeEdges(sk1), standardizeEdges(sk1_em))
})

dsk1 <- dual(sk1)

dgr1_al <- dual(gr1_al)
dgr1_am <- dual(gr1_am)
dgr1_el <- dual(gr1_el)
dgr1_em <- dual(gr1_em)

dsk1_al <- skeleton(dgr1_al)
dsk1_am <- skeleton(dgr1_am)
dsk1_el <- skeleton(dgr1_el)
dsk1_em <- skeleton(dgr1_em)

test_that("dual works", {
  expect_equal(standardizeEdges(dsk1), standardizeEdges(dsk1_al))
  expect_equal(standardizeEdges(dsk1), standardizeEdges(dsk1_am))
  expect_equal(standardizeEdges(dsk1), standardizeEdges(dsk1_el))
  expect_equal(standardizeEdges(dsk1), standardizeEdges(dsk1_em))
})

dsk1a <- dsk1[c(1,3,4)]

dgr1a_al <- dual(gr1_al[-2])
dgr1a_am <- dual(gr1_am[-2])
dgr1a_el <- dual(gr1_el[-2])
dgr1a_em <- dual(gr1_em[-2])

dsk1a_al <- skeleton(dgr1a_al)
dsk1a_am <- skeleton(dgr1a_am)
dsk1a_el <- skeleton(dgr1a_el)
dsk1a_em <- skeleton(dgr1a_em)

test_that("dual works on subgraph", {
  expect_equal(standardizeEdges(dsk1a), standardizeEdges(dsk1a_al))
  expect_equal(standardizeEdges(dsk1a), standardizeEdges(dsk1a_am))
  expect_equal(standardizeEdges(dsk1a), standardizeEdges(dsk1a_el))
  expect_equal(standardizeEdges(dsk1a), standardizeEdges(dsk1a_em))
})
