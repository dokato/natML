
test_that("test get_spine_length", {
  nrn <- nat::Cell07PNs[[1]]
  res <- get_spine_length(nrn)
  expect_equal(round(res), 186)
})

test_that("test get_cable_length", {
  nrn <- nat::Cell07PNs[[1]]
  res <- get_cable_length(nrn)
  expect_equal(round(res), 297)
})

test_that("test get_soma_size", {
  nrn <- nat::Cell07PNs[[1]]
  res <- get_soma_size(nrn)
  expect_equal(round(res), 1)
})

test_that("test get_radius", {
  nrn <- nat::Cell07PNs[[1]]
  res <- get_radius(nrn)
  expect_equal(round(res*10), 7)
})

test_that("test get_spine_radius", {
  nrn <- nat::Cell07PNs[[1]]
  res <- get_spine_radius(nrn)
  expect_equal(round(res*10), 8)
})

test_that("test get_n_branchpoints", {
  nrn <- nat::Cell07PNs[[1]]
  res <- get_n_branchpoints(nrn)
  expect_equal(res, 1636)
})
