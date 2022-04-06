
test_that("test encode_ordinal", {
  in_vec <- c("dog", "cat", "cat")
  res <- encode_ordinal(in_vec)
  expect_equal(sort(unique(res)), c(1, 2))
})

test_that("test scale", {
  # test standard case
  in_vec <- c(-0.3, 0.3, 0.9)
  res <- scale(in_vec)
  expect_equal(min(res), 0)
  expect_equal(max(res), 1)
  # test edge cases
  in_vec <- c(0.3, 0.3, 0.3)
  res <- scale(in_vec)
  expect_equal(res[[1]], 0.5)
  expect_equal(res[[3]], 0.5)

  expect_warning(scale(c()))
})

test_that("test zscore", {
  # test standard case
  in_vec <- c(-1, 0, 1)
  res <- zscore(in_vec)
  expect_true(all(res == in_vec))

  in_vec <- c(-3, 6, 10)
  res <- zscore(in_vec)
  expect_true(mean(res) < 0.005) # almost zero
  # test edge cases
  in_vec <- c(0.3, 0.3, 0.3)
  res <- zscore(in_vec)
  expect_equal(res[[1]], 0)
  expect_equal(res[[3]], 0)

})
