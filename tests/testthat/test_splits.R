
test_that("test get_spine_length standard", {
  y <- c(1,1,1,1,2,2,2,2,2,2,2,3,3)
  out <- split_test_train(y,0.2)
  expect_equal(length(unique(y[out$trainids])), 3)
  expect_equal(length(unique(y[out$testids])), 3)
})

test_that("test get_spine_length edgecases", {
  y <- c(1,1,1,1,2,2,2,2,2,2,2,3)
  expect_warning(split_test_train(y,0.2))
  expect_error(split_test_train(y,101))
})
