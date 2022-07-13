
test_that("test .make_feature_names", {
  in_list <- list(a = 1, 3, c = 4)
  res <- .make_feature_names(in_list)
  expect_equal(length(res), 3)
  expect_equal(res[[2]], "F1")
})

test_that("test .get_feature with character", {
  nl <- nat::Cell07PNs
  feat <- "MBP1"
  res <- .get_feature(nl, feat)
  expect_true(all(res == nl[,feat]))
})

test_that("test .get_feature with function", {
  nl <- nat::Cell07PNs
  nrnfunc <- function(x) x$d[x$StartPoint,]$W
  res <- .get_feature(nl, nrnfunc)
  expect_equal(class(res), "numeric")
  expect_equal(length(res), length(nl))
})

