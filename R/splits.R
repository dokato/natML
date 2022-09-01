#' Split into test train sets
#'
#' It makes sure that there is at least one representative of each class
#' in the test set
#'
#' @param yclass any vector with Y labels
#' @param split_prop proportion of test samples
#' @return indices of train test splits
#' @importFrom glue glue
#' @examples
#' y <- c(1,1,1,1,2,2,2,2,2,2,2,3,3)
#' split_test_train(y,0.2)
#' @export
split_test_train <- function(yclass, split_prop) {
  if (split_prop > 1 && split_prop < 100)
    split_prop <- split_prop / 100
  if (split_prop > 100)
    stop("Wrong split proprotion")
  tbl <- table(yclass)
  trainids <- c()
  testids <- c()
  for (ic in names(tbl)) {
    catvec = which(as.character(yclass) == ic)
    if (length(catvec) == 1) {
      warning(glue("Only one sample found per class item `{ic}`"))
      trainids <- c(trainids, catvec)
    } else {
      sample <- sample(c(TRUE, FALSE), length(catvec), replace=TRUE, prob=c(split_prop, 1-split_prop))
      if (length(unique(sample)) == 1)
        sample[[1]] <- !sample[[1]]
      trainids <- c(trainids, catvec[!sample])
      testids <- c(testids, catvec[sample])
    }
  }
  list(
    trainids = trainids[sample(1:length(trainids))], # shuffle examples
    testids = testids[sample(1:length(testids))]
  )
}
