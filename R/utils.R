#' Encode ordinal
#'
#' to integer values
#'
#' @param x vector to encode
#' @param order vector with x elements in order of encoding
#'
#' @return numeric vector with encoded values
encode_ordinal <- function(x, order = sort(unique(x))) {
  x <- as.numeric(factor(x, levels = order, exclude = NULL))
  x
}

#' Scale vector from 0 to 1
#'
#' Note that vector with all the same values will be filled with 0.5.
#'
#' @param vec vector with numbers
#' @param na_rm flag stating whether to ignore NA values or not
#'
#' @return vector with scaled values
#' @family scaling
scale <- function(vec, na_rm = TRUE) {
  if (length(unique(vec)) == 1)
    return(rep(0.5, length(vec)))
  .min <- min(vec, na.rm = na_rm)
  (vec - .min)/(max(vec, na.rm = na_rm) - .min)
}

#' Z score vector
#'
#' Note that vector with all the same values will be filled with 0.
#'
#' @param vec vector with numbers
#' @param na_rm flag stating whether to ignore NA values or not
#'
#' @return vector with z-scored values
#' @importFrom stats sd
#' @family scaling
zscore <- function(vec, na_rm = TRUE) {
  if (length(unique(vec)) == 1)
    return(rep(0, length(vec)))
  (vec-mean(vec, na.rm = na_rm))/sd(vec, na.rm = na_rm)
}
