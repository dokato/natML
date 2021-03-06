#' Extract features from neuronlist
#'
#' @param neurons_list neuronlist of neurons (nat)
#' @param features_list list with feature names of functions that
#' generate them
#' @param y target class (can be character of function), will be added as column
#' "Y" if name is NULL.
#' @param to_numeric flag that says whether all factor/ character features are
#' meant to be transformed to numeric (default: F)
#' @param normalise what normalization to use
#' @param split_proportion if numeric then it splits dataset into train test.
#'    The number should be a target proportion of test cases (although not
#'    guaranteed as it makes sure that there's at least one element per class).
#' @param remove_nas flags whether to remove NAs or not
#'
#' @return data frame with features
#' @export
#' @rdname extract_features
#' @examples
#' features_list <- list( "upstream", "downstream", "soma")
#' kc_train_features <- extract_features(
#'   kc_train,
#'   features_list = features_list,
#'   y = "type"
#' )
extract_features <- function(neurons_list,
                             features_list = NULL,
                             y = NULL,
                             to_numeric = FALSE,
                             normalise = c("none", "zscore", "scale"),
                             split_proportion = NULL,
                             remove_nas = TRUE) {
  if (!(class(features_list) %in% c("list", "character")))
    stop("Wrong features_list types. Check the docs!")
  UseMethod('extract_features')
}

#' @description extract features from a neuronlist
#' @param ... extra arguments
#' @export
#' @import glue
#' @importFrom stats na.omit
#' @rdname extract_features
extract_features.neuronlist <- function(neurons_list, features_list = NULL,
                                        y = NULL,
                                        to_numeric = FALSE,
                                        normalise = c("none", "zscore", "scale"),
                                        split_proportion = NULL,
                                        remove_nas = TRUE,
                                        ...) {
  normalise <- match.arg(normalise)

  feature_names <- .make_feature_names(features_list)
  names(features_list) <- feature_names

  if (normalise != "none")
    normalise_fun <- eval(parse(text = normalise))

  features_df <- data.frame(placeholder = matrix(nrow = length(neurons_list)))
  for (feature_name in names(features_list)) {
    feature <- features_list[[feature_name]]
    features_df[feature_name] <- .get_feature(neurons_list, feature)

    if (isTRUE(to_numeric) && !is.numeric(features_df[[feature_name]]))
      features_df[feature_name] <- encode_ordinal(features_df[[feature_name]])
    if (normalise != "none" && is.numeric(features_df[[feature_name]]))
      features_df[feature_name] <- normalise_fun(features_df[[feature_name]])
  }
  features_df$placeholder <- NULL
  if (!is.null(y)) {
    if (is.null(names(y)))
      features_df$Y <- .get_feature(neurons_list, y, as_factor = TRUE)
    else
      features_df[names(y)] <- .get_feature(neurons_list, y, as_factor = TRUE)
  }
  rownames(features_df) <- rownames(neurons_list[,])
  if (isTRUE(remove_nas)) {
    prev_names <- rownames(features_df)
    features_df <- na.omit(features_df)
    diff <- setdiff(prev_names, rownames(features_df))
    if (length(diff) > 0) {
      warning(paste("Neurons with NA values removed:", diff))
    }
    rm(prev_names, diff)
  }
  if (is.null(split_proportion))
    return(features_df)
  else {
    if (is.null(y)) stop("To make splits you need to define Y")
    y_name <- if (is.null(names(y))) "Y" else names(y)
    splits <- split_test_train(features_df[[y_name]], split_proportion)
    list(
      train = features_df[splits$trainids,],
      test  = features_df[splits$testids,]
    )
  }
}


#' Get feature
#'
#' from a neuron list.
#'
#' @param neurons_list neuronlist of neurons
#' @param feature callable or character with feature
#' @param as_factor flag saying whether feature should be transformed to factor
#'
#' @return vector with feature values
.get_feature <- function(neurons_list, feature, as_factor = FALSE) {
  feat_type <- class(feature)
  if (feat_type == "character") {
    if (feature %in% colnames(neurons_list[,]))
      out <- neurons_list[,feature]
    else
      stop(glue("`{feature}` column does not exists in neurons metadata"))
  } else if (feat_type == "function") {
    out <- sapply(neurons_list, feature)
  } else {
    stop(glue("Not recognised type (`{feat_type}`) of feature: `{feature}`"))
  }
  if (isTRUE(as_factor))
    out <- as.factor(out)
  out
}


#' Make feature names
#'
#' @param features_list list with feature names of functions that
#' generate them
#'
#' @return character vector with feature names
.make_feature_names <- function(features_list) {
  feature_names <- names(features_list)
  if (is.null(feature_names))
    return(paste0("F", seq(length(features_list))))
  no_names <- sapply(feature_names, nchar) == 0
  if (any(no_names))
    feature_names[no_names] <- paste0("F", seq(sum(no_names)))
  feature_names
}
