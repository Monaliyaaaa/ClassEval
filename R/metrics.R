#' Calculate Accuracy
#'
#' Computes the classification accuracy given true and predicted labels.
#'
#' @param y_true A vector of true labels
#' @param y_pred A vector of predicted labels
#'
#' @return A numeric value representing accuracy
#' @export
accuracy_score <- function(y_true, y_pred) {

  if (length(y_true) != length(y_pred)) {
    stop("y_true and y_pred must have the same length")
  }

  mean(y_true == y_pred)
}


#' Calculate Precision
#'
#' Computes precision for binary classification.
#'
#' @param y_true A vector of true labels (0/1 or factor)
#' @param y_pred A vector of predicted labels (0/1 or factor)
#'
#' @return A numeric value representing precision
#' @export
precision_score <- function(y_true, y_pred) {

  cm <- confusion_matrix(y_true, y_pred)

  tp <- cm[1, 1]
  fp <- cm[2, 1]

  if ((tp + fp) == 0) {
    return(0)
  }

  tp / (tp + fp)
}


#' Calculate Recall
#'
#' Computes recall for binary classification.
#'
#' @param y_true A vector of true labels (0/1 or factor)
#' @param y_pred A vector of predicted labels (0/1 or factor)
#'
#' @return A numeric value representing recall
#' @export
recall_score <- function(y_true, y_pred) {

  if (length(y_true) != length(y_pred)) {
    stop("y_true and y_pred must have the same length")
  }

  tp <- sum(y_true == 1 & y_pred == 1)
  fn <- sum(y_true == 1 & y_pred == 0)

  if ((tp + fn) == 0) {
    return(0)
  }

  tp / (tp + fn)
}


#' Calculate F1 Score
#'
#' Computes F1 score for binary classification.
#'
#' @param y_true A vector of true labels
#' @param y_pred A vector of predicted labels
#'
#' @return A numeric value representing F1 score
#' @export
f1_score <- function(y_true, y_pred) {

  precision <- precision_score(y_true, y_pred)
  recall <- recall_score(y_true, y_pred)

  if ((precision + recall) == 0) {
    return(0)
  }

  2 * precision * recall / (precision + recall)
}


