#' Predict Using Model
#'
#' Generates predictions from a trained model.
#'
#' @param model A trained model (logistic or SVM)
#' @param newdata A data frame for prediction
#' @param threshold Classification threshold (default = 0.5)
#'
#' @return A vector of predicted class labels (0/1)
#' @export
predict_model <- function(model, newdata, threshold = 0.5) {

  # logistic regression
  if (inherits(model, "glm")) {

    probs <- stats::predict(model, newdata, type = "response")
    return(ifelse(probs > threshold, 1, 0))

  }

  # SVM
  if (inherits(model, "svm")) {

    preds <- stats::predict(model, newdata)
    return(as.numeric(as.character(preds)))

  }

  stop("Unsupported model type")
}
