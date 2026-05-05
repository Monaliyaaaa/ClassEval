#' Generate Confusion Matrix
#'
#' Computes confusion matrix for binary classification.
#'
#' @param y_true A vector of true labels (0/1)
#' @param y_pred A vector of predicted labels (0/1)
#'
#' @return A 2x2 matrix with TP, FP, FN, TN
#' @export
confusion_matrix <- function(y_true, y_pred) {

  if (length(y_true) != length(y_pred)) {
    stop("y_true and y_pred must have the same length")
  }

  tp <- sum(y_true == 1 & y_pred == 1)
  tn <- sum(y_true == 0 & y_pred == 0)
  fp <- sum(y_true == 0 & y_pred == 1)
  fn <- sum(y_true == 1 & y_pred == 0)

  matrix(
    c(tp, fp, fn, tn),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(
      "Actual" = c("Positive", "Negative"),
      "Predicted" = c("Positive", "Negative")
    )
  )
}


#' K-Fold Cross Validation
#'
#' Performs k-fold cross validation for a given model.
#'
#' @param formula A formula specifying the model
#' @param data A data frame
#' @param k Number of folds (default = 5)
#' @param model_type "logistic" or "svm"
#'
#' @return A numeric value representing average accuracy
#' @export
k_fold_cv <- function(formula, data, k = 5, model_type = "logistic") {

  n <- nrow(data)

  # 随机打乱
  set.seed(123)
  indices <- sample(1:n)

  folds <- split(indices, cut(seq_along(indices), k, labels = FALSE))

  acc_list <- c()

  for (i in 1:k) {

    test_idx <- folds[[i]]
    train_idx <- setdiff(indices, test_idx)

    train_data <- data[train_idx, ]
    test_data <- data[test_idx, ]

    # 训练模型
    if (model_type == "logistic") {
      model <- train_logistic_model(formula, train_data)
    } else if (model_type == "svm") {
      model <- train_svm_model(formula, train_data)
    } else {
      stop("Unsupported model_type")
    }

    # 预测
    y_true <- test_data[[all.vars(formula)[1]]]
    y_pred <- predict_model(model, test_data)

    # 计算 accuracy
    acc <- accuracy_score(y_true, y_pred)

    acc_list <- c(acc_list, acc)
  }

  mean(acc_list)
}


#' Compare Models
#'
#' Compares logistic regression and SVM using k-fold CV.
#'
#' @param formula A formula specifying the model
#' @param data A data frame
#' @param k Number of folds (default = 5)
#'
#' @return A data frame comparing model performance
#' @export
compare_models <- function(formula, data, k = 5) {

  logistic_acc <- k_fold_cv(formula, data, k, model_type = "logistic")
  svm_acc <- k_fold_cv(formula, data, k, model_type = "svm")

  result <- data.frame(
    Model = c("Logistic Regression", "SVM"),
    Accuracy = c(logistic_acc, svm_acc)
  )

  return(result)
}
