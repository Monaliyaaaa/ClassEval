#' Train Logistic Regression Model
#'
#' Trains a logistic regression model using glm.
#'
#' @param formula A formula specifying the model
#' @param data A data frame
#'
#' @return A trained logistic regression model
#' @export
train_logistic_model <- function(formula, data) {

  model <- stats::glm(formula, data = data, family = "binomial")

  return(model)
}



#' Train SVM Model
#'
#' Trains a support vector machine model.
#'
#' @param formula A formula specifying the model
#' @param data A data frame
#'
#' @return A trained SVM model
#' @export
train_svm_model <- function(formula, data) {

  response_var <- all.vars(formula)[1]

  data[[response_var]] <- as.factor(data[[response_var]])

  model <- e1071::svm(formula, data = data, probability = TRUE)

  return(model)
}


