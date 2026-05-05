
utils::globalVariables(c("Actual", "Predicted", "Freq"))

#' Plot Confusion Matrix
#'
#' Visualizes confusion matrix as a heatmap using ggplot2.
#'
#' @param y_true A vector of true labels
#' @param y_pred A vector of predicted labels
#'
#' @return A ggplot object
#' @export
plot_confusion_matrix <- function(y_true, y_pred) {

  cm <- confusion_matrix(y_true, y_pred)

  df <- as.data.frame(as.table(cm))
  colnames(df) <- c("Actual", "Predicted", "Freq")

  ggplot2::ggplot(df, ggplot2::aes(x = Predicted, y = Actual, fill = Freq)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(ggplot2::aes(label = Freq), size = 5) +
    ggplot2::scale_fill_gradient(low = "lightblue", high = "steelblue") +
    ggplot2::labs(
      title = "Confusion Matrix",
      x = "Predicted Label",
      y = "True Label"
    ) +
    ggplot2::theme_minimal()
}
