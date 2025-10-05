#' Plot Bias-Variance Decomposition
#'
#' Creates visualization of bias-variance tradeoff from analysis results
#'
#' @param x A bias_variance_analysis object
#' @param ... Additional arguments passed to ggplot
#' @return A ggplot object
#' @export
#' @importFrom ggplot2 ggplot aes geom_col geom_line geom_point labs theme_minimal theme element_text position_nudge scale_x_continuous
plot_bias_variance <- function(x, ...) {
  if (!inherits(x, "bias_variance_analysis")) {
    stop("x must be a bias_variance_analysis object")
  }
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required for plotting. Please install it.")
  }
  
  results <- x$bias_variance
  results$complexity <- 1:nrow(results)
  
  ggplot2::ggplot(results, ggplot2::aes(x = complexity)) +
    ggplot2::geom_col(ggplot2::aes(y = bias_squared), fill = "steelblue", alpha = 0.8, width = 0.35) +
    ggplot2::geom_col(ggplot2::aes(y = variance), fill = "lightcoral", alpha = 0.8, width = 0.35,
                     position = ggplot2::position_nudge(x = 0.4)) +
    ggplot2::geom_line(ggplot2::aes(y = total_error), color = "red", linewidth = 2, group = 1) +
    ggplot2::geom_point(ggplot2::aes(y = total_error), color = "red", size = 4) +
    ggplot2::scale_x_continuous(breaks = 1:nrow(results), labels = results$model) +
    ggplot2::labs(title = "Bias-Variance Analysis",
                 subtitle = "Blue: Bias² | Red: Variance | Red Line: Total Error",
                 x = "Model Complexity", y = "Error Component") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

#' Plot Model Performance Comparison
#'
#' Creates visualization comparing model performance metrics
#'
#' @param x A bias_variance_analysis object
#' @param metric Character string specifying metric to plot ("auc", "mse", "r_squared")
#' @param ... Additional arguments passed to ggplot
#' @return A ggplot object  
#' @export
#' @importFrom ggplot2 ggplot aes geom_col labs theme_minimal
#' @importFrom dplyr group_by summarise across where
plot_model_performance <- function(x, metric = NULL, ...) {
  if (!inherits(x, "bias_variance_analysis")) {
    stop("x must be a bias_variance_analysis object")
  }
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required for plotting. Please install it.")
  }
  
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("dplyr package is required for plotting. Please install it.")
  }
  
  metrics_summary <- x$metrics %>%
    dplyr::group_by(model) %>%
    dplyr::summarise(dplyr::across(dplyr::where(is.numeric), ~ mean(.x, na.rm = TRUE)), .groups = "drop")
  
  # Auto-select metric if not specified
  if (is.null(metric)) {
    if (x$data_info$outcome_type == "binary" && "auc" %in% colnames(metrics_summary)) {
      metric <- "auc"
    } else if ("r_squared" %in% colnames(metrics_summary)) {
      metric <- "r_squared" 
    } else {
      metric <- "mse"
    }
  }
  
  # Create plot based on metric
  if (metric == "auc" && "auc" %in% colnames(metrics_summary)) {
    ggplot2::ggplot(metrics_summary, ggplot2::aes(x = model, y = auc)) +
      ggplot2::geom_col(fill = "navy", alpha = 0.7) +
      ggplot2::labs(title = "Model Performance: AUC", x = "Model", y = "AUC") +
      ggplot2::theme_minimal()
  } else if (metric == "r_squared" && "r_squared" %in% colnames(metrics_summary)) {
    ggplot2::ggplot(metrics_summary, ggplot2::aes(x = model, y = r_squared)) +
      ggplot2::geom_col(fill = "darkgreen", alpha = 0.7) +
      ggplot2::labs(title = "Model Performance: R²", x = "Model", y = "R²") +
      ggplot2::theme_minimal()
  } else {
    ggplot2::ggplot(metrics_summary, ggplot2::aes(x = model, y = mse)) +
      ggplot2::geom_col(fill = "darkred", alpha = 0.7) +
      ggplot2::labs(title = "Model Performance: MSE", x = "Model", y = "Mean Squared Error") +
      ggplot2::theme_minimal()
  }
}

#' Plot method for bias_variance_analysis objects
#' @param x A bias_variance_analysis object
#' @param type Character string: "bias_variance" or "performance"
#' @param ... Additional arguments
#' @export
plot.bias_variance_analysis <- function(x, type = "bias_variance", ...) {
  if (type == "bias_variance") {
    plot_bias_variance(x, ...)
  } else if (type == "performance") {
    plot_model_performance(x, ...)
  } else {
    stop("type must be either \"bias_variance\" or \"performance\"")
  }
}

