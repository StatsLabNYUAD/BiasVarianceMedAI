#' Get Model Recommendations
#'
#' Provides recommendations based on bias-variance analysis results
#'
#' @param x A bias_variance_analysis object
#' @return A character vector of recommendations
#' @export
get_model_recommendations <- function(x) {
  if (!inherits(x, "bias_variance_analysis")) {
    stop("x must be a bias_variance_analysis object")
  }
  
  results <- x$bias_variance
  best_model <- results$model[which.min(results$total_error)]
  best_error <- min(results$total_error)
  
  # Calculate relative differences
  results$error_diff <- results$total_error - best_error
  results$bias_ratio <- results$bias_squared / results$variance
  
  recommendations <- c()
  
  # Main recommendation
  recommendations <- c(recommendations, 
    paste("BEST MODEL:", best_model, "provides the optimal bias-variance tradeoff"))
  
  # Bias vs variance analysis
  best_result <- results[results$model == best_model, ]
  if (best_result$bias_squared > best_result$variance) {
    recommendations <- c(recommendations,
      "The optimal model still shows higher bias than variance - consider adding more complexity")
  } else if (best_result$variance > 2 * best_result$bias_squared) {
    recommendations <- c(recommendations,
      "The optimal model shows high variance - consider regularization or more data")
  } else {
    recommendations <- c(recommendations,
      "The optimal model shows good bias-variance balance")
  }
  
  # Model-specific insights
  for (i in 1:nrow(results)) {
    model <- results$model[i]
    if (model != best_model && results$error_diff[i] < 0.1 * best_error) {
      recommendations <- c(recommendations,
        paste(model, "model performs similarly - consider for simpler interpretation"))
    }
  }
  
  # Data size recommendation
  if (x$data_info$n_clean < 1000) {
    recommendations <- c(recommendations,
      "Consider collecting more data to reduce variance and improve model stability")
  }
  
  # Bootstrap stability
  if (x$data_info$n_bootstrap < 50) {
    recommendations <- c(recommendations,
      "Consider increasing bootstrap samples for more stable estimates")
  }
  
  return(recommendations)
}

#' Summary method for bias_variance_analysis
#' @param object A bias_variance_analysis object
#' @param ... Additional arguments (ignored)
#' @export
summary.bias_variance_analysis <- function(object, ...) {
  cat("Bias-Variance Analysis Summary
")
  cat("==============================

")
  
  # Data info
  cat("Dataset Information:
")
  cat(sprintf("- Original size: %d observations
", object$data_info$n_total))
  cat(sprintf("- Analysis size: %d observations (after cleaning)
", object$data_info$n_clean))
  cat(sprintf("- Test set: %d observations
", object$data_info$n_test))
  cat(sprintf("- Outcome type: %s
", object$data_info$outcome_type))
  cat(sprintf("- Bootstrap samples: %d

", object$data_info$n_bootstrap))
  
  # Model comparison
  cat("Model Performance Summary:
")
  results <- object$bias_variance[order(object$bias_variance$total_error), ]
  results$rank <- 1:nrow(results)
  
  cat(sprintf("%-10s %-12s %-12s %-12s %-6s
", 
              "Rank", "Model", "Bias²", "Variance", "Total"))
  cat(sprintf("%-10s %-12s %-12s %-12s %-6s
", 
              "----", "-----", "-----", "--------", "-----"))
  
  for (i in 1:nrow(results)) {
    cat(sprintf("%-10d %-12s %-12.6f %-12.6f %-6.6f
",
                results$rank[i], results$model[i], 
                results$bias_squared[i], results$variance[i], results$total_error[i]))
  }
  
  cat("
Recommendations:
")
  recs <- get_model_recommendations(object)
  for (i in 1:length(recs)) {
    cat(sprintf("%d. %s
", i, recs[i]))
  }
}

