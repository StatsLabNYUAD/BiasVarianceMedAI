#' Create Interactive Bias-Variance Decomposition Plot
#'
#' @param bias_variance_results Results from bias-variance analysis
#' @return Plotly object
#' @export
plot_bias_variance_interactive <- function(bias_variance_results) {
  
  library(plotly)
  library(ggplot2)
  library(dplyr)
  
  # Prepare data
  plot_data <- bias_variance_results %>%
    mutate(
      model = factor(model, levels = c("Simple", "Basic Clinical", 
                                     "Standard Model", "Complex Model", 
                                     "Very Complex")),
      complexity = as.numeric(model)
    ) %>%
    filter(!is.na(bias_squared))
  
  # Create the plot
  p <- ggplot(plot_data, aes(x = complexity)) +
    # Bias component
    geom_col(aes(y = bias_squared, text = paste("Model:", model, 
                                               "<br>Bias²:", round(bias_squared, 4))),
             fill = "steelblue", alpha = 0.7, width = 0.3) +
    
    # Variance component (stacked)
    geom_col(aes(y = variance, text = paste("Model:", model,
                                          "<br>Variance:", round(variance, 4))),
             fill = "lightcoral", alpha = 0.7, width = 0.3,
             position = position_nudge(x = 0.35)) +
    
    # Total error line
    geom_line(aes(y = total_error, text = paste("Model:", model,
                                               "<br>Total Error:", round(total_error, 4))),
              color = "red", size = 1.2, group = 1) +
    geom_point(aes(y = total_error), color = "red", size = 3) +
    
    # Optimal point
    geom_vline(xintercept = which.min(plot_data$total_error), 
               linetype = "dashed", color = "darkgreen", size = 1) +
    
    annotate("text", x = which.min(plot_data$total_error) + 0.5, 
             y = max(plot_data$total_error) * 0.9,
             label = "Optimal\nComplexity", color = "darkgreen", 
             fontface = "bold") +
    
    scale_x_continuous(breaks = 1:5, 
                      labels = c("Simple", "Basic\nClinical", "Standard\nModel", 
                               "Complex\nModel", "Very\nComplex")) +
    
    labs(
      title = "Bias-Variance Tradeoff in Medical AI Models",
      subtitle = "Lower total error indicates better performance",
      x = "Model Complexity",
      y = "Error Component",
      caption = "Blue: Bias² | Red: Variance | Red Line: Total Error"
    ) +
    
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.text.x = element_text(size = 10),
      panel.grid.minor = element_blank()
    )
  
  # Convert to interactive plot
  ggplotly(p, tooltip = "text") %>%
    layout(
      title = list(text = "Bias-Variance Tradeoff in Medical AI Models",
                  font = list(size = 16)),
      annotations = list(
        list(x = 0.5, y = -0.15, text = "Blue: Bias² | Red: Variance | Red Line: Total Error",
             showarrow = FALSE, xref = "paper", yref = "paper",
             font = list(size = 10, color = "gray"))
      )
    )
}

#' Create Clinical Performance Dashboard
#'
#' @param metrics_results Results from simulation
#' @return Plotly object
#' @export
plot_clinical_dashboard <- function(metrics_results) {
  
  library(plotly)
  library(dplyr)
  
  # Calculate summary statistics
  summary_stats <- metrics_results %>%
    filter(!is.na(auc)) %>%
    group_by(model) %>%
    summarise(
      mean_auc = mean(auc, na.rm = TRUE),
      se_auc = sd(auc, na.rm = TRUE) / sqrt(n()),
      mean_sensitivity = mean(sensitivity, na.rm = TRUE),
      se_sensitivity = sd(sensitivity, na.rm = TRUE) / sqrt(n()),
      mean_specificity = mean(specificity, na.rm = TRUE),
      se_specificity = sd(specificity, na.rm = TRUE) / sqrt(n()),
      .groups = 'drop'
    ) %>%
    mutate(
      model = factor(model, levels = c("Simple", "Basic Clinical", 
                                     "Standard Model", "Complex Model", 
                                     "Very Complex"))
    )
  
  # AUC plot
  p1 <- ggplot(summary_stats, aes(x = model, y = mean_auc)) +
    geom_col(fill = "steelblue", alpha = 0.7) +
    geom_errorbar(aes(ymin = mean_auc - 1.96*se_auc, 
                     ymax = mean_auc + 1.96*se_auc),
                 width = 0.2) +
    labs(title = "AUC by Model Complexity", y = "AUC", x = "") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Sensitivity/Specificity plot
  sens_spec_data <- summary_stats %>%
    select(model, mean_sensitivity, mean_specificity) %>%
    tidyr::pivot_longer(cols = c(mean_sensitivity, mean_specificity),
                       names_to = "metric", values_to = "value") %>%
    mutate(metric = ifelse(metric == "mean_sensitivity", "Sensitivity", "Specificity"))
  
  p2 <- ggplot(sens_spec_data, aes(x = model, y = value, fill = metric)) +
    geom_col(position = "dodge", alpha = 0.7) +
    scale_fill_manual(values = c("Sensitivity" = "lightcoral", 
                                "Specificity" = "lightblue")) +
    labs(title = "Sensitivity vs Specificity", y = "Performance", x = "",
         fill = "Metric") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Combine plots
  subplot(ggplotly(p1), ggplotly(p2), nrows = 2) %>%
    layout(title = "Clinical Performance Metrics")
}
