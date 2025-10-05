#' Get Models with Different Complexity Levels
#'
#' @return Named list of model functions with increasing complexity
#' @export
get_complexity_models <- function() {
  models <- list(
    # Level 1: High Bias (Underfitting)
    "Simple" = function(data) {
      glm(cancer ~ age, data = data, family = binomial)
    },
    
    # Level 2: Moderate Bias
    "Basic Clinical" = function(data) {
      glm(cancer ~ age + smoking_years, data = data, family = binomial)
    },
    
    # Level 3: Optimal Complexity
    "Standard Model" = function(data) {
      glm(cancer ~ age + smoking_years + log(nodule_size) + 
          ct_density + family_history, 
          data = data, family = binomial)
    },
    
    # Level 4: Moderate Variance
    "Complex Model" = function(data) {
      glm(cancer ~ age + smoking_years + log(nodule_size) + 
          ct_density + family_history +
          age:smoking_years + smoking_years:nodule_size,
          data = data, family = binomial)
    },
    
    # Level 5: High Variance (Overfitting)
    "Very Complex" = function(data) {
      glm(cancer ~ poly(age, 3) + poly(smoking_years, 3) + 
          poly(log(nodule_size), 3) + poly(ct_density, 2) +
          family_history + age:smoking_years + age:nodule_size + 
          age:ct_density + smoking_years:nodule_size + 
          smoking_years:ct_density + nodule_size:ct_density,
          data = data, family = binomial)
    }
  )
  return(models)
}

#' Run Bias-Variance Decomposition
#'
#' @param n_bootstrap Number of bootstrap samples
#' @param n_patients Sample size per bootstrap
#' @param test_size Size of test set
#' @return Bias-variance analysis results
#' @export
run_bias_variance_analysis <- function(n_bootstrap = 50, 
                                     n_patients = 800,
                                     test_size = 200) {
  
  # Generate a fixed test set (no noise)
  test_data <- generate_patient_data(test_size, noise_level = 0)
  
  # Get all models
  models <- get_complexity_models()
  
  # Store predictions for each bootstrap sample
  all_predictions <- list()
  all_metrics <- list()
  
  pb <- txtProgressBar(min = 0, max = n_bootstrap, style = 3)
  
  for (boot in 1:n_bootstrap) {
    setTxtProgressBar(pb, boot)
    
    # Generate bootstrap training data
    train_data <- generate_patient_data(n_patients, noise_level = 0.3)
    
    boot_predictions <- list()
    boot_metrics <- list()
    
    for (model_name in names(models)) {
      tryCatch({
        # Fit model
        model_fit <- models[[model_name]](train_data)
        
        # Predict on test set
        predictions <- predict(model_fit, test_data, type = "response")
        predictions <- pmax(0.001, pmin(0.999, predictions))
        
        # Store predictions
        boot_predictions[[model_name]] <- predictions
        
        # Calculate metrics
        metrics <- calculate_performance_metrics(predictions, test_data)
        metrics$bootstrap <- boot
        metrics$model <- model_name
        boot_metrics[[model_name]] <- metrics
        
      }, error = function(e) {
        # Handle errors
        boot_predictions[[model_name]] <- rep(NA, nrow(test_data))
        boot_metrics[[model_name]] <- data.frame(
          bootstrap = boot, model = model_name,
          mse = NA, accuracy = NA, sensitivity = NA, 
          specificity = NA, auc = NA, brier_score = NA
        )
      })
    }
    
    all_predictions[[boot]] <- boot_predictions
    all_metrics[[boot]] <- do.call(rbind, boot_metrics)
  }
  
  close(pb)
  
  # Calculate bias and variance
  bias_variance_results <- calculate_bias_variance(all_predictions, test_data)
  metrics_results <- do.call(rbind, all_metrics)
  
  return(list(
    bias_variance = bias_variance_results,
    metrics = metrics_results,
    test_data = test_data
  ))
}

#' Calculate Bias and Variance Components
#'
#' @param all_predictions List of prediction matrices
#' @param test_data Test dataset
#' @return Bias-variance decomposition
calculate_bias_variance <- function(all_predictions, test_data) {
  
  models <- names(all_predictions[[1]])
  n_bootstrap <- length(all_predictions)
  
  results <- list()
  
  for (model_name in models) {
    # Extract all predictions for this model
    pred_matrix <- sapply(all_predictions, function(x) x[[model_name]])
    
    if (all(is.na(pred_matrix))) {
      results[[model_name]] <- data.frame(
        model = model_name,
        bias_squared = NA,
        variance = NA,
        total_error = NA
      )
      next
    }
    
    # Calculate mean prediction across bootstrap samples
    mean_predictions <- rowMeans(pred_matrix, na.rm = TRUE)
    
    # True probabilities
    true_probs <- test_data$cancer_prob
    
    # Bias squared: (mean_prediction - true_value)^2
    bias_squared <- mean((mean_predictions - true_probs)^2, na.rm = TRUE)
    
    # Variance: average of (prediction - mean_prediction)^2
    variance_per_point <- apply(pred_matrix, 1, function(x) var(x, na.rm = TRUE))
    variance <- mean(variance_per_point, na.rm = TRUE)
    
    # Total error
    total_error <- bias_squared + variance
    
    results[[model_name]] <- data.frame(
      model = model_name,
      bias_squared = bias_squared,
      variance = variance,
      total_error = total_error
    )
  }
  
  return(do.call(rbind, results))
}
