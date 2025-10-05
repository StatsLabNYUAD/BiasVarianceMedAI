#' Bias-Variance Analysis for Medical AI Models
#'
#' Performs bias-variance decomposition analysis on medical prediction models
#' using bootstrap resampling to evaluate model complexity tradeoffs.
#'
#' @param data A data frame containing the dataset for analysis
#' @param outcome Character string specifying the outcome variable name
#' @param predictors_simple Character vector of predictor variables for simple model
#' @param predictors_standard Character vector of predictor variables for standard model  
#' @param predictors_complex Character vector of predictor variables for complex model
#' @param n_bootstrap Integer specifying number of bootstrap samples (default: 50)
#' @param train_split Proportion of data used for training (default: 0.8)
#' @param outcome_type Character string: "binary" or "continuous" (auto-detected if NULL)
#' @param include_interactions Logical: include interaction terms in complex model
#' @param seed Integer for reproducible results
#'
#' @return A list containing bias-variance decomposition results
#' @export
#'
#' @examples
#' # Generate example data
#' data <- generate_lung_cancer_data(n = 500)
#' 
#' # Run bias-variance analysis
#' results <- bias_variance_analysis(
#'   data = data,
#'   outcome = "cancer",
#'   predictors_simple = c("age", "smoking_years"),
#'   predictors_standard = c("age", "smoking_years", "family_history"),
#'   predictors_complex = c("age", "smoking_years", "family_history", "nodule_size")
#' )
#' 
#' print(results)
bias_variance_analysis <- function(data, 
                                  outcome, 
                                  predictors_simple,
                                  predictors_standard, 
                                  predictors_complex,
                                  n_bootstrap = 50,
                                  train_split = 0.8,
                                  outcome_type = NULL,
                                  include_interactions = FALSE,
                                  seed = NULL) {
  
  # Set seed for reproducibility
  if (!is.null(seed)) set.seed(seed)
  
  # Input validation
  if (!is.data.frame(data)) {
    stop("data must be a data.frame")
  }
  
  if (!outcome %in% colnames(data)) {
    stop("outcome variable not found in data")
  }
  
  # Check if predictors exist in data
  all_predictors <- unique(c(predictors_simple, predictors_standard, predictors_complex))
  missing_vars <- setdiff(all_predictors, colnames(data))
  if (length(missing_vars) > 0) {
    stop("Variables not found in data: ", paste(missing_vars, collapse = ", "))
  }
  
  # Auto-detect outcome type if not specified
  if (is.null(outcome_type)) {
    outcome_values <- data[[outcome]]
    unique_vals <- unique(outcome_values)
    
    if (length(unique_vals) == 2 && all(unique_vals %in% c(0, 1))) {
      outcome_type <- "binary"
    } else if (is.numeric(outcome_values) && length(unique_vals) > 10) {
      outcome_type <- "continuous"  
    } else {
      outcome_type <- "binary"  # default assumption
    }
  }
  
  # Clean data - remove missing values
  key_vars <- c(outcome, all_predictors)
  clean_data <- data[, key_vars, drop = FALSE]
  clean_data <- clean_data[complete.cases(clean_data), , drop = FALSE]
  
  if (nrow(clean_data) < 50) {
    stop("Not enough complete observations. Need at least 50 rows.")
  }
  
  # Create model formulas
  simple_formula <- as.formula(paste("`", outcome, "`", " ~ ", 
                                    paste("`", predictors_simple, "`", collapse = " + "), sep = ""))
  
  standard_formula <- as.formula(paste("`", outcome, "`", " ~ ", 
                                      paste("`", predictors_standard, "`", collapse = " + "), sep = ""))
  
  # Complex formula with optional interactions
  if (include_interactions && length(predictors_complex) > 1) {
    if (length(predictors_complex) <= 5) {
      complex_formula <- as.formula(paste("`", outcome, "`", " ~ (", 
                                         paste("`", predictors_complex, "`", collapse = " + "), ")^2", sep = ""))
    } else {
      # Too many variables for full interactions
      complex_formula <- as.formula(paste("`", outcome, "`", " ~ ", 
                                         paste("`", predictors_complex, "`", collapse = " + "), sep = ""))
    }
  } else {
    complex_formula <- as.formula(paste("`", outcome, "`", " ~ ", 
                                       paste("`", predictors_complex, "`", collapse = " + "), sep = ""))
  }
  
  # Set up model functions
  if (outcome_type == "binary") {
    model_family <- binomial
    safe_glm <- function(formula, data) {
      tryCatch({
        model <- glm(formula, data = data, family = model_family)
        if (!model$converged) {
          warning("Model did not converge")
        }
        return(model)
      }, error = function(e) {
        # Fallback to intercept-only model
        fallback_formula <- as.formula(paste("`", outcome, "`", " ~ 1"))
        glm(fallback_formula, data = data, family = model_family)
      })
    }
  } else {
    model_family <- gaussian
    safe_glm <- function(formula, data) {
      tryCatch({
        glm(formula, data = data, family = model_family)
      }, error = function(e) {
        # Fallback to intercept-only model
        fallback_formula <- as.formula(paste("`", outcome, "`", " ~ 1"))
        glm(fallback_formula, data = data, family = model_family)
      })
    }
  }
  
  # Define models
  models <- list(
    "Simple" = function(data) safe_glm(simple_formula, data),
    "Standard" = function(data) safe_glm(standard_formula, data),
    "Complex" = function(data) safe_glm(complex_formula, data)
  )
  
  # Bootstrap analysis
  n_total <- nrow(clean_data)
  train_size <- floor(n_total * train_split)
  
  # Create fixed test set
  test_indices <- sample(1:n_total, min(100, floor(n_total * 0.2)))
  test_data <- clean_data[test_indices, , drop = FALSE]
  
  all_predictions <- list()
  all_metrics <- list()
  
  # Progress tracking
  message("Running ", n_bootstrap, " bootstrap iterations...")
  
  for (boot in 1:n_bootstrap) {
    if (boot %% 10 == 0) message("Bootstrap iteration: ", boot)
    
    # Bootstrap sample from remaining data
    remaining_indices <- setdiff(1:n_total, test_indices)
    train_indices <- sample(remaining_indices, min(train_size, length(remaining_indices)), replace = TRUE)
    train_data <- clean_data[train_indices, , drop = FALSE]
    
    boot_predictions <- list()
    boot_metrics <- list()
    
    for (model_name in names(models)) {
      tryCatch({
        model_fit <- models[[model_name]](train_data)
        
        # Make predictions
        if (outcome_type == "binary") {
          predictions <- predict(model_fit, test_data, type = "response")
          predictions <- pmax(0.001, pmin(0.999, predictions))
        } else {
          predictions <- predict(model_fit, test_data, type = "response")
        }
        
        boot_predictions[[model_name]] <- predictions
        
        # Calculate metrics
        true_values <- test_data[[outcome]]
        if (outcome_type == "binary") {
          # For binary outcomes
          auc <- tryCatch({
            if (length(unique(true_values)) > 1 && !requireNamespace("pROC", quietly = TRUE)) {
              # Simple AUC calculation if pROC not available
              roc_calc <- function(labels, scores) {
                n1 <- sum(labels == 1)
                n0 <- sum(labels == 0)
                if (n1 == 0 || n0 == 0) return(NA)
                
                U <- sum(rank(scores)[labels == 1]) - n1 * (n1 + 1) / 2
                return(U / (n1 * n0))
              }
              roc_calc(true_values, predictions)
            } else if (length(unique(true_values)) > 1) {
              as.numeric(pROC::auc(pROC::roc(true_values, predictions, quiet = TRUE)))
            } else {
              NA
            }
          }, error = function(e) NA)
          
          mse <- mean((predictions - true_values)^2, na.rm = TRUE)
          boot_metrics[[model_name]] <- data.frame(
            bootstrap = boot, model = model_name, auc = auc, mse = mse
          )
        } else {
          # For continuous outcomes
          mse <- mean((predictions - true_values)^2, na.rm = TRUE)
          r_squared <- tryCatch({
            1 - mse / var(true_values, na.rm = TRUE)
          }, error = function(e) NA)
          boot_metrics[[model_name]] <- data.frame(
            bootstrap = boot, model = model_name, mse = mse, r_squared = r_squared
          )
        }
        
      }, error = function(e) {
        # Fallback predictions
        fallback_pred <- if (outcome_type == "binary") 0.5 else mean(train_data[[outcome]], na.rm = TRUE)
        boot_predictions[[model_name]] <- rep(fallback_pred, nrow(test_data))
        
        boot_metrics[[model_name]] <- data.frame(
          bootstrap = boot, model = model_name, 
          mse = NA, auc = if (outcome_type == "binary") NA else NULL,
          r_squared = if (outcome_type == "continuous") NA else NULL
        )
      })
    }
    
    all_predictions[[boot]] <- boot_predictions
    all_metrics[[boot]] <- do.call(rbind, boot_metrics)
  }
  
  # Calculate bias-variance decomposition
  bias_variance_results <- list()
  for (model_name in names(models)) {
    pred_matrix <- sapply(all_predictions, function(x) {
      if (model_name %in% names(x)) {
        return(x[[model_name]])
      } else {
        return(rep(NA, nrow(test_data)))
      }
    })
    
    if (!all(is.na(pred_matrix)) && ncol(pred_matrix) > 1) {
      mean_predictions <- rowMeans(pred_matrix, na.rm = TRUE)
      true_values <- test_data[[outcome]]
      
      # Calculate bias squared
      bias_squared <- mean((mean_predictions - true_values)^2, na.rm = TRUE)
      
      # Calculate variance  
      variance_per_point <- apply(pred_matrix, 1, function(x) var(x, na.rm = TRUE))
      variance <- mean(variance_per_point, na.rm = TRUE)
      
      # Handle NAs
      if (is.na(variance)) variance <- 0
      if (is.na(bias_squared)) bias_squared <- 1
      
      bias_variance_results[[model_name]] <- data.frame(
        model = model_name,
        bias_squared = bias_squared,
        variance = variance,
        total_error = bias_squared + variance
      )
    }
  }
  
  # Combine results
  bias_variance_df <- do.call(rbind, bias_variance_results)
  metrics_df <- do.call(rbind, all_metrics)
  
  # Create result object
  result <- list(
    bias_variance = bias_variance_df,
    metrics = metrics_df,
    formulas = list(
      simple = simple_formula,
      standard = standard_formula, 
      complex = complex_formula
    ),
    data_info = list(
      n_total = nrow(data),
      n_clean = nrow(clean_data),
      n_test = nrow(test_data),
      outcome_type = outcome_type,
      n_bootstrap = n_bootstrap
    ),
    test_data = test_data,
    all_predictions = all_predictions
  )
  
  class(result) <- "bias_variance_analysis"
  
  message("Analysis complete!")
  return(result)
}

#' Print method for bias_variance_analysis
#' @param x A bias_variance_analysis object
#' @param ... Additional arguments (ignored)
#' @export
print.bias_variance_analysis <- function(x, ...) {
  cat("Bias-Variance Analysis Results
")
  cat("==============================

")
  
  cat("Data Information:
")
  cat("- Total observations:", x$data_info$n_total, "
")
  cat("- Clean observations:", x$data_info$n_clean, "
") 
  cat("- Test set size:", x$data_info$n_test, "
")
  cat("- Outcome type:", x$data_info$outcome_type, "
")
  cat("- Bootstrap samples:", x$data_info$n_bootstrap, "

")
  
  cat("Bias-Variance Decomposition:
")
  results_display <- x$bias_variance
  results_display$bias_squared <- round(results_display$bias_squared, 6)
  results_display$variance <- round(results_display$variance, 6)
  results_display$total_error <- round(results_display$total_error, 6)
  results_display$rank <- rank(results_display$total_error)
  
  print(results_display[order(results_display$total_error), ])
  
  best_model <- results_display$model[which.min(results_display$total_error)]
  cat("
Best Model:", best_model, "
")
}

