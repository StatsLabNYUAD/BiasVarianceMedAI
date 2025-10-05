#' Generate Synthetic Lung Cancer Patient Data
#'
#' Creates realistic patient data for bias-variance analysis
#'
#' @param n_patients Number of patients to generate
#' @param cancer_prevalence Baseline cancer prevalence (0-1)
#' @param noise_level Amount of noise in features (0-1)
#' @return Data frame with patient features and outcomes
#' @export
generate_patient_data <- function(n_patients = 1000, 
                                cancer_prevalence = 0.15,
                                noise_level = 0.3) {
  
  set.seed(42)  # For reproducibility
  
  # Generate patient features
  age <- pmax(18, pmin(100, rnorm(n_patients, 65, 12)))
  smoking_years <- pmax(0, rnorm(n_patients, 20, 15))
  nodule_size <- pmax(0.5, rlnorm(n_patients, 1.5, 0.8))
  ct_density <- rnorm(n_patients, -400, 200)
  family_history <- rbinom(n_patients, 1, 0.15)
  
  # Add noise to features
  if (noise_level > 0) {
    age <- age + rnorm(n_patients, 0, noise_level * 5)
    smoking_years <- pmax(0, smoking_years + rnorm(n_patients, 0, noise_level * 10))
    ct_density <- ct_density + rnorm(n_patients, 0, noise_level * 100)
  }
  
  # True cancer probability (complex relationship)
  linear_predictor <- -3.5 + 
                     0.04 * (age - 65) + 
                     0.025 * smoking_years + 
                     0.6 * log(nodule_size) + 
                     0.001 * (ct_density + 400) +
                     0.8 * family_history +
                     0.02 * age * smoking_years / 100  # Interaction
  
  cancer_prob <- plogis(linear_predictor)
  
  # Adjust for desired prevalence
  cancer_prob <- cancer_prob * (cancer_prevalence / mean(cancer_prob))
  cancer_prob <- pmax(0.01, pmin(0.99, cancer_prob))
  
  cancer <- rbinom(n_patients, 1, cancer_prob)
  
  data.frame(
    patient_id = 1:n_patients,
    age = round(age, 1),
    smoking_years = round(smoking_years, 1),
    nodule_size = round(nodule_size, 2),
    ct_density = round(ct_density, 1),
    family_history = family_history,
    cancer_prob = round(cancer_prob, 4),
    cancer = cancer
  )
}

#' Calculate Performance Metrics
#'
#' @param predictions Model predictions
#' @param test_data Test dataset
#' @return Performance metrics
#' @export
calculate_performance_metrics <- function(predictions, test_data) {
  
  # Convert predictions to binary at 0.5 threshold
  pred_binary <- as.numeric(predictions > 0.5)
  actual <- test_data$cancer
  
  # Calculate confusion matrix components
  tp <- sum(pred_binary == 1 & actual == 1)
  tn <- sum(pred_binary == 0 & actual == 0)
  fp <- sum(pred_binary == 1 & actual == 0)
  fn <- sum(pred_binary == 0 & actual == 1)
  
  # Calculate metrics
  accuracy <- (tp + tn) / length(actual)
  sensitivity <- ifelse(tp + fn > 0, tp / (tp + fn), 0)
  specificity <- ifelse(tn + fp > 0, tn / (tn + fp), 0)
  
  # AUC calculation
  if (length(unique(actual)) > 1) {
    tryCatch({
      roc_obj <- pROC::roc(actual, predictions, quiet = TRUE)
      auc <- as.numeric(pROC::auc(roc_obj))
    }, error = function(e) {
      auc <- 0.5
    })
  } else {
    auc <- 0.5
  }
  
  # Brier Score
  brier_score <- mean((predictions - actual)^2)
  
  # MCC (Matthews Correlation Coefficient)
  mcc_denom <- sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))
  mcc <- ifelse(mcc_denom > 0, (tp * tn - fp * fn) / mcc_denom, 0)
  
  return(data.frame(
    accuracy = accuracy,
    sensitivity = sensitivity,
    specificity = specificity,
    auc = auc,
    brier_score = brier_score,
    mcc = mcc,
    mse = brier_score  # For bias-variance analysis
  ))
}
