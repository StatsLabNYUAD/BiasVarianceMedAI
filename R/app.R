#' Launch BiasVarianceMedAI Shiny Application
#'
#' @description Launch the interactive Shiny application for medical AI bias-variance analysis
#' 
#' @return Launches the Shiny app
#' @export
#' 
#' @examples
#' \dontrun{
#' # Launch the app
#' run_app()
#' }
run_app <- function() {
  app_dir <- system.file("shiny-app", package = "BiasVarianceMedAI")
  if (app_dir == "") {
    stop("Could not find app directory. Try re-installing BiasVarianceMedAI")
  }
  shiny::runApp(app_dir, display.mode = "normal")
}

#' Generate Synthetic Lung Cancer Dataset
#'
#' @description Generate a realistic synthetic dataset for lung cancer diagnosis
#' 
#' @param n Number of patients (default: 1000)
#' @param seed Random seed for reproducibility (default: 123)
#' 
#' @return A data frame with lung cancer diagnostic data
#' @export
generate_lung_cancer_data <- function(n = 1000, seed = 123) {
  set.seed(seed)
  
  # Patient demographics
  age <- rnorm(n, 65, 12)
  age <- pmax(30, pmin(90, age))
  
  smoking_years <- pmax(0, rnorm(n, 20, 15))
  family_history <- rbinom(n, 1, 0.15)
  
  # Imaging features
  nodule_size <- rlnorm(n, log(8), 0.8)
  spiculation <- rbinom(n, 1, 0.3)
  
  # Generate cancer diagnosis
  linear_predictor <- -4 + 0.05*age + 0.03*smoking_years + 1.2*family_history + 0.15*log(nodule_size) + 0.8*spiculation
  cancer_prob <- plogis(linear_predictor)
  cancer_diagnosis <- rbinom(n, 1, cancer_prob)
  
  data.frame(
    patient_id = 1:n,
    age = round(age, 1),
    smoking_years = round(smoking_years, 1),
    family_history = family_history,
    nodule_size = round(nodule_size, 2),
    spiculation = spiculation,
    cancer_diagnosis = cancer_diagnosis,
    cancer_probability = round(cancer_prob, 3)
  )
}
