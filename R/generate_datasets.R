#' Generate Realistic Lung Cancer Dataset
#'
#' Creates a synthetic lung cancer dataset with realistic patient characteristics
#' and outcomes for bias-variance analysis demonstrations.
#'
#' @param n_patients Integer specifying number of patients to generate (default: 1000)
#' @param scenario Character string: "screening", "high_risk", or "symptomatic"
#' @param seed Integer for reproducible results (default: NULL)
#'
#' @return A data frame with patient characteristics and cancer outcomes
#' @export
#'
#' @examples
#' # Generate standard screening dataset
#' data <- generate_lung_cancer_data(n_patients = 500)
#' head(data)
#' 
#' # Generate high-risk population
#' high_risk_data <- generate_lung_cancer_data(n_patients = 300, scenario = "high_risk")
generate_lung_cancer_data <- function(n_patients = 1000, scenario = "screening", seed = NULL) {
  
  # Set seed if provided
  if (!is.null(seed)) set.seed(seed)
  
  # Validate inputs
  if (n_patients <= 0) stop("n_patients must be positive")
  if (!scenario %in% c("screening", "high_risk", "symptomatic")) {
    stop("scenario must be one of: screening, high_risk, symptomatic")
  }
  
  # Scenario-specific parameters
  params <- switch(scenario,
    "screening" = list(
      age_mean = 62, age_sd = 8, age_min = 50, age_max = 80,
      smoking_mean = 25, smoking_sd = 15,
      cancer_prevalence = 0.12,
      nodule_detection_rate = 0.15
    ),
    "high_risk" = list(
      age_mean = 68, age_sd = 10, age_min = 55, age_max = 85,
      smoking_mean = 35, smoking_sd = 20,
      cancer_prevalence = 0.28,
      nodule_detection_rate = 0.45
    ),
    "symptomatic" = list(
      age_mean = 65, age_sd = 12, age_min = 45, age_max = 90,
      smoking_mean = 30, smoking_sd = 18,
      cancer_prevalence = 0.35,
      nodule_detection_rate = 0.75
    )
  )
  
  # Generate patient demographics
  age <- pmax(params$age_min, pmin(params$age_max,
                                  rnorm(n_patients, params$age_mean, params$age_sd)))
  
  # Smoking history (correlated with age)
  smoking_years <- pmax(0, rnorm(n_patients, params$smoking_mean, params$smoking_sd))
  
  # Some never-smokers
  never_smoker_prob <- 0.15
  smoking_years[rbinom(n_patients, 1, never_smoker_prob) == 1] <- 0
  
  # Family history (genetic component)
  family_history <- rbinom(n_patients, 1, 0.18)
  
  # Generate nodule characteristics (only for patients with detected nodules)
  has_nodule <- rbinom(n_patients, 1, params$nodule_detection_rate)
  
  # Nodule size (log-normal distribution, realistic medical values)
  nodule_size <- ifelse(has_nodule == 1,
                       rlnorm(n_patients, meanlog = 0.8, sdlog = 0.6),
                       NA)
  
  # CT scan density (Hounsfield units)
  ct_density <- ifelse(has_nodule == 1,
                      rnorm(n_patients, mean = -300, sd = 200),
                      NA)
  
  # Previous screening history
  prior_screening <- rbinom(n_patients, 1, 0.6)
  
  # Respiratory symptoms
  symptoms_present <- rbinom(n_patients, 1, 
                           ifelse(scenario == "symptomatic", 0.8, 0.1))
  
  # Generate cancer outcome using realistic logistic model
  # Base risk from age and smoking
  log_odds <- -8 + 
    0.08 * age +                          # Age effect
    0.04 * smoking_years +                # Smoking pack-years effect
    1.2 * family_history +                # Family history effect
    0.8 * symptoms_present +              # Symptoms effect
    ifelse(is.na(nodule_size), 0,         # Nodule size effect
           0.5 * log(pmax(nodule_size, 0.1))) +
    ifelse(is.na(ct_density), 0,          # CT density effect
           0.001 * pmax(ct_density + 400, 0)) +
    -0.3 * prior_screening                # Previous screening (protective)
  
  # Convert to probability and adjust for desired prevalence
  cancer_prob <- plogis(log_odds)
  
  # Calibrate to desired prevalence
  current_mean <- mean(cancer_prob)
  target_mean <- params$cancer_prevalence
  adjustment <- log(target_mean / (1 - target_mean)) - log(current_mean / (1 - current_mean))
  adjusted_prob <- plogis(log_odds + adjustment)
  
  # Generate binary cancer outcome
  cancer <- rbinom(n_patients, 1, adjusted_prob)
  
  # Create patient IDs
  patient_id <- sprintf("P%04d", 1:n_patients)
  
  # Combine into data frame
  data.frame(
    patient_id = patient_id,
    age = round(age, 1),
    smoking_years = round(smoking_years, 1),
    family_history = as.factor(family_history),
    has_nodule = as.factor(has_nodule),
    nodule_size = round(nodule_size, 2),
    ct_density = round(ct_density, 0),
    prior_screening = as.factor(prior_screening),
    symptoms_present = as.factor(symptoms_present),
    cancer = as.factor(cancer),
    scenario = scenario,
    stringsAsFactors = FALSE
  )
}

#' Generate Multiple Medical Datasets
#'
#' Creates multiple lung cancer datasets for comparison and testing
#'
#' @param seed Integer for reproducible results (default: 2024)
#' @return A list containing multiple datasets
#' @export
#'
#' @examples
#' datasets <- generate_medical_datasets()
#' names(datasets)
generate_medical_datasets <- function(seed = 2024) {
  
  if (!is.null(seed)) set.seed(seed)
  
  datasets <- list(
    screening = generate_lung_cancer_data(1000, "screening", seed = seed),
    high_risk = generate_lung_cancer_data(500, "high_risk", seed = seed + 1),
    symptomatic = generate_lung_cancer_data(750, "symptomatic", seed = seed + 2)
  )
  
  return(datasets)
}

#' Export Medical Data to File
#'
#' Saves generated medical datasets to CSV files
#'
#' @param data A data frame to export
#' @param filename Character string specifying output filename
#' @param path Character string specifying output directory (default: current directory)
#'
#' @return Invisibly returns the file path
#' @export
#'
#' @examples
#' data <- generate_lung_cancer_data(100)
#' export_medical_data(data, "lung_cancer_data.csv")
export_medical_data <- function(data, filename, path = ".") {
  
  # Validate inputs
  if (!is.data.frame(data)) {
    stop("data must be a data.frame")
  }
  
  if (missing(filename)) {
    stop("filename must be specified")
  }
  
  # Ensure filename has CSV extension
  if (!grepl("[.]csv$", filename)) {
    filename <- paste0(filename, ".csv")
  }
  
  # Create full path
  full_path <- file.path(path, filename)
  
  # Create directory if it does not exist
  dir.create(dirname(full_path), recursive = TRUE, showWarnings = FALSE)
  
  # Write file
  tryCatch({
    utils::write.csv(data, full_path, row.names = FALSE)
    message("Data exported successfully to: ", full_path)
    message("Rows: ", nrow(data), ", Columns: ", ncol(data))
    
    # Show summary
    if ("cancer" %in% colnames(data)) {
      cancer_rate <- round(mean(as.numeric(as.character(data$cancer)), na.rm = TRUE) * 100, 1)
      message("Cancer prevalence: ", cancer_rate, "%")
    }
    
    return(invisible(full_path))
    
  }, error = function(e) {
    stop("Failed to export data: ", e$message)
  })
}

