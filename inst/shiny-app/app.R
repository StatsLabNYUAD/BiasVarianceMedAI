library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(ggplot2)
library(dplyr)
library(shinyWidgets)
library(shinycssloaders)
library(readxl)
library(tools)
library(pROC)

# Load our package functions
# devtools::load_all()

# Enhanced UI with flexible variables
ui <- dashboardPage(
  dashboardHeader(title = "Flexible Bias-Variance Analysis Tool", titleWidth = 350),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Data & Variables", tabName = "data_setup", icon = icon("database")),
      menuItem("Bias-Variance Analysis", tabName = "bias_variance", icon = icon("chart-line")),
      menuItem("Model Comparison", tabName = "comparison", icon = icon("balance-scale")),
      menuItem("Educational Guide", tabName = "education", icon = icon("graduation-cap"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side { background-color: #f8f9fa; }
        .box { border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
        .btn-primary { background-color: #007bff; border-color: #007bff; }
        .btn-success { background-color: #28a745; border-color: #28a745; }
        .selected-vars { background-color: #e8f4fd; padding: 10px; border-radius: 5px; margin: 5px 0; }
      "))
    ),
    
    tabItems(
      # Data Setup and Variable Selection Tab
      tabItem(tabName = "data_setup",
        fluidRow(
          box(title = "Data Input", status = "primary", solidHeader = TRUE, width = 6,
            h4("Choose Data Source:"),
            
            radioButtons("data_source", "Data Input Method:",
              choices = list(
                "Upload CSV/Excel File" = "upload",
                "Example Medical Dataset" = "example",
                "Generate Synthetic Data" = "synthetic"
              ),
              selected = "upload"
            ),
            
            # File upload interface
            conditionalPanel(
              condition = "input.data_source == 'upload'",
              h4("Upload Your Data"),
              fileInput("data_file", "Choose File:",
                       accept = c(".csv", ".xlsx", ".txt")),
              checkboxInput("header", "Header row", TRUE),
              radioButtons("sep", "Separator:",
                          choices = c(Comma = ",", Semicolon = ";", Tab = "	"),
                          selected = ",")
            ),
            
            # Example dataset interface
            conditionalPanel(
              condition = "input.data_source == 'example'",
              h4("Example Datasets"),
              selectInput("example_dataset", "Choose Dataset:",
                choices = list(
                  "Lung Cancer (Medical)" = "lung_cancer",
                  "Heart Disease" = "heart_disease", 
                  "Diabetes Prediction" = "diabetes",
                  "Iris Classification" = "iris",
                  "Titanic Survival" = "titanic"
                )
              ),
              actionButton("load_example", "Load Example Data", class = "btn-info")
            ),
            
            # Synthetic data interface
            conditionalPanel(
              condition = "input.data_source == 'synthetic'",
              h4("Generate Synthetic Data"),
              numericInput("synth_n", "Number of observations:", value = 1000, min = 100, max = 5000),
              numericInput("synth_p", "Number of predictors:", value = 5, min = 2, max = 20),
              selectInput("synth_outcome_type", "Outcome type:",
                choices = list("Binary (0/1)" = "binary", "Continuous" = "continuous")),
              sliderInput("synth_noise", "Noise level:", min = 0, max = 1, value = 0.3),
              actionButton("generate_synthetic", "Generate Data", class = "btn-warning")
            )
          ),
          
          box(title = "Data Preview", status = "info", solidHeader = TRUE, width = 6,
            conditionalPanel(
              condition = "output.data_loaded",
              h4("Data Summary:"),
              verbatimTextOutput("data_summary"),
              
              h4("Data Preview:"),
              DT::dataTableOutput("data_preview_mini", height = "300px")
            ),
            
            conditionalPanel(
              condition = "!output.data_loaded",
              div(style = "text-align: center; padding: 50px;",
                h3("No Data Loaded"),
                p("Please select a data source and load your data to begin."),
                br(),
                p("The tool will automatically detect variable types and suggest outcome/predictors.")
              )
            )
          )
        ),
        
        # Variable Selection Section
        conditionalPanel(
          condition = "output.data_loaded",
          fluidRow(
            box(title = "Variable Selection & Model Setup", status = "success", solidHeader = TRUE, width = 12,
              fluidRow(
                column(4,
                  h4("Select Outcome Variable"),
                  selectInput("outcome_var", "Outcome Variable:",
                             choices = NULL,
                             selected = NULL),
                  
                  radioButtons("outcome_type", "Outcome Type:",
                              choices = list(
                                "Binary Classification" = "binary",
                                "Continuous Regression" = "continuous"
                              )),
                  
                  conditionalPanel(
                    condition = "input.outcome_type == 'binary'",
                    p("For binary outcomes, the tool will use logistic regression."),
                    div(class = "selected-vars",
                      h5("Outcome Summary:"),
                      verbatimTextOutput("outcome_summary")
                    )
                  ),
                  
                  conditionalPanel(
                    condition = "input.outcome_type == 'continuous'",
                    p("For continuous outcomes, the tool will use linear regression."),
                    div(class = "selected-vars",
                      h5("Outcome Summary:"),
                      verbatimTextOutput("outcome_summary")
                    )
                  )
                ),
                
                column(8,
                  h4("Select Predictor Variables"),
                  p("Choose variables for different model complexity levels:"),
                  
                  fluidRow(
                    column(6,
                      h5("Available Variables:"),
                      div(style = "border: 1px solid #ddd; padding: 10px; height: 200px; overflow-y: auto;",
                        checkboxGroupInput("available_predictors", NULL,
                                         choices = NULL)
                      )
                    ),
                    
                    column(6,
                      h5("Model Complexity Setup:"),
                      
                      div(class = "selected-vars",
                        h6("Simple Model (High Bias):"),
                        selectInput("simple_vars", "Select 1-2 variables:",
                                   choices = NULL, multiple = TRUE)
                      ),
                      
                      div(class = "selected-vars",
                        h6("Standard Model (Balanced):"),
                        selectInput("standard_vars", "Select 3-5 variables:",
                                   choices = NULL, multiple = TRUE)
                      ),
                      
                      div(class = "selected-vars",
                        h6("Complex Model (High Variance):"),
                        checkboxInput("include_interactions", "Include interaction terms", FALSE),
                        checkboxInput("include_polynomials", "Include polynomial terms", FALSE),
                        selectInput("complex_vars", "Select 5+ variables:",
                                   choices = NULL, multiple = TRUE)
                      )
                    )
                  )
                )
              ),
              
              br(),
              div(style = "text-align: center;",
                actionButton("setup_complete", "Setup Complete - Ready for Analysis!", 
                           class = "btn-success btn-lg", width = "300px")
              )
            )
          )
        )
      ),
      
      # Bias-Variance Analysis Tab
      tabItem(tabName = "bias_variance",
        conditionalPanel(
          condition = "!output.setup_complete",
          fluidRow(
            box(title = "Setup Required", status = "warning", solidHeader = TRUE, width = 12,
              div(style = "text-align: center; padding: 50px;",
                h3("Please Complete Data Setup First"),
                p("Go to the Data & Variables tab to:"),
                tags$ol(
                  tags$li("Load your data"),
                  tags$li("Select outcome variable"),
                  tags$li("Choose predictor variables for different complexity levels"),
                  tags$li("Click Setup Complete")
                ),
                br(),
                actionButton("goto_setup", "Go to Data Setup", class = "btn-primary btn-lg")
              )
            )
          )
        ),
        
        conditionalPanel(
          condition = "output.setup_complete",
          fluidRow(
            box(title = "Analysis Parameters", status = "primary", solidHeader = TRUE, width = 4,
              h4("Current Setup:"),
              verbatimTextOutput("current_setup"),
              
              br(),
              h4("Analysis Parameters:"),
              sliderInput("n_bootstrap", "Bootstrap Samples:", 
                         min = 5, max = 100, value = 30, step = 5),
              sliderInput("train_split", "Training Split (%):", 
                         min = 60, max = 90, value = 80),
              
              conditionalPanel(
                condition = "input.data_source == 'synthetic'",
                sliderInput("test_size", "Test Size:", 
                           min = 50, max = 500, value = 200, step = 25)
              ),
              
              br(),
              actionButton("run_analysis", "Run Bias-Variance Analysis", 
                          class = "btn-primary btn-lg", width = "100%"),
              br(), br(),
              
              conditionalPanel(
                condition = "output.analysis_complete",
                downloadButton("download_results", "Download Results", 
                              class = "btn-success", width = "100%")
              )
            ),
            
            box(title = "Analysis Status & Results", status = "info", solidHeader = TRUE, width = 8,
              h4("Status:"),
              verbatimTextOutput("analysis_status"),
              
              conditionalPanel(
                condition = "output.analysis_complete",
                br(),
                h4("Bias-Variance Summary:"),
                DT::dataTableOutput("bias_variance_table")
              )
            )
          ),
          
          conditionalPanel(
            condition = "output.analysis_complete",
            fluidRow(
              box(title = "Bias-Variance Decomposition", status = "primary", 
                  solidHeader = TRUE, width = 6, height = "500px",
                plotOutput("bias_variance_plot", height = "400px")
              ),
              box(title = "Model Performance Comparison", status = "warning", 
                  solidHeader = TRUE, width = 6, height = "500px",
                plotOutput("performance_plot", height = "400px")
              )
            ),
            
            fluidRow(
              box(title = "Detailed Results & Insights", status = "success", solidHeader = TRUE, width = 12,
                verbatimTextOutput("detailed_insights")
              )
            )
          )
        )
      ),
      
      # Model Comparison Tab
      tabItem(tabName = "comparison",
        fluidRow(
          box(title = "Dynamic Model Architecture", status = "primary", solidHeader = TRUE, width = 12,
            conditionalPanel(
              condition = "output.setup_complete",
              h3("Your Custom Model Setup:"),
              verbatimTextOutput("model_formulas")
            ),
            conditionalPanel(
              condition = "!output.setup_complete",
              p("Complete the data setup to see your model architectures here.")
            )
          )
        )
      ),
      
      # Educational Tab
      tabItem(tabName = "education",
        fluidRow(
          box(title = "Understanding Bias-Variance Tradeoff", status = "info", 
              solidHeader = TRUE, width = 12,
            h3("What is the Bias-Variance Tradeoff?"),
            p("The bias-variance tradeoff is fundamental to machine learning and statistical modeling:"),
            
            tags$div(
              style = "background-color: #e7f3ff; padding: 15px; border-radius: 5px; margin: 10px 0;",
              h4("Bias (Underfitting)"),
              p("High bias occurs when a model is too simple to capture the underlying patterns."),
              tags$ul(
                tags$li("May miss important relationships between variables"),
                tags$li("Consistently underperforms across different datasets"),
                tags$li("Example: Using only one variable to predict a complex outcome")
              )
            ),
            
            tags$div(
              style = "background-color: #fff2e7; padding: 15px; border-radius: 5px; margin: 10px 0;",
              h4("Variance (Overfitting)"),
              p("High variance occurs when a model is too complex and learns noise instead of signal."),
              tags$ul(
                tags$li("Performance varies dramatically between datasets"),
                tags$li("May not generalize to new observations"),
                tags$li("Example: Using too many variables or complex interactions")
              )
            ),
            
            tags$div(
              style = "background-color: #e7ffe7; padding: 15px; border-radius: 5px; margin: 10px 0;",
              h4("Optimal Balance"),
              p("The sweet spot minimizes total error = bias² + variance + irreducible error."),
              tags$ul(
                tags$li("Captures important patterns without overfitting"),
                tags$li("Generalizes well to new data"),
                tags$li("Provides consistent, reliable predictions")
              )
            ),
            
            h3("How to Use This Flexible Tool"),
            tags$ol(
              tags$li("Load your data (any CSV/Excel file or use examples)"),
              tags$li("Select your outcome variable (binary or continuous)"),
              tags$li("Choose predictor variables for different complexity levels"),
              tags$li("Run the bias-variance analysis"),
              tags$li("Interpret results in the context of your specific problem"),
              tags$li("Consider the optimal complexity for your use case")
            ),
            
            h3("Applications Across Domains"),
            tags$ul(
              tags$li(strong("Medicine:"), "Disease diagnosis, treatment response, risk prediction"),
              tags$li(strong("Finance:"), "Credit risk, stock prediction, fraud detection"),
              tags$li(strong("Marketing:"), "Customer churn, sales forecasting, A/B testing"),
              tags$li(strong("Engineering:"), "Quality control, failure prediction, optimization"),
              tags$li(strong("Research:"), "Any predictive modeling or classification problem")
            )
          )
        )
      )
    )
  )
)

# Enhanced Server with flexible variables
server <- function(input, output, session) {
  
  # Reactive values for data and setup
  app_data <- reactiveValues(
    raw_data = NULL,
    data_loaded = FALSE,
    setup_complete = FALSE,
    analysis_results = NULL,
    analysis_complete = FALSE,
    outcome_var = NULL,
    outcome_type = NULL,
    model_vars = list(),
    status = "Ready to load data."
  )
  
  # Navigation helper
  observeEvent(input$goto_setup, {
    updateTabItems(session, "sidebar", "data_setup")
  })
  
  # Data loading functions
  observeEvent(input$data_file, {
    req(input$data_file)
    
    tryCatch({
      file_ext <- tools::file_ext(input$data_file$datapath)
      
      if (file_ext == "csv") {
        app_data$raw_data <- read.csv(input$data_file$datapath, 
                                     header = input$header, 
                                     sep = input$sep,
                                     stringsAsFactors = FALSE)
      } else if (file_ext %in% c("xlsx", "xls")) {
        app_data$raw_data <- readxl::read_excel(input$data_file$datapath)
      }
      
      if (nrow(app_data$raw_data) == 0) {
        stop("The uploaded file appears to be empty.")
      }
      
      app_data$data_loaded <- TRUE
      app_data$setup_complete <- FALSE
      
      # Update variable choices
      update_variable_choices()
      
      showNotification("Data uploaded successfully!", type = "message", duration = 3)
      
    }, error = function(e) {
      app_data$data_loaded <- FALSE
      showNotification(paste("Error loading file:", e$message), type = "error", duration = 10)
    })
  })
  
  # Example data loading
  observeEvent(input$load_example, {
    tryCatch({
      app_data$raw_data <- switch(input$example_dataset,
        "lung_cancer" = {
          # Generate lung cancer data
          set.seed(123)
          n <- 1000
          age <- rnorm(n, 65, 10)
          smoking_years <- pmax(0, rnorm(n, 20, 15))
          family_history <- rbinom(n, 1, 0.2)
          nodule_size <- rlnorm(n, 0, 0.5)
          ct_density <- rnorm(n, -200, 150)
          
          # Generate outcome
          linear_pred <- -3 + 0.05*age + 0.03*smoking_years + 1.2*family_history + 0.8*log(nodule_size)
          cancer_prob <- plogis(linear_pred)
          cancer <- rbinom(n, 1, cancer_prob)
          
          data.frame(age, smoking_years, family_history, nodule_size, ct_density, cancer)
        },
        "heart_disease" = {
          # Generate heart disease data
          set.seed(456)
          n <- 800
          age <- rnorm(n, 60, 12)
          cholesterol <- rnorm(n, 220, 40)
          blood_pressure <- rnorm(n, 130, 20)
          exercise_hours <- rpois(n, 3)
          
          linear_pred <- -4 + 0.06*age + 0.01*cholesterol + 0.02*blood_pressure - 0.3*exercise_hours
          heart_disease_prob <- plogis(linear_pred)
          heart_disease <- rbinom(n, 1, heart_disease_prob)
          
          data.frame(age, cholesterol, blood_pressure, exercise_hours, heart_disease)
        },
        "diabetes" = {
          # Generate diabetes data
          set.seed(789)
          n <- 600
          glucose <- rnorm(n, 120, 30)
          bmi <- rnorm(n, 28, 5)
          age <- rnorm(n, 50, 15)
          family_history <- rbinom(n, 1, 0.3)
          
          linear_pred <- -5 + 0.03*glucose + 0.1*bmi + 0.02*age + 1.5*family_history
          diabetes_prob <- plogis(linear_pred)
          diabetes <- rbinom(n, 1, diabetes_prob)
          
          data.frame(glucose, bmi, age, family_history, diabetes)
        },
        "iris" = {
          # Use built-in iris dataset
          iris_data <- iris
          iris_data$species_binary <- as.numeric(iris_data$Species == "setosa")
          iris_data[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "species_binary")]
        },
        "titanic" = {
          # Generate Titanic-like data
          set.seed(101)
          n <- 800
          age <- rnorm(n, 35, 15)
          fare <- rlnorm(n, 3, 1)
          sex <- rbinom(n, 1, 0.5)  # 1 = female
          class <- sample(1:3, n, replace = TRUE, prob = c(0.2, 0.3, 0.5))
          
          linear_pred <- -1 + 0.01*age + 0.0005*fare + 2*sex - 0.8*class
          survived_prob <- plogis(linear_pred)
          survived <- rbinom(n, 1, survived_prob)
          
          data.frame(age, fare, sex, class, survived)
        }
      )
      
      app_data$data_loaded <- TRUE
      app_data$setup_complete <- FALSE
      
      update_variable_choices()
      showNotification("Example dataset loaded!", type = "message", duration = 3)
      
    }, error = function(e) {
      showNotification(paste("Error loading example data:", e$message), type = "error", duration = 5)
    })
  })
  
  # Synthetic data generation
  observeEvent(input$generate_synthetic, {
    tryCatch({
      n <- input$synth_n
      p <- input$synth_p
      
      # Generate predictors
      X <- matrix(rnorm(n * p), n, p)
      colnames(X) <- paste0("X", 1:p)
      
      # Generate outcome
      if (input$synth_outcome_type == "binary") {
        # Binary outcome
        true_coefs <- rnorm(p, 0, 1)
        linear_pred <- X %*% true_coefs
        y_prob <- plogis(linear_pred)
        y <- rbinom(n, 1, y_prob)
        outcome_name <- "Y_binary"
      } else {
        # Continuous outcome
        true_coefs <- rnorm(p, 0, 2)
        y <- X %*% true_coefs + rnorm(n, 0, input$synth_noise * 5)
        outcome_name <- "Y_continuous"
      }
      
      app_data$raw_data <- data.frame(X, outcome = y)
      colnames(app_data$raw_data)[ncol(app_data$raw_data)] <- outcome_name
      
      app_data$data_loaded <- TRUE
      app_data$setup_complete <- FALSE
      
      update_variable_choices()
      showNotification("Synthetic data generated!", type = "message", duration = 3)
      
    }, error = function(e) {
      showNotification(paste("Error generating synthetic data:", e$message), type = "error", duration = 5)
    })
  })
  
  # Function to update variable choices
  update_variable_choices <- function() {
    req(app_data$raw_data)
    
    vars <- colnames(app_data$raw_data)
    
    # Update outcome variable choices
    updateSelectInput(session, "outcome_var", choices = vars)
    
    # Update predictor choices
    updateCheckboxGroupInput(session, "available_predictors", choices = vars)
    updateSelectInput(session, "simple_vars", choices = vars)
    updateSelectInput(session, "standard_vars", choices = vars)
    updateSelectInput(session, "complex_vars", choices = vars)
  }
  
  # Auto-detect outcome type
  observeEvent(input$outcome_var, {
    req(input$outcome_var, app_data$raw_data)
    
    outcome_values <- app_data$raw_data[[input$outcome_var]]
    unique_vals <- unique(outcome_values)
    
    # Auto-detect if binary
    if (length(unique_vals) == 2 && all(unique_vals %in% c(0, 1))) {
      updateRadioButtons(session, "outcome_type", selected = "binary")
    } else if (is.numeric(outcome_values) && length(unique_vals) > 10) {
      updateRadioButtons(session, "outcome_type", selected = "continuous")
    }
  })
  
  # Setup completion
  observeEvent(input$setup_complete, {
    req(input$outcome_var)
    
    # Check if variables are selected
    if (is.null(input$simple_vars) || length(input$simple_vars) == 0) {
      showNotification("Please select variables for Simple Model", type = "warning", duration = 5)
      return()
    }
    
    if (is.null(input$standard_vars) || length(input$standard_vars) == 0) {
      showNotification("Please select variables for Standard Model", type = "warning", duration = 5)
      return()
    }
    
    app_data$outcome_var <- input$outcome_var
    app_data$outcome_type <- input$outcome_type
    app_data$model_vars <- list(
      simple = input$simple_vars,
      standard = input$standard_vars,
      complex = if(is.null(input$complex_vars) || length(input$complex_vars) == 0) input$standard_vars else input$complex_vars,
      include_interactions = input$include_interactions,
      include_polynomials = input$include_polynomials
    )
    
    app_data$setup_complete <- TRUE
    showNotification("Setup completed! Ready for analysis.", type = "message", duration = 3)
  })
  
  # Main analysis - FIXED VERSION
  observeEvent(input$run_analysis, {
    req(app_data$setup_complete)
    
    app_data$status <- "Starting bias-variance analysis..."
    
    withProgress(message = "Running Analysis...", value = 0, {
      
      tryCatch({
        data <- app_data$raw_data
        outcome_var <- app_data$outcome_var
        outcome_type <- app_data$outcome_type
        
        # Clean and prepare data - FIXED
        incProgress(0.1, detail = "Preparing data...")
        
        # Remove rows with missing values in key variables
        key_vars <- unique(c(outcome_var, 
                            app_data$model_vars$simple,
                            app_data$model_vars$standard,
                            app_data$model_vars$complex))
        
        # Filter to only include key variables and remove missing values
        clean_data <- data[, key_vars, drop = FALSE]
        clean_data <- clean_data[complete.cases(clean_data), , drop = FALSE]
        
        if (nrow(clean_data) < 50) {
          stop("Not enough complete observations after removing missing values. Need at least 50 rows.")
        }
        
        # Ensure numeric variables are properly formatted
        for (col in colnames(clean_data)) {
          if (col != outcome_var && !is.numeric(clean_data[[col]])) {
            # Try to convert to numeric, if fails keep as factor
            clean_data[[col]] <- tryCatch({
              as.numeric(clean_data[[col]])
            }, error = function(e) {
              as.factor(clean_data[[col]])
            })
          }
        }
        
        # Split data
        n_total <- nrow(clean_data)
        train_size <- floor(n_total * input$train_split / 100)
        
        # Create model formulas - FIXED
        incProgress(0.1, detail = "Setting up models...")
        
        # Ensure we have valid variable names
        simple_vars <- intersect(app_data$model_vars$simple, colnames(clean_data))
        standard_vars <- intersect(app_data$model_vars$standard, colnames(clean_data))
        complex_vars <- intersect(app_data$model_vars$complex, colnames(clean_data))
        
        if (length(simple_vars) == 0 || length(standard_vars) == 0) {
          stop("Selected variables not found in data. Please check variable selection.")
        }
        
        # Create safe formulas
        simple_formula <- as.formula(paste("`", outcome_var, "`", " ~ ", 
                                          paste("`", simple_vars, "`", collapse = " + ", sep = ""), sep = ""))
        standard_formula <- as.formula(paste("`", outcome_var, "`", " ~ ", 
                                            paste("`", standard_vars, "`", collapse = " + ", sep = ""), sep = ""))
        
        # Complex formula - be more careful with interactions
        if (length(complex_vars) > 1 && app_data$model_vars$include_interactions) {
          # Limit interactions to avoid overly complex models
          if (length(complex_vars) <= 5) {
            complex_formula <- as.formula(paste("`", outcome_var, "`", " ~ (", 
                                               paste("`", complex_vars, "`", collapse = " + ", sep = ""), ")^2", sep = ""))
          } else {
            # Too many variables for full interactions, just use main effects
            complex_formula <- as.formula(paste("`", outcome_var, "`", " ~ ", 
                                               paste("`", complex_vars, "`", collapse = " + ", sep = ""), sep = ""))
          }
        } else {
          complex_formula <- as.formula(paste("`", outcome_var, "`", " ~ ", 
                                             paste("`", complex_vars, "`", collapse = " + ", sep = ""), sep = ""))
        }
        
        # Choose model family and create safe model functions - FIXED
        if (outcome_type == "binary") {
          model_family <- binomial
          safe_glm <- function(formula, data) {
            tryCatch({
              # Check if we have variation in outcome
              if (length(unique(data[[outcome_var]])) < 2) {
                stop("No variation in outcome variable")
              }
              
              # Fit model
              model <- glm(formula, data = data, family = model_family)
              
              # Check convergence
              if (!model$converged) {
                warning("Model did not converge")
              }
              
              return(model)
            }, error = function(e) {
              # Return a simple intercept-only model as fallback
              fallback_formula <- as.formula(paste("`", outcome_var, "`", " ~ 1", sep = ""))
              glm(fallback_formula, data = data, family = model_family)
            })
          }
        } else {
          model_family <- gaussian
          safe_glm <- function(formula, data) {
            tryCatch({
              model <- glm(formula, data = data, family = model_family)
              return(model)
            }, error = function(e) {
              # Return a simple intercept-only model as fallback
              fallback_formula <- as.formula(paste("`", outcome_var, "`", " ~ 1", sep = ""))
              glm(fallback_formula, data = data, family = model_family)
            })
          }
        }
        
        models <- list(
          "Simple" = function(data) safe_glm(simple_formula, data),
          "Standard" = function(data) safe_glm(standard_formula, data),
          "Complex" = function(data) safe_glm(complex_formula, data)
        )
        
        # Bootstrap analysis - FIXED
        all_predictions <- list()
        all_metrics <- list()
        
        # Create a fixed test set for consistent bias-variance calculation
        test_indices <- sample(1:n_total, min(100, floor(n_total * 0.2)))
        test_data <- clean_data[test_indices, , drop = FALSE]
        
        for (boot in 1:input$n_bootstrap) {
          incProgress(0.7 / input$n_bootstrap, detail = paste("Bootstrap", boot, "of", input$n_bootstrap))
          
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
              
              # Ensure predictions have the right length
              if (length(predictions) != nrow(test_data)) {
                predictions <- rep(mean(train_data[[outcome_var]], na.rm = TRUE), nrow(test_data))
              }
              
              boot_predictions[[model_name]] <- predictions
              
              # Calculate metrics
              true_values <- test_data[[outcome_var]]
              if (outcome_type == "binary") {
                auc <- tryCatch({
                  if (length(unique(true_values)) > 1) {
                    pROC::auc(pROC::roc(true_values, predictions, quiet = TRUE))
                  } else {
                    NA
                  }
                }, error = function(e) NA)
                mse <- mean((predictions - true_values)^2, na.rm = TRUE)
                boot_metrics[[model_name]] <- data.frame(
                  bootstrap = boot, model = model_name, auc = as.numeric(auc), mse = mse
                )
              } else {
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
              if (outcome_type == "binary") {
                boot_predictions[[model_name]] <- rep(0.5, nrow(test_data))
                boot_metrics[[model_name]] <- data.frame(
                  bootstrap = boot, model = model_name, auc = NA, mse = NA
                )
              } else {
                boot_predictions[[model_name]] <- rep(mean(train_data[[outcome_var]], na.rm = TRUE), nrow(test_data))
                boot_metrics[[model_name]] <- data.frame(
                  bootstrap = boot, model = model_name, mse = NA, r_squared = NA
                )
              }
            })
          }
          
          all_predictions[[boot]] <- boot_predictions
          all_metrics[[boot]] <- do.call(rbind, boot_metrics)
        }
        
        incProgress(0.1, detail = "Calculating bias-variance decomposition...")
        
        # Calculate bias-variance decomposition - FIXED
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
            true_values <- test_data[[outcome_var]]
            
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
        
        if (length(bias_variance_results) == 0) {
          stop("Unable to calculate bias-variance decomposition. Check your data and variable selection.")
        }
        
        app_data$analysis_results <- list(
          bias_variance = do.call(rbind, bias_variance_results),
          metrics = do.call(rbind, all_metrics),
          formulas = list(simple = simple_formula, standard = standard_formula, complex = complex_formula)
        )
        
        app_data$analysis_complete <- TRUE
        app_data$status <- "Analysis complete!"
        
        showNotification("Analysis completed successfully!", type = "message", duration = 3)
        
      }, error = function(e) {
        app_data$status <- paste("Error during analysis:", e$message)
        showNotification(paste("Analysis error:", e$message), type = "error", duration = 10)
      })
    })
  })
  
  # Outputs
  output$data_loaded <- reactive({
    return(app_data$data_loaded)
  })
  outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)
  
  output$setup_complete <- reactive({
    return(app_data$setup_complete)
  })
  outputOptions(output, "setup_complete", suspendWhenHidden = FALSE)
  
  output$analysis_complete <- reactive({
    return(app_data$analysis_complete)
  })
  outputOptions(output, "analysis_complete", suspendWhenHidden = FALSE)
  
  output$data_summary <- renderText({
    req(app_data$raw_data)
    
    data <- app_data$raw_data
    paste(
      "DATASET SUMMARY
",
      "===============
",
      "Rows:", nrow(data), "
",
      "Columns:", ncol(data), "
",
      "Column Names:", paste(colnames(data), collapse = ", "), "
",
      "Missing Values:", sum(is.na(data))
    )
  })
  
  output$data_preview_mini <- DT::renderDataTable({
    req(app_data$raw_data)
    
    DT::datatable(head(app_data$raw_data, 50), 
                 options = list(scrollX = TRUE, pageLength = 5, dom = "tp"))
  })
  
  output$outcome_summary <- renderText({
    req(input$outcome_var, app_data$raw_data)
    
    outcome_values <- app_data$raw_data[[input$outcome_var]]
    
    if (input$outcome_type == "binary") {
      table_summary <- table(outcome_values)
      paste(
        "Binary Outcome Summary:
",
        paste(names(table_summary), ":", table_summary, collapse = "
"),
        "
Prevalence:", round(mean(outcome_values, na.rm = TRUE) * 100, 1), "%"
      )
    } else {
      paste(
        "Continuous Outcome Summary:
",
        "Mean:", round(mean(outcome_values, na.rm = TRUE), 3), "
",
        "SD:", round(sd(outcome_values, na.rm = TRUE), 3), "
",
        "Range:", round(min(outcome_values, na.rm = TRUE), 3), "-", round(max(outcome_values, na.rm = TRUE), 3)
      )
    }
  })
  
  output$current_setup <- renderText({
    req(app_data$setup_complete)
    
    paste(
      "CURRENT ANALYSIS SETUP
",
      "=====================
",
      "Outcome:", app_data$outcome_var, "(", app_data$outcome_type, ")
",
      "Simple Model:", paste(app_data$model_vars$simple, collapse = ", "), "
",
      "Standard Model:", paste(app_data$model_vars$standard, collapse = ", "), "
",
      "Complex Model:", paste(app_data$model_vars$complex, collapse = ", "), "
",
      "Interactions:", ifelse(app_data$model_vars$include_interactions, "Yes", "No"), "
",
      "Polynomials:", ifelse(app_data$model_vars$include_polynomials, "Yes", "No")
    )
  })
  
  output$analysis_status <- renderText({
    app_data$status
  })
  
  output$bias_variance_table <- DT::renderDataTable({
    req(app_data$analysis_results)
    
    results <- app_data$analysis_results$bias_variance %>%
      mutate(
        Model = model,
        `Bias²` = round(bias_squared, 6),
        `Variance` = round(variance, 6),
        `Total Error` = round(total_error, 6),
        `Rank` = rank(total_error)
      ) %>%
      select(Rank, Model, `Bias²`, Variance, `Total Error`) %>%
      arrange(`Total Error`)
    
    DT::datatable(results, options = list(dom = "t"), rownames = FALSE)
  })
  
  output$bias_variance_plot <- renderPlot({
    req(app_data$analysis_results)
    
    results <- app_data$analysis_results$bias_variance
    results$complexity <- 1:nrow(results)
    
    ggplot(results, aes(x = complexity)) +
      geom_col(aes(y = bias_squared), fill = "steelblue", alpha = 0.8, width = 0.35) +
      geom_col(aes(y = variance), fill = "lightcoral", alpha = 0.8, width = 0.35,
               position = position_nudge(x = 0.4)) +
      geom_line(aes(y = total_error), color = "red", linewidth = 2, group = 1) +
      geom_point(aes(y = total_error), color = "red", size = 4) +
      scale_x_continuous(breaks = 1:nrow(results), labels = results$model) +
      labs(title = paste("Bias-Variance Analysis:", app_data$outcome_var),
           subtitle = "Blue: Bias² | Red: Variance | Red Line: Total Error",
           x = "Model Complexity", y = "Error Component") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$performance_plot <- renderPlot({
    req(app_data$analysis_results)
    
    metrics_summary <- app_data$analysis_results$metrics %>%
      group_by(model) %>%
      summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), .groups = "drop")
    
    if (app_data$outcome_type == "binary" && "auc" %in% colnames(metrics_summary)) {
      ggplot(metrics_summary, aes(x = model, y = auc)) +
        geom_col(fill = "navy", alpha = 0.7) +
        labs(title = "Model Performance: AUC", x = "Model", y = "AUC") +
        theme_minimal()
    } else if ("r_squared" %in% colnames(metrics_summary)) {
      ggplot(metrics_summary, aes(x = model, y = r_squared)) +
        geom_col(fill = "darkgreen", alpha = 0.7) +
        labs(title = "Model Performance: R²", x = "Model", y = "R²") +
        theme_minimal()
    } else {
      ggplot(metrics_summary, aes(x = model, y = mse)) +
        geom_col(fill = "darkred", alpha = 0.7) +
        labs(title = "Model Performance: MSE", x = "Model", y = "Mean Squared Error") +
        theme_minimal()
    }
  })
  
  output$model_formulas <- renderText({
    req(app_data$analysis_results)
    
    formulas <- app_data$analysis_results$formulas
    paste(
      "MODEL FORMULAS
",
      "==============
",
      "Simple Model:
", deparse(formulas$simple), "

",
      "Standard Model:
", deparse(formulas$standard), "

",
      "Complex Model:
", deparse(formulas$complex)
    )
  })
  
  output$detailed_insights <- renderText({
    req(app_data$analysis_results)
    
    results <- app_data$analysis_results$bias_variance
    best_model <- results$model[which.min(results$total_error)]
    
    paste(
      "DETAILED ANALYSIS INSIGHTS
",
      "==========================
",
      "Dataset:", nrow(app_data$raw_data), "observations
",
      "Outcome Variable:", app_data$outcome_var, "(", app_data$outcome_type, ")
",
      "Best Performing Model:", best_model, "
",
      "Lowest Total Error:", round(min(results$total_error), 6), "

",
      "Recommendations:
",
      "- The", best_model, "model provides the best bias-variance tradeoff
",
      "- Consider this model for prediction on new data
",
      "- Validate results on external datasets before deployment"
    )
  })
  
  # Download handler
  output$download_results <- downloadHandler(
    filename = function() {
      paste("bias_variance_analysis_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      if (!is.null(app_data$analysis_results)) {
        write.csv(app_data$analysis_results$bias_variance, file, row.names = FALSE)
      }
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)

