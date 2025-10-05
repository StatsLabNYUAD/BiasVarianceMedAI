library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)

# Load our package functions
devtools::load_all()

ui <- dashboardPage(
  dashboardHeader(title = "Bias-Variance Debug"),
  dashboardSidebar(
    sliderInput("n_bootstrap", "Bootstrap Samples:", min = 5, max = 20, value = 10),
    sliderInput("n_patients", "Training Size:", min = 100, max = 500, value = 200),
    actionButton("run_analysis", "Run Analysis", class = "btn-primary")
  ),
  dashboardBody(
    fluidRow(
      box(title = "Status", status = "info", solidHeader = TRUE, width = 12,
        verbatimTextOutput("status_output")
      )
    ),
    fluidRow(
      box(title = "Results", status = "primary", solidHeader = TRUE, width = 6,
        verbatimTextOutput("results_output")
      ),
      box(title = "Plot", status = "success", solidHeader = TRUE, width = 6,
        plotOutput("results_plot")
      )
    )
  )
)

server <- function(input, output, session) {
  
  values <- reactiveValues(
    status = "Ready to run analysis...",
    results = NULL
  )
  
  # Update status display
  output$status_output <- renderText({
    values$status
  })
  
  observeEvent(input$run_analysis, {
    
    values$status <- "Starting analysis..."
    
    tryCatch({
      
      values$status <- "Generating test data..."
      test_data <- generate_patient_data(100, cancer_prevalence = 0.15, noise_level = 0)
      
      values$status <- paste("Generated test data with", nrow(test_data), "patients")
      
      # Simple models
      models <- list(
        "Simple" = function(data) {
          values$status <<- "Fitting Simple model..."
          glm(cancer ~ age, data = data, family = binomial)
        },
        "Standard" = function(data) {
          values$status <<- "Fitting Standard model..."
          glm(cancer ~ age + smoking_years, data = data, family = binomial)
        }
      )
      
      values$status <- "Running bootstrap analysis..."
      
      # Bootstrap analysis
      all_predictions <- list()
      for (boot in 1:input$n_bootstrap) {
        values$status <- paste("Bootstrap", boot, "of", input$n_bootstrap)
        
        # Generate training data
        train_data <- generate_patient_data(input$n_patients, 
                                          cancer_prevalence = 0.15, 
                                          noise_level = 0.3)
        
        boot_pred <- list()
        for (model_name in names(models)) {
          model_fit <- models[[model_name]](train_data)
          pred <- predict(model_fit, test_data, type = "response")
          boot_pred[[model_name]] <- pred
        }
        all_predictions[[boot]] <- boot_pred
      }
      
      values$status <- "Calculating bias and variance..."
      
      # Calculate bias-variance
      bias_variance_results <- list()
      for (model_name in names(models)) {
        pred_matrix <- sapply(all_predictions, function(x) x[[model_name]])
        mean_pred <- rowMeans(pred_matrix)
        true_prob <- test_data$cancer_prob
        
        bias_sq <- mean((mean_pred - true_prob)^2)
        variance <- mean(apply(pred_matrix, 1, var))
        
        bias_variance_results[[model_name]] <- data.frame(
          model = model_name,
          bias_squared = bias_sq,
          variance = variance,
          total_error = bias_sq + variance
        )
      }
      
      values$results <- do.call(rbind, bias_variance_results)
      values$status <- "Analysis complete!"
      
    }, error = function(e) {
      values$status <- paste("ERROR:", e$message)
    })
  })
  
  # Results output
  output$results_output <- renderText({
    if (is.null(values$results)) {
      return("No results yet. Click 'Run Analysis' to start.")
    }
    
    paste(
      "BIAS-VARIANCE RESULTS:\n",
      "=====================\n",
      paste(capture.output(print(values$results, row.names = FALSE)), collapse = "\n")
    )
  })
  
  # Results plot
  output$results_plot <- renderPlot({
    req(values$results)
    
    results <- values$results
    results$complexity <- 1:nrow(results)
    
    ggplot(results, aes(x = complexity)) +
      geom_col(aes(y = bias_squared), fill = "blue", alpha = 0.7, width = 0.3) +
      geom_col(aes(y = variance), fill = "red", alpha = 0.7, width = 0.3,
               position = position_nudge(x = 0.35)) +
      geom_line(aes(y = total_error), color = "black", linewidth = 2, group = 1) +
      geom_point(aes(y = total_error), color = "black", size = 4) +
      scale_x_continuous(breaks = 1:nrow(results), labels = results$model) +
      labs(title = "Bias-Variance Tradeoff", 
           subtitle = "Blue: Bias² | Red: Variance | Black: Total Error",
           x = "Model", y = "Error") +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)
