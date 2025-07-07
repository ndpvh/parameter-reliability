# PURPOSE: General utilities and helper functions for parameter reliability study
# AUTHORS: Kenny Yu & Niels Vanhasbroeck
# DATE: July 2025

# Check required packages 
check_packages <- function(required_packages = c("parallel"), 
                           optional_packages = c("ggplot2")) {
  
  # Check required packages
  missing_required <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  if(length(missing_required) > 0) {
    message("Installing required packages: ", paste(missing_required, collapse = ", "))
    install.packages(missing_required)
  }
  
  # Check optional packages
  missing_optional <- optional_packages[!sapply(optional_packages, requireNamespace, quietly = TRUE)]
  if(length(missing_optional) > 0) {
    message("Optional packages missing: ", paste(missing_optional, collapse = ", "))
    message("Install for full functionality: install.packages(c('", 
            paste(missing_optional, collapse = "','"), "'))")
  }
  
  invisible(TRUE)
}

# Load all framework scripts
load_framework <- function(script_dir = ".", scripts = c("model.R", "simulate.R", "fitting.R", 
                                                         "reliability.R", "compensatory.R", "testretest.R")) {
  
  message("Loading parameter reliability framework...")
  
  for(script in scripts) {
    script_path <- file.path(script_dir, script)
    if(file.exists(script_path)) {
      source(script_path)
      message("Loaded: ", script)
    } else {
      warning("Missing script: ", script_path)
    }
  }
  
  # Check packages
  check_packages()
  
  message("Framework loaded successfully")
  invisible(TRUE)
}

# Extract summary statistics from study results
extract_summary_stats <- function(study_results) {
  
  if(study_results$analysis_type == "compensatory") {
    return(extract_compensatory_summary(study_results))
  } else if(study_results$analysis_type == "test_retest") {
    return(extract_testretest_summary(study_results))
  } else {
    stop("Unknown analysis type: ", study_results$analysis_type)
  }
}

# Extract compensatory study summary
extract_compensatory_summary <- function(study_results) {
  
  # Check for streaming summary first (for large studies)
  if(!is.null(study_results$summary_results)) {
    summary_data <- study_results$summary_results
    
    summary_stats <- list()
    for(model in unique(summary_data$model_comparison)) {
      model_data <- summary_data[summary_data$model_comparison == model, ]
      
      summary_stats[[model]] <- list(
        n_conditions = nrow(model_data),
        mean_diagnostic_power = mean(model_data$diagnostic_power, na.rm = TRUE),
        median_diagnostic_power = median(model_data$diagnostic_power, na.rm = TRUE),
        sd_diagnostic_power = sd(model_data$diagnostic_power, na.rm = TRUE),
        min_diagnostic_power = min(model_data$diagnostic_power, na.rm = TRUE),
        max_diagnostic_power = max(model_data$diagnostic_power, na.rm = TRUE),
        mean_max_bias = mean(model_data$max_bias_diff, na.rm = TRUE),
        mean_max_snr_degrad = mean(model_data$max_snr_degrad, na.rm = TRUE)
      )
    }
    
    return(list(
      summary_data = summary_data,
      summary_stats = summary_stats,
      total_successful = nrow(summary_data),
      analysis_type = "compensatory",
      streaming_summary = TRUE
    ))
  }
  
  # Fallback to full results processing
  results_list <- study_results$results
  successful <- !sapply(results_list, function(x) isTRUE(x$error_occurred))
  results_list <- results_list[successful]
  
  if(length(results_list) == 0) {
    return(list(message = "No successful results to summarize"))
  }
  
  # Extract key metrics
  summary_data <- data.frame(
    model_comparison = character(0),
    sample_size = numeric(0),
    error_sd = numeric(0),
    diagnostic_power = numeric(0),
    stringsAsFactors = FALSE
  )
  
  for(result in results_list) {
    if(!is.null(result$diagnostic_metrics$summary)) {
      diagnostic_power <- result$diagnostic_metrics$summary$study_diagnostic_power
    } else {
      diagnostic_power <- NA
    }
    
    summary_data <- rbind(summary_data, data.frame(
      model_comparison = result$model_comparison,
      sample_size = result$sample_size,
      error_sd = result$condition_params$error_sd,
      diagnostic_power = diagnostic_power,
      stringsAsFactors = FALSE
    ))
  }
  
  # Calculate summary statistics by model comparison
  summary_stats <- list()
  for(model in unique(summary_data$model_comparison)) {
    model_data <- summary_data[summary_data$model_comparison == model, ]
    
    summary_stats[[model]] <- list(
      n_conditions = nrow(model_data),
      mean_diagnostic_power = mean(model_data$diagnostic_power, na.rm = TRUE),
      median_diagnostic_power = median(model_data$diagnostic_power, na.rm = TRUE),
      sd_diagnostic_power = sd(model_data$diagnostic_power, na.rm = TRUE),
      min_diagnostic_power = min(model_data$diagnostic_power, na.rm = TRUE),
      max_diagnostic_power = max(model_data$diagnostic_power, na.rm = TRUE)
    )
  }
  
  return(list(
    summary_data = summary_data,
    summary_stats = summary_stats,
    total_successful = length(results_list),
    analysis_type = "compensatory",
    streaming_summary = FALSE
  ))
}

# Extract test-retest study summary (placeholder for Niels)
extract_testretest_summary <- function(study_results) {
  
  # TODO: Niels to implement test-retest summary extraction
  stop("Test-retest summary extraction not yet implemented. This is Niels' part.")
}

# Print study summary
print_study_summary <- function(summary_results) {
  
  if(!is.null(summary_results$message)) {
    message(summary_results$message)
    return(invisible(NULL))
  }
  
  if(summary_results$analysis_type == "compensatory") {
    
    message("\nCompensatory Analysis Summary")
    message("============================")
    message("Total successful conditions: ", summary_results$total_successful)
    
    if(summary_results$streaming_summary) {
      message("Analysis method: Streaming summary")
    } else {
      message("Analysis method: Full results processing")
    }
    
    for(model in names(summary_results$summary_stats)) {
      stats <- summary_results$summary_stats[[model]]
      message(sprintf("\n%s:", model))
      message(sprintf("  Conditions: %d", stats$n_conditions))
      message(sprintf("  Mean diagnostic power: %.1f%%", stats$mean_diagnostic_power))
      message(sprintf("  Median diagnostic power: %.1f%%", stats$median_diagnostic_power))
      message(sprintf("  Range: %.1f%% - %.1f%%", stats$min_diagnostic_power, stats$max_diagnostic_power))
      
      if(!is.null(stats$mean_max_bias)) {
        message(sprintf("  Mean maximum bias: %.3f", stats$mean_max_bias))
      }
      if(!is.null(stats$mean_max_snr_degrad)) {
        message(sprintf("  Mean maximum SNR degradation: %.3f", stats$mean_max_snr_degrad))
      }
    }
    
  } else if(summary_results$analysis_type == "test_retest") {
    
    # TODO: Niels to implement test-retest summary printing
    message("\nTest-retest summary printing not yet implemented. This is Niels' part.")
    
  }
  
  invisible(summary_results)
}

# Save results summary
save_results_summary <- function(summary_results, output_file) {
  
  # Create summary report
  report <- list(
    summary_date = Sys.time(),
    analysis_type = summary_results$analysis_type,
    total_successful = summary_results$total_successful,
    summary_statistics = summary_results$summary_stats,
    raw_data = summary_results$summary_data,
    streaming_summary = summary_results$streaming_summary
  )
  
  # Save as RData
  save(report, file = output_file)
  message("Summary report saved: ", output_file)
  
  # Also save as CSV if possible
  csv_file <- gsub("\\.RData$", ".csv", output_file)
  if(!is.null(summary_results$summary_data)) {
    write.csv(summary_results$summary_data, csv_file, row.names = FALSE)
    message("Summary data saved: ", csv_file)
  }
  
  invisible(report)
}

# Compare studies
compare_studies <- function(study1_file, study2_file) {
  
  # Load studies
  load(study1_file)
  study1_results <- study_results
  
  load(study2_file)
  study2_results <- study_results
  
  # Extract summaries
  summary1 <- extract_summary_stats(study1_results)
  summary2 <- extract_summary_stats(study2_results)
  
  # Compare if same analysis type
  if(summary1$analysis_type != summary2$analysis_type) {
    message("Cannot compare different analysis types")
    return(invisible(NULL))
  }
  
  message(sprintf("\nStudy Comparison (%s)", summary1$analysis_type))
  message("===================")
  message(sprintf("Study 1: %d successful conditions", summary1$total_successful))
  message(sprintf("Study 2: %d successful conditions", summary2$total_successful))
  
  if(summary1$analysis_type == "compensatory") {
    
    common_models <- intersect(names(summary1$summary_stats), names(summary2$summary_stats))
    
    for(model in common_models) {
      stats1 <- summary1$summary_stats[[model]]
      stats2 <- summary2$summary_stats[[model]]
      
      message(sprintf("\n%s:", model))
      message(sprintf("  Study 1 mean diagnostic power: %.1f%%", stats1$mean_diagnostic_power))
      message(sprintf("  Study 2 mean diagnostic power: %.1f%%", stats2$mean_diagnostic_power))
      message(sprintf("  Difference: %.1f%%", stats2$mean_diagnostic_power - stats1$mean_diagnostic_power))
    }
  }
  
  return(list(summary1 = summary1, summary2 = summary2))
}

# Load streaming summary file
load_streaming_summary <- function(summary_file) {
  
  if(!file.exists(summary_file)) {
    stop("Summary file not found: ", summary_file)
  }
  
  summary_data <- read.csv(summary_file, stringsAsFactors = FALSE)
  
  # Calculate summary statistics
  summary_stats <- list()
  for(model in unique(summary_data$model_comparison)) {
    model_data <- summary_data[summary_data$model_comparison == model, ]
    
    summary_stats[[model]] <- list(
      n_conditions = nrow(model_data),
      mean_diagnostic_power = mean(model_data$diagnostic_power, na.rm = TRUE),
      median_diagnostic_power = median(model_data$diagnostic_power, na.rm = TRUE),
      sd_diagnostic_power = sd(model_data$diagnostic_power, na.rm = TRUE),
      min_diagnostic_power = min(model_data$diagnostic_power, na.rm = TRUE),
      max_diagnostic_power = max(model_data$diagnostic_power, na.rm = TRUE)
    )
  }
  
  message("Streaming Summary Analysis:")
  for(model in names(summary_stats)) {
    stats <- summary_stats[[model]]
    message(sprintf("  %s: %.1f%% mean diagnostic power", model, stats$mean_diagnostic_power))
  }
  
  return(list(
    summary_data = summary_data,
    summary_stats = summary_stats,
    total_conditions = nrow(summary_data)
  ))
}

# Monitor study progress
monitor_study_progress <- function(output_dir, refresh_interval = 30) {
  
  summary_file <- file.path(output_dir, "streaming_summary_full.csv")
  
  if(!file.exists(summary_file)) {
    message("No streaming summary file found. Study may not have started yet.")
    return(invisible(NULL))
  }
  
  message("Monitoring study progress. Press Ctrl+C to stop.")
  
  tryCatch({
    repeat {
      if(file.exists(summary_file)) {
        summary_data <- read.csv(summary_file, stringsAsFactors = FALSE)
        completed_conditions <- nrow(summary_data)
        
        # Get recent progress
        if(completed_conditions > 10) {
          recent_power <- mean(tail(summary_data$diagnostic_power, 10), na.rm = TRUE)
          message(sprintf("Completed: %d conditions | Recent avg diagnostic power: %.1f%%", 
                          completed_conditions, recent_power))
        } else {
          message(sprintf("Completed: %d conditions", completed_conditions))
        }
      }
      
      Sys.sleep(refresh_interval)
    }
  }, interrupt = function(e) {
    message("\nMonitoring stopped.")
  })
}

# Test all framework components
test_framework <- function() {
  
  message("Testing parameter reliability framework...")
  
  # Test model creation
  test_models <- tryCatch({
    lin_model <- linear(parameters = c(2, 1), sd = 1)
    quad_model <- quadratic(parameters = c(2, 1, 0.5), sd = 1)
    cubic_model <- cubic(parameters = c(2, 1, 0.5, 0.2), sd = 1)
    int_model <- interaction(parameters = c(2, 1, 0.5, 0.3), sd = 1)
    TRUE
  }, error = function(e) {
    message("Model creation test failed: ", e$message)
    FALSE
  })
  
  # Test simulation
  test_simulation <- tryCatch({
    lin_data <- simulate(linear(parameters = c(2, 1), sd = 1), N = 50)
    quad_data <- simulate(quadratic(parameters = c(2, 1, 0.5), sd = 1), N = 50)
    cubic_data <- simulate(cubic(parameters = c(2, 1, 0.5, 0.2), sd = 1), N = 50)
    TRUE
  }, error = function(e) {
    message("Simulation test failed: ", e$message)
    FALSE
  })
  
  # Test fitting
  test_fitting <- tryCatch({
    test_data <- data.frame(y = rnorm(50), x = rnorm(50), z = rnorm(50))
    lin_fit <- fit_linear(test_data)
    quad_fit <- fit_quadratic(test_data)
    cubic_fit <- fit_cubic(test_data)
    TRUE
  }, error = function(e) {
    message("Fitting test failed: ", e$message)
    FALSE
  })
  
  # Test reliability calculation
  test_reliability <- tryCatch({
    coef_matrix <- matrix(rnorm(100), 20, 5)
    se_matrix <- matrix(abs(rnorm(100, 0.1, 0.02)), 20, 5)
    reliability <- calculate_reliability(coef_matrix, se_matrix)
    TRUE
  }, error = function(e) {
    message("Reliability test failed: ", e$message)
    FALSE
  })
  
  # Test compensatory analysis
  test_compensatory_analysis <- tryCatch({
    test_compensatory()
  }, error = function(e) {
    message("Compensatory test failed: ", e$message)
    FALSE
  })
  
  # Test test-retest analysis (placeholder)
  test_testretest_analysis <- tryCatch({
    # TODO: Niels to implement test-retest testing
    message("Test-retest analysis not yet implemented (Niels' part)")
    TRUE  # Skip for now
  }, error = function(e) {
    message("Test-retest test failed: ", e$message)
    FALSE
  })
  
  all_tests <- c(test_models, test_simulation, test_fitting, test_reliability, 
                 test_compensatory_analysis, test_testretest_analysis)
  
  if(all(all_tests)) {
    message("All framework tests passed")
    return(TRUE)
  } else {
    message("Some framework tests failed")
    return(FALSE)
  }
}