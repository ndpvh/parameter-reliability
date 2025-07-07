# Purpose: Compensatory mechanism analysis for reliability diagnostics
# Authors: Kenny Yu & Niels Vanhasbroeck
# Date: July 2025
# Focus: Kenny's compensatory analysis

# Generate Parameter Grid for Compensatory Analysis
generate_compensatory_grid <- function(n_intercepts = 7, n_slopes = 7, n_higher_order = 5) {
  
  # Parameter ranges for comprehensive coverage
  intercept_range <- seq(-10, 10, length.out = n_intercepts)
  slope_range <- seq(-5, 5, length.out = n_slopes)
  
  # Non-zero higher-order effects to ensure meaningful compensation
  if(n_higher_order <= 3) {
    higher_order_range <- c(-0.5, -0.2, 0.2, 0.5)[1:n_higher_order]
  } else if(n_higher_order <= 5) {
    higher_order_range <- c(-1, -0.5, -0.2, 0.2, 0.5)[1:n_higher_order]
  } else {
    higher_order_range <- c(-1.5, -1, -0.5, -0.2, 0.2, 0.5, 1, 1.5)[1:n_higher_order]
  }
  
  parameter_grids <- list()
  
  # Under-parameterization: Complex data with simple models (compensatory mechanisms)
  
  # Model Comparison 1: Quadratic data vs Linear model
  quadratic_grid <- expand.grid(
    true_intercept = intercept_range,
    true_linear_slope = slope_range,
    true_quadratic_slope = higher_order_range,
    model_comparison = "quadratic_vs_linear",
    stringsAsFactors = FALSE
  )
  parameter_grids$quadratic_vs_linear <- quadratic_grid
  
  # Model Comparison 2: Interaction data vs Main effects model
  interaction_grid <- expand.grid(
    true_intercept = intercept_range,
    true_x_slope = slope_range,
    true_z_slope = slope_range,
    true_interaction_slope = higher_order_range,
    model_comparison = "interaction_vs_main", 
    stringsAsFactors = FALSE
  )
  parameter_grids$interaction_vs_main <- interaction_grid
  
  # Model Comparison 3: Cubic data vs Quadratic model
  cubic_grid <- expand.grid(
    true_intercept = intercept_range,
    true_linear_slope = slope_range,
    true_quadratic_slope = higher_order_range,
    true_cubic_slope = higher_order_range,
    model_comparison = "cubic_vs_quadratic",
    stringsAsFactors = FALSE
  )
  parameter_grids$cubic_vs_quadratic <- cubic_grid
  
  # Over-parameterization: Simple data with complex models (redundancy effects)
  
  # Model Comparison 4: Linear data vs Quadratic model
  linear_grid <- expand.grid(
    true_intercept = intercept_range,
    true_linear_slope = slope_range,
    model_comparison = "linear_vs_quadratic",
    stringsAsFactors = FALSE
  )
  parameter_grids$linear_vs_quadratic <- linear_grid
  
  # Model Comparison 5: Main effects data vs Interaction model
  main_grid <- expand.grid(
    true_intercept = intercept_range,
    true_x_slope = slope_range,
    true_z_slope = slope_range,
    model_comparison = "main_vs_interaction",
    stringsAsFactors = FALSE
  )
  parameter_grids$main_vs_interaction <- main_grid
  
  # Model Comparison 6: Quadratic data vs Cubic model
  quadratic_over_grid <- expand.grid(
    true_intercept = intercept_range,
    true_linear_slope = slope_range,
    true_quadratic_slope = higher_order_range,
    model_comparison = "quadratic_vs_cubic",
    stringsAsFactors = FALSE
  )
  parameter_grids$quadratic_vs_cubic <- quadratic_over_grid
  
  return(parameter_grids)
}

# Run Compensatory Analysis for Single Condition
run_compensatory_condition <- function(condition_params, n_datasets = 100, sample_size = 100, error_sd = 1) {
  
  model_comparison <- condition_params$model_comparison
  
  # Initialize storage
  correct_estimates <- list()
  misspec_estimates <- list()
  
  # Fixed seed handling for consistent X values within condition
  base_seed <- 12345
  condition_seed <- base_seed + condition_params$condition_id
  set.seed(condition_seed)
  
  # Run analysis based on model comparison type
  if(model_comparison == "quadratic_vs_linear") {
    
    true_params <- c(condition_params$true_intercept, 
                     condition_params$true_linear_slope, 
                     condition_params$true_quadratic_slope)
    true_model <- quadratic(parameters = true_params, sd = error_sd)
    
    X_vals <- runif(sample_size, -2, 2)
    
    for(i in 1:n_datasets) {
      data <- simulate(true_model, X = X_vals)
      
      correct_fit <- fit_quadratic(data)
      misspec_fit <- fit_linear(data)
      
      correct_estimates[[i]] <- list(
        coefficients = correct_fit$coefficients,
        standard_errors = correct_fit$se
      )
      misspec_estimates[[i]] <- list(
        coefficients = misspec_fit$coefficients,
        standard_errors = misspec_fit$se
      )
    }
    
    correct_true_params <- true_params
    misspec_true_params <- true_params[1:2]
    
  } else if(model_comparison == "interaction_vs_main") {
    
    true_params <- c(condition_params$true_intercept,
                     condition_params$true_x_slope,
                     condition_params$true_z_slope,
                     condition_params$true_interaction_slope)
    true_model <- interaction(parameters = true_params, sd = error_sd)
    
    X_vals <- cbind(runif(sample_size, -2, 2), runif(sample_size, -2, 2))
    
    for(i in 1:n_datasets) {
      data <- simulate(true_model, X = X_vals)
      
      correct_fit <- fit_interaction(data)
      misspec_fit <- fit_main(data)
      
      correct_estimates[[i]] <- list(
        coefficients = correct_fit$coefficients,
        standard_errors = correct_fit$se
      )
      misspec_estimates[[i]] <- list(
        coefficients = misspec_fit$coefficients,
        standard_errors = misspec_fit$se
      )
    }
    
    correct_true_params <- true_params
    misspec_true_params <- true_params[1:3]
    
  } else if(model_comparison == "cubic_vs_quadratic") {
    
    true_params <- c(condition_params$true_intercept,
                     condition_params$true_linear_slope,
                     condition_params$true_quadratic_slope,
                     condition_params$true_cubic_slope)
    true_model <- cubic(parameters = true_params, sd = error_sd)
    
    X_vals <- runif(sample_size, -2, 2)
    
    for(i in 1:n_datasets) {
      data <- simulate(true_model, X = X_vals)
      
      correct_fit <- fit_cubic(data)
      misspec_fit <- fit_quadratic(data)
      
      correct_estimates[[i]] <- list(
        coefficients = correct_fit$coefficients,
        standard_errors = correct_fit$se
      )
      misspec_estimates[[i]] <- list(
        coefficients = misspec_fit$coefficients,
        standard_errors = misspec_fit$se
      )
    }
    
    correct_true_params <- true_params
    misspec_true_params <- true_params[1:3]
    
  } else if(model_comparison == "linear_vs_quadratic") {
    
    true_params <- c(condition_params$true_intercept, 
                     condition_params$true_linear_slope)
    true_model <- linear(parameters = true_params, sd = error_sd)
    
    X_vals <- runif(sample_size, -2, 2)
    
    for(i in 1:n_datasets) {
      data <- simulate(true_model, X = X_vals)
      
      correct_fit <- fit_linear(data)
      misspec_fit <- fit_quadratic(data)
      
      correct_estimates[[i]] <- list(
        coefficients = correct_fit$coefficients,
        standard_errors = correct_fit$se
      )
      misspec_estimates[[i]] <- list(
        coefficients = misspec_fit$coefficients,
        standard_errors = misspec_fit$se
      )
    }
    
    correct_true_params <- true_params
    # For over-parameterized model, true quadratic coefficient is 0
    misspec_true_params <- c(true_params, 0)
    
  } else if(model_comparison == "main_vs_interaction") {
    
    true_params <- c(condition_params$true_intercept,
                     condition_params$true_x_slope,
                     condition_params$true_z_slope)
    true_model_with_zero_interaction <- interaction(parameters = c(true_params, 0), sd = error_sd)
    
    X_vals <- cbind(runif(sample_size, -2, 2), runif(sample_size, -2, 2))
    
    for(i in 1:n_datasets) {
      data <- simulate(true_model_with_zero_interaction, X = X_vals)
      
      correct_fit <- fit_main(data)
      misspec_fit <- fit_interaction(data)
      
      correct_estimates[[i]] <- list(
        coefficients = correct_fit$coefficients,
        standard_errors = correct_fit$se
      )
      misspec_estimates[[i]] <- list(
        coefficients = misspec_fit$coefficients,
        standard_errors = misspec_fit$se
      )
    }
    
    correct_true_params <- true_params
    # For over-parameterized model, true interaction coefficient is 0
    misspec_true_params <- c(true_params, 0)
    
  } else if(model_comparison == "quadratic_vs_cubic") {
    
    true_params <- c(condition_params$true_intercept,
                     condition_params$true_linear_slope,
                     condition_params$true_quadratic_slope)
    true_model <- quadratic(parameters = true_params, sd = error_sd)
    
    X_vals <- runif(sample_size, -2, 2)
    
    for(i in 1:n_datasets) {
      data <- simulate(true_model, X = X_vals)
      
      correct_fit <- fit_quadratic(data)
      misspec_fit <- fit_cubic(data)
      
      correct_estimates[[i]] <- list(
        coefficients = correct_fit$coefficients,
        standard_errors = correct_fit$se
      )
      misspec_estimates[[i]] <- list(
        coefficients = misspec_fit$coefficients,
        standard_errors = misspec_fit$se
      )
    }
    
    correct_true_params <- true_params
    # For over-parameterized model, true cubic coefficient is 0
    misspec_true_params <- c(true_params, 0)
    
  } else {
    stop("Unknown model comparison: ", model_comparison)
  }
  
  # Convert estimates to matrices
  correct_coef <- do.call(rbind, lapply(correct_estimates, function(x) x$coefficients))
  correct_se <- do.call(rbind, lapply(correct_estimates, function(x) x$standard_errors))
  misspec_coef <- do.call(rbind, lapply(misspec_estimates, function(x) x$coefficients))
  misspec_se <- do.call(rbind, lapply(misspec_estimates, function(x) x$standard_errors))
  
  # Calculate reliability metrics
  correct_reliability <- calculate_reliability(correct_coef, correct_se, correct_true_params)
  misspec_reliability <- calculate_reliability(misspec_coef, misspec_se, misspec_true_params)
  
  # Compare reliability between models
  reliability_comparison <- compare_reliability(
    correct_reliability, misspec_reliability, 
    "Correct", "Misspecified"
  )
  
  # Use sensitive thresholds for interaction models
  if(grepl("interaction", model_comparison)) {
    thresholds <- list(bias = 0.02, snr = 0.05, mse = 1.2, coverage = 0.05)
  } else {
    thresholds <- list(bias = 0.1, snr = 0.2, mse = 2.0, coverage = 0.2)
  }
  
  diagnostic_metrics <- calculate_diagnostic_performance(reliability_comparison, thresholds)
  
  # Extract summary metrics for efficient storage
  summary_metrics <- extract_condition_summary(diagnostic_metrics, reliability_comparison)
  
  # Return comprehensive results
  list(
    model_comparison = model_comparison,
    condition_params = condition_params,
    n_datasets = n_datasets,
    sample_size = sample_size,
    
    correct_reliability = correct_reliability,
    misspec_reliability = misspec_reliability,
    reliability_comparison = reliability_comparison,
    diagnostic_metrics = diagnostic_metrics,
    summary_metrics = summary_metrics,
    
    error_occurred = FALSE
  )
}

# Extract Condition Summary for Efficient Storage
extract_condition_summary <- function(diagnostic_metrics, reliability_comparison) {
  
  if(is.null(diagnostic_metrics) || is.null(reliability_comparison)) {
    return(list(
      diagnostic_power = NA,
      max_bias_diff = NA,
      max_snr_degrad = NA,
      max_coverage_diff = NA
    ))
  }
  
  # Extract diagnostic power
  diagnostic_power <- NA
  if(!is.null(diagnostic_metrics$summary)) {
    diagnostic_power <- diagnostic_metrics$summary$study_diagnostic_power
  }
  
  # Extract maximum effects across parameters
  common_params <- reliability_comparison$metadata$common_parameters
  bias_diffs <- sapply(common_params, function(p) {
    if(p %in% names(reliability_comparison)) {
      abs(reliability_comparison[[p]]$bias_difference)
    } else NA
  })
  
  snr_degrads <- sapply(common_params, function(p) {
    if(p %in% names(reliability_comparison)) {
      reliability_comparison[[p]]$snr_degradation
    } else NA
  })
  
  coverage_diffs <- sapply(common_params, function(p) {
    if(p %in% names(reliability_comparison)) {
      abs(reliability_comparison[[p]]$coverage_difference)
    } else NA
  })
  
  list(
    diagnostic_power = diagnostic_power,
    max_bias_diff = max(bias_diffs, na.rm = TRUE),
    max_snr_degrad = max(snr_degrads, na.rm = TRUE),
    max_coverage_diff = max(coverage_diffs, na.rm = TRUE)
  )
}

# Generate Complete Compensatory Study Design
generate_compensatory_study <- function(study_size = "pilot") {
  
  if(study_size == "pilot") {
    n_intercepts <- 3
    n_slopes <- 3
    n_higher_order <- 3
    sample_sizes <- c(100)
    error_sds <- c(1.0)
    n_datasets <- 100
  } else if(study_size == "medium") {
    n_intercepts <- 5
    n_slopes <- 5
    n_higher_order <- 4
    sample_sizes <- c(50, 100)
    error_sds <- c(1.0, 2.0)
    n_datasets <- 200
  } else if(study_size == "full") {
    n_intercepts <- 7
    n_slopes <- 7
    n_higher_order <- 5
    sample_sizes <- c(50, 100, 200)
    error_sds <- c(0.5, 1.0, 2.0)
    n_datasets <- 400
  } else {
    stop("Study size must be 'pilot', 'medium', or 'full'")
  }
  
  # Generate parameter grids
  parameter_grids <- generate_compensatory_grid(n_intercepts, n_slopes, n_higher_order)
  
  # Create complete study design
  study_conditions <- list()
  condition_id <- 1
  
  for(grid_name in names(parameter_grids)) {
    grid <- parameter_grids[[grid_name]]
    
    for(i in 1:nrow(grid)) {
      for(ss in sample_sizes) {
        for(err in error_sds) {
          
          condition <- list(
            condition_id = condition_id,
            grid_name = grid_name,
            parameter_row = i,
            sample_size = ss,
            error_sd = err,
            n_datasets = n_datasets
          )
          
          # Add parameter values
          for(col in names(grid)) {
            condition[[col]] <- grid[i, col]
          }
          
          study_conditions[[condition_id]] <- condition
          condition_id <- condition_id + 1
        }
      }
    }
  }
  
  return(study_conditions)
}

# Run Complete Compensatory Study
run_compensatory_study <- function(study_size = "pilot", output_dir = "compensatory_results") {
  
  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  study_conditions <- generate_compensatory_study(study_size)
  total_conditions <- length(study_conditions)
  
  message("Starting compensatory study: ", total_conditions, " conditions")
  message("Model comparisons included:")
  grids <- generate_compensatory_grid()
  for(name in names(grids)) {
    message("  - ", name, ": ", nrow(grids[[name]]), " parameter combinations")
  }
  
  start_time <- Sys.time()
  results <- list()
  summary_results <- list()
  
  # Initialize summary file for streaming large studies
  if(study_size == "full") {
    summary_file <- file.path(output_dir, paste0("streaming_summary_", study_size, ".csv"))
  }
  
  for(i in 1:total_conditions) {
    
    if(i %% 50 == 0 || i == 1) {
      progress <- round(100 * i / total_conditions, 1)
      elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
      
      if(i > 1) {
        estimated_total <- elapsed / (i / total_conditions)
        remaining <- estimated_total - elapsed
        message(sprintf("Progress: %d/%d (%.1f%%) - %.1f min remaining", 
                        i, total_conditions, progress, remaining))
      } else {
        message(sprintf("Starting condition %d/%d", i, total_conditions))
      }
    }
    
    condition <- study_conditions[[i]]
    
    result <- tryCatch({
      run_compensatory_condition(
        condition_params = condition,
        n_datasets = condition$n_datasets,
        sample_size = condition$sample_size,
        error_sd = condition$error_sd
      )
    }, error = function(e) {
      list(
        condition_id = condition$condition_id,
        error_message = as.character(e),
        error_occurred = TRUE
      )
    })
    
    # Store summary for efficient analysis of large studies
    if(!result$error_occurred && study_size == "full") {
      summary_row <- data.frame(
        condition_id = i,
        model_comparison = result$model_comparison,
        sample_size = result$sample_size,
        error_sd = condition$error_sd,
        diagnostic_power = result$summary_metrics$diagnostic_power,
        max_bias_diff = result$summary_metrics$max_bias_diff,
        max_snr_degrad = result$summary_metrics$max_snr_degrad,
        max_coverage_diff = result$summary_metrics$max_coverage_diff,
        stringsAsFactors = FALSE
      )
      
      # Append to streaming summary
      if(i == 1) {
        write.csv(summary_row, summary_file, row.names = FALSE)
      } else {
        write.table(summary_row, summary_file, sep = ",", append = TRUE, 
                    row.names = FALSE, col.names = FALSE)
      }
      
      summary_results[[i]] <- summary_row
    }
    
    # Store full results
    if(!result$error_occurred) {
      results[[i]] <- result
    }
  }
  
  # Compile study results
  end_time <- Sys.time()
  runtime <- difftime(end_time, start_time, units = "hours")
  n_errors <- sum(sapply(results, function(x) isTRUE(x$error_occurred)))
  
  study_results <- list(
    results = results,
    study_conditions = study_conditions,
    study_size = study_size,
    total_conditions = total_conditions,
    successful_conditions = total_conditions - n_errors,
    failed_conditions = n_errors,
    runtime_hours = as.numeric(runtime),
    completion_time = end_time,
    analysis_type = "compensatory"
  )
  
  # Add summary results for full studies
  if(study_size == "full" && length(summary_results) > 0) {
    study_results$summary_results <- do.call(rbind, summary_results)
  }
  
  # Save results
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M")
  output_file <- file.path(output_dir, paste0("compensatory_", study_size, "_", timestamp, ".RData"))
  save(study_results, file = output_file)
  
  message(sprintf("Study completed: %d successful, %d failed (%.2f hours)", 
                  total_conditions - n_errors, n_errors, as.numeric(runtime)))
  message("Results saved: ", output_file)
  
  if(study_size == "full") {
    message("Streaming summary: ", summary_file)
    return(list(study_results = study_results, output_file = output_file, summary_file = summary_file))
  } else {
    return(list(study_results = study_results, output_file = output_file))
  }
}

# Analyze Compensatory Study Results
analyze_compensatory_results <- function(results_file, use_streaming = TRUE) {
  
  load(results_file)
  if(!exists("study_results")) {
    stop("No study_results object found in file")
  }
  
  # Use streaming summary for large studies if available
  if(use_streaming && !is.null(study_results$summary_results)) {
    compensatory_data <- study_results$summary_results
  } else {
    results_list <- study_results$results
    successful <- !sapply(results_list, function(x) isTRUE(x$error_occurred))
    results_list <- results_list[successful]
    
    # Extract key metrics
    compensatory_data <- data.frame(
      condition_id = integer(0),
      model_comparison = character(0),
      sample_size = numeric(0),
      error_sd = numeric(0),
      diagnostic_power = numeric(0),
      stringsAsFactors = FALSE
    )
    
    for(i in 1:length(results_list)) {
      result <- results_list[[i]]
      
      diagnostic_power <- NA
      if(!is.null(result$diagnostic_metrics$summary)) {
        diagnostic_power <- result$diagnostic_metrics$summary$study_diagnostic_power
      }
      
      compensatory_data <- rbind(compensatory_data, data.frame(
        condition_id = i,
        model_comparison = result$model_comparison,
        sample_size = result$sample_size,
        error_sd = result$condition_params$error_sd,
        diagnostic_power = diagnostic_power,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # Calculate summary statistics
  summary_stats <- aggregate(diagnostic_power ~ model_comparison, 
                             data = compensatory_data, 
                             FUN = function(x) c(mean = mean(x, na.rm = TRUE),
                                                 median = median(x, na.rm = TRUE),
                                                 sd = sd(x, na.rm = TRUE),
                                                 min = min(x, na.rm = TRUE),
                                                 max = max(x, na.rm = TRUE)))
  
  message("Compensatory Analysis Summary:")
  for(i in 1:nrow(summary_stats)) {
    model <- summary_stats$model_comparison[i]
    stats <- summary_stats$diagnostic_power[i, ]
    message(sprintf("  %s: %.1f%% mean diagnostic power (range: %.1f%% - %.1f%%)", 
                    model, stats["mean"], stats["min"], stats["max"]))
  }
  
  return(list(
    compensatory_data = compensatory_data,
    summary_stats = summary_stats,
    source_file = results_file
  ))
}

# Quick Test of Compensatory Analysis
test_compensatory <- function() {
  
  test_condition <- list(
    condition_id = 1,
    true_intercept = 2,
    true_linear_slope = 1,
    true_quadratic_slope = 0.5,
    model_comparison = "quadratic_vs_linear"
  )
  
  result <- run_compensatory_condition(
    condition_params = test_condition,
    n_datasets = 20,
    sample_size = 100,
    error_sd = 1
  )
  
  if(!result$error_occurred && !is.null(result$diagnostic_metrics)) {
    message("Compensatory analysis test passed")
    return(TRUE)
  } else {
    message("Compensatory analysis test failed")
    return(FALSE)
  }
}