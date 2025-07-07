# Purpose: Reliability metrics for parameter estimation
# Authors: Kenny Yu & Niels Vanhasbroeck
# Date: July 2025

# Calculate Comprehensive Reliability Metrics
calculate_reliability <- function(coef_matrix, se_matrix, true_parameters = NULL, 
                                  alpha = 0.05, parameter_names = NULL) {
  
  if(!is.matrix(coef_matrix)) coef_matrix <- as.matrix(coef_matrix)
  if(!is.matrix(se_matrix)) se_matrix <- as.matrix(se_matrix)
  
  if(nrow(coef_matrix) != nrow(se_matrix) || ncol(coef_matrix) != ncol(se_matrix)) {
    stop("Coefficient and SE matrices must have same dimensions")
  }
  
  n_params <- ncol(coef_matrix)
  n_sims <- nrow(coef_matrix)
  
  if(n_sims < 3) {
    stop("Need at least 3 simulations for reliability calculation")
  }
  
  # Handle parameter names
  if(is.null(parameter_names)) {
    parameter_names <- colnames(coef_matrix)
    if(is.null(parameter_names)) {
      parameter_names <- paste0("param_", 1:n_params)
    }
  }
  
  # Clean parameter names consistently
  parameter_names <- gsub("\\(|\\)", "", parameter_names)
  parameter_names <- gsub("I\\(x\\^2\\)", "x2", parameter_names)
  parameter_names <- gsub("I\\(x\\^3\\)", "x3", parameter_names)
  parameter_names <- gsub(":", "_", parameter_names)
  
  colnames(coef_matrix) <- parameter_names
  colnames(se_matrix) <- parameter_names
  
  # Handle true parameters
  if(!is.null(true_parameters)) {
    if(length(true_parameters) < n_params) {
      true_parameters <- c(true_parameters, rep(NA, n_params - length(true_parameters)))
    } else if(length(true_parameters) > n_params) {
      true_parameters <- true_parameters[1:n_params]
    }
    names(true_parameters) <- parameter_names
  }
  
  reliability_metrics <- list()
  
  for(p in 1:n_params) {
    param_name <- parameter_names[p]
    coefs <- coef_matrix[, p]
    ses <- se_matrix[, p]
    
    # Remove missing and infinite values
    valid_idx <- is.finite(coefs) & is.finite(ses) & ses > 0
    coefs_clean <- coefs[valid_idx]
    ses_clean <- ses[valid_idx]
    n_valid <- length(coefs_clean)
    
    if(n_valid < 3) {
      reliability_metrics[[param_name]] <- list(
        mean_estimate = NA, sd_estimate = NA, mean_se = NA, n_valid = n_valid,
        cv = NA, snr = NA, test_retest = NA, icc = NA,
        bias = NA, relative_bias = NA, mse = NA, coverage = NA
      )
      next
    }
    
    # Basic descriptive statistics
    mean_est <- mean(coefs_clean)
    sd_est <- sd(coefs_clean)
    mean_se <- mean(ses_clean)
    
    # Coefficient of Variation
    cv <- ifelse(abs(mean_est) > 1e-10, sd_est / abs(mean_est), NA)
    
    # Signal-to-Noise Ratio
    true_param <- if(!is.null(true_parameters)) true_parameters[p] else NA
    if(!is.na(true_param)) {
      # SNR = true signal strength / estimation noise
      snr <- ifelse(sd_est > 1e-10, abs(true_param) / sd_est, NA)
    } else {
      # If no true parameter, use mean estimate magnitude as signal
      snr <- ifelse(sd_est > 1e-10 && abs(mean_est) > 1e-10, abs(mean_est) / sd_est, NA)
    }
    
    # Test-Retest Reliability (split-half correlation)
    test_retest <- NA
    if(n_valid >= 10) {
      half_size <- floor(n_valid / 2)
      if(half_size >= 3) {
        half1 <- coefs_clean[1:half_size]
        half2 <- coefs_clean[(half_size + 1):(2 * half_size)]
        
        test_retest <- tryCatch({
          if(sd(half1) > 1e-10 && sd(half2) > 1e-10) {
            cor(half1, half2, use = "complete.obs")
          } else {
            NA
          }
        }, error = function(e) NA)
      }
    }
    
    # Intraclass Correlation Coefficient
    icc <- NA
    if(n_valid >= 10) {
      half_size <- floor(n_valid / 2)
      if(half_size >= 3) {
        half1 <- coefs_clean[1:half_size]
        half2 <- coefs_clean[(half_size + 1):(2 * half_size)]
        
        tryCatch({
          grand_mean <- mean(c(half1, half2))
          between_var <- var(c(mean(half1), mean(half2))) * half_size
          within_var <- (var(half1) + var(half2)) / 2
          
          if(is.finite(between_var) && is.finite(within_var) && 
             (between_var + within_var) > 1e-10) {
            icc <- between_var / (between_var + within_var)
          }
        }, error = function(e) {
          icc <- NA
        })
      }
    }
    
    # Bias and Accuracy Metrics
    bias <- NA
    relative_bias <- NA
    mse <- NA
    coverage <- NA
    
    if(!is.na(true_param)) {
      # Bias
      bias <- mean_est - true_param
      relative_bias <- ifelse(abs(true_param) > 1e-10, bias / true_param, NA)
      
      # Mean Squared Error
      mse <- mean((coefs_clean - true_param)^2)
      
      # Coverage Probability
      z_critical <- qnorm(1 - alpha / 2)
      ci_lower <- coefs_clean - z_critical * ses_clean
      ci_upper <- coefs_clean + z_critical * ses_clean
      coverage <- mean(ci_lower <= true_param & ci_upper >= true_param)
    }
    
    # Store all metrics
    reliability_metrics[[param_name]] <- list(
      # Basic descriptives
      mean_estimate = mean_est,
      sd_estimate = sd_est,
      mean_se = mean_se,
      n_valid = n_valid,
      
      # Core reliability metrics
      cv = cv,
      snr = snr,
      test_retest = test_retest,
      icc = icc,
      
      # Bias and accuracy metrics
      bias = bias,
      relative_bias = relative_bias,
      mse = mse,
      coverage = coverage
    )
  }
  
  return(reliability_metrics)
}

# Compare Reliability Between Two Models
compare_reliability <- function(model1_reliability, model2_reliability, 
                                model1_name = "Model1", model2_name = "Model2") {
  
  # Get common parameters
  model1_params <- names(model1_reliability)
  model2_params <- names(model2_reliability)
  common_params <- intersect(model1_params, model2_params)
  
  if(length(common_params) == 0) {
    return(NULL)
  }
  
  comparison_metrics <- list()
  
  for(param in common_params) {
    model1 <- model1_reliability[[param]]
    model2 <- model2_reliability[[param]]
    
    # Skip if either model has insufficient data
    if(is.na(model1$n_valid) || is.na(model2$n_valid) || 
       model1$n_valid < 3 || model2$n_valid < 3) {
      comparison_metrics[[param]] <- list(
        snr_degradation = NA, test_retest_degradation = NA,
        bias_difference = NA, mse_ratio = NA, coverage_difference = NA
      )
      next
    }
    
    # SNR degradation (positive when model2 has worse SNR than model1)
    snr_degradation <- NA
    if(!is.na(model1$snr) && !is.na(model2$snr) && model1$snr > 1e-10) {
      snr_degradation <- (model1$snr - model2$snr) / model1$snr
    }
    
    # Test-retest reliability degradation
    test_retest_degradation <- NA
    if(!is.na(model1$test_retest) && !is.na(model2$test_retest) && 
       abs(model1$test_retest) > 1e-10) {
      test_retest_degradation <- (model1$test_retest - model2$test_retest) / abs(model1$test_retest)
    }
    
    # Bias difference (absolute bias increase in model2)
    bias_difference <- NA
    if(!is.na(model1$bias) && !is.na(model2$bias)) {
      bias_difference <- abs(model2$bias) - abs(model1$bias)
    }
    
    # MSE ratio (model2/model1, >1 means worse MSE in model2)
    mse_ratio <- NA
    if(!is.na(model1$mse) && !is.na(model2$mse) && model1$mse > 1e-10) {
      mse_ratio <- model2$mse / model1$mse
    }
    
    # Coverage difference (model2 - model1, negative means worse coverage in model2)
    coverage_difference <- NA
    if(!is.na(model1$coverage) && !is.na(model2$coverage)) {
      coverage_difference <- model2$coverage - model1$coverage
    }
    
    comparison_metrics[[param]] <- list(
      snr_degradation = snr_degradation,
      test_retest_degradation = test_retest_degradation,
      bias_difference = bias_difference,
      mse_ratio = mse_ratio,
      coverage_difference = coverage_difference
    )
  }
  
  # Add metadata
  comparison_metrics$metadata <- list(
    model1_name = model1_name,
    model2_name = model2_name,
    common_parameters = common_params
  )
  
  return(comparison_metrics)
}

# Calculate Diagnostic Performance
calculate_diagnostic_performance <- function(reliability_comparison, 
                                             thresholds = list(bias = 0.1, snr = 0.2, mse = 2.0, coverage = 0.2)) {
  
  if(is.null(reliability_comparison) || is.null(reliability_comparison$metadata)) {
    return(NULL)
  }
  
  common_params <- reliability_comparison$metadata$common_parameters
  diagnostic_performance <- list()
  
  total_diagnostic_score <- 0
  max_possible_score <- 0
  
  for(param in common_params) {
    comparison <- reliability_comparison[[param]]
    
    # Calculate diagnostic strength for each metric
    bias_diagnostic <- ifelse(!is.na(comparison$bias_difference) && 
                                abs(comparison$bias_difference) > thresholds$bias, 1, 0)
    
    snr_diagnostic <- ifelse(!is.na(comparison$snr_degradation) && 
                               comparison$snr_degradation > thresholds$snr, 1, 0)
    
    mse_diagnostic <- ifelse(!is.na(comparison$mse_ratio) && 
                               comparison$mse_ratio > thresholds$mse, 1, 0)
    
    coverage_diagnostic <- ifelse(!is.na(comparison$coverage_difference) && 
                                    abs(comparison$coverage_difference) > thresholds$coverage, 1, 0)
    
    # Parameter-level diagnostic strength
    param_diagnostic_score <- bias_diagnostic + snr_diagnostic + mse_diagnostic + coverage_diagnostic
    param_max_possible <- 4
    
    diagnostic_performance[[param]] <- list(
      bias_diagnostic = bias_diagnostic,
      snr_diagnostic = snr_diagnostic,
      mse_diagnostic = mse_diagnostic,
      coverage_diagnostic = coverage_diagnostic,
      total_diagnostic_score = param_diagnostic_score,
      diagnostic_power = (param_diagnostic_score / param_max_possible) * 100
    )
    
    # Accumulate for study-level metric
    total_diagnostic_score <- total_diagnostic_score + param_diagnostic_score
    max_possible_score <- max_possible_score + param_max_possible
  }
  
  # Calculate study-level diagnostic power
  study_diagnostic_power <- ifelse(max_possible_score > 0, 
                                   (total_diagnostic_score / max_possible_score) * 100, 0)
  
  diagnostic_performance$summary <- list(
    study_diagnostic_power = study_diagnostic_power,
    total_parameters = length(common_params),
    thresholds_used = thresholds
  )
  
  return(diagnostic_performance)
}