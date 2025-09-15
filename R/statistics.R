#' Compute the Descriptive Statistics for the Parameters
#' 
#' @description 
#' Descriptive statistics include the mean, standard deviation, median, and 95%
#' confidence interval across participants. Statistics are computed per bin-type.
#' 
#' @param estimated Dataframe containing estimated parameters per participant and 
#' bin, allowing for a decomposition analysis. 
#' @param ... Obsolete. Ensures compatibility with 
#' \code{\link[paramrel]{execute_study}}
#' 
#' @return Dataframe containing the parameter and bin (\code{"parameter"}, 
#' \code{"bin"}), as well as the summary statistics for each.
#' 
#' @export
descriptives <- function(estimated, ...) {
    # Retrieve the parameter names
    cols <- colnames(estimated)
    cols <- cols[-which(cols %in% c("participant", "bin"))]
    cols <- cols[!grepl("se_", cols, fixed = TRUE)]

    # Loop over all the parameters and compute the reliability coefficients we 
    # are interested in here
    results <- lapply(
        cols,
        function(x) {
            results <- lapply(
                unique(estimates$bin),
                function(y) {
                    # Select the data of interest from the data.frame
                    est <- estimates[estimates$bin == y, x]
                    se <- estimates[estimates$bin == y, paste0("se_", x)]

                    # Compute the descriptives of interest
                    return(
                        data.frame(
                            parameter = x,
                            bin = y,
                            mean = mean(est),
                            sd = sd(est),
                            q025 = quantile(est, prob = 0.025),
                            median = median(est),
                            q975 = quantile(est, prob = 0.975),
                            mean_se = mean(se),
                            sd_se = sd(se),
                            q025_se = quantile(se, prob = 0.025),
                            median_se = median(se),
                            q975_se = quantile(se, prob = 0.975)
                        )
                    )
                }
            )
            results <- do.call("rbind", results)

            return(results)            
        }
    )
    results <- do.call("rbind", results)

    return(results)
}

#' Compute the \eqn{ICC(A, 1)} for Participant and Bin.
#'
#' @description
#' In our study, we define the \eqn{ICC(A, 1)} as:
#' 
#' \deqn{ICC(A, 1) = \frac{\sigma_\text{participants}}^2}{\sigma_\text{total}^2}
#' 
#' where \eqn{\sigma^2} denotes the variance. This \eqn{ICC} puts the systematic 
#' variance -- that is, the interindividual differences in the values of the 
#' parameters -- against the total observed variance in the parameters, checking 
#' whether individual differences can be reliably picked up on.
#' 
#' @param estimated Dataframe containing estimated parameters per participant and 
#' bin, allowing for a decomposition analysis. 
#' @param ... Obsolete. Ensures compatibility with 
#' \code{\link[paramrel]{execute_study}}
#' 
#' @return Dataframe containing the parameter (\code{"parameter"}), systematic 
#' and unsystematic variances (\code{"systematic"}, \code{"unsystematic"}), and 
#' the estimated \eqn{ICC(A, 1)} for each parameter (\code{"icc"}) 
#' 
#' @export
icc <- function(estimated, ...) {
    # Filter out those occasions where bins are not of import
    estimated <- estimated[estimated$bin != -1, ]

    # Get the column names of the data and identify all parameter names
    cols <- colnames(estimated)
    cols <- cols[-which(cols %in% c("participant", "bin"))]
    cols <- cols[!grepl("se_", cols, fixed = TRUE)]

    # Loop over all columns and compute the ICC per parameter
    icc <- lapply(
        cols,
        function(x) {
            # Select the correct data and change the names as appropriate
            data <- estimated[, c(x, "participant", "bin")] |>
                setNames(c("param", "participant", "bin"))

            # Do the variance decomposition through lmer
            result <- lme4::lmer(
                data = data, 
                param ~ 1 + (1 | participant)
            )

            # Get a summary of the estimation from lmer and extract the 
            # variance components of interest from the model, it being the 
            # systematic variance and the total variance
            fit <- summary(result)
    
            systematic <- as.numeric(fit$varcor[1])
            residual <- as.numeric(fit$sigma^2)

            # Compute the ICC(A, 1) for this parameter and return as the result
            # of the procedure
            return(
                data.frame(
                    "parameter" = x,
                    "icc" = systematic / (systematic + residual),
                    "systematic" = systematic, 
                    "residual" = residual
                )
            )
        }
    )
    icc <- do.call("rbind", icc)

    return(icc)
}

#' Compute the Reliability based on Standard Errors
#' 
#' @param estimated Dataframe containing estimated parameters per participant and 
#' bin, allowing for a decomposition analysis. 
#' @param ... Obsolete. Ensures compatibility with 
#' \code{\link[paramrel]{execute_study}}
#' 
#' @return Dataframe containing the parameter (\code{"parameter"}), systematic 
#' and unsystematic variances (\code{"systematic"}, \code{"unsystematic"}), and 
#' the estimated \eqn{ICC(A, 1)} for each parameter (\code{"icc"})
#' 
#' @export
reliability <- function(estimated, ...) {
    # Filter out the bin-specific estimates. Not needed for this analysis
    estimated <- estimated[estimated$bin == -1, ]

    # Check whether you have enough data to compute the reliability coefficient
    if(nrow(estimated) < 3) {
        stop("At least 3 simulations per parameter set are needed to estimate the reliability coefficient.")
    }

    # Retrieve the parameter names
    cols <- colnames(estimated)
    cols <- cols[-which(cols %in% c("participant", "bin"))]
    cols <- cols[!grepl("se_", cols, fixed = TRUE)]

    # Handle true parameters
    if(!is.null(true_parameters)) {
      if(length(true_parameters) < n_params) {
        true_parameters <- c(true_parameters, rep(NA, n_params - length(true_parameters)))
      } else if(length(true_parameters) > n_params) {
        true_parameters <- true_parameters[1:n_params]
      }
      names(true_parameters) <- parameter_names
    }

    # Loop over all the parameters and compute the reliability coefficients we 
    # are interested in here
    results <- lapply(
        cols,
        function(x) {
            # Select the data of interest from the data.frame
            est <- estimates[, x]
            se <- estimates[, paste0("se_", x)]
        }
    )



}

calculate_reliability <- function(coef_matrix, se_matrix, true_parameters = NULL, 
                                  alpha = 0.05, parameter_names = NULL) {
  
  
  
  
  
  
  
  reliability_metrics <- list()
  
  for(p in 1:n_params) {
    param_name <- parameter_names[p]
    coefs <- coef_matrix[, p]
    ses <- se_matrix[, p]
    
    
    
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