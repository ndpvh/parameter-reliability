# PURPOSE: Define analyses we wish to run on the parameters
# AUTHORS: Kenny Yu & Niels Vanhasbroeck
# DATE: Sep 2025
# FOCUS: Preparation of the estimated and true parameter values and their analysis

#' Prepare True and Estimated Parameters for Analysis
#' 
#' @param estimated Dataframe containing estimated parameters per participant and 
#' bin, allowing for a decomposition analysis. 
#' @param simulated Dataframe containing the simulated values for the parameters
#' per participant. 
#' 
#' @return Named list returning parameter names (\code{"parameters"}) and 
#' adjusted data.frames for the estimated and true parameter values 
#' (\code{"estimated"}, \code{"simulated"}) so that they can be readily compared
#' to each other.
#' 
#' @export 
prepare <- function(estimated, 
                    simulated) {
    
    # Retrieve the parameter names from the estimated parameters
    cols <- colnames(estimated)
    cols <- cols[-which(cols %in% c("participant", "bin"))]
    cols <- cols[!grepl("se_", cols, fixed = TRUE)]

    # Retrieve the parameter names from the true parameters
    cols_sim <- colnames(simulated) 

    # Handle the true parameters so that you can compare between the two types 
    # of parameters. If more estimated than simulated parameters, then we add 
    # columns to the true parameters indicating that these parameters were set 
    # to 0. If more true parameters than estimated ones, then we do the reverse 
    # operation.
    if(ncol(estimated) > ncol(simulated)) {
        new_cols <- cols[!(cols %in% cols_sim)]
        simulated[, new_cols] <- 0 

    } else if(ncol(estimated) < ncol(simulated)) {
        new_cols <- cols_sim[!(cols_sim %in% cols)]
        new_cols <- paste0(
            rep(c("", "se_"), each = length(new_cols), 
            rep(new_cols, times = 2)
        )

        estimated[, new_cols] <- 0
    }

    return(
        list(
            "parameters" = colnames(simulated),
            "estimated" = estimated,
            "simulated" = simulated
        )
    )
}

#' Compute the Descriptive Statistics for the Parameters
#' 
#' @description 
#' Descriptive statistics include the mean, standard deviation, median, and 95%
#' confidence interval across participants. Statistics are computed per bin-type.
#' 
#' @param data Named list containing the parameter names under \code{"parameters"},
#' the true parameter values under \code{"simulated"}, and the estimated 
#' parameter values under \code{"estimated"}. Typically the result of 
#' \code{\link[paramrel]{prepare}}
#' 
#' @return Dataframe containing the parameter and bin (\code{"parameter"}, 
#' \code{"bin"}), as well as the summary statistics for each.
#' 
#' @export
descriptives <- function(data) {
    # Retrieve the relevant information from the list
    estimated <- data$estimated
    simulated <- data$simulated
    cols <- data$parameters

    # Loop over all the parameters and compute the reliability coefficients we 
    # are interested in here
    results <- lapply(
        cols,
        function(x) {
            if(!("bin" %in% colnames(estimated))) {
                estimated$bin <- 1
            }

            results <- lapply(
                unique(estimates$bin),
                function(y) {
                    # Select the data of interest from the data.frame
                    sim <- simulated[, x]
                    est <- estimates[estimates$bin == y, x]
                    se <- estimates[estimates$bin == y, paste0("se_", x)]

                    # Compute the descriptives of interest
                    return(
                        data.frame(
                            parameter = x,
                            bin = y,
                            true_value = mean(sim),
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
#' @param data Named list containing the parameter names under \code{"parameters"},
#' the true parameter values under \code{"simulated"}, and the estimated 
#' parameter values under \code{"estimated"}. Typically the result of 
#' \code{\link[paramrel]{prepare}}
#' 
#' @return Dataframe containing the parameter (\code{"parameter"}), systematic 
#' and unsystematic variances (\code{"systematic"}, \code{"unsystematic"}), and 
#' the estimated \eqn{ICC(A, 1)} for each parameter (\code{"icc"}) 
#' 
#' @export
icc <- function(data) {
    # Retrieve the relevant information from the list
    estimated <- data$estimated
    simulated <- data$simulated
    cols <- data$parameters

    # Filter out those occasions where bins are not of import
    if("bin" %in% colnames(estimated)) {
        stop("The column name 'bin' not found in the data.frame. Need repetitions to compute the ICC.")
    }

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
#' @param data Named list containing the parameter names under \code{"parameters"},
#' the true parameter values under \code{"simulated"}, and the estimated 
#' parameter values under \code{"estimated"}. Typically the result of 
#' \code{\link[paramrel]{prepare}}
#' 
#' @return Dataframe containing the parameter (\code{"parameter"}), the 
#' coefficient of variation, and the signal-to-noise ratio
#' 
#' @export
reliability <- function(data) {
    # Retrieve the relevant information from the list
    estimated <- data$estimated
    simulated <- data$simulated
    cols <- data$parameters

    # Filter out the bin-specific estimates. Not needed for this analysis
    if("bin" %in% colnames(estimated)) {
        estimated <- estimated[estimated$bin == -1, ]
    }

    # Loop over all the parameters and compute the reliability coefficients we 
    # are interested in here
    results <- lapply(
        cols,
        function(x) {
            # Select the data of interest from the data.frame
            est <- estimates[, x]
            se <- estimates[, paste0("se_", x)]

            # TO DISCUSS WITH KENNY!
            #
            # # Compute the coefficient of variation
            # cv <- ifelse(abs(mean_est) > 1e-10, sd_est / abs(mean_est), NA)
    
            # # Signal-to-Noise Ratio
            # true_param <- if(!is.null(true_parameters)) true_parameters[p] else NA
            # if(!is.na(true_param)) {
            #   # SNR = true signal strength / estimation noise
            #   snr <- ifelse(sd_est > 1e-10, abs(true_param) / sd_est, NA)
            # } else {
            #   # If no true parameter, use mean estimate magnitude as signal
            #   snr <- ifelse(sd_est > 1e-10 && abs(mean_est) > 1e-10, abs(mean_est) / sd_est, NA)
            # }
        }
    )
    results <- do.call("rbind", results)

    return(results)
}

#' Compute the Bias and MSE of the Estimates
#' 
#' @description 
#' This function depends on the assumption that the order of the estimated 
#' parameters is the same as for the generating parameters. This is automatically
#' true in the \code{\link[paramrel]{execute_study}} function.
#' 
#' @param data Named list containing the parameter names under \code{"parameters"},
#' the true parameter values under \code{"simulated"}, and the estimated 
#' parameter values under \code{"estimated"}. Typically the result of 
#' \code{\link[paramrel]{prepare}}
#' 
#' @return Dataframe containing the parameter (\code{"parameter"}), the 
#' bias, and the MSE
#' 
#' @export
accuracy <- function(data) {
    # Retrieve the relevant information from the list
    estimated <- data$estimated
    simulated <- data$simulated
    cols <- data$parameters

    # Filter out the bin-specific estimates. Not needed for this analysis
    if("bin" %in% colnames(estimated)) {
        estimated <- estimated[estimated$bin == -1, ]
    }

    # Loop over all the parameters and compute the reliability coefficients we 
    # are interested in here
    results <- lapply(
        cols,
        function(x) {
            # Select the data of interest from the data.frame
            sim <- simulated[, x]
            est <- estimates[, x]
            se <- estimates[, paste0("se_", x)]

            # Compute the average directed and undirected bias
            bias_directed <- mean(est - sim)
            bias_undirected <- abs(bias_directed)

            # Compute the relative bias, which is defined as the bias divided by
            # the sum of all true values. Create a directed and undirected 
            # variante
            bias_relative <- sum(est - sim) / sum(sim)
            bias_relative_undirected <- abs(bias_relative)

            # Compute the MSE
            mse <- mean((est - sim)^2)
            
            # Compute coverage of the true parameter based on the analytic 
            # formula for the 95%CI
            z <- qnorm(c(0.025, 0.975))
            ci <- est + z * se
            
            coverage <- mean(sim >= ci[1] & sim <= ci[2])

            # Add all of these in a data.frame and return
            return(
                data.frame(
                    parameter = x,
                    true_value = mean(sim),
                    estimated_value = mean(est),
                    standard_error = mean(se),
                    bias_directed = bias_directed,
                    bias_undirected = bias_undirected,
                    bias_relative = bias_relative,
                    bias_relative_undirected = bias_relative_undirected,
                    mse = mse, 
                    ci_lower = mean(ci[1]),
                    ci_upper = mean(ci[2]),
                    coverage = coverage
                )
            )
        }
    )
    results <- do.call("rbind", results)

    return(results)
}