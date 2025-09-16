# PURPOSE: Test-retest reliability analysis for parameter reliability study
# AUTHORS: Kenny Yu & Niels Vanhasbroeck
# DATE: Sep 2025
# FOCUS: Providing a function to set up and execute our simulation study

#' Set-up Study
#' 
#' Given a particular simulation model, estimate a same or other statistical 
#' model and compute the summary statistics of interest. In our simulation, we 
#' assume that a same participant has a same set of parameters that underlie 
#' their responses throughout the full study. During the study, the participant 
#' encounters the same string of values for the independent variable(s) 
#' multiple times throughout the study, allowing for the assessment of test-retest
#' reliability.
#' 
#' @param sim_model Object of the \code{\link[paramrel]{model-class}} containing
#' parameters around which participant-specific parameters will be generated 
#' using the \code{\link[paramrel]{generate_parameters}} function. This model 
#' will be used to simulate data.
#' @param est_model Object of the \code{\link[paramrel]{model-class}} that will 
#' be fitted to the data simulated by \code{sim_model}
#' @param n_participants Integer denoting the number of participants to include
#' in the study
#' @param n_outcomes Integer denoting the number of outcomes that exists within
#' each string. Together with \code{n_bins}, the total number of observations per 
#' participant then becomes \code{N = n_bins * n_outcomes}. Defaults to 
#' \code{20}.
#' @param n_bins Integer denoting the number of repeated strings of outcomes 
#' to include in the study. Together with \code{n_outcomes}, the total number of
#' observations per participant then becomes \code{N = n_bins * n_outcomes}. 
#' Defaults to \code{5}.
#' @param parameter_sd (Positive) numeric denoting the population standard 
#' deviation of the parameters, therefore indicating how many individual 
#' differences that exist on each parameter of the simulating model 
#' \code{sim_model}. Defaults to \code{1}
#' @param R2 Numeric between 0 and 1 denoting the \eqn{R^2} of the model on the 
#' within-person level. If specified, it will compute a residual standard 
#' deviation that allows for the determinstic part of the model to have an 
#' \eqn{R^2} as specified. Defaults to \code{NA}, triggering the use of the 
#' model-specified residual standard deviation. Passed on to 
#' \code{\link[paramrel]{simulate}}
#' @param ICC Numeric between 0 and 1 denoting the value of the ICC that you 
#' want to simulate in the data. If specified, it will change the values of each 
#' of the parameters in the model across bins, emulating variation across bins  
#' and therefore the consistency with which they can be recovered. Defaults to 
#' \code{1}, meaning that parameters don't change across bins.
#' @param save_results Logical denoting whether to save the results. Defaults to
#' \code{TRUE}
#' @param path Path to the folder where you want to save the different results.
#' Defaults to a folder \code{"results"} in the current working directory
#' @param filename Character denoting the delineating name to be given to the 
#' files that contain the different results. Defaults to \code{"results"}
#' @param statistics Named list containing single-argument functions that take 
#' in the estimated and generating values of the parameters and perform analyses 
#' on these values. Defaults to all functions defined in _statistics.R_.
#' @param ... Additional arguments passed on to 
#' \code{\link[paramrel]{simulate_x}} 
#' 
#' @return Test-retest reliability results
#' 
#' @export
execute_study <- function(sim_model,
                          est_model,
                          n_participants = 100,
                          n_outcomes = 20,
                          n_bins = 5,
                          parameter_sd = 1,
                          ICC = 1,
                          R2 = NA,
                          save_results = TRUE,
                          path = file.path("results"),
                          filename = "results",
                          statistics = list(
                              "descriptives" = descriptives,
                              "reliability" = reliability,
                              "icc" = icc,
                              "accuracy" = accuracy
                          ),
                          ...) {

    # Check whether the ICC is within bounds
    if(ICC < 0) {
        stop("ICC is lower than 0. Please specify a number between 0 and 1.")
    }

    if(ICC > 1) {
        stop("ICC is larger than 1. Please specify a number between 0 and 1.")
    }
  
    # Generate multiple random parameter sets based on the parameter means and 
    # standard deviations
    params <- generate_parameters(
        n_participants,
        sim_model@parameters,
        parameter_sd
    )

    # Simulate a dataset from each of these parameters
    datasets <- lapply(
        seq_len(nrow(params)),
        function(i) {
            # Create the values for the independent variables. Crucially, these 
            # values should be repeated multiple times, allowing for the 
            # assessment of test-retest
            X <- simulate_x(
                sim_model,
                N = n_outcomes,
                ...
            )

            # Differentiate different cases based on the ICC that is specified.
            # Specifically, we differentiate:
            #   - ICC = 1; In this case, we use the same parameters across all 
            #              bins, meaning that the participant does not change 
            #              over time. 
            #   - ICC = 0; In this case, the participant shows no consistency 
            #              across bins, meaning we resample their parameters 
            #              at each time. Note that in reality, the within-person
            #              standard deviation would have to be infinite for the 
            #              ICC to be 0. In this simulation, we are bounded to 
            #              a standard deviation of 100 instead
            #   - Otherwise: In this case, the participant shows some 
            #                consistency, meaning their parameters change but
            #                with a somewhat lower standard deviation than the 
            #                population standard deviation.
            #
            # Luckily, we can use the same procedure for all ICCs, albeit with 
            # bounds on what the within-person standard deviation can be

            # Define the within-person standard deviation based on the 
            # provided ICC. Introduce some bounds on this standard deviation as
            # well
            within_sd <- sqrt((1 - ICC) * parameter_sd^2 / ICC)
            within_sd <- ifelse(within_sd > 100, 100, within_sd)

            # We have to loop over the bins, resample their parameters, and
            # generate new data
            y <- lapply(
                1:n_bins, 
                function(j) {
                    # Adjust the parameters of the model
                    new_params <- rnorm(
                        length(params[i, ]), 
                        mean = params[i, ],
                        sd = within_sd
                    )
                    sim_model@parameters <- new_params

                    # Simulate data and add the bin number to it
                    y <- simulate(
                        sim_model,
                        X = X,
                        R2 = R2
                    )
                    y$bin <- j

                    return(y)
                }
            )
            y <- do.call("rbind", y)

            return(y)
        }
    )

    # Estimate the estimation model based on the supplied data
    estimates <- lapply(
        seq_along(datasets),
        function(i) {
            # Estimate the parameters of the model for each bin separately
            result <- lapply(
                1:n_bins, 
                function(j) {
                    result <- estimate(
                        est_model, 
                        datasets[[i]][datasets[[i]]$bin == j, ]
                    )
                    
                    # Combine parameters with standard errors in one numeric and
                    # return
                    estimates <- result$model@parameters
                    standard_error <- summary(result$fit)$coefficients[, 2]

                    return(c(estimates, standard_error))
                }
            ) 
            result <- do.call("rbind", result)

            # Make a data.frame with useful labels and information on the person
            # and bins
            cols <- c("intercept", paste0("slope_", 2:ncol(result) - 1))
            cols <- paste0(
                rep(c("", "se_"), each = length(cols)),
                rep(cols, times = 2)
            )

            result <- result |>
                as.data.frame() |>
                setNames()
            result$participant <- i 
            result$bin <- 1:n_bins

            # Also estimate the model on the full dataset, without accounting 
            # for the bins
            full <- estimate(
                est_model,
                datasets[[i]]
            )

            estimates <- full$model@parameters
            standard_error <- summary(full$fit)$coefficients[, 2]

            result <- rbind(
                result, 
                c(estimates, standard_error, i, -1) |>
                    matrix(nrow = 1) |>
                    as.data.frame() |>
                    setNames(colnames(result))
            )

            # Return the result
            return(result)
        }
    )
    estimates <- do.call("rbind", estimates)

    # Prepare the estimates for analysis 
    cols <- c("intercept", paste0("slope_", 2:ncol(params) - 1))
    params <- params |>
        as.data.frame() |>
        setNames(cols)

    prepared <- prepare(estimates, params)

    # Loop over all the statistical analyses we want to do on these data and 
    # provide the result with a useful name
    results <- lapply(
        statistics, 
        \(fx) fx(prepared)
    )
    names(results) <- names(statistics)

    # Save all results in the path if requested
    if(save_results) {
        saveRDS(
            params,
            file.path(path, paste0(filename, "_simulates.RDS"))
        )
        saveRDS(
            datasets,
            file.path(path, paste0(filename, "_data.RDS"))
        )
        saveRDS(
            estimates,
            file.path(path, paste0(filename, "_estimates.RDS"))
        )
        saveRDS(
            results,
            file.path(path, paste0(filename, "_results.RDS"))
        )
    }
    
    return(results)
}
