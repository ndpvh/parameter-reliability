# PURPOSE: Test-retest reliability analysis for parameter reliability study
# AUTHORS: Kenny Yu & Niels Vanhasbroeck
# DATE: July 2025
# FOCUS: Niels' test-retest analysis
# STATUS: TO BE IMPLEMENTED BY NIELS

#' Generate parameter grid
#' 
#' Use the bounds of the parameters of a model and create a grid of parameter 
#' values. Each parameter value is taken at an equal distance across the interval
#' of the parameter.
#' 
#' @param model Object of one of the different model classes (e.g., linear, 
#' quadratic,...)
#' @param n_int Integer denoting the number of intercepts to draw. Needs to be 
#' larger than or equal to 2. Defaults to \code{5}.
#' @param n_slope Integer denoting the number of slopes to draw. Note that the 
#' autoregressive parameter is also taken to be a slope. Needs to be 
#' larger than or equal to 2. Defaults to \code{5}.
#' @param ... Additional arguments passed on to \code{\link[paramrel]{bounds}}
#' 
#' @return Matrix with dimensions N x k, where k is the number of parameters 
#' of the model and N the total number of combinations possible with these 
#' parameters
#' 
#' @export
parameter_grid <- function(model,
                           n_int = 5,
                           n_slope = 5,
                           ...) {
  
    # Get the bounds on the parameters of the models
    bnd <- bounds(model, ...)

    # Get the drawing numbers per parameter type, as well as the number of 
    # parameters
    k <- nrow(bnd)
    n <- c(
        n_int, 
        rep(n_slope, k - 1)
    )

    # Create a matrix of the correct size
    params <- matrix(
        0,
        nrow = prod(n),
        ncol = k
    )

    # Loop over all parameters, draw the different values, and put them inside 
    # the matrix.
    #
    # Logic behind the assignment is the following: When making unique 
    # combinations of parameters, one can do this deterministically by repeating
    # the same parameters for a given number of times, either repeating each 
    # value a number of times or the complete vector. The number of repetitions 
    # of each type that is needed is a function of the number of unique values 
    # of each column, which are contained in n
    for(i in 1:k) {
        params[, i] <- rep(
            rep(
                seq(bnd[i, 1], bnd[i, 2], length.out = n[i]),
                times = ifelse(i == 1, 1, prod(n[1:(i - 1)]))
            ),
            each = ifelse(i == k, 1, prod(n[(i + 1):k]))
        )
    }

    return(params)  
}

#' Generate a Random Parameter Set
#' 
#' @param n Integer denoting the number of parameters to generate
#' @param mean Numeric vector containing the means of the parameters
#' @param sd Numeric denoting the (shared) standard deviation of the parameters
#' 
#' @return Numeric matrix of size n x k, where n is the number of randomly
#' generated parameter sets and k the number of parameters of the model
#' 
#' @export 
generate_parameters <- function(n, 
                                mean,
                                sd) {
    
    # Loop over all means and generate multiple new values from a normal 
    # distribution
    params <- sapply(
        mean,
        \(x) rnorm(n, mean = x, sd = sd)
    )

    return(params)
}

#' Run Test-Retest Analysis
#' 
#' Given a particular simulation model, estimate a same or other statistical 
#' model and compute the consistency of the estimated parameters. For this, we 
#' use the \eqn{ICC(A, 1)}, testing the agreement of the exact values of the 
#' parameters within participants.
#' 
#' @details 
#' This function assesses the test-retest consistency of a set of estimated 
#' parameters in a particular setting. Specifically, we assume that a same 
#' participant has a same set of parameters that underlie their responses. With
#' this assumption in place, we then simulate a dataset where a participant 
#' encounters the same string of values for the independent variable(s) 
#' multiple times throughout the study. By estimating a statistical model for 
#' each participant and each repeated string of outcomes separately, we can 
#' then assess the test-retest consistency of these estimated parameters, which
#' is the goal of this study
#' 
#' As the operationalization of consistency, we compute the \eqn{ICC(A, 1)} of 
#' the parameters across participants, in our context being defined as:
#' 
#' \deqn{ICC(A, 1) = \frac{\sigma_\text{participants}}^2}{\sigma_\text{total}^2}
#' 
#' where \eqn{\sigma^2} denotes the variance. This \eqn{ICC} puts the systematic 
#' variance -- that is, the interindividual differences in the values of the 
#' parameters -- against the total observed variance in the parameters, checking 
#' whether individual differences can be reliably picked up on.
#' 
#' @param sim_model Object of the \code{\link[paramrel]{model-class}} containing
#' parameters around which participant-specific parameters will be generated 
#' using the \code{\link[paramrel]{generate_parameters}} function. This model 
#' will be used to simulate data.
#' @param est_model Object of the \code{\link[paramrel]{model-class}} that will 
#' be fitted to the data simulated by \code{sim_model}
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
#' @param path Path to the folder where you want to save the different results.
#' Defaults to a folder \code{"results"} in the current working directory
#' @param filename Character denoting the delineating name to be given to the 
#' files that contain the different results. Defaults to \code{"results"}
#' @param ... Additional arguments passed on to 
#' \code{\link[paramrel]{simulate_x}} 
#' 
#' @return Test-retest reliability results
#' 
#' @export
test_retest <- function(sim_model,
                        est_model,
                        n_outcomes = 20,
                        n_bins = 5,
                        parameter_sd = 1,
                        path = file.path("results"),
                        filename = "results") {
  
    # Generate multiple random parameter sets based on the parameter means and 
    # standard deviations
    params <- generate_parameters(
        n_datasets,
        sim_model@parameters,
        parameter_sd
    )

    # Simulate a dataset from each of these parameters
    datasets <- lapply(
        seq_len(nrow(params)),
        function(i) {
            # Change the simulation model's parameters to the individual-
            # specific ones here
            sim_model@parameters <- params[i, ]

            # Create the values for the independent variables. Crucially, these 
            # values should be repeated multiple times, allowing for the 
            # assessment of test-retest
            X <- simulate_x(
                sim_model,
                Xfun = Xfun, 
                N = n_outcomes
            )

            X <- do.call(
                "rbind",
                lapply(1:n_bins, \(i) X)
            )

            # Simulate the data actual data using the values of X
            y <- simulate(
                sim_model,
                X = X
            )

            # Add information on the bins in the dataframe
            y$bin <- rep(1:n_bins, each = n_outcomes)

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
                \(j) estimate(
                    est_model, 
                    datasets[[i]][datasets[[i]]$bin == j, ]
                )$model@parameters
            ) 
            result <- do.call("rbind", result)

            # Make a data.frame with useful labels and information on the person
            # and bins
            result <- result |>
                as.data.frame() |>
                setNames(c("intercept", paste0("slope_", 2:ncol(result) - 1)))
            result$participant <- i 
            result$bin <- 1:n_bins

            return(result)
        }
    )
    estimates <- do.call("rbind", estimates)

    # Do a variance decomposition and compute the ICC for each of the estimated
    # parameters
    cols <- colnames(estimates)
    cols <- cols[-which(cols %in% c("participant", "bin"))]

    icc <- lapply(
        cols,
        function(x) {
            # Select the correct data and change the names as appropriate
            data <- estimates[, c(x, "participant", "bin")] |>
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
            total <- var(data$param)

            # Compute the ICC(A, 1) for this parameter and return as the result
            # of the procedure
            return(
                list(
                    "icc" = systematic / total,
                    "systematic" = systematic, 
                    "total" = total
                )
            )
        }
    )
    names(icc) <- cols  

    # Save all results in the path
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
        icc,
        file.path(path, paste0(filename, "_icc.RDS"))
    )
    
    return(icc)
}
