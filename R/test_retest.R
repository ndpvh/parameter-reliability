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
    # the matrix
    for(i in 1:k) {
        params[, i] <- rep(
            rep(
                seq(bnd[i, 1], bnd[i, 2], length.out = n[i]),
                times = i
            ),
            each = k - i + 1
        )
    }

    return(params)  
}

#' Run Test-Retest Analysis for Single Condition
#' 
#' @param condition_params Parameters for the condition
#' @param n_datasets Number of datasets to generate
#' @param sample_size Sample size per dataset
#' @param error_sd Error standard deviation
#' @return Test-retest reliability results
run_testretest_condition <- function(condition_params, n_datasets = 100, sample_size = 100, error_sd = 1) {
  
  # TODO: Niels to implement
  # Should:
  # 1. Generate multiple datasets with same parameters
  # 2. Fit models to each dataset
  # 3. Calculate test-retest reliability metrics
  # 4. Return reliability statistics
  
  stop("Test-retest analysis not yet implemented. This is Niels' part.")
}

#' Generate Complete Test-Retest Study Design
#' 
#' @param study_size Size of study ("pilot", "medium", "full")
#' @return List of study conditions
generate_testretest_study <- function(study_size = "pilot") {
  
  # TODO: Niels to implement
  # Should create complete study design with:
  # - Parameter combinations
  # - Sample size variations
  # - Error variance variations
  
  stop("Test-retest analysis not yet implemented. This is Niels' part.")
}

#' Run Complete Test-Retest Study
#' 
#' @param study_size Size of study
#' @param output_dir Output directory
#' @return Study results
run_testretest_study <- function(study_size = "pilot", output_dir = "testretest_results") {
  
  # TODO: Niels to implement
  # Should:
  # 1. Generate study design
  # 2. Run all conditions
  # 3. Save results
  # 4. Provide summary
  
  stop("Test-retest analysis not yet implemented.")
}

#' Quick Test of Test-Retest Analysis
#' 
#' @return TRUE if test passes, FALSE otherwise
test_testretest <- function() {
  
  # TODO: Niels to implement
  # Should run a quick test to verify test-retest analysis works
  
  message("Test-retest analysis not yet implemented.")
  return(FALSE)
}

