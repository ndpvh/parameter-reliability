# PURPOSE: Test-retest reliability analysis for parameter reliability study
# AUTHORS: Kenny Yu & Niels Vanhasbroeck
# DATE: Sep 2025
# FOCUS: Handling parameters of the models

#' Generate parameter grid
#' 
#' Use the bounds of the parameters of a model and create a grid of parameter 
#' values. Each parameter value is taken at an equal distance across the interval
#' of the parameter.
#' 
#' @param model Object of one of the different model classes (e.g., linear, 
#' quadratic,...)
#' @param n Integer denoting the number of parameter sets to generate. Overwrites
#' \code{n_int} and \code{n_slope} when specific. Defaults to \code{NA}, meaning 
#' that \code{n_int} and \code{n_slope} are used by default. 
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
                           n = NA,
                           n_int = 5,
                           n_slope = 5,
                           ...) {
  
    # Get the bounds on the parameters of the models
    bnd <- bounds(model, ...)

    # Get the bounds of each parameter type
    k <- nrow(bnd)
    
    # Different approaches to simulating data based on whether n == NA
    if(!(is.na(n))) {
        # Create a matrix that will contain the parameters
        params <- matrix(
            0,
            nrow = n, 
            ncol = k
        )

        # Sample from a uniform distribution between the predefined bounds
        for(i in 1:k) {
            params[, i] <- runif(
                n, 
                min = bnd[i, 1],
                max = bnd[i, 2]
            )
        }

    } else {
        # Define number of parameters that we're going to simulate
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