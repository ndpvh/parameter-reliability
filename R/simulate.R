# Purpose: Data simulation methods for parameter reliability study
# Authors: Kenny Yu & Niels Vanhasbroeck  
# Date: July 2025

#' Simulate Data from a Model
#' 
#' Method for the different models that allows the simulation of data for these
#' models.
#'
#' @param model Object of one of the different model classes (e.g., linear, 
#' quadratic,...)
#' @param X Numeric vector or matrix containing the values of the independent 
#' variables. For \code{\link[param-rel]{linear-class}}, 
#' \code{\link[param-rel]{quadratic-class}}, \code{\link[param-rel]{arx-class}},
#' a numeric vector suffices, as only one independent variable is required. For 
#' \code{\link[param-rel]{main_effect-class}} and 
#' \code{\link[param-rel]{interaction-class}}, a matrix is needed instead, where 
#' the first column is taken to be \eqn{x} (paired with parameter \eqn{b}) and 
#' the second column is taken to be \eqn{z} (paired with parameter \eqn{c}). For
#' the \code{\link[param-rel]{ar1-class}}, this argument is ignored.
#' Defaults to \code{NULL}, forcing the user to specify its value for all models
#' except for the \code{\link[param-rel]{ar1-class}}, or alternatively
#' to specify a function with which to generate values of \code{X} through the
#' \code{Xfun} argument.
#' @param Xfun Function with which to simulate values for \code{X}. Should take 
#' in only a single argument, namely \code{N}, specifying how many values for
#' \code{X} to simulate. Defaults to \code{NULL}, forcing the user to either 
#' specify \code{X} or \code{Xfun}. Note that the output of \code{Xfun} is used 
#' as an alternative to \code{X}, meaning that it should conform to distinction 
#' between numeric vector and numeric matrix specified for the \code{X} argument.
#' @param N Integer denoting the number of values that should be simulated. 
#' Ignored when \code{X} is defined. Defaults to \code{100}.
#' 
#' @return Dataframe containing the values of the variables (\eqn{y}, \eqn{x}, 
#' and if applicable \eqn{z}, named as such) and time (\code{time})
#' 
#' @examples 
#' 
#' @rdname simulate-method
#' 
#' @export
setGeneric(
    "simulate",
    function(model, ...) standardGeneric("simulate"),
    signature = "model"
)





#' @rdname simulate-method
setMethod(
    "simulate",
    "linear",
    function(model,
             X = NULL,
             Xfun = \(x) runif(x, -2, 2),
             N = 100) {

        # Check if X and Xfun are NULL. If so, then we cannot proceed
        if(is.null(X) & is.null(Xfun)) {
            stop("X and Xfun are not defined, but linear model needs input. Cannot proceed.")
        }

        # Check whether X is defined or not. If not, then we have to use Xfun 
        # to generate values of X
        if(is.null(X)) {
            X <- simulate_x(
                model,
                Xfun = Xfun,
                N = N
            )
        }

        # If X is not a matrix, then we need to make it one
        if(is.null(ncol(X))) {
            X <- matrix(X, ncol = 1)
        }

        # Check whether X has more than one column. If so, this isn't expected and 
        # we have to select only one
        if(ncol(X) > 1) {
            warning("Multiple columns found in X, but only one needed. Selecting the first one.")
            X <- X[, 1]
        }

        # Simulate values of y
        params <- model@parameters 
        sd <- model@sd 

        y <- rnorm(
            nrow(X),
            mean = params[1] + params[2] * X, 
            sd = sd
        )

        # Create a data.frame and return as output
        output <- data.frame(
            time = 1:length(y),
            y = y, 
            x = X[, 1], 
            z = numeric(length(y))
        )

        return(output)
    }
)





#' @rdname simulate-method
setMethod(
    "simulate",
    "quadratic",
    function(model,
             X = NULL,
             Xfun = \(x) runif(x, -2, 2),
             N = 100) {

        # Check if X and Xfun are NULL. If so, then we cannot proceed
        if(is.null(X) & is.null(Xfun)) {
            stop("X and Xfun are not defined, but quadratic model needs input. Cannot proceed.")
        }

        # Check whether X is defined or not. If not, then we have to use Xfun 
        # to generate values of X
        if(is.null(X)) {
            X <- simulate_x(
                model,
                Xfun = Xfun,
                N = N
            )
        }

        # If X is not a matrix, then we need to make it one
        if(is.null(ncol(X))) {
            X <- matrix(X, ncol = 1)
        }

        # Check whether X has more than one column. If so, this isn't expected and 
        # we have to select only one
        if(ncol(X) > 1) {
            warning("Multiple columns found in X, but only one needed. Selecting the first one.")
            X <- X[, 1]
        }

        # Simulate values of y
        params <- model@parameters 
        sd <- model@sd 

        y <- rnorm(
            nrow(X),
            mean = params[1] + params[2] * X + params[3] * X^2, 
            sd = sd
        )

        # Create a data.frame and return as output
        output <- data.frame(
            time = 1:length(y),
            y = y, 
            x = X[, 1], 
            z = numeric(length(y))
        )

        return(output)
    }
)





#' @rdname simulate-method
setMethod(
    "simulate",
    "cubic",
    function(model,
             X = NULL,
             Xfun = \(x) runif(x, -2, 2),
             N = 100) {

        # Check if X and Xfun are NULL. If so, then we cannot proceed
        if(is.null(X) & is.null(Xfun)) {
            stop("X and Xfun are not defined, but cubic model needs input. Cannot proceed.")
        }

        # Check whether X is defined or not. If not, then we have to use Xfun 
        # to generate values of X
        if(is.null(X)) {
            X <- simulate_x(
                model,
                Xfun = Xfun,
                N = N
            )
        }

        # If X is not a matrix, then we need to make it one
        if(is.null(ncol(X))) {
            X <- matrix(X, ncol = 1)
        }

        # Check whether X has more than one column. If so, this isn't expected and 
        # we have to select only one
        if(ncol(X) > 1) {
            warning("Multiple columns found in X, but only one needed. Selecting the first one.")
            X <- X[, 1]
        }

        # Simulate values of y
        params <- model@parameters 
        sd <- model@sd 

        y <- rnorm(
            nrow(X),
            mean = params[1] + params[2] * X + params[3] * X^2 + params[4] * X^3, 
            sd = sd
        )

        # Create a data.frame and return as output
        output <- data.frame(
            time = 1:length(y),
            y = y, 
            x = X[, 1], 
            z = numeric(length(y))
        )

        return(output)
    }
)





#' @rdname simulate-method
setMethod(
    "simulate",
    "main_effect",
    function(model,
             X = NULL,
             Xfun = list(
                 \(x) runif(x, -2, 2),
                 \(x) runif(x, -2, 2)
             ),
             N = 100) {

        # Check if X and Xfun are NULL. If so, then we cannot proceed
        if(is.null(X) & is.null(Xfun)) {
            stop("X and Xfun are not defined, but main_effects model needs input. Cannot proceed.")
        }

        # Check whether X is defined or not. If not, then we have to use Xfun 
        # to generate values of X
        if(is.null(X)) {
            X <- simulate_x(
                model,
                Xfun = Xfun,
                N = N
            )
        }

        # If X is not a matrix, then we need to make it one
        if(is.null(ncol(X))) {
            stop("X is a numeric vector, but the main_effect effects model needs more than one independent variable. Cannot proceed.")
        }

        # Check whether X has more than two columns. If so, this isn't expected and 
        # we have to select only two
        if(ncol(X) > 2) {
            warning("More than two columns found in X where only two are needed. Selecting the first two.")
            X <- X[, 1:2]
        }

        # Simulate values of y
        params <- model@parameters 
        sd <- model@sd 

        y <- rnorm(
            nrow(X),
            mean = params[1] + params[2] * X[, 1] + params[3] * X[, 2], 
            sd = sd
        )

        # Create a data.frame and return as output
        output <- data.frame(
            time = 1:length(y),
            y = y, 
            x = X[, 1], 
            z = X[, 2]
        )

        return(output)
    }
)





#' @rdname simulate-method
setMethod(
    "simulate",
    "interaction",
    function(model,
             X = NULL,
             Xfun = list(
                 \(x) runif(x, -2, 2),
                 \(x) runif(x, -2, 2)
             ),
             N = 100) {

        # Check if X and Xfun are NULL. If so, then we cannot proceed
        if(is.null(X) & is.null(Xfun)) {
            stop("X and Xfun are not defined, but interaction model needs input. Cannot proceed.")
        }

        # Check whether X is defined or not. If not, then we have to use Xfun 
        # to generate values of X
        if(is.null(X)) {
            X <- simulate_x(
                model,
                Xfun = Xfun,
                N = N
            )
        }

        # If X is not a matrix, then we need to make it one
        if(is.null(ncol(X))) {
            stop("X is a numeric vector, but the interaction model needs more than one independent variable. Cannot proceed.")
        }

        # Check whether X has more than two columns. If so, this isn't expected and 
        # we have to select only two
        if(ncol(X) > 2) {
            warning("More than two columns found in X where only two are needed. Selecting the first two.")
            X <- X[, 1:2]
        }

        # Simulate values of y
        params <- model@parameters 
        sd <- model@sd 

        y <- rnorm(
            nrow(X),
            mean = params[1] + params[2] * X[, 1] + params[3] * X[, 2] + params[4] * X[, 1] * X[, 2], 
            sd = sd
        )

        # Create a data.frame and return as output
        output <- data.frame(
            time = 1:length(y),
            y = y, 
            x = X[, 1], 
            z = X[, 2]
        )

        return(output)
    }
)





#' @rdname simulate-method
setMethod(
    "simulate",
    "ar1",
    function(model,
             X = NULL,
             Xfun = \(x) runif(x, -2, 2),
             N = 100) {

        # If X is defined, we have to change N in favor of X. Otherwise we keep
        # N
        if(!is.null(X)) {
            # Differentiate between vector and matrix
            if(is.null(ncol(X))) {
                N <- length(X)
            } else {
                N <- nrow(X)
            }

        } else {
            X <- numeric(N)
        }

        # Retrieve values of the parameters
        params <- model@parameters 
        sd <- model@sd 

        # Draw an initial value of y from equilibrium
        y <- numeric(N)
        y[1] <- rnorm(
            1,
            mean = params[1] / (1 - params[2]),
            sd = sd
        )

        # Simulate the remaining values of y
        for(i in 2:N) {
            y[i] <- rnorm(
                1,
                mean = params[1] + params[2] * y[i - 1],
                sd = sd 
            )
        }

        # Create a data.frame and return as output
        output <- data.frame(
            time = 1:length(y),
            y = y, 
            x = X, 
            z = numeric(length(y))
        )

        return(output)
    }
)





#' @rdname simulate-method
setMethod(
    "simulate",
    "arx",
    function(model,
             X = NULL,
             Xfun = \(x) runif(x, -2, 2),
             N = 100) {

        # Check if X and Xfun are NULL. If so, then we cannot proceed
        if(is.null(X) & is.null(Xfun)) {
            stop("X and Xfun are not defined, but arx model needs input. Cannot proceed.")
        }

        # Check whether X is defined or not. If not, then we have to use Xfun 
        # to generate values of X
        if(is.null(X)) {
            X <- simulate_x(
                model,
                Xfun = Xfun,
                N = N
            )
        }

        # If X is not a matrix, then we need to make it one
        if(is.null(ncol(X))) {
            X <- matrix(X, ncol = 1)
        }

        # Check whether X has more than one column. If so, this isn't expected and 
        # we have to select only one
        if(ncol(X) > 1) {
            warning("Multiple columns found in X, but only one needed. Selecting the first one.")
            X <- X[, 1]
        }

        # Retrieve values of the parameters
        params <- model@parameters 
        sd <- model@sd 

        # Draw an initial value of y from equilibrium
        y <- numeric(N)
        y[1] <- rnorm(
            1,
            mean = (params[1] + params[3] * X[1, 1]) / (1 - params[2]),
            sd = sd
        )

        # Simulate the remaining values of y
        for(i in 2:N) {
            y[i] <- rnorm(
                1,
                mean = params[1] + params[2] * y[i - 1] + params[3] * X[i, 1],
                sd = sd 
            )
        }

        # Create a data.frame and return as output
        output <- data.frame(
            time = 1:length(y),
            y = y, 
            x = X[, 1], 
            z = numeric(length(y))
        )

        return(output)
    }
)










################################################################################
# SIMULATION OF INDEPENDENT VARIABLES

single_x <- function(model, 
                     Xfun = \(x) runif(x, -2, 2),
                     N = 100) {

    # Check whether multiple functions were provided. If so, then we choose the 
    # first one and throw a warning
    if(is.list(Xfun)) {
        warning("Provided more than one function to generate values of the independent variable where only one is needed. Using the first function.")
        Xfun <- Xfun[[1]]
    }

    # Create simulated values with Xfun
    X <- Xfun(N)

    # If the result is a matrix, we only use the first column
    if(!is.null(ncol(X))) {
        warning("Simulated values for the independent variable is a matrix where a vector is expected. Using first column.")
        X <- X[, 1]
    }

    # Transform to a matrix and return
    return(matrix(X, ncol = 1))
}

double_x <- function(model, 
                     Xfun = list(
                        \(x) runif(x, -2, 2),
                        \(x) runif(x, -2, 2)
                     ),
                     N = 100) {

    # Check whether only a single function has been provided. If so, then we 
    # throw a warning and create a list anyway
    if(!is.list(Xfun)) {
        warning("Provided a single function to generate the independent variables where two are needed. Using the same function for both independent variables.")
        Xfun <- list(Xfun, Xfun)
    }

    # Check how many functions have been provided and whether this corresponds to
    # the expectations
    if(length(Xfun) > 2) {
        warning("Provided more than two functions to generate values of the independent variable where only two are needed. Using the first two functions.")
        Xfun <- Xfun[1:2]
    }

    if(length(Xfun) < 2) {
        if(length(Xfun) == 1) {
            warning("Provided a single function to generate the independent variables where two are needed. Using the same function for both independent variables.")
            Xfun <- list(Xfun[[1]], Xfun[[1]])
        } else {
            stop("Provided no function to generate values of the independent variable where two are needed. Cannot proceed.")
        }
    }

    # Create simulated values with Xfun
    X <- sapply(
        1:2,
        \(i) Xfun[[i]](N)
    )

    # If the functions provided matrix output
    if(nrow(X) > N) {
        warning("The provided function provides a matrix as output, leading to more than N simulated values for the independent variables. Using only the first N rows.")
        X <- X[1:N, ]
    }

    # Transform to a matrix and return
    return(X)
}

#' Simulate Values for the Independent Variable for a Model
#' 
#' @param model Object of one of the different model classes (e.g., linear, 
#' quadratic,...)
#' @param Xfun One or more functions with which to simulate values for the independent 
#' variable(s). Should take in only a single argument, namely \code{N}, 
#' specifying how many values for \code{X} to simulate. Defaults to a uniform 
#' distribution bounded between -2 and 2.
#' @param N Integer denoting the number of values that should be simulated. 
#' Defaults to \code{100}.
#' 
#' @return Matrix containing the values of the independent variables
#' 
#' @examples 
#' 
#' @rdname simulate_x-method
#' 
#' @export
setGeneric(
    "simulate_x",
    function(model, ...) standardGeneric("simulate_x"),
    signature = "model"
)

#' @rdname simulate_x-method
setMethod("simulate_x", "linear", single_x)

#' @rdname simulate_x-method
setMethod("simulate_x", "quadratic", single_x)

#' @rdname simulate_x-method
setMethod("simulate_x", "cubic", single_x)

#' @rdname simulate_x-method
setMethod("simulate_x", "main", double_x)

#' @rdname simulate_x-method
setMethod("simulate_x", "interaction", double_x)

#' @rdname simulate_x-method
setMethod("simulate_x", "ar1", single_x)

#' @rdname simulate_x-method
setMethod("simulate_x", "arx", single_x)