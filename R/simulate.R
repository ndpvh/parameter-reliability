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
#' \code{\link[param-rel]{main-class}} and 
#' \code{\link[param-rel]{interaction-class}}, a matrix is needed instead, where 
#' the first column is taken to be \eqn{x} (paired with parameter \eqn{b}) and 
#' the second column is taken to be \eqn{z} (paired with parameter \eqn{c}). For
#' the \code{\link[param-rel]{autoregressive-class}}, this argument is ignored.
#' Defaults to \code{NULL}, forcing the user to specify its value for all models
#' except for the \code{\link[param-rel]{autoregressive-class}}, or alternatively
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
             Xfun = NULL,
             N = 100) {

        # Check the values for Xfun. If specified, it will override X
        if(!is.null(Xfun)) {
            X <- Xfun(N)
        }

        # Check if X is still NULL. If so, then we cannot proceed
        if(is.null(X)) {
            stop("X and Xfun are not defined, but linear model needs input. Cannot proceed.")
        }

        # Check whether X is a numeric vector or a matrix. If a matrix, only 
        # select the first column and throw a warning
        if(!is.null(ncol(X))) {
            warning("Multiple columns found in X, but only 1 needed. Selecting the first one.")
            X <- X[, 1]
        }

        # Simulate values of y
        params <- model@parameters 
        sd <- model@sd 

        y <- rnorm(
            mean = params[1] + params[2] * X, 
            sd = sd
        )

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
    "quadratic",
    function(model,
             X = NULL,
             Xfun = NULL,
             N = 100) {

        # Check the values for Xfun. If specified, it will override X
        if(!is.null(Xfun)) {
            X <- Xfun(N)
        }

        # Check if X is still NULL. If so, then we cannot proceed
        if(is.null(X)) {
            stop("X and Xfun are not defined, but quadratic model needs input. Cannot proceed.")
        }

        # Check whether X is a numeric vector or a matrix. If a matrix, only 
        # select the first column and throw a warning
        if(!is.null(ncol(X))) {
            warning("Multiple columns found in X, but only 1 needed. Selecting the first one.")
            X <- X[, 1]
        }

        # Simulate values of y
        params <- model@parameters 
        sd <- model@sd 

        y <- rnorm(
            mean = params[1] + params[2] * X + params[3] * X^2, 
            sd = sd
        )

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
    "main",
    function(model,
             X = NULL,
             Xfun = NULL,
             N = 100) {

        # Check the values for Xfun. If specified, it will override X
        if(!is.null(Xfun)) {
            X <- Xfun(N)
        }

        # Check if X is still NULL. If so, then we cannot proceed
        if(is.null(X)) {
            stop("X and Xfun are not defined, but linear model needs input. Cannot proceed.")
        }

        # Check whether X is a numeric vector or a matrix. If a vector, cannot
        # proceed
        if(is.null(ncol(X))) {
            stop("X is a numeric vector, but the main effects model needs more than one independent variable. Cannot proceed.")
        }

        # If more than 2 columns, throw a warning and select the first 2 columns 
        if(!is.null(ncol(X))) {
            warning("More than 2 columns found in X, but only 2 needed. Selecting the first two.")
            X <- X[, 1:2]
        }

        # Simulate values of y
        params <- model@parameters 
        sd <- model@sd 

        y <- rnorm(
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
             Xfun = NULL,
             N = 100) {

        # Check the values for Xfun. If specified, it will override X
        if(!is.null(Xfun)) {
            X <- Xfun(N)
        }

        # Check if X is still NULL. If so, then we cannot proceed
        if(is.null(X)) {
            stop("X and Xfun are not defined, but linear model needs input. Cannot proceed.")
        }

        # Check whether X is a numeric vector or a matrix. If a vector, cannot
        # proceed
        if(is.null(ncol(X))) {
            stop("X is a numeric vector, but the main effects model needs more than one independent variable. Cannot proceed.")
        }

        # If more than 2 columns, throw a warning and select the first 2 columns 
        if(!is.null(ncol(X))) {
            warning("More than 2 columns found in X, but only 2 needed. Selecting the first two.")
            X <- X[, 1:2]
        }

        # Simulate values of y
        params <- model@parameters 
        sd <- model@sd 

        y <- rnorm(
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
    "autoregressive",
    function(model,
             X = NULL,
             Xfun = NULL,
             N = 100) {

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
            x = numeric(length(y)), 
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
             Xfun = NULL,
             N = 100) {

        # Check the values for Xfun. If specified, it will override X
        if(!is.null(Xfun)) {
            X <- Xfun(N)
        }

        # Check if X is still NULL. If so, then we cannot proceed
        if(is.null(X)) {
            stop("X and Xfun are not defined, but linear model needs input. Cannot proceed.")
        }

        # Check whether X is a numeric vector or a matrix. If a matrix, only 
        # select the first column and throw a warning
        if(!is.null(ncol(X))) {
            warning("Multiple columns found in X, but only 1 needed. Selecting the first one.")
            X <- X[, 1]
        }

        # Retrieve values of the parameters
        params <- model@parameters 
        sd <- model@sd 

        # Draw an initial value of y from equilibrium
        y <- numeric(N)
        y[1] <- rnorm(
            1,
            mean = (params[1] + params[3] * X[1]) / (1 - params[2]),
            sd = sd
        )

        # Simulate the remaining values of y
        for(i in 2:N) {
            y[i] <- rnorm(
                1,
                mean = params[1] + params[2] * y[i - 1] + params[3] * X[i],
                sd = sd 
            )
        }

        # Create a data.frame and return as output
        output <- data.frame(
            time = 1:length(y),
            y = y, 
            x = numeric(length(y)), 
            z = numeric(length(y))
        )

        return(output)
    }
)