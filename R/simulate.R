# Purpose: Data simulation methods for parameter reliability study
# Authors: Kenny Yu & Niels Vanhasbroeck  
# Date: July 2025

# Generic Simulate Method
setGeneric(
    "simulate",
    function(model, ...) standardGeneric("simulate"),
    signature = "model"
)

# Simulate Linear Model Data
setMethod(
    "simulate",
    "linear",
    function(model, X = NULL, N = 100) {
        
        if(is.null(X)) {
            X <- runif(N, -2, 2)
        }
        
        if(is.matrix(X) || is.data.frame(X)) {
            X <- as.numeric(X[, 1])
        }
        
        if(!is.numeric(X)) {
            stop("X must be numeric")
        }
        
        params <- model@parameters
        if(length(params) < 2) {
            stop("Linear model requires 2 parameters")
        }
        
        y <- params[1] + params[2] * X + rnorm(length(X), 0, model@sd)
        
        data.frame(
            y = y,
            x = X,
            z = rep(0, length(y)),
            time = seq_along(y)
        )
    }
)

# Simulate Quadratic Model Data
setMethod(
    "simulate",
    "quadratic",
    function(model, X = NULL, N = 100) {
        
        if(is.null(X)) {
            X <- runif(N, -2, 2)
        }
        
        if(is.matrix(X) || is.data.frame(X)) {
            X <- as.numeric(X[, 1])
        }
        
        if(!is.numeric(X)) {
            stop("X must be numeric")
        }
        
        params <- model@parameters
        if(length(params) < 3) {
            stop("Quadratic model requires 3 parameters")
        }
        
        y <- params[1] + params[2] * X + params[3] * X^2 + rnorm(length(X), 0, model@sd)
        
        data.frame(
            y = y,
            x = X,
            z = rep(0, length(y)),
            time = seq_along(y)
        )
    }
)

# Simulate Cubic Model Data
setMethod(
    "simulate",
    "cubic",
    function(model, X = NULL, N = 100) {
        
        if(is.null(X)) {
            X <- runif(N, -2, 2)
        }
        
        if(is.matrix(X) || is.data.frame(X)) {
            X <- as.numeric(X[, 1])
        }
        
        if(!is.numeric(X)) {
            stop("X must be numeric")
        }
        
        params <- model@parameters
        if(length(params) < 4) {
            stop("Cubic model requires 4 parameters")
        }
        
        y <- params[1] + params[2] * X + params[3] * X^2 + params[4] * X^3 + rnorm(length(X), 0, model@sd)
        
        data.frame(
            y = y,
            x = X,
            z = rep(0, length(y)),
            time = seq_along(y)
        )
    }
)

# Simulate Interaction Model Data
setMethod(
    "simulate",
    "interaction",
    function(model, X = NULL, N = 100) {
        
        if(is.null(X)) {
            X <- cbind(runif(N, -2, 2), runif(N, -2, 2))
        }
        
        if(is.vector(X)) {
            stop("Interaction model requires 2 predictors")
        }
        if(ncol(X) < 2) {
            stop("Interaction model requires 2 predictors")
        }
        if(ncol(X) > 2) {
            X <- X[, 1:2]
        }
        
        params <- model@parameters
        if(length(params) < 4) {
            stop("Interaction model requires 4 parameters")
        }
        
        y <- params[1] + params[2] * X[, 1] + params[3] * X[, 2] + 
            params[4] * X[, 1] * X[, 2] + rnorm(nrow(X), 0, model@sd)
        
        data.frame(
            y = y,
            x = X[, 1],
            z = X[, 2],
            time = seq_along(y)
        )
    }
)

# Simulate AR(1) Model Data
setMethod(
    "simulate",
    "ar1",
    function(model, X = NULL, N = 100, burn_in = 100) {
        
        params <- model@parameters
        if(length(params) < 3) {
            stop("AR(1) model requires 3 parameters")
        }
        
        intercept <- params[1]
        x_slope <- params[2]
        ar_coef <- params[3]
        
        # Check for stationarity
        if(abs(ar_coef) >= 0.99) {
            stop("AR coefficient must be less than 0.99 for stationarity")
        }
        
        total_n <- N + burn_in
        
        if(is.null(X)) {
            X <- runif(total_n, -2, 2)
        }
        
        if(is.matrix(X) || is.data.frame(X)) {
            X <- as.numeric(X[, 1])
        }
        
        if(length(X) < total_n) {
            X <- c(X, runif(total_n - length(X), -2, 2))
        } else if(length(X) > total_n) {
            X <- X[1:total_n]
        }
        
        # Initialize y vector
        y <- numeric(total_n)
        
        # Stationary initial value
        if(abs(ar_coef) > 0.01) {
            stationary_var <- model@sd^2 / (1 - ar_coef^2)
            y[1] <- rnorm(1, intercept / (1 - ar_coef), sqrt(stationary_var))
        } else {
            y[1] <- rnorm(1, intercept, model@sd)
        }
        
        # Generate AR(1) process
        for(t in 2:total_n) {
            mean_y <- intercept + x_slope * X[t] + ar_coef * y[t-1]
            y[t] <- mean_y + rnorm(1, 0, model@sd)
        }
        
        # Remove burn-in period
        keep_indices <- (burn_in + 1):total_n
        y_final <- y[keep_indices]
        x_final <- X[keep_indices]
        y_lag_final <- y[keep_indices - 1]
        
        # Check for reasonable values
        if(any(!is.finite(y_final)) || any(!is.finite(y_lag_final))) {
            warning("Non-finite values in AR(1) simulation")
        }
        
        data.frame(
            y = y_final,
            x = x_final,
            y_lag = y_lag_final,
            z = rep(0, N),
            time = seq_len(N)
        )
    }
)