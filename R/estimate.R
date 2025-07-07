#' Estimate a Model from Data
#' 
#' Estimation of the different models from a particular data set. Uses the 
#' \code{lm} function under the hood.
#'
#' @param model Object representing the model to be used. Note that this may just
#' be the class name and does not need any particular parameters to be specified.
#' @param data Data.frame containing the relevant variables, specifically 
#' \code{"y"}, \code{"x"}, \code{"z"}, and \code{"time"}. Ideally, all variables
#' are present in the data.frame, even when they are not applicable to a 
#' particular model.
#' 
#' @return List containing the result of \code{lm} under \code{"fit"} and the 
#' model with estimated parameters under \code{"model"}
#' 
#' @examples 
#' 
#' @rdname estimate-method
#' 
#' @export
setGeneric(
    "estimate",
    function(model, ...) standardGeneric("estimate"),
    signature = "model"
)





#' @rdname estimate-method
setMethod(
    "estimate",
    "linear",
    function(model,
             data) {

        # Check if the relevant columns are present in the data
        if(!all(c("y", "x") %in% colnames(data))) {
            stop("The relevant columns y and/or x are not present in the data. Cannot proceed.")
        }

        # Estimate the linear model
        fit <- lm(
            y ~ x,
            data = data
        )

        # Create a model with the estimated parameters and create the output
        output <- list(
            "fit" = fit, 
            "model" = linear(
                parameters = as.numeric(fit$coefficients),
                sd = sd(fit$residuals)
            )
        )

        return(output)
    }
)





#' @rdname estimate-method
setMethod(
    "estimate",
    "quadratic",
    function(model,
             data) {

        # Check if the relevant columns are present in the data
        if(!all(c("y", "x") %in% colnames(data))) {
            stop("The relevant columns y and/or x are not present in the data. Cannot proceed.")
        }

        # Estimate the quadratic model
        fit <- lm(
            y ~ x + I(x^2),
            data = data
        )

        # Create a model with the estimated parameters and create the output
        output <- list(
            "fit" = fit, 
            "model" = quadratic(
                parameters = as.numeric(fit$coefficients),
                sd = sd(fit$residuals)
            )
        )

        return(output)
    }
)





#' @rdname estimate-method
setMethod(
    "estimate",
    "main",
    function(model,
             data) {

        # Check if the relevant columns are present in the data
        if(!all(c("y", "x", "z") %in% colnames(data))) {
            stop("The relevant columns y and/or x and/or z are not present in the data. Cannot proceed.")
        }

        # Estimate the main effects model
        fit <- lm(
            y ~ x + z,
            data = data
        )

        # Create a model with the estimated parameters and create the output
        output <- list(
            "fit" = fit, 
            "model" = main(
                parameters = as.numeric(fit$coefficients),
                sd = sd(fit$residuals)
            )
        )

        return(output)
    }
)





#' @rdname estimate-method
setMethod(
    "estimate",
    "interaction",
    function(model,
             data) {

        # Check if the relevant columns are present in the data
        if(!all(c("y", "x", "z") %in% colnames(data))) {
            stop("The relevant columns y and/or x and/or z are not present in the data. Cannot proceed.")
        }

        # Estimate the interaction model
        fit <- lm(
            y ~ x + z + x * z,
            data = data
        )

        # Create a model with the estimated parameters and create the output
        output <- list(
            "fit" = fit, 
            "model" = interaction(
                parameters = as.numeric(fit$coefficients),
                sd = sd(fit$residuals)
            )
        )

        return(output)
    }
)





#' @rdname estimate-method
setMethod(
    "estimate",
    "autoregressive",
    function(model,
             data) {

        # Check if the relevant columns are present in the data
        if(!all("y" %in% colnames(data))) {
            stop("The relevant columns y and/or time are not present in the data. Cannot proceed.")
        }

        # Sort the data based on time
        data <- dplyr::arrange(time)

        # Estimate the linear model
        lagged_data <- data.frame(
            y0 = data$y[2:nrow(data) - 1],
            y = data$y[2:nrow(data)]
        )

        fit <- lm(
            y ~ y0,
            data = lagged_data
        )

        # Create a model with the estimated parameters and create the output
        output <- list(
            "fit" = fit, 
            "model" = autoregressive(
                parameters = as.numeric(fit$coefficients),
                sd = sd(fit$residuals)
            )
        )

        return(output)
    }
)





#' @rdname estimate-method
setMethod(
    "estimate",
    "arx",
    function(model,
             data) {

        # Check if the relevant columns are present in the data
        if(!all(c("y", "x") %in% colnames(data))) {
            stop("The relevant columns y and/or x and/or time are not present in the data. Cannot proceed.")
        }

        # Sort the data based on time
        data <- dplyr::arrange(time)

        # Estimate the linear model
        lagged_data <- data.frame(
            y0 = data$y[2:nrow(data) - 1],
            y = data$y[2:nrow(data)],
            x = data$x[2:nrow(data)]
        )

        fit <- lm(
            y ~ y0 + x,
            data = lagged_data
        )

        # Create a model with the estimated parameters and create the output
        output <- list(
            "fit" = fit, 
            "model" = arx(
                parameters = as.numeric(fit$coefficients),
                sd = sd(fit$residuals)
            )
        )

        return(output)
    }
)