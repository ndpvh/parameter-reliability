# Purpose: Model class definitions for parameter reliability study
# Authors: Kenny Yu & Niels Vanhasbroeck
# Date: July 2025

# Base Model Class
model <- setClass(
    "model",
    slots = list(
        parameters = "numeric",
        sd = "numeric"
    )
)

# Base Model Constructor
setMethod(
    "initialize",
    "model",
    function(.Object, parameters = numeric(0), sd = 1) {
        if(length(sd) > 1) {
            sd <- sd[1]
        }
        .Object@parameters <- parameters
        .Object@sd <- sd 
        return(.Object)
    }
)

# Linear Model Class: y = a + bx + e
linear <- setClass("linear", contains = "model")

setMethod(
    "initialize",
    "linear",
    function(.Object, parameters = c(0, 0), sd = 1) {
        if(length(parameters) < 2) {
            stop("Linear model requires 2 parameters: intercept, slope")
        }
        if(length(parameters) > 2) {
            parameters <- parameters[1:2]
        }
        .Object <- callNextMethod(.Object, parameters = parameters, sd = sd)
        return(.Object)
    }
)

# Quadratic Model Class: y = a + bx + cx² + e
quadratic <- setClass("quadratic", contains = "model")

setMethod(
    "initialize",
    "quadratic",
    function(.Object, parameters = c(0, 0, 0), sd = 1) {
        if(length(parameters) < 3) {
            stop("Quadratic model requires 3 parameters: intercept, linear_slope, quadratic_slope")
        }
        if(length(parameters) > 3) {
            parameters <- parameters[1:3]
        }
        .Object <- callNextMethod(.Object, parameters = parameters, sd = sd)
        return(.Object)
    }
)

# Cubic Model Class: y = a + bx + cx² + dx³ + e
cubic <- setClass("cubic", contains = "model")

setMethod(
    "initialize",
    "cubic",
    function(.Object, parameters = c(0, 0, 0, 0), sd = 1) {
        if(length(parameters) < 4) {
            stop("Cubic model requires 4 parameters: intercept, linear_slope, quadratic_slope, cubic_slope")
        }
        if(length(parameters) > 4) {
            parameters <- parameters[1:4]
        }
        .Object <- callNextMethod(.Object, parameters = parameters, sd = sd)
        return(.Object)
    }
)

# Interaction Model Class: y = a + bx + cz + dxz + e
interaction <- setClass("interaction", contains = "model")

setMethod(
    "initialize",
    "interaction",
    function(.Object, parameters = c(0, 0, 0, 0), sd = 1) {
        if(length(parameters) < 4) {
            stop("Interaction model requires 4 parameters: intercept, x_slope, z_slope, interaction_slope")
        }
        if(length(parameters) > 4) {
            parameters <- parameters[1:4]
        }
        .Object <- callNextMethod(.Object, parameters = parameters, sd = sd)
        return(.Object)
    }
)

# AR(1) Model Class: y_t = a + bx_t + cy_{t-1} + e_t
ar1 <- setClass("ar1", contains = "model")

setMethod(
    "initialize",
    "ar1",
    function(.Object, parameters = c(0, 0, 0), sd = 1) {
        if(length(parameters) < 3) {
            stop("AR(1) model requires 3 parameters: intercept, x_slope, ar_coefficient")
        }
        if(length(parameters) > 3) {
            parameters <- parameters[1:3]
        }
        if(abs(parameters[3]) >= 0.8) {
            stop("AR coefficient must be in (-0.8, 0.8) for stationarity and numerical stability")
        }
        .Object <- callNextMethod(.Object, parameters = parameters, sd = sd)
        return(.Object)
    }
)