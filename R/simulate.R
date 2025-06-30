#' Simulate Data from a Model
#' 
#' Method for the different models that allows the simulation of data for these
#' models.
#'
#' @param object Object of the different model classes (e.g., linear, quadratic,
#' ...)
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
    function(object, ...) standardGeneric("simulate"),
    signature = "model"
)