#' Get the Bounds of the Model
#'
#' @param model Object of one of the different model classes (e.g., linear, 
#' quadratic,...)
#' @param ... Additional arguments provided to the model of choice.
#' 
#' @return Matrix where the left/right column specifies the lower/upper bounds 
#' of the parameters in the order specified in the description of the models
#' 
#' @examples 
#' # Example given for the linear model, but also possible for the other models
#' bounds(linear())
#' 
#' bounds(
#'   linear(),
#'   intercept = c(-10, 10),
#'   slope = c(0, 10)
#' )
#' 
#' @rdname bounds-method
#' 
#' @export
setGeneric(
    "bounds",
    function(model, ...) standardGeneric("bounds"),
    signature = "model"
)





#' @param intercept Numeric vector with two values denoting the lower and upper
#' bound of the intercept parameter. Defaults to \code{c(-10, 10)}.
#' @param slope Numeric vector with two values denoting the lower and upper 
#' bound of all slope parameters. Defaults to \code{c(-5, 5)}.
#' 
#' @rdname bounds-method
setMethod(
    "bounds",
    "linear",
    function(model, 
             intercept = c(-10, 10),
             slope = c(-5, 5)) {
        return(
            cbind(
                c(intercept[1], slope[1]),
                c(intercept[2], slope[2])
            )
        )
    }
)





#' @param intercept Numeric vector with two values denoting the lower and upper
#' bound of the intercept parameter. Defaults to \code{c(-10, 10)}.
#' @param slope Numeric vector with two values denoting the lower and upper 
#' bound of all slope parameters. Defaults to \code{c(-5, 5)}.
#' 
#' @rdname bounds-method
setMethod(
    "bounds",
    "quadratic",
    function(model, 
             intercept = c(-10, 10),
             slope = c(-5, 5)) {
        return(
            cbind(
                c(intercept[1], rep(slope[1], 2)),
                c(intercept[2], rep(slope[2], 2))
            )
        )
    }
)





#' @param intercept Numeric vector with two values denoting the lower and upper
#' bound of the intercept parameter. Defaults to \code{c(-10, 10)}.
#' @param slope Numeric vector with two values denoting the lower and upper 
#' bound of all slope parameters. Defaults to \code{c(-5, 5)}.
#' 
#' @rdname bounds-method
setMethod(
    "bounds",
    "cubic",
    function(model, 
             intercept = c(-10, 10),
             slope = c(-5, 5)) {
        return(
            cbind(
                c(intercept[1], rep(slope[1], 3)),
                c(intercept[2], rep(slope[2], 3))
            )
        )
    }
)





#' @param intercept Numeric vector with two values denoting the lower and upper
#' bound of the intercept parameter. Defaults to \code{c(-10, 10)}.
#' @param slope Numeric vector with two values denoting the lower and upper 
#' bound of all slope parameters. Defaults to \code{c(-5, 5)}.
#' 
#' @rdname bounds-method
setMethod(
    "bounds",
    "main_effect",
    function(model, 
             intercept = c(-10, 10),
             slope = c(-5, 5)) {
        return(
            cbind(
                c(intercept[1], rep(slope[1], 2)),
                c(intercept[2], rep(slope[2], 2))
            )
        )
    }
)





#' @param intercept Numeric vector with two values denoting the lower and upper
#' bound of the intercept parameter. Defaults to \code{c(-10, 10)}.
#' @param slope Numeric vector with two values denoting the lower and upper 
#' bound of all slope parameters. Defaults to \code{c(-5, 5)}.
#' 
#' @rdname bounds-method
setMethod(
    "bounds",
    "interaction",
    function(model, 
             intercept = c(-10, 10),
             slope = c(-5, 5)) {
        return(
            cbind(
                c(intercept[1], rep(slope[1], 3)),
                c(intercept[2], rep(slope[2], 3))
            )
        )
    }
)





#' @param intercept Numeric vector with two values denoting the lower and upper
#' bound of the intercept parameter. Defaults to \code{c(-10, 10)}.
#' @param slope Numeric vector with two values denoting the lower and upper 
#' bound of all slope parameters. Ignored for this model.
#' 
#' @rdname bounds-method
setMethod(
    "bounds",
    "ar1",
    function(model, 
             intercept = c(-10, 10),
             slope = c(-5, 5)) {
        return(
            cbind(
                c(intercept[1], -0.99),
                c(intercept[2], 0.99)
            )
        )
    }
)





#' @param intercept Numeric vector with two values denoting the lower and upper
#' bound of the intercept parameter. Defaults to \code{c(-10, 10)}.
#' @param slope Numeric vector with two values denoting the lower and upper 
#' bound of all slope parameters for the covariates. Defaults to \code{c(-5, 5)}.
#' 
#' @rdname bounds-method
setMethod(
    "bounds",
    "arx",
    function(model, 
             intercept = c(-10, 10),
             slope = c(-5, 5)) {
        return(
            cbind(
                c(intercept[1], -0.99, slope[1]),
                c(intercept[2], 0.99, slope[2])
            )
        )
    }
)