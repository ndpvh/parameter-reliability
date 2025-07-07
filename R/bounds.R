#' Get the Bounds of the Model
#'
#' @param model Object of one of the different model classes (e.g., linear, 
#' quadratic,...)
#' 
#' @return Matrix where the left/right column specifies the lower/upper bounds 
#' of the parameters in the order specified in the description of the models
#' 
#' @examples 
#' 
#' @rdname bounds-method
#' 
#' @export
setGeneric(
    "bounds",
    function(model, ...) standardGeneric("bounds"),
    signature = "model"
)





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