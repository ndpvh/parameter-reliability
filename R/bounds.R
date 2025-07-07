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
    function(...) {
        return(
            cbind(
                c(-10, -5),
                c(10, 5)
            )
        )
    }
)





#' @rdname bounds-method
setMethod(
    "bounds",
    "quadratic",
    function(...) {
        return(
            cbind(
                c(-10, -5, -5),
                c(10, 5, 5)
            )
        )
    }
)





#' @rdname bounds-method
setMethod(
    "bounds",
    "cubic",
    function(...) {
        return(
            cbind(
                c(-10, -5, -5, -5),
                c(10, 5, 5, 5)
            )
        )
    }
)





#' @rdname bounds-method
setMethod(
    "bounds",
    "main",
    function(...) {
        return(
            cbind(
                c(-10, -5, -5),
                c(10, 5, 5)
            )
        )
    }
)





#' @rdname bounds-method
setMethod(
    "bounds",
    "interaction",
    function(...) {
        return(
            cbind(
                c(-10, -5, -5, -5),
                c(10, 5, 5, 5)
            )
        )
    }
)





#' @rdname bounds-method
setMethod(
    "bounds",
    "ar1",
    function(...) {
        return(
            cbind(
                c(-10, -0.99),
                c(10, 0.99)
            )
        )
    }
)





#' @rdname bounds-method
setMethod(
    "bounds",
    "arx",
    function(...) {
        return(
            cbind(
                c(-10, -0.99, -5),
                c(10, 0.99, 5)
            )
        )
    }
)