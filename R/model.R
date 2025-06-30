# PURPOSE: Define the different model classes that are used in our principle 
#          analyses. These are the linear, quadratic, main, interaction, 
#          AR(1), and ARX(1) models





#' An S4 Class to Represent all Models.
#' 
#' @details
#' Defines the \code{model} class, to which all other models belong. This class 
#' is solely defined by its parameters and the error around it.
#'
#' @slot parameters Numeric vector containing the values of the parameters of 
#' the model. 
#' 
#' @rdname model-class
#'
#' @export
model <- setClass(
    "model",
    slots = list(
        parameters = "numeric"
    )
)





#' An S4 Class to Represent the Linear Model.
#' 
#' @details
#' Defines the \code{linear} class, which states that for a given input \eqn{x}, 
#' the relationship between \eqn{x} and the dependent variable \eqn{y} is linear:
#' 
#' \deqn{y = a + bx}
#' 
#' This class is solely defined by its parameters \eqn{a} and \eqn{b}, which 
#' should be provided through the construction of this class.
#'
#' @slot parameters Numeric vector containing the values of the parameters of 
#' the model, namely \eqn{a} and \eqn{b} in this order. If left unspecified, 
#' the model will default to \code{c(0, 0)}. 
#' 
#' @rdname linear-class
#'
#' @export
linear <- setClass(
    "linear",
    slots = list(
        parameters = "numeric"
    ),
    contains = "model"
)

#' Constructor for the \code{\link[paramrel]{linear-class}}
#' 
#' @param parameters Numeric vector containing the values of the parameters of 
#' the model, namely \eqn{a} and \eqn{b} in this order. If left unspecified, 
#' the parameters will default to \code{c(0, 0)}. 
#' 
#' @export
setMethod(
    "initialize",
    "linear",
    function(.Object,
             parameters = c(0, 0)) {

        # Check if there are too few parameters. If so, we throw an error
        if(length(parameters) < 2) {
            stop("Too few parameters provided for the linear class. Cannot proceed.")
        }

        # Check if there are too many parameters. If so, we throw a warning
        # and only select the first few values
        if(length(parameters) > 2) {
            warning("Too many parameters provided for the linear class. Selecting the first 2.")
            parameters <- parameters[1:2]
        }
        
        # Assigning the parameters
        .Object@parameters <- parameters 
        return(.Object)
    }
)





#' An S4 Class to Represent the Quadratic Model.
#' 
#' @details
#' Defines the \code{quadratic} class, which states that for a given input \eqn{x}, 
#' the relationship between \eqn{x} and the dependent variable \eqn{y} is 
#' quadratic:
#' 
#' \deqn{y = a + bx + cx^2}
#' 
#' This class is solely defined by its parameters \eqn{a}, \eqn{b}, and \eqn{c},
#' which should be provided through the construction of this class.
#'
#' @slot parameters Numeric vector containing the values of the parameters of 
#' the model, namely \eqn{a}, \eqn{b}, and \eqn{c} in this order. If left 
#' unspecified, the model will default to \code{c(0, 0, 0)}. 
#' 
#' @rdname quadratic-class
#'
#' @export
quadratic <- setClass(
    "quadratic",
    slots = list(
        parameters = "numeric"
    ),
    contains = "model"
)

#' Constructor for the \code{\link[paramrel]{quadratic-class}}
#' 
#' @param parameters Numeric vector containing the values of the parameters of 
#' the model, namely \eqn{a}, \eqn{b}, and \eqn{c} in this order. If left 
#' unspecified, the parameters will default to \code{c(0, 0, 0)}.  
#' 
#' @export
setMethod(
    "initialize",
    "quadratic",
    function(.Object,
             parameters = c(0, 0, 0)) {
        
        # Check if there are too few parameters. If so, we throw an error
        if(length(parameters) < 3) {
            stop("Too few parameters provided for the quadratic class. Cannot proceed.")
        }

        # Check if there are too many parameters. If so, we throw a warning
        # and only select the first few values
        if(length(parameters) > 3) {
            warning("Too many parameters provided for the quadratic class. Selecting the first 3.")
            parameters <- parameters[1:3]
        }
        
        # Assigning the parameters
        .Object@parameters <- parameters 
        return(.Object)
    }
)





#' An S4 Class to Represent the Main Effect Model.
#' 
#' @details
#' Defines the \code{main} class, which states that for two given input
#' variables \eqn{x} and \eqn{z}, the relationship between these variables and 
#' the dependent variable \eqn{y} is one with only a main effect:
#' 
#' \deqn{y = a + bx + cz}
#' 
#' This class is solely defined by its parameters \eqn{a}, \eqn{b}, and \eqn{c}, 
#' which should all be provided through the construction of this class.
#'
#' @slot parameters Numeric vector containing the values of the parameters of 
#' the model, namely \eqn{a}, \eqn{b}, and \eqn{c} in this order. If left 
#' unspecified, the model will default to \code{c(0, 0, 0)}. 
#' 
#' @rdname main-class
#'
#' @export
main <- setClass(
    "main",
    slots = list(
        parameters = "numeric"
    ),
    contains = "model"
)

#' Constructor for the \code{\link[paramrel]{main-class}}
#' 
#' @param parameters Numeric vector containing the values of the parameters of 
#' the model, namely \eqn{a}, \eqn{b}, and \eqn{c} in this order. If left 
#' unspecified, the model will default to \code{c(0, 0, 0)}. 
#' 
#' @export
setMethod(
    "initialize",
    "main",
    function(.Object,
             parameters = c(0, 0, 0)) {
        
        # Check if there are too few parameters. If so, we throw an error
        if(length(parameters) < 3) {
            stop("Too few parameters provided for the main class. Cannot proceed.")
        }

        # Check if there are too many parameters. If so, we throw a warning
        # and only select the first few values
        if(length(parameters) > 3) {
            warning("Too many parameters provided for the main class. Selecting the first 3.")
            parameters <- parameters[1:3]
        }
        
        # Assigning the parameters
        .Object@parameters <- parameters 
        return(.Object)
    }
)





#' An S4 Class to Represent the Interaction Model.
#' 
#' @details
#' Defines the \code{interaction} class, which states that for two given input
#' variables \eqn{x} and \eqn{z}, the relationship between these variables and 
#' the dependent variable \eqn{y} is one with a main and interaction effect:
#' 
#' \deqn{y = a + bx + cz + dxz}
#' 
#' This class is solely defined by its parameters \eqn{a}, \eqn{b}, \eqn{c},
#' and \eqn{d}, which should all be provided through the construction of this 
#' class.
#'
#' @slot parameters Numeric vector containing the values of the parameters of 
#' the model, namely \eqn{a}, \eqn{b}, \eqn{c}, and \eqn{d} in this order. If left 
#' unspecified, the model will default to \code{c(0, 0, 0, 0)}. 
#' 
#' @rdname interaction-class
#'
#' @export
interaction <- setClass(
    "interaction",
    slots = list(
        parameters = "numeric"
    ),
    contains = "model"
)

#' Constructor for the \code{\link[paramrel]{interaction-class}}
#' 
#' @param parameters Numeric vector containing the values of the parameters of 
#' the model, namely \eqn{a}, \eqn{b}, \eqn{c}, and \eqn{d} in this order. If left 
#' unspecified, the model will default to \code{c(0, 0, 0, 0)}.
#' 
#' @export
setMethod(
    "initialize",
    "interaction",
    function(.Object,
             parameters = c(0, 0, 0, 0)) {
        
        # Check if there are too few parameters. If so, we throw an error
        if(length(parameters) < 4) {
            stop("Too few parameters provided for the interaction class. Cannot proceed.")
        }

        # Check if there are too many parameters. If so, we throw a warning
        # and only select the first few values
        if(length(parameters) > 4) {
            warning("Too many parameters provided for the interaction class. Selecting the first 4.")
            parameters <- parameters[1:4]
        }
        
        # Assigning the parameters
        .Object@parameters <- parameters 
        return(.Object)
    }
)





#' An S4 Class to Represent the Autoregressive Model.
#' 
#' @details
#' Defines the \code{autoregressive} class, which states that for a given dependent
#' variable \eqn{y}, the previous values of this variable in part determine 
#' current values of the variable:
#' 
#' \deqn{y_{t} = a + by_{t - 1}}
#' 
#' This class is solely defined by its parameters \eqn{a} and \eqn{b}, which 
#' should all be provided through the construction of this class.
#'
#' @slot parameters Numeric vector containing the values of the parameters of 
#' the model, namely \eqn{a} and \eqn{b} in this order. If left unspecified, the 
#' model will default to \code{c(0, 0)}. 
#' 
#' @rdname autoregressive-class
#'
#' @export
autoregressive <- setClass(
    "autoregressive",
    slots = list(
        parameters = "numeric"
    ),
    contains = "model"
)

#' Constructor for the \code{\link[paramrel]{autoregressive-class}}
#' 
#' @param parameters Numeric vector containing the values of the parameters of 
#' the model, namely \eqn{a} and \eqn{b} in this order. If left unspecified, the 
#' model will default to \code{c(0, 0)}.
#' 
#' @export
setMethod(
    "initialize",
    "autoregressive",
    function(.Object,
             parameters = c(0, 0)) {
        
        # Check if there are too few parameters. If so, we throw an error
        if(length(parameters) < 2) {
            stop("Too few parameters provided for the autoregressive class. Cannot proceed.")
        }

        # Check if there are too many parameters. If so, we throw a warning
        # and only select the first few values
        if(length(parameters) > 2) {
            warning("Too many parameters provided for the autoregressive class. Selecting the first 2.")
            parameters <- parameters[1:2]
        }
        
        # Assigning the parameters
        .Object@parameters <- parameters 
        return(.Object)
    }
)





#' An S4 Class to Represent the ARX(1) Model.
#' 
#' @details
#' Defines the \code{arx} class, which states that for a given dependent
#' variable \eqn{y}, the previous values of this variable in part determine 
#' current values of the variable:
#' 
#' \deqn{y_{t} = a + by_{t - 1} + cx_{t}}
#' 
#' This class is solely defined by its parameters \eqn{a}, \eqn{b} and \eqn{c}, 
#' which should all be provided through the construction of this class.
#'
#' @slot parameters Numeric vector containing the values of the parameters of 
#' the model, namely \eqn{a}, \eqn{b}, and \eqn{c} in this order. If left 
#' unspecified, the model will default to \code{c(0, 0, 0)}.
#' 
#' @rdname arx-class
#'
#' @export
arx <- setClass(
    "arx",
    slots = list(
        parameters = "numeric"
    ),
    contains = "model"
)

#' Constructor for the \code{\link[paramrel]{arx-class}}
#' 
#' @param parameters Numeric vector containing the values of the parameters of 
#' the model, namely \eqn{a}, \eqn{b}, and \eqn{c} in this order. If left 
#' unspecified, the model will default to \code{c(0, 0, 0)}.
#' 
#' @export
setMethod(
    "initialize",
    "arx",
    function(.Object,
             parameters = c(0, 0, 0)) {
        
        # Check if there are too few parameters. If so, we throw an error
        if(length(parameters) < 3) {
            stop("Too few parameters provided for the arx class. Cannot proceed.")
        }

        # Check if there are too many parameters. If so, we throw a warning
        # and only select the first few values
        if(length(parameters) > 3) {
            warning("Too many parameters provided for the arx class. Selecting the first 3.")
            parameters <- parameters[1:3]
        }
        
        # Assigning the parameters
        .Object@parameters <- parameters 
        return(.Object)
    }
)