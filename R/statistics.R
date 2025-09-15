#' Compute the \eqn{ICC(A, 1)} for Participant and Bin.
#'
#' @description
#' In our study, we define the \eqn{ICC(A, 1)} as:
#' 
#' \deqn{ICC(A, 1) = \frac{\sigma_\text{participants}}^2}{\sigma_\text{total}^2}
#' 
#' where \eqn{\sigma^2} denotes the variance. This \eqn{ICC} puts the systematic 
#' variance -- that is, the interindividual differences in the values of the 
#' parameters -- against the total observed variance in the parameters, checking 
#' whether individual differences can be reliably picked up on.
#' 
#' @param estimated Dataframe containing estimated parameters per participant and 
#' bin, allowing for a decomposition analysis. 
#' @param ... Obsolete. Ensures compatibility with 
#' \code{\link[paramrel]{execute_study}}
#' 
#' @return Named list containing numerics between \eqn{0} and \eqn{1} denoting 
#' the \eqn{ICC(A, 1)} for each parameter in \code{data}
#' 
#' @export
icc <- function(estimated, ...) {
    # Filter out those occasions where bins are not of import
    estimated <- estimated[estimated$bin != -1, ]

    # Get the column names of the data and identify all parameter names
    cols <- colnames(estimated)
    cols <- cols[-which(cols %in% c("participant", "bin"))]

    # Loop over all columns and compute the ICC per parameter
    icc <- lapply(
        cols,
        function(x) {
            # Select the correct data and change the names as appropriate
            data <- estimated[, c(x, "participant", "bin")] |>
                setNames(c("param", "participant", "bin"))

            # Do the variance decomposition through lmer
            result <- lme4::lmer(
                data = data, 
                param ~ 1 + (1 | participant)
            )

            # Get a summary of the estimation from lmer and extract the 
            # variance components of interest from the model, it being the 
            # systematic variance and the total variance
            fit <- summary(result)
    
            systematic <- as.numeric(fit$varcor[1])
            residual <- as.numeric(fit$sigma^2)

            # Compute the ICC(A, 1) for this parameter and return as the result
            # of the procedure
            return(
                data.frame(
                    "parameter" = x,
                    "icc" = systematic / (systematic + residual),
                    "systematic" = systematic, 
                    "residual" = residual
                )
            )
        }
    )
    icc <- do.call("rbind", icc)

    return(icc)
}