devtools::load_all()

# Create a function to perform a test of test-retest reliability
#
# @param sim Model to be used for the simulation
# @param est Model to be used for the estimation
# @param N Number of participants/parameter sets to be used and across which to 
# assess test-retest reliability
# @param T Number of time points per participant per bin
# @param bins Number of bins across which test-retest reliability can be assessed
# @param sd Standard deviation to be used for the simulation
# @param ... Additional arguments to be passed on to \code{simulate}
#
# @return ICC(A, 1) for the models specified, the data that came out of the 
# simulation, the simulating parameters for the people, and the estimated 
# parameters at each bin
test_retest <- function(sim, 
                        est, 
                        N, 
                        T, 
                        bins,
                        sd = 1,
                        ...) {

    # Simulate the parameters for each person
    params <- lapply(
        1:N, 
        function(i) {
            # Get the bounds of the parameters of the model and simulate a 
            # a random parameter set from a uniform distribution
            bounds <- paramrel::bounds(sim)
            return(runif(min = bounds[, 1], max = bounds[, 2]))
        }
    )
    params <- do.call("rbind", params)

    # Create a big data set containing the simulated data for all participants
    data <- lapply(
        1:N, 
        function(i) {
            # Define the model for this particular participant
            model_i <- sim(
                parameters = params[i, ],
                sd = sd
            )

            # Simulate data and return
            data <- paramrel::simulate(
                model_i, 
                N = bins * T, 
                ...
            )

            return(data)
        }
    )

    # For each data set, divide up the data into different bins and estimate the 
    # parameters of the estimation model per bin
    estimated <- lapply(
        data,
        function(x) {
            # Get the number of parameters of the estimated model and create 
            for(i in 1:bins) {

            }
        }
    )
}