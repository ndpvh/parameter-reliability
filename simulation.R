devtools::load_all()

################################################################################
# PRELIMINARIES

# Define the number of cores to use for the analysis and the number of simulations
# to run per condition
ncores <- 3
N <- 1000

# Define the number of different intercepts and slopes to generate for the 
# simulation models as well as their natural bounds. Also include a standard 
# deviation for the residuals of the simulation model and for the individual 
# differences on the parameters
n_int <- 5
n_slope <- 5

intercept <- c(-10, 10)
slope <- c(-5, 5)

residual_sd <- 0.5
parameter_sd <- 1

# Define the study parameters, being the number of participants, bins, and 
# outcomes per bin
n_bins <- 2:5
n_outcomes <- c(15, 25, 50, 100)
n_participants <- c(25, 100, 500)

# Define the models you want to test against each other. The comparisons will 
# always be made in two directions
models <- list(
    "part-1" = list(
        "linear" = paramrel::linear,
        "quadratic" = paramrel::quadratic
    ),
    "part-2" = list(
        "quadratic" = paramrel::quadratic,
        "cubic" = paramrel::cubic
    ),
    "part-3" = list(
        "main" = paramrel::main_effect,
        "interaction" = paramrel::interaction
    )
)





################################################################################
# CREATING CONDITIONS

# Create a data.frame containing all of the combinations of conditions, namely
# which simulation model, which estimation model, and the number of bins and 
# outcomes per bin
simest <- lapply(
    names(models),
    \(x) cbind(
        rep(x, each = 4),
        rep(names(models[[x]]), each = 2),
        rep(names(models[[x]]), times = 2)
    )
)
simest <- do.call("rbind", simest) |>
    as.data.frame() |>
    setNames(c("part", "sim_model", "est_model"))
simest$misspecified <- simest$sim_model != simest$est_model

x_specs <- data.frame(
    n_bins = rep(
        n_bins, 
        times = length(n_outcomes) * length(n_participants)
    ),
    n_outcomes = rep(
        rep(
            n_outcomes, 
            times = length(n_participants)
        ),
        each = length(n_bins)
    ),
    n_participants = rep(
        n_participants, 
        each = length(n_outcomes) * length(n_bins)
    )
)

conditions <- lapply(
    1:nrow(x_specs),
    function(i) {
        simest$n_bins <- x_specs$n_bins[i]
        simest$n_outcomes <- x_specs$n_outcomes[i]
        simest$n_participants <- x_specs$n_participants[i]

        return(simest)
    }
)
conditions <- do.call("rbind", conditions)





################################################################################
# SIMULATION STUDY

# With the different conditions specified, we can start looping over each 
# condition and doing our analysis
set.seed(9) # Deadbolts - Thrice
results <- lapply(
    1:nrow(grid), 
    function(i) {
        # Generate a parameter grid for the simulation model under investigation
        part <- conditions$part[i]
        sim <- conditions$sim_model[i]
        est <- conditions$est_model[i]

        grid <- paramrel::parameter_grid(
            models[[part]][[sim]](),
            n = N,
            intercept = intercept,
            slope = slope
        )

        # Define the two models of interest, one of which is endowed with 
        # the parameters in grid. These will serve as dummy's and keeping them here
        # increases the speed of the computation, as we're not increasingly creating
        # new classes of models but modifying existing ones
        sim_model <- models[[part]][[sim]](
                    grid[1, ],
                    sd = residual_sd
                )
        est_model <- models[[part]][[est]]()

        # Do the simulation and save the results in an intermediate results 
        # data.frame. This is done in parallel, but can also be achieved in sequence
        # by replacing mclapply by lapply instead
        intermediate <- parallel::mclapply(
            1:nrow(grid),
            function(j) {
                # Change the values of the simulating model parameters
                sim_model@parameters <- grid[j, ]

                # Execute the study for this set of parameters
                results <- paramrel::execute_study(
                    sim_model,
                    est_model,
                    n_participants = conditions$n_participants[i],
                    n_outcomes = conditions$n_outcomes[i],
                    n_bins = conditions$n_bins[i],
                    parameter_sd = parameter_sd,
                    ICC = 0.8,
                    R2 = 0.9,
                    save_results = FALSE,
                    path = file.path("results", part),
                    filename = paste0(sim, "-", est, "-", j)
                )

                # Bind all relevant together in a big data.frame
                results <- results[["descriptives"]] |>
                    dplyr::filter(bin == -1) |>
                    dplyr::full_join(
                        results[["icc"]],
                        by = "parameter"
                    ) |>
                    dplyr::full_join(
                        results[["signal"]], 
                        by = "parameter"
                    ) |>
                    dplyr::full_join(
                        results[["accuracy"]] |>
                            dplyr::select(
                                -true_value,
                                -estimated_value,
                                -standard_error
                            ),
                        by = "parameter"
                    ) |>
                    dplyr::select(-bin)

                return(results)
            },
            mc.cores = ncores
        )
        intermediate <- do.call("rbind", intermediate)

        # Save these intermediate results, as its a very comprehensive data.frame
        data.table::fwrite(
            intermediate,
            file.path(
                "results", 
                part, 
                paste0(
                    sim, 
                    "-", 
                    est, 
                    "_B", 
                    conditions$n_bins[i], 
                    "_O",
                    conditions$n_outcomes[i],
                    "_P",
                    conditions$n_participants[i],
                    ".csv"
                )
            )
        )

        # Summarize the results in the comprehensive data.frame and add them to the
        # general results data.frame
        results <- intermediate |>
            dplyr::group_by(parameter) |>
            dplyr::summarize(
                icc_mean = mean(icc),
                icc_sd = sd(icc),
                icc_ci_lower = quantile(icc, prob = 0.025),
                icc_ci_upper = quantile(icc, prob = 0.975),

                cv_mean = mean(coefficient_variation),
                cv_sd = sd(coefficient_variation),
                cv_ci_lower = quantile(coefficient_variation, prob = 0.025),
                cv_ci_upper = quantile(coefficient_variation, prob = 0.975),

                snr_mean = mean(signal_to_noise),
                snr_sd = sd(signal_to_noise),
                snr_ci_lower = quantile(signal_to_noise, prob = 0.025),
                snr_ci_upper = quantile(signal_to_noise, prob = 0.975),

                se_mean = mean(se),
                se_sd = sd(se),
                se_ci_lower = quantile(se, prob = 0.025),
                se_ci_upper = quantile(se, prob = 0.975),

                bias_mean = mean(bias),
                bias_sd = sd(bias),
                bias_ci_lower = quantile(bias, prob = 0.025),
                bias_ci_upper = quantile(bias, prob = 0.975),

                ubias_mean = mean(ubias),
                ubias_sd = sd(ubias),
                ubias_ci_lower = quantile(ubias, prob = 0.025),
                ubias_ci_upper = quantile(ubias, prob = 0.975),

                rbias_mean = mean(rbias),
                rbias_sd = sd(rbias),
                rbias_ci_lower = quantile(rbias, prob = 0.025),
                rbias_ci_upper = quantile(rbias, prob = 0.975),

                mse_mean = mean(mse),
                mse_sd = sd(mse),
                mse_ci_lower = quantile(mse, prob = 0.025),
                mse_ci_upper = quantile(mse, prob = 0.975),

                coverage_mean = mean(coverage),
                coverage_sd = sd(coverage),
                coverage_ci_lower = quantile(coverage, prob = 0.025),
                coverage_ci_upper = quantile(coverage, prob = 0.975)
            ) |>
            dplyr::ungroup() |>
            dplyr::mutate(
                part = part = conditions$part[i],
                sim_model = conditions$sim_model[i],
                est_model = conditions$est_model[i],
                misspecified = conditions$misspecified[i],
                n_bins = conditions$n_bins[i],
                n_outcomes = conditions$n_outcomes[i],
                n_participants = conditions$n_participants[i]
            ) |>
            dplyr::relocate(
                part:n_participants
            )

        return(results)
    }
)
results <- do.call("rbind", results)

# Save the full results of the test retest analysis
saveRDS(
    results,
    file.path("results", "results.csv")
)

