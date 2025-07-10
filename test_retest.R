devtools::load_all()

################################################################################
# PRELIMINARIES

# Define the number of different intercepts and slopes to generate for the 
# simulation models as well as their natural bounds. Also include a standard 
# deviation for the residuals of the simulation model and for the individual 
# differences on the parameters
n_int <- 3
n_slope <- 3

intercept <- c(-10, 10)
slope <- c(-5, 5)

residual_sd <- 0.5
parameter_sd <- 1

# Define the number of ICCs to compute for each set of parameters of each model
N <- 10

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
# ANALYSIS

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

# Add some generic consistency measures at the end of this data.frame. This will
# allow us to quickly look at the results without all detailed information that 
# is saved elsewhere
conditions$icc_mean <- 0
conditions$icc_median <- 0
conditions$icc_sd <- 0
conditions$icc_025 <- 0
conditions$icc_975 <- 0
conditions$icc_min <- 0
conditions$icc_max <- 0

# With the different conditions specified, we can start looping over each 
# condition and doing our analysis
for(i in 1:nrow(conditions)) {
    # Generate a parameter grid for the simulation model under investigation
    part <- conditions$part[i]
    sim <- conditions$sim_model[i]
    est <- conditions$est_model[i]

    grid <- paramrel::parameter_grid(
        models[[part]][[sim]](),
        n_int = n_int,
        n_slope = n_slope,
        intercept = intercept,
        slope = slope
    )

    # Loop across all parameters to perform the actual test-retest analysis
    icc <- lapply(
        1:nrow(grid),
        function(j) {
            # Define the two models of interest
            sim_model <- models[[part]][[sim]](
                grid[j, ],
                sd = residual_sd
            )
            est_model <- models[[part]][[est]]()

            # Compute the icc
            icc <- paramrel::test_retest(
                sim_model,
                est_model,
                n_participants = conditions$n_participants[i],
                n_outcomes = conditions$n_outcomes[i],
                n_bins = conditions$n_bins[i],
                parameter_sd = parameter_sd,
                save_results = FALSE,
                path = file.path("results", part),
                filename = paste0(sim, "-", est, "-", j)
            )

            # Convert to a data.frame and return
            icc <- do.call("rbind", icc) |>
                as.data.frame()
            icc$iteration <- j
            icc$parameter <- rownames(icc)

            icc$icc <- as.numeric(icc$icc)
            icc$systematic <- as.numeric(icc$icc)
            icc$residual <- as.numeric(icc$residual)

            rownames(icc) <- NULL

            return(icc)
        }
    )

    # Bind into one big data.frame, add information on the conditions, and save 
    # the result to somewhere
    icc <- do.call("rbind", icc)
    icc$n_bins <- conditions$n_bins[i]
    icc$n_outcomes <- conditions$n_outcomes[i]
    icc$n_participants <- conditions$n_participants[i]

    saveRDS(
        icc,
        file.path(
            "results", 
            part, 
            paste0(
                sim, 
                "-", 
                est, 
                "_icc_B", 
                conditions$n_bins[i], 
                "_O",
                conditions$n_outcomes[i],
                "_P",
                conditions$n_participants[i],
                ".RDS"
            )
        )
    )

    # Add the summary statistics to the big data.frame
    conditions$icc_mean[i] <- mean(icc$icc)
    conditions$icc_median[i] <- median(icc$icc)
    conditions$icc_sd[i] <- sd(icc$icc)
    conditions$icc_025[i] <- quantile(icc$icc, prob = 0.025)
    conditions$icc_975[i] <- quantile(icc$icc, prob = 0.975)
    conditions$icc_min[i] <- min(icc$icc)
    conditions$icc_max[i] <- max(icc$icc)
}

# Save the full results of the test retest analysis
saveRDS(
    conditions,
    file.path("results", "test_retest.RDS")
)

# Make a combined data.frame that contains all of the information needed for each 
# separate model
parts <- unique(conditions$part)
for(i in seq_along(parts)) {
    # Define the models that belong to this part
    selection <- conditions[conditions$part == parts[i], ]
    models <- unique(selection$sim_model)

    # Make the unique combinations again
    combinations <- cbind(
        rep(models, each = length(models)),
        rep(models, times = length(models))
    )

    # Loop over these combinations and load in all data for each
    for(j in 1:nrow(combinations)) {
        # Find all files in the required directory with the required naming 
        # format
        files <- list.files(file.path("results", parts[i]))
        files <- stringr::str_subset(
            files,
            paste0(combinations[j, 1], "-", combinations[j, 2], "_icc_")
        )

        # Bind all these files together
        data <- lapply(
            files,
            \(x) readRDS(x)
        )
        data <- do.call("rbind", data)

        # Save the dataset in the designated folder
        saveRDS(
            data,
            file.path(
                "results",
                parts[i],
                paste0(combinations[j, 1], "-", combinations[j, 2], "_icc.RDS")
            )
        )
    }
}





################################################################################
# VISUALIZATION

# GENERAL PLOTS: Aggregating over the number of bins, outcomes, and participants
parts <- unique(conditions$part)
for(i in seq_along(parts)) {
    # Define the models that belong to this part
    selection <- conditions[conditions$part == parts[i], ]
    models <- unique(selection$sim_model)

    # Make the unique combinations again
    combinations <- cbind(
        rep(models, each = length(models)),
        rep(models, times = length(models))
    )

    # Loop over these combinations and make a plot for each type
    for(j in 1:nrow(combinations)) {
        # Read in the ICC results for this combination of models
        icc <- readRDS(
            file.path(
                "results",
                parts[i], 
                paste0(combinations[j, 1], "-", combinations[j, 2], "_icc.RDS")
            )
        )

        browser()
    }
}
