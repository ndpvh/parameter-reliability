devtools::load_all()

################################################################################
# PRELIMINARIES

# Define the number of cores to use for the analysis
ncores <- 4

# Define the number of different intercepts and slopes to generate for the 
# simulation models as well as their natural bounds. Also include a standard 
# deviation for the residuals of the simulation model and for the individual 
# differences on the parameters
n_int <- 4
n_slope <- 4

intercept <- c(-10, 10)
slope <- c(-5, 5)

residual_sd <- 0.5
parameter_sd <- 1

# Define the number of ICCs to compute for each set of parameters of each model
N <- 100

# Define the study parameters, being the number of participants, bins, and 
# outcomes per bin
n_bins <- 2:5
n_outcomes <- c(15, 25, 50, 100)
n_participants <- c(25, 100, 500)
# n_bins <- c(2, 5)
# n_outcomes <- c(15, 50, 100)
# n_participants <- c(25, 100)

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
set.seed(9) # Deadbolts - Thrice
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
    icc <- parallel::mclapply(
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
                icc = 0.8,
                R2 = 0.9,
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
        },
        mc.cores = ncores
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
            \(x) readRDS(
                file.path(
                    "results", 
                    parts[i], 
                    x
                )
            )
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
    icc <- lapply(
        1:nrow(combinations),
        function(j) {
            # Read in the ICC results for this combination of models
            icc <- readRDS(
                file.path(
                    "results",
                    parts[i], 
                    paste0(combinations[j, 1], "-", combinations[j, 2], "_icc.RDS")
                )
            )

            # Add model information to it
            icc$sim <- combinations[j, 1]
            icc$est <- combinations[j, 2]

            return(icc)
        }
    )
    icc <- do.call("rbind", icc)

    # Create two barplots: One for each estimation model
    models <- unique(icc$est) %>%
        sort()
    plt <- lapply(
        models,
        function(x) {
            # Select the relevant data
            data <- dplyr::filter(
                icc,
                est == x
            )

            # Make into plottable data
            data <- data %>%
                dplyr::group_by(parameter, sim) %>% 
                dplyr::summarize(
                    M = mean(icc),
                    Q025 = quantile(icc, prob = 0.025),
                    Q975 = quantile(icc, prob = 0.975)
                ) %>% 
                dplyr::ungroup()

            data$sim <- factor(
                data$sim, 
                levels = models
            )

            # Define colors etc
            colors <- c("cornflowerblue", "salmon")
            names(colors) <- models

            # Create a barplot from these data
            plt <- ggplot2::ggplot(
                data = data,
                ggplot2::aes(
                    x = factor(parameter),
                    y = M,
                    ymin = Q025,
                    ymax = Q975,
                    fill = factor(sim)
                )
            ) +
                ggplot2::geom_errorbar(
                    position = ggplot2::position_dodge(0.9),
                    width = 0.4
                ) +
                ggplot2::geom_bar(
                    stat = "identity",
                    position = ggplot2::position_dodge()
                ) + 
                # Labels, colors, and limits    
                ggplot2::scale_y_continuous(
                    limits = c(0, 1),
                    expand = c(0, 0)
                ) +
                ggplot2::scale_fill_manual(values = colors) +
                ggplot2::labs(
                    x = "Parameter",
                    y = "ICC",
                    fill = "Simulating model",
                    title = x
                ) +
                # Change theme elements
                ggplot2::theme_minimal() +
                ggplot2::theme(
                    panel.border = ggplot2::element_rect(
                        fill = NA, 
                        color = "black",
                        linewidth = 1
                    ),
                    axis.text = ggplot2::element_text(
                        size = 20
                    ),
                    axis.title = ggplot2::element_text(
                        size = 30,
                        face = "bold"
                    ),
                    plot.title = ggplot2::element_text(
                        size = 40,
                        hjust = 0.5,
                        face = "bold"
                    ),
                    legend.title = ggplot2::element_text(
                        size = 20,
                        face = "bold"
                    ),
                    legend.text = ggplot2::element_text(
                        size = 20
                    )
                )

            return(plt)
        }
    )

    # Bind them together
    plt <- ggpubr::ggarrange(
        plotlist = plt,
        nrow = 1,
        common.legend = TRUE,
        legend = "bottom"
    )

    # Save them in the correct folder under the correct name
    ggplot2::ggsave(
        file.path(
            "figures",
            paste0(
                "test-retest_", 
                parts[i], 
                ".png"
            )
        ),
        plt,
        width = 4000,
        height = 2400,
        unit = "px"
    )
}





# INTERACTION PLOTS: Taking into account the number of bins, outcomes, and 
# participants, as well as the specification of the models
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
    icc <- lapply(
        1:nrow(combinations),
        function(j) {
            # Read in the ICC results for this combination of models
            icc <- readRDS(
                file.path(
                    "results",
                    parts[i], 
                    paste0(combinations[j, 1], "-", combinations[j, 2], "_icc.RDS")
                )
            )

            # Add model information to it
            icc$sim <- combinations[j, 1]
            icc$est <- combinations[j, 2]

            return(icc)
        }
    )
    icc <- do.call("rbind", icc)

    # Specify the correct models
    models <- unique(icc$est) %>%
        sort()

    # Define the columns to look out for
    cols <- c("n_bins", "n_outcomes", "n_participants")
    col_titles <- c(
        "n_bins" = "Number of repetitions",
        "n_outcomes" = "Outcomes per repetition",
        "n_participants" = "Number of participants"
    )

    # Loop over these columns and make a plot showing their effect
    for(j in cols) {
        # We again make one barplot per estimated model
        plt <- lapply(
            models,
            function(x) {
                # Select the relevant data
                data <- dplyr::filter(
                    icc,
                    est == x
                )
                data <- data[, c("icc", "sim", j)] %>%
                    setNames(c("icc", "sim", "condition"))

                # Make into plottable data
                data <- data %>%
                    dplyr::group_by(condition, sim) %>% 
                    dplyr::summarize(
                        M = mean(icc),
                        Q025 = quantile(icc, prob = 0.025),
                        Q975 = quantile(icc, prob = 0.975)
                    ) %>% 
                    dplyr::ungroup()

                data$sim <- factor(
                    data$sim, 
                    levels = models
                )
                data$condition <- factor(
                    data$condition,
                    levels = sort(unique(data$condition))
                )

                # Define colors etc
                colors <- c("cornflowerblue", "salmon")
                names(colors) <- models

                # Create a barplot from these data
                plt <- ggplot2::ggplot(
                    data = data,
                    ggplot2::aes(
                        x = condition,
                        y = M,
                        ymin = Q025,
                        ymax = Q975,
                        fill = factor(sim)
                    )
                ) +
                    ggplot2::geom_errorbar(
                        position = ggplot2::position_dodge(0.9),
                        width = 0.4
                    ) +
                    ggplot2::geom_bar(
                        stat = "identity",
                        position = ggplot2::position_dodge()
                    ) + 
                    # Labels, colors, and limits    
                    ggplot2::scale_y_continuous(
                        limits = c(0, 1),
                        expand = c(0, 0)
                    ) +
                    ggplot2::scale_fill_manual(values = colors) +
                    ggplot2::labs(
                        x = col_titles[j],
                        y = "ICC",
                        fill = "Simulating model",
                        title = x
                    ) +
                    # Change theme elements
                    ggplot2::theme_minimal() +
                    ggplot2::theme(
                        panel.border = ggplot2::element_rect(
                            fill = NA, 
                            color = "black",
                            linewidth = 1
                        ),
                        axis.text = ggplot2::element_text(
                            size = 20
                        ),
                        axis.title = ggplot2::element_text(
                            size = 30,
                            face = "bold"
                        ),
                        plot.title = ggplot2::element_text(
                            size = 40,
                            hjust = 0.5,
                            face = "bold"
                        ),
                        legend.title = ggplot2::element_text(
                            size = 20,
                            face = "bold"
                        ),
                        legend.text = ggplot2::element_text(
                            size = 20
                        )
                    )

                return(plt)
            }
        )

        # Bind them together
        plt <- ggpubr::ggarrange(
            plotlist = plt,
            nrow = 1,
            common.legend = TRUE,
            legend = "bottom"
        )

        # Save them in the correct folder under the correct name
        ggplot2::ggsave(
            file.path(
                "figures",
                paste0(
                    "test-retest_", 
                    j, 
                    "_",
                    parts[i], 
                    ".png"
                )
            ),
            plt,
            width = 4000,
            height = 2400,
            unit = "px"
        )
    }
}
