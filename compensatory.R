################################################################################
# VISUALIZATION

# Read in the data for which we will make the plots
data <- data.table::fread(
    file.path("results", "compensatory_results_summary.csv"),
    data.table = FALSE
)

# Create combinations of models, starting with the simulating model and going to 
# the estimating model
combinations <- data %>%
    # Get only the unique scenario's
    dplyr::group_by(scenario) %>% 
    tidyr::nest() %>% 
    dplyr::select(-data) %>% 

    # Separate them into two different columns
    dplyr::rowwise() %>% 
    dplyr::mutate(
        sim = stringr::str_split(
            scenario,
            " → "
        )[[1]][1], 
        est = stringr::str_split(
            scenario,
            " → "
        )[[1]][2]
    ) %>% 
    dplyr::ungroup() %>%
    
    # Define which scenarios are interesting and to which "part" they belong
    dplyr::rowwise() %>% 
    dplyr::mutate(
        part = ifelse(
            (sim %in% c("linear", "quadratic")) & (est %in% c("linear", "quadratic")),
            "part-1",
            ifelse(
                (sim %in% c("cubic", "quadratic")) & (est %in% c("cubic", "quadratic")), 
                "part-2", 
                ifelse( 
                    (sim %in% c("main_effects", "interaction")) & (est %in% c("main_effects", "interaction")), 
                    "part-3", 
                    NA
                )
            )
        )
    ) %>% 
    dplyr::filter(!is.na(part)) %>% 
    dplyr::select(-scenario) %>% 
    rbind(c("quadratic", "quadratic", "part-2")) 

# GENERAL PLOTS: Aggregating over the number of bins, outcomes, and participants
parts <- unique(combinations$part)
cols <- c("variance", "se_ratio", "reliability_coef", "bias", "mse")
col_titles <- c(
    "variance" = "Variance",
    "se_ratio" = "SE ratio",
    "reliability_coef" = "Reliability coefficient",
    "bias" = "Bias",
    "mse" = "SE"
)

for(i in seq_along(parts)) {
    # Define the models that belong to this part and select the data that belong 
    # here
    models <- combinations[combinations$part == parts[i], ]
    selection <- data %>% 
        dplyr::filter(
            true_model_type %in% unique(models$sim), 
            fit_type %in% unique(models$est)
        ) %>% 
        dplyr::rename(
            sim = true_model_type, 
            est = fit_type
        ) %>% 
        dplyr::select(
            -condition_id, 
            -misspec_type, 
            -x_distribution, 
            -scenario, 
            -false_confidence
        ) %>% 
        dplyr::mutate(
            parameter = ifelse(
                parameter == "(Intercept)",
                "intercept", 
                ifelse(
                    parameter %in% c("x", "x1"), 
                    "slope_1", 
                    ifelse( 
                        parameter %in% c("I(x^2)", "x2"), 
                        "slope_2", 
                        ifelse( 
                            parameter %in% c("I(x^3)", "x1:x2"), 
                            "slope_3", 
                            NA
                        )
                    )
                )
            )
        )

    # Loop over all summary statistics that Kenny used
    for(j in cols) {
        # Create two boxplots: One for each estimation model
        models <- unique(selection$est) %>%
            sort()
        plt <- lapply(
            models,
            function(x) {
                # Define the limits for this column for these models
                ylims <- range(selection[, j])

                # Select the relevant data
                tmp <- dplyr::filter(
                    selection,
                    est == x
                )
                tmp <- tmp[, c("sim", "parameter", j)] %>% 
                    setNames(c("sim", "parameter", "stat"))

                # Make into plottable data
                tmp$sim <- factor(
                    tmp$sim, 
                    levels = models
                )

                # Define colors etc
                colors <- c("cornflowerblue", "salmon")
                names(colors) <- models

                # Create a barplot from these data
                plt <- ggplot2::ggplot(
                    data = tmp,
                    ggplot2::aes(
                        x = factor(parameter),
                        y = stat,
                        fill = factor(sim)
                    )
                ) +
                    ggplot2::geom_boxplot(
                        # stat = "identity",
                        position = ggplot2::position_dodge()
                    ) + 
                    # Labels, colors, and limits  
                    ggplot2::scale_y_continuous(limits = ylims) +
                    ggplot2::scale_fill_manual(values = colors) +
                    ggplot2::labs(
                        x = "Parameter",
                        y = col_titles[j],
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
        ) #%>% 
            # ggpubr::annotate_figure(
            #     top = ggpubr::text_grob(
            #         j, 
            #         face = "bold", 
            #         hjust = 0.5,
            #         size = 50
            #     )
            # )

        # Save them in the correct folder under the correct name
        ggplot2::ggsave(
            file.path(
                "figures",
                paste0(
                    "compensatory_", 
                    parts[i], 
                    "_",
                    j,
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





# INTERACTION PLOTS: Taking into account the number of bins, outcomes, and 
# participants, as well as the specification of the models
parts <- unique(combinations$part)
cols <- c("variance", "se_ratio", "reliability_coef", "bias", "mse")
col_titles <- c(
    "variance" = "Variance",
    "se_ratio" = "SE ratio",
    "reliability_coef" = "Reliability coefficient",
    "bias" = "Bias",
    "mse" = "SE"
)

for(i in seq_along(parts)) {
    # Define the models that belong to this part and select the data that belong 
    # here
    models <- combinations[combinations$part == parts[i], ]
    selection <- data %>% 
        dplyr::filter(
            true_model_type %in% unique(models$sim), 
            fit_type %in% unique(models$est)
        ) %>% 
        dplyr::rename(
            sim = true_model_type, 
            est = fit_type
        ) %>% 
        dplyr::select(
            -condition_id, 
            -misspec_type, 
            -x_distribution, 
            -scenario, 
            -false_confidence
        ) %>% 
        dplyr::mutate(
            parameter = ifelse(
                parameter == "(Intercept)",
                "intercept", 
                ifelse(
                    parameter %in% c("x", "x1"), 
                    "slope_1", 
                    ifelse( 
                        parameter %in% c("I(x^2)", "x2"), 
                        "slope_2", 
                        ifelse( 
                            parameter %in% c("I(x^3)", "x1:x2"), 
                            "slope_3", 
                            NA
                        )
                    )
                )
            )
        )

    # Loop over all summary statistics that Kenny used
    for(j in cols) {
        # Create two boxplots: One for each estimation model
        models <- unique(selection$est) %>%
            sort()
        plt <- lapply(
            models,
            function(x) {
                # Define the limits for this column for these models
                ylims <- range(selection[, j])

                # Select the relevant data
                tmp <- dplyr::filter(
                    selection,
                    est == x
                )
                tmp <- tmp[, c("sim", "sample_size", j)] %>% 
                    setNames(c("sim", "sample_size", "stat"))

                # Make into plottable data
                tmp$sim <- factor(
                    tmp$sim, 
                    levels = models
                )

                # Define colors etc
                colors <- c("cornflowerblue", "salmon")
                names(colors) <- models

                # Create a barplot from these data
                plt <- ggplot2::ggplot(
                    data = tmp,
                    ggplot2::aes(
                        x = factor(sample_size),
                        y = stat,
                        fill = factor(sim)
                    )
                ) +
                    ggplot2::geom_boxplot(
                        # stat = "identity",
                        position = ggplot2::position_dodge()
                    ) + 
                    # Labels, colors, and limits  
                    ggplot2::scale_y_continuous(limits = ylims) +
                    ggplot2::scale_fill_manual(values = colors) +
                    ggplot2::labs(
                        x = "Sample size",
                        y = col_titles[j],
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
        ) #%>% 
            # ggpubr::annotate_figure(
            #     top = ggpubr::text_grob(
            #         j, 
            #         face = "bold", 
            #         hjust = 0.5,
            #         size = 50
            #     )
            # )

        # Save them in the correct folder under the correct name
        ggplot2::ggsave(
            file.path(
                "figures",
                paste0(
                    "compensatory_", 
                    parts[i], 
                    "_",
                    j,
                    "-samplesize.png"
                )
            ),
            plt,
            width = 4000,
            height = 2400,
            unit = "px"
        )
    }
}
