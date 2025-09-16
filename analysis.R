# TO DO
#
# Run all the analyses we want to run on the simulation results. Currently 
# contains visualizations that Niels used for the IMPS conference


# ################################################################################
# # VISUALIZATION

# # GENERAL PLOTS: Aggregating over the number of bins, outcomes, and participants
# parts <- unique(conditions$part)
# for(i in seq_along(parts)) {
#     # Define the models that belong to this part
#     selection <- conditions[conditions$part == parts[i], ]
#     models <- unique(selection$sim_model)

#     # Make the unique combinations again
#     combinations <- cbind(
#         rep(models, each = length(models)),
#         rep(models, times = length(models))
#     )

#     # Loop over these combinations and make a plot for each type
#     icc <- lapply(
#         1:nrow(combinations),
#         function(j) {
#             # Read in the ICC results for this combination of models
#             icc <- readRDS(
#                 file.path(
#                     "results",
#                     parts[i], 
#                     paste0(combinations[j, 1], "-", combinations[j, 2], "_icc.RDS")
#                 )
#             )

#             # Add model information to it
#             icc$sim <- combinations[j, 1]
#             icc$est <- combinations[j, 2]

#             return(icc)
#         }
#     )
#     icc <- do.call("rbind", icc)

#     # Create two barplots: One for each estimation model
#     models <- unique(icc$est) %>%
#         sort()
#     plt <- lapply(
#         models,
#         function(x) {
#             # Select the relevant data
#             data <- dplyr::filter(
#                 icc,
#                 est == x
#             )

#             # Make into plottable data
#             data <- data %>%
#                 dplyr::group_by(parameter, sim) %>% 
#                 dplyr::summarize(
#                     M = mean(icc),
#                     Q025 = quantile(icc, prob = 0.025),
#                     Q975 = quantile(icc, prob = 0.975)
#                 ) %>% 
#                 dplyr::ungroup()

#             data$sim <- factor(
#                 data$sim, 
#                 levels = models
#             )

#             # Define colors etc
#             colors <- c("cornflowerblue", "salmon")
#             names(colors) <- models

#             # Create a barplot from these data
#             plt <- ggplot2::ggplot(
#                 data = data,
#                 ggplot2::aes(
#                     x = factor(parameter),
#                     y = M,
#                     ymin = Q025,
#                     ymax = Q975,
#                     fill = factor(sim)
#                 )
#             ) +
#                 ggplot2::geom_errorbar(
#                     position = ggplot2::position_dodge(0.9),
#                     width = 0.4
#                 ) +
#                 ggplot2::geom_bar(
#                     stat = "identity",
#                     position = ggplot2::position_dodge()
#                 ) + 
#                 # Labels, colors, and limits    
#                 ggplot2::scale_y_continuous(
#                     limits = c(0, 1),
#                     expand = c(0, 0)
#                 ) +
#                 ggplot2::scale_fill_manual(values = colors) +
#                 ggplot2::labs(
#                     x = "Parameter",
#                     y = "ICC",
#                     fill = "Simulating model",
#                     title = x
#                 ) +
#                 # Change theme elements
#                 ggplot2::theme_minimal() +
#                 ggplot2::theme(
#                     panel.border = ggplot2::element_rect(
#                         fill = NA, 
#                         color = "black",
#                         linewidth = 1
#                     ),
#                     axis.text = ggplot2::element_text(
#                         size = 20
#                     ),
#                     axis.title = ggplot2::element_text(
#                         size = 30,
#                         face = "bold"
#                     ),
#                     plot.title = ggplot2::element_text(
#                         size = 40,
#                         hjust = 0.5,
#                         face = "bold"
#                     ),
#                     legend.title = ggplot2::element_text(
#                         size = 20,
#                         face = "bold"
#                     ),
#                     legend.text = ggplot2::element_text(
#                         size = 20
#                     )
#                 )

#             return(plt)
#         }
#     )

#     # Bind them together
#     plt <- ggpubr::ggarrange(
#         plotlist = plt,
#         nrow = 1,
#         common.legend = TRUE,
#         legend = "bottom"
#     )

#     # Save them in the correct folder under the correct name
#     ggplot2::ggsave(
#         file.path(
#             "figures",
#             paste0(
#                 "test-retest_", 
#                 parts[i], 
#                 ".png"
#             )
#         ),
#         plt,
#         width = 4000,
#         height = 2400,
#         unit = "px"
#     )
# }





# # INTERACTION PLOTS: Taking into account the number of bins, outcomes, and 
# # participants, as well as the specification of the models
# parts <- unique(conditions$part)
# for(i in seq_along(parts)) {
#     # Define the models that belong to this part
#     selection <- conditions[conditions$part == parts[i], ]
#     models <- unique(selection$sim_model)

#     # Make the unique combinations again
#     combinations <- cbind(
#         rep(models, each = length(models)),
#         rep(models, times = length(models))
#     )

#     # Loop over these combinations and make a plot for each type
#     icc <- lapply(
#         1:nrow(combinations),
#         function(j) {
#             # Read in the ICC results for this combination of models
#             icc <- readRDS(
#                 file.path(
#                     "results",
#                     parts[i], 
#                     paste0(combinations[j, 1], "-", combinations[j, 2], "_icc.RDS")
#                 )
#             )

#             # Add model information to it
#             icc$sim <- combinations[j, 1]
#             icc$est <- combinations[j, 2]

#             return(icc)
#         }
#     )
#     icc <- do.call("rbind", icc)

#     # Specify the correct models
#     models <- unique(icc$est) %>%
#         sort()

#     # Define the columns to look out for
#     cols <- c("n_bins", "n_outcomes", "n_participants")
#     col_titles <- c(
#         "n_bins" = "Number of repetitions",
#         "n_outcomes" = "Outcomes per repetition",
#         "n_participants" = "Number of participants"
#     )

#     # Loop over these columns and make a plot showing their effect
#     for(j in cols) {
#         # We again make one barplot per estimated model
#         plt <- lapply(
#             models,
#             function(x) {
#                 # Select the relevant data
#                 data <- dplyr::filter(
#                     icc,
#                     est == x
#                 )
#                 data <- data[, c("icc", "sim", j)] %>%
#                     setNames(c("icc", "sim", "condition"))

#                 # Make into plottable data
#                 data <- data %>%
#                     dplyr::group_by(condition, sim) %>% 
#                     dplyr::summarize(
#                         M = mean(icc),
#                         Q025 = quantile(icc, prob = 0.025),
#                         Q975 = quantile(icc, prob = 0.975)
#                     ) %>% 
#                     dplyr::ungroup()

#                 data$sim <- factor(
#                     data$sim, 
#                     levels = models
#                 )
#                 data$condition <- factor(
#                     data$condition,
#                     levels = sort(unique(data$condition))
#                 )

#                 # Define colors etc
#                 colors <- c("cornflowerblue", "salmon")
#                 names(colors) <- models

#                 # Create a barplot from these data
#                 plt <- ggplot2::ggplot(
#                     data = data,
#                     ggplot2::aes(
#                         x = condition,
#                         y = M,
#                         ymin = Q025,
#                         ymax = Q975,
#                         fill = factor(sim)
#                     )
#                 ) +
#                     ggplot2::geom_errorbar(
#                         position = ggplot2::position_dodge(0.9),
#                         width = 0.4
#                     ) +
#                     ggplot2::geom_bar(
#                         stat = "identity",
#                         position = ggplot2::position_dodge()
#                     ) + 
#                     # Labels, colors, and limits    
#                     ggplot2::scale_y_continuous(
#                         limits = c(0, 1),
#                         expand = c(0, 0)
#                     ) +
#                     ggplot2::scale_fill_manual(values = colors) +
#                     ggplot2::labs(
#                         x = col_titles[j],
#                         y = "ICC",
#                         fill = "Simulating model",
#                         title = x
#                     ) +
#                     # Change theme elements
#                     ggplot2::theme_minimal() +
#                     ggplot2::theme(
#                         panel.border = ggplot2::element_rect(
#                             fill = NA, 
#                             color = "black",
#                             linewidth = 1
#                         ),
#                         axis.text = ggplot2::element_text(
#                             size = 20
#                         ),
#                         axis.title = ggplot2::element_text(
#                             size = 30,
#                             face = "bold"
#                         ),
#                         plot.title = ggplot2::element_text(
#                             size = 40,
#                             hjust = 0.5,
#                             face = "bold"
#                         ),
#                         legend.title = ggplot2::element_text(
#                             size = 20,
#                             face = "bold"
#                         ),
#                         legend.text = ggplot2::element_text(
#                             size = 20
#                         )
#                     )

#                 return(plt)
#             }
#         )

#         # Bind them together
#         plt <- ggpubr::ggarrange(
#             plotlist = plt,
#             nrow = 1,
#             common.legend = TRUE,
#             legend = "bottom"
#         )

#         # Save them in the correct folder under the correct name
#         ggplot2::ggsave(
#             file.path(
#                 "figures",
#                 paste0(
#                     "test-retest_", 
#                     j, 
#                     "_",
#                     parts[i], 
#                     ".png"
#                 )
#             ),
#             plt,
#             width = 4000,
#             height = 2400,
#             unit = "px"
#         )
#     }
# }