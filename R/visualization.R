# Purpose: Visualization functions for parameter reliability study
# Authors: Kenny Yu & Niels Vanhasbroeck
# Date: July 2025
# Structure: Unified visualization for compensatory and test-retest analyses

# Setup and Infrastructure
# ===============================================================================

# Setup Visualization Environment
setup_visualization <- function() {
  
  required_packages <- c("ggplot2", "gridExtra", "reshape2")
  optional_packages <- c("RColorBrewer", "viridis")
  
  missing_required <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  if(length(missing_required) > 0) {
    message("Installing visualization packages: ", paste(missing_required, collapse = ", "))
    install.packages(missing_required)
  }
  
  missing_optional <- optional_packages[!sapply(optional_packages, requireNamespace, quietly = TRUE)]
  if(length(missing_optional) > 0) {
    message("Optional packages: ", paste(missing_optional, collapse = ", "))
  }
  
  suppressPackageStartupMessages({
    library(ggplot2)
    library(gridExtra)
    library(reshape2)
    if(requireNamespace("RColorBrewer", quietly = TRUE)) library(RColorBrewer)
  })
  
  theme_set(
    theme_minimal() + 
      theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray30"),
        axis.title = element_text(size = 11),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 10),
        strip.text = element_text(size = 11, face = "bold"),
        panel.grid.minor = element_blank()
      )
  )
  
  invisible(TRUE)
}

# Create Output Directory
create_output_directory <- function(output_dir) {
  if(!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  invisible(TRUE)
}

# Data Extraction
# ===============================================================================

# Extract Visualization Data from Study Results
extract_study_data <- function(study_results) {
  
  if(is.null(study_results) || is.null(study_results$results)) {
    stop("Invalid study results")
  }
  
  analysis_type <- study_results$analysis_type
  
  if(analysis_type == "compensatory") {
    return(extract_compensatory_data(study_results))
  } else if(analysis_type == "test_retest") {
    return(extract_testretest_data(study_results))
  } else {
    stop("Unknown analysis type: ", analysis_type)
  }
}

# Extract Compensatory Analysis Data
extract_compensatory_data <- function(study_results) {
  
  results_list <- study_results$results
  
  # Remove NULL entries and check for errors more carefully
  results_list <- results_list[!sapply(results_list, is.null)]
  successful <- !sapply(results_list, function(x) {
    if(is.null(x)) return(TRUE)
    if("error_occurred" %in% names(x)) return(isTRUE(x$error_occurred))
    return(FALSE)
  })
  results_list <- results_list[successful]
  
  if(length(results_list) == 0) {
    stop("No successful results to visualize")
  }
  
  message("Processing ", length(results_list), " successful results")
  
  viz_data <- list()
  
  for(i in 1:length(results_list)) {
    result <- results_list[[i]]
    
    # Safely extract basic fields with NULL checks
    model_comp <- if(is.null(result$model_comparison)) "unknown" else result$model_comparison
    sample_sz <- if(is.null(result$sample_size)) NA else result$sample_size
    error_sd_val <- if(is.null(result$condition_params) || is.null(result$condition_params$error_sd)) {
      NA
    } else {
      result$condition_params$error_sd
    }
    
    condition_data <- data.frame(
      condition_id = i,
      model_comparison = model_comp,
      sample_size = sample_sz,
      error_sd = error_sd_val,
      analysis_type = "compensatory",
      stringsAsFactors = FALSE
    )
    
    # Classify as under-parameterization or over-parameterization
    under_param <- c("quadratic_vs_linear", "interaction_vs_main", "cubic_vs_quadratic")
    condition_data$param_type <- ifelse(model_comp %in% under_param, 
                                        "Under-parameterization", "Over-parameterization")
    
    # Safely extract diagnostic power
    if(!is.null(result$diagnostic_metrics) && 
       !is.null(result$diagnostic_metrics$summary) && 
       !is.null(result$diagnostic_metrics$summary$study_diagnostic_power)) {
      condition_data$diagnostic_power <- result$diagnostic_metrics$summary$study_diagnostic_power
    } else {
      condition_data$diagnostic_power <- NA
    }
    
    # ALWAYS initialize all possible parameter columns
    for(param in c("intercept", "x", "z", "x_z", "x2", "x3", "y_lag")) {
      condition_data[[paste0(param, "_bias_diff")]] <- NA
      condition_data[[paste0(param, "_snr_degrad")]] <- NA
      condition_data[[paste0(param, "_coverage_diff")]] <- NA
      condition_data[[paste0(param, "_mse_ratio")]] <- NA
    }
    
    # Fill in available parameter data if reliability comparison exists
    if(!is.null(result$reliability_comparison) && 
       !is.null(result$reliability_comparison$metadata) &&
       !is.null(result$reliability_comparison$metadata$common_parameters)) {
      
      comp <- result$reliability_comparison
      common_params <- comp$metadata$common_parameters
      
      for(param in common_params) {
        if(param %in% names(comp) && !is.null(comp[[param]])) {
          metrics <- comp[[param]]
          if(!is.null(metrics$bias_difference) && is.finite(metrics$bias_difference)) {
            condition_data[[paste0(param, "_bias_diff")]] <- metrics$bias_difference
          }
          if(!is.null(metrics$snr_degradation) && is.finite(metrics$snr_degradation)) {
            condition_data[[paste0(param, "_snr_degrad")]] <- metrics$snr_degradation
          }
          if(!is.null(metrics$coverage_difference) && is.finite(metrics$coverage_difference)) {
            condition_data[[paste0(param, "_coverage_diff")]] <- metrics$coverage_difference
          }
          if(!is.null(metrics$mse_ratio) && is.finite(metrics$mse_ratio)) {
            condition_data[[paste0(param, "_mse_ratio")]] <- metrics$mse_ratio
          }
        }
      }
    }
    
    viz_data[[i]] <- condition_data
    
    # Progress indicator for large datasets
    if(i %% 5000 == 0) {
      message("Processed ", i, "/", length(results_list), " results")
    }
  }
  
  message("Combining data frames...")
  complete_data <- do.call(rbind, viz_data)
  
  # Clean model comparison names
  complete_data$model_comparison <- gsub("_vs_", " vs ", complete_data$model_comparison)
  complete_data$model_comparison <- gsub("_", " ", complete_data$model_comparison)
  complete_data$model_comparison <- tools::toTitleCase(complete_data$model_comparison)
  
  message("Data extraction completed: ", nrow(complete_data), " conditions")
  
  return(complete_data)
}

# Extract Test-Retest Analysis Data (Placeholder)
extract_testretest_data <- function(study_results) {
  message("Test-retest data extraction not implemented")
  stop("Test-retest visualization not yet implemented")
}

# Compensatory Visualizations
# ===============================================================================

# Create Diagnostic Power Overview
create_compensatory_diagnostic_plot <- function(viz_data, output_dir) {
  
  if(!"diagnostic_power" %in% names(viz_data) || all(is.na(viz_data$diagnostic_power))) {
    message("No diagnostic power data available")
    return(NULL)
  }
  
  p1 <- ggplot(viz_data, aes(x = model_comparison, y = diagnostic_power, fill = param_type)) +
    geom_violin(alpha = 0.7, trim = FALSE) +
    geom_boxplot(width = 0.3, alpha = 0.9, outlier.alpha = 0.7, position = position_dodge(0.8)) +
    stat_summary(fun = mean, geom = "point", size = 3, color = "darkred", 
                 position = position_dodge(0.8)) +
    geom_hline(yintercept = 50, linetype = "dashed", color = "red", size = 1, alpha = 0.8) +
    geom_hline(yintercept = 80, linetype = "dashed", color = "darkgreen", size = 1, alpha = 0.8) +
    scale_fill_manual(values = c("Under-parameterization" = "#66C2A5", 
                                 "Over-parameterization" = "#FC8D62")) +
    facet_wrap(~ param_type, scales = "free_x") +
    labs(
      title = "Compensatory Mechanism Diagnostic Power",
      subtitle = "Reliability metrics detecting model misspecification",
      x = "Model Comparison",
      y = "Diagnostic Power (%)",
      fill = "Parameterization Type",
      caption = "Red line: 50% threshold | Green line: 80% threshold | Red dots: means"
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    ) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20))
  
  ggsave(file.path(output_dir, "compensatory_diagnostic_power.png"), p1, 
         width = 16, height = 10, dpi = 300)
  
  return(p1)
}

# Create Compensatory Mechanism Heatmap
create_compensatory_heatmap <- function(viz_data, output_dir) {
  
  bias_cols <- grep("_bias_diff$", names(viz_data), value = TRUE)
  snr_cols <- grep("_snr_degrad$", names(viz_data), value = TRUE)
  
  if(length(bias_cols) == 0 && length(snr_cols) == 0) {
    message("No compensatory mechanism data for heatmap")
    return(NULL)
  }
  
  effect_data <- viz_data[, c("model_comparison", "param_type", bias_cols, snr_cols)]
  melted_data <- melt(effect_data, id.vars = c("model_comparison", "param_type"), na.rm = TRUE)
  
  melted_data$parameter <- gsub("_(bias_diff|snr_degrad)$", "", melted_data$variable)
  melted_data$metric_type <- ifelse(grepl("bias_diff", melted_data$variable), 
                                    "Bias Compensation", "SNR Change")
  
  # Clean parameter names for display
  melted_data$parameter <- gsub("x_z", "x × z", melted_data$parameter)
  melted_data$parameter <- gsub("x2", "x²", melted_data$parameter)
  melted_data$parameter <- gsub("x3", "x³", melted_data$parameter)
  melted_data$parameter <- tools::toTitleCase(melted_data$parameter)
  
  # For SNR degradation, preserve sign to show improvement vs degradation
  melted_data$effect_magnitude <- ifelse(melted_data$metric_type == "SNR Change",
                                         melted_data$value, abs(melted_data$value))
  
  heatmap_summary <- aggregate(effect_magnitude ~ model_comparison + parameter + metric_type + param_type, 
                               data = melted_data, FUN = mean, na.rm = TRUE)
  
  # Filter out parameters with all missing data
  heatmap_summary <- heatmap_summary[is.finite(heatmap_summary$effect_magnitude), ]
  
  if(nrow(heatmap_summary) == 0) {
    message("No valid heatmap data after filtering")
    return(NULL)
  }
  
  p2 <- ggplot(heatmap_summary, aes(x = parameter, y = model_comparison, fill = effect_magnitude)) +
    geom_tile(color = "white", size = 0.8) +
    geom_text(aes(label = round(effect_magnitude, 2)), color = "black", size = 2.5) +
    scale_fill_gradient2(
      low = "blue", mid = "white", high = "red",
      midpoint = 0, name = "Effect\nMagnitude",
      guide = guide_colorbar(title.position = "top")
    ) +
    facet_grid(param_type ~ metric_type, scales = "free", space = "free") +
    labs(
      title = "Compensatory Mechanism Strength by Parameter",
      subtitle = "Effect magnitude across parameterization types",
      x = "Parameter",
      y = "Model Comparison"
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank(),
      legend.position = "right"
    )
  
  ggsave(file.path(output_dir, "compensatory_heatmap.png"), p2, 
         width = 16, height = 12, dpi = 300)
  
  return(p2)
}

# Create Comprehensive Parameter Analysis
create_compensatory_parameter_analysis <- function(viz_data, output_dir) {
  
  # Define parameters available for each model comparison
  param_mapping <- list(
    "Quadratic Vs Linear" = c("intercept", "x"),
    "Linear Vs Quadratic" = c("intercept", "x"),
    "Interaction Vs Main" = c("intercept", "x", "z"),
    "Main Vs Interaction" = c("intercept", "x", "z"),
    "Cubic Vs Quadratic" = c("intercept", "x", "x2"),
    "Quadratic Vs Cubic" = c("intercept", "x", "x2")
  )
  
  # Create plots for each parameter type
  param_plots <- list()
  
  for(param in c("intercept", "x", "z", "x2")) {
    
    # Check which models have this parameter
    models_with_param <- names(param_mapping)[sapply(param_mapping, function(params) param %in% params)]
    
    if(length(models_with_param) == 0) next
    
    # Filter data for models that have this parameter
    param_viz_data <- viz_data[viz_data$model_comparison %in% models_with_param, ]
    
    if(nrow(param_viz_data) == 0) next
    
    # Check for available metrics
    bias_col <- paste0(param, "_bias_diff")
    snr_col <- paste0(param, "_snr_degrad")
    coverage_col <- paste0(param, "_coverage_diff")
    
    available_metrics <- intersect(c(bias_col, snr_col, coverage_col), names(param_viz_data))
    
    if(length(available_metrics) == 0) next
    
    param_data <- param_viz_data[, c("model_comparison", "sample_size", "param_type", available_metrics)]
    param_data$sample_size_factor <- factor(param_data$sample_size)
    
    # Create parameter-specific plots
    if(bias_col %in% available_metrics && !all(is.na(param_data[[bias_col]]))) {
      
      param_label <- switch(param,
                            "intercept" = "Intercept",
                            "x" = "Linear Slope (x)",
                            "z" = "Linear Slope (z)",
                            "x2" = "Quadratic Term (x²)",
                            "x3" = "Cubic Term (x³)")
      
      p_bias <- ggplot(param_data, aes_string(x = "model_comparison", y = paste0("abs(", bias_col, ")"), 
                                              fill = "sample_size_factor")) +
        geom_boxplot(alpha = 0.8, outlier.alpha = 0.6, position = position_dodge(0.8)) +
        scale_fill_brewer(type = "seq", palette = "Blues", name = "Sample\nSize") +
        facet_wrap(~ param_type, scales = "free_x") +
        labs(
          title = paste(param_label, "Bias Compensation"),
          subtitle = "Absolute bias difference between correct and misspecified models",
          x = "Model Comparison",
          y = "Absolute Bias Difference"
        ) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      param_plots[[paste0(param, "_bias")]] <- p_bias
    }
    
    if(snr_col %in% available_metrics && !all(is.na(param_data[[snr_col]]))) {
      
      param_label <- switch(param,
                            "intercept" = "Intercept",
                            "x" = "Linear Slope (x)",
                            "z" = "Linear Slope (z)",
                            "x2" = "Quadratic Term (x²)",
                            "x3" = "Cubic Term (x³)")
      
      p_snr <- ggplot(param_data, aes_string(x = "model_comparison", y = paste0(snr_col, " * 100"), 
                                             fill = "sample_size_factor")) +
        geom_boxplot(alpha = 0.8, outlier.alpha = 0.6, position = position_dodge(0.8)) +
        geom_hline(yintercept = 0, linetype = "solid", color = "red", alpha = 0.7) +
        scale_fill_brewer(type = "seq", palette = "Greens", name = "Sample\nSize") +
        facet_wrap(~ param_type, scales = "free_x") +
        labs(
          title = paste(param_label, "SNR Change"),
          subtitle = "Reliability change from misspecification (negative = improved reliability)",
          x = "Model Comparison",
          y = "SNR Change (%)"
        ) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      param_plots[[paste0(param, "_snr")]] <- p_snr
    }
  }
  
  # Save individual parameter plots
  if(length(param_plots) > 0) {
    for(i in 1:length(param_plots)) {
      plot_name <- names(param_plots)[i]
      filename <- file.path(output_dir, paste0("compensatory_", plot_name, "_analysis.png"))
      ggsave(filename, param_plots[[i]], width = 16, height = 10, dpi = 300)
    }
    
    # Create combined overview if multiple plots
    if(length(param_plots) >= 2) {
      n_plots <- length(param_plots)
      ncol <- min(2, n_plots)
      nrow <- ceiling(n_plots / ncol)
      
      combined_plot <- do.call(grid.arrange, c(param_plots[1:min(4, n_plots)], ncol = ncol))
      ggsave(file.path(output_dir, "compensatory_parameter_overview.png"), combined_plot, 
             width = 8 * ncol, height = 6 * nrow, dpi = 300)
    }
  }
  
  return(param_plots)
}

# Create Sample Size Effects
create_compensatory_sample_size_analysis <- function(viz_data, output_dir) {
  
  if(!"diagnostic_power" %in% names(viz_data) || length(unique(viz_data$sample_size)) < 2) {
    message("Insufficient sample size variation")
    return(NULL)
  }
  
  ss_summary <- aggregate(diagnostic_power ~ sample_size + model_comparison + param_type, 
                          data = viz_data, FUN = function(x) c(mean = mean(x, na.rm = TRUE),
                                                               sd = sd(x, na.rm = TRUE)))
  
  ss_data <- data.frame(
    sample_size = ss_summary$sample_size,
    model_comparison = ss_summary$model_comparison,
    param_type = ss_summary$param_type,
    mean_power = ss_summary$diagnostic_power[, "mean"],
    sd_power = ss_summary$diagnostic_power[, "sd"]
  )
  
  p4 <- ggplot(ss_data, aes(x = sample_size, y = mean_power, color = model_comparison)) +
    geom_line(size = 1.2, alpha = 0.8) +
    geom_point(size = 3, alpha = 0.9) +
    geom_errorbar(aes(ymin = mean_power - sd_power, ymax = mean_power + sd_power),
                  width = 5, alpha = 0.6) +
    scale_color_brewer(type = "qual", palette = "Set3", name = "Model\nComparison") +
    facet_wrap(~ param_type, scales = "free_y") +
    labs(
      title = "Diagnostic Power vs Sample Size",
      subtitle = "Sample size effects on misspecification detection by parameterization type",
      x = "Sample Size",
      y = "Mean Diagnostic Power (%)",
      caption = "Error bars: ± 1 standard deviation"
    ) +
    geom_hline(yintercept = 80, linetype = "dashed", color = "darkgreen", alpha = 0.7) +
    geom_hline(yintercept = 50, linetype = "dashed", color = "red", alpha = 0.7) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
    theme(legend.position = "bottom",
          legend.text = element_text(size = 8))
  
  ggsave(file.path(output_dir, "compensatory_sample_size_effects.png"), p4, 
         width = 14, height = 10, dpi = 300)
  
  return(p4)
}

# Parameter-Specific SNR Comparison
create_snr_comparison_plot <- function(viz_data, output_dir) {
  
  # Extract SNR data for all parameters
  snr_cols <- grep("_snr_degrad$", names(viz_data), value = TRUE)
  
  if(length(snr_cols) == 0) {
    message("No SNR degradation data available")
    return(NULL)
  }
  
  snr_data <- viz_data[, c("model_comparison", "param_type", snr_cols)]
  melted_snr <- melt(snr_data, id.vars = c("model_comparison", "param_type"), na.rm = TRUE)
  
  melted_snr$parameter <- gsub("_snr_degrad$", "", melted_snr$variable)
  melted_snr$parameter <- gsub("x_z", "x × z", melted_snr$parameter)
  melted_snr$parameter <- gsub("x2", "x²", melted_snr$parameter)
  melted_snr$parameter <- gsub("x3", "x³", melted_snr$parameter)
  melted_snr$parameter <- tools::toTitleCase(melted_snr$parameter)
  
  p_snr <- ggplot(melted_snr, aes(x = parameter, y = value * 100, fill = param_type)) +
    geom_boxplot(alpha = 0.8, outlier.alpha = 0.6) +
    geom_hline(yintercept = 0, linetype = "solid", color = "red", size = 1, alpha = 0.8) +
    scale_fill_manual(values = c("Under-parameterization" = "#66C2A5", 
                                 "Over-parameterization" = "#FC8D62")) +
    facet_wrap(~ model_comparison, scales = "free_x") +
    labs(
      title = "SNR Change by Parameter and Model Comparison",
      subtitle = "Negative values indicate improved reliability in misspecified model",
      x = "Parameter",
      y = "SNR Change (%)",
      fill = "Parameterization Type"
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )
  
  ggsave(file.path(output_dir, "compensatory_snr_comparison.png"), p_snr, 
         width = 18, height = 12, dpi = 300)
  
  return(p_snr)
}

# Test-Retest Visualizations (Placeholder)
# ===============================================================================

# Create Test-Retest Plots (Placeholder)
create_testretest_reliability_plots <- function(viz_data, output_dir) {
  message("Test-retest visualization not implemented")
  return(NULL)
}

create_testretest_sample_size_effects <- function(viz_data, output_dir) {
  message("Test-retest sample size analysis not implemented")
  return(NULL)
}

# Main Visualization Interface
# ===============================================================================

# Create Complete Visualization Suite
create_study_visualizations <- function(study_results, output_dir = "study_plots") {
  
  message("Creating study visualizations")
  
  setup_visualization()
  create_output_directory(output_dir)
  
  viz_data <- extract_study_data(study_results)
  analysis_type <- study_results$analysis_type
  
  plots <- list()
  
  if(analysis_type == "compensatory") {
    
    plots$diagnostic <- create_compensatory_diagnostic_plot(viz_data, output_dir)
    plots$heatmap <- create_compensatory_heatmap(viz_data, output_dir)
    plots$parameters <- create_compensatory_parameter_analysis(viz_data, output_dir)
    plots$sample_size <- create_compensatory_sample_size_analysis(viz_data, output_dir)
    plots$snr_comparison <- create_snr_comparison_plot(viz_data, output_dir)
    
  } else if(analysis_type == "test_retest") {
    
    plots$reliability <- create_testretest_reliability_plots(viz_data, output_dir)
    plots$sample_size <- create_testretest_sample_size_effects(viz_data, output_dir)
    
  }
  
  summary_stats <- list(
    analysis_type = analysis_type,
    total_conditions = nrow(viz_data),
    creation_time = Sys.time()
  )
  
  if(analysis_type == "compensatory") {
    summary_stats$model_comparisons <- table(viz_data$model_comparison)
    summary_stats$param_types <- table(viz_data$param_type)
    summary_stats$sample_sizes <- unique(viz_data$sample_size)
    if("diagnostic_power" %in% names(viz_data)) {
      summary_stats$mean_diagnostic_power <- aggregate(diagnostic_power ~ model_comparison + param_type, 
                                                       data = viz_data, FUN = mean, na.rm = TRUE)
    }
  }
  
  save(viz_data, summary_stats, file = file.path(output_dir, "visualization_data.RData"))
  
  message("Visualizations complete: ", output_dir)
  
  return(list(
    plots = plots,
    data = viz_data,
    summary = summary_stats,
    output_dir = output_dir
  ))
}

# Visualize Pilot Results
visualize_pilot_results <- function(pilot_results, output_dir = "pilot_visualizations") {
  
  if(is.null(pilot_results) || is.null(pilot_results$study_results)) {
    stop("Invalid pilot results")
  }
  
  return(create_study_visualizations(pilot_results$study_results, output_dir))
}

# Testing
# ===============================================================================

# Test Visualization Functions
test_visualization <- function() {
  
  tryCatch({
    setup_visualization()
    
    set.seed(123)
    dummy_results <- list(
      analysis_type = "compensatory",
      results = lapply(1:60, function(i) {
        model_comp <- sample(c("quadratic_vs_linear", "interaction_vs_main", "cubic_vs_quadratic",
                               "linear_vs_quadratic", "main_vs_interaction", "quadratic_vs_cubic"), 1)
        list(
          model_comparison = model_comp,
          sample_size = sample(c(50, 100), 1),
          condition_params = list(error_sd = 1.0),
          diagnostic_metrics = list(summary = list(study_diagnostic_power = rnorm(1, 65, 15))),
          reliability_comparison = list(
            intercept = list(bias_difference = rnorm(1, 0, 0.2), snr_degradation = rnorm(1, 0, 0.3)),
            x = list(bias_difference = rnorm(1, 0, 0.1), snr_degradation = rnorm(1, 0, 0.2)),
            metadata = list(common_parameters = c("intercept", "x"))
          ),
          error_occurred = FALSE
        )
      })
    )
    
    create_study_visualizations(dummy_results, "test_viz")
    message("Visualization test passed")
    return(TRUE)
    
  }, error = function(e) {
    message("Visualization test failed: ", e$message)
    return(FALSE)
  })
}