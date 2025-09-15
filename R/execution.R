# Purpose: Main execution script for parameter reliability study
# Authors: Kenny Yu & Niels Vanhasbroeck
# Date: July 2025
# Study: Parameter reliability with focus on compensatory mechanisms (Kenny) and test-retest reliability (Niels)

# Load required packages
check_packages <- function(required_packages = c("parallel"), 
                           optional_packages = c("ggplot2", "gridExtra", "reshape2")) {
  
  missing_required <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  if(length(missing_required) > 0) {
    message("Installing required packages: ", paste(missing_required, collapse = ", "))
    install.packages(missing_required)
  }
  
  missing_optional <- optional_packages[!sapply(optional_packages, requireNamespace, quietly = TRUE)]
  if(length(missing_optional) > 0) {
    message("Optional packages available: ", paste(missing_optional, collapse = ", "))
  }
  
  invisible(TRUE)
}

# Load complete parameter reliability framework
load_parameter_reliability_framework <- function(script_dir = NULL) {
  
  message("Loading Parameter Reliability Framework")
  
  if(is.null(script_dir)) {
    if(file.exists("model.R")) {
      script_dir <- "."
    } else if(file.exists("R/model.R")) {
      script_dir <- "R"
    } else {
      script_dir <- "."
    }
  }
  
  message("Script directory: ", script_dir)
  
  script_order <- c("model.R", "simulate.R", "fitting.R", "reliability.R", 
                    "utils.R", "compensatory.R", "testretest.R", "visualization.R")
  
  loaded_scripts <- character(0)
  for(script in script_order) {
    script_path <- file.path(script_dir, script)
    
    if(file.exists(script_path)) {
      tryCatch({
        source(script_path)
        loaded_scripts <- c(loaded_scripts, script)
      }, error = function(e) {
        warning("Could not load ", script, ": ", e$message)
      })
    }
  }
  
  check_packages()
  
  message("Loaded scripts: ", paste(loaded_scripts, collapse = ", "))
  
  if(length(loaded_scripts) >= 6) {
    message("Framework ready")
    return(TRUE)
  } else {
    warning("Some scripts missing")
    return(FALSE)
  }
}

# Run compensatory analysis pilot study
run_compensatory_pilot <- function(output_dir = "compensatory_results", create_plots = TRUE) {
  
  message("Compensatory Analysis Pilot Study")
  message("Model comparisons:")
  message("Under-parameterization (compensatory):")
  message("  - Quadratic vs Linear (missing curvature)")
  message("  - Interaction vs Main effects (missing interaction)")
  message("  - Cubic vs Quadratic (missing higher-order curvature)")
  message("Over-parameterization (redundancy):")
  message("  - Linear vs Quadratic (redundant quadratic)")
  message("  - Main vs Interaction (redundant interaction)")
  message("  - Quadratic vs Cubic (redundant cubic)")
  
  if(!exists("run_compensatory_study")) {
    stop("Compensatory analysis functions not loaded")
  }
  
  result <- run_compensatory_study(study_size = "pilot", output_dir = output_dir)
  
  if(!is.null(result) && !is.null(result$output_file) && file.exists(result$output_file)) {
    
    analysis <- NULL
    if(exists("analyze_compensatory_results")) {
      tryCatch({
        analysis <- analyze_compensatory_results(result$output_file)
        summary_file <- file.path(output_dir, "pilot_summary.RData")
        save(analysis, file = summary_file)
      }, error = function(e) {
        message("Analysis failed: ", e$message)
      })
    }
    
    visualizations <- NULL
    if(create_plots && exists("create_study_visualizations")) {
      tryCatch({
        viz_dir <- file.path(output_dir, "plots")
        visualizations <- create_study_visualizations(result$study_results, viz_dir)
      }, error = function(e) {
        message("Visualization failed: ", e$message)
      })
    }
    
    return(list(
      study_results = result,
      analysis = analysis,
      visualizations = visualizations,
      output_dir = output_dir
    ))
    
  } else {
    return(result)
  }
}

# Run test-retest analysis pilot study
run_testretest_pilot <- function(output_dir = "testretest_results", create_plots = TRUE) {
  
  message("Test-Retest Analysis Framework")
  message("Structure ready for implementation by Niels")
  message("Models: Linear, Quadratic, Cubic, Interaction, AR(1)")
  
  if(exists("run_testretest_study")) {
    result <- run_testretest_study(study_size = "pilot", output_dir = output_dir)
    
    visualizations <- NULL
    if(create_plots && exists("create_study_visualizations")) {
      tryCatch({
        viz_dir <- file.path(output_dir, "plots")
        visualizations <- create_study_visualizations(result, viz_dir)
      }, error = function(e) {
        message("Visualization failed: ", e$message)
      })
    }
    
    return(list(study_results = result, visualizations = visualizations, output_dir = output_dir))
  } else {
    return(list(status = "pending", output_dir = output_dir))
  }
}

# Run complete parameter reliability study
run_complete_study <- function(focus = "compensatory", study_size = "pilot", create_plots = TRUE) {
  
  message("Parameter Reliability Study: ", toupper(focus), " (", study_size, ")")
  
  # Display study scope based on size
  if(focus == "compensatory") {
    if(study_size == "pilot") {
      message("Pilot scope: ~540 conditions, 54,000 simulations")
    } else if(study_size == "medium") {
      message("Medium scope: ~9,600 conditions, 1,920,000 simulations")  
    } else if(study_size == "full") {
      message("Full scope: ~32,400 conditions, 12,960,000 simulations")
    }
  }
  
  if(focus == "compensatory") {
    
    if(exists("run_compensatory_study")) {
      result <- run_compensatory_study(
        study_size = study_size,
        output_dir = paste0("compensatory_", study_size, "_results")
      )
      
      if(create_plots && exists("create_study_visualizations")) {
        tryCatch({
          viz_dir <- paste0("compensatory_", study_size, "_plots")
          visualizations <- create_study_visualizations(result$study_results, viz_dir)
          result$visualizations <- visualizations
        }, error = function(e) {
          message("Visualization failed: ", e$message)
        })
      }
      
      return(result)
    } else {
      stop("Compensatory analysis functions not available")
    }
    
  } else if(focus == "test_retest") {
    
    if(exists("run_testretest_study")) {
      result <- run_testretest_study(
        study_size = study_size,
        output_dir = paste0("testretest_", study_size, "_results")
      )
      
      if(create_plots && exists("create_study_visualizations")) {
        tryCatch({
          viz_dir <- paste0("testretest_", study_size, "_plots")
          visualizations <- create_study_visualizations(result, viz_dir)
          result$visualizations <- visualizations
        }, error = function(e) {
          message("Visualization failed: ", e$message)
        })
      }
      
      return(result)
    } else {
      return(list(status = "pending", focus = "test_retest"))
    }
    
  } else if(focus == "both") {
    
    compensatory_results <- NULL
    if(exists("run_compensatory_study")) {
      compensatory_results <- run_compensatory_study(
        study_size = study_size,
        output_dir = paste0("compensatory_", study_size, "_results")
      )
    }
    
    testretest_results <- NULL
    if(exists("run_testretest_study")) {
      testretest_results <- run_testretest_study(
        study_size = study_size,
        output_dir = paste0("testretest_", study_size, "_results")
      )
    } else {
      testretest_results <- list(status = "pending")
    }
    
    return(list(compensatory = compensatory_results, testretest = testretest_results))
    
  } else {
    stop("Focus must be 'compensatory', 'test_retest', or 'both'")
  }
}

# Quick framework test
quick_test <- function() {
  
  message("Framework Testing")
  
  framework_loaded <- load_parameter_reliability_framework()
  
  if(!framework_loaded) {
    message("Framework loading incomplete")
  }
  
  if(exists("test_framework")) {
    framework_ok <- test_framework()
    
    if(framework_ok) {
      message("Core framework: OK")
      
      if(exists("test_compensatory")) {
        comp_test <- test_compensatory()
        if(comp_test) {
          message("Compensatory analysis: OK")
        } else {
          message("Compensatory analysis: FAILED")
          return(FALSE)
        }
      }
      
      if(exists("test_visualization")) {
        viz_test <- test_visualization()
        if(viz_test) {
          message("Visualization: OK")
        } else {
          message("Visualization: FAILED (non-critical)")
        }
      }
      
      message("Framework ready for compensatory studies")
      return(TRUE)
      
    } else {
      message("Core framework: FAILED")
      return(FALSE)
    }
  } else {
    message("Basic loading: ", ifelse(framework_loaded, "OK", "FAILED"))
    return(framework_loaded)
  }
}

# Analyze existing study results
analyze_results <- function(results_file, create_plots = TRUE) {
  
  if(!file.exists(results_file)) {
    stop("Results file not found: ", results_file)
  }
  
  load(results_file)
  if(!exists("study_results")) {
    stop("No study_results object found")
  }
  
  analysis_type <- study_results$analysis_type
  
  if(analysis_type == "compensatory") {
    
    analysis <- NULL
    if(exists("analyze_compensatory_results")) {
      analysis <- analyze_compensatory_results(results_file)
    }
    
    visualizations <- NULL
    if(create_plots && exists("create_study_visualizations")) {
      tryCatch({
        viz_dir <- paste0(dirname(results_file), "_plots")
        visualizations <- create_study_visualizations(study_results, viz_dir)
      }, error = function(e) {
        message("Visualization failed: ", e$message)
      })
    }
    
    return(list(analysis = analysis, visualizations = visualizations))
    
  } else if(analysis_type == "test_retest") {
    
    if(exists("analyze_testretest_results")) {
      return(analyze_testretest_results(results_file))
    } else {
      message("Test-retest analysis not implemented")
      return(NULL)
    }
    
  } else {
    message("Unknown analysis type: ", analysis_type)
    return(NULL)
  }
}

# Main interface function
main <- function(action = "help", ...) {
  
  if(action == "help") {
    
    message("Parameter Reliability Study Framework")
    message("====================================")
    message("")
    message("Actions:")
    message("1. main('load')                    - Load framework")
    message("2. main('test')                    - Test framework") 
    message("3. main('compensatory')            - Run compensatory pilot study")
    message("4. main('testretest')              - Run test-retest pilot study")
    message("5. main('both')                    - Run both studies")
    message("6. main('analyze', 'file.RData')   - Analyze results")
    message("")
    message("Study sizes: 'pilot', 'medium', 'full'")
    message("  Pilot:  ~540 conditions, 54,000 simulations")
    message("  Medium: ~9,600 conditions, 1,920,000 simulations")
    message("  Full:   ~32,400 conditions, 12,960,000 simulations")
    message("")
    message("Compensatory mechanisms study:")
    message("Under-parameterization (compensatory):")
    message("- Quadratic vs Linear: Missing curvature compensation")
    message("- Interaction vs Main: Missing interaction compensation")
    message("- Cubic vs Quadratic: Missing higher-order curvature compensation")
    message("Over-parameterization (redundancy):")
    message("- Linear vs Quadratic: Redundant quadratic parameter")
    message("- Main vs Interaction: Redundant interaction parameter")
    message("- Quadratic vs Cubic: Redundant cubic parameter")
    message("")
    message("Parameter ranges: Intercepts [-10,10], Slopes [-5,5], Higher-order effects [-3,3]")
    
  } else if(action == "load") {
    
    return(load_parameter_reliability_framework())
    
  } else if(action == "test") {
    
    return(quick_test())
    
  } else if(action == "compensatory") {
    
    load_parameter_reliability_framework()
    return(run_compensatory_pilot())
    
  } else if(action == "testretest") {
    
    load_parameter_reliability_framework()
    return(run_testretest_pilot())
    
  } else if(action == "both") {
    
    load_parameter_reliability_framework()
    
    comp_results <- run_compensatory_pilot()
    tr_results <- run_testretest_pilot()
    
    return(list(compensatory = comp_results, testretest = tr_results))
    
  } else if(action == "analyze") {
    
    args <- list(...)
    if(length(args) == 0) {
      stop("Specify results file: main('analyze', 'filename.RData')")
    }
    
    load_parameter_reliability_framework()
    return(analyze_results(args[[1]]))
    
  } else {
    
    message("Unknown action. Use main('help') for options")
    
  }
}

# Framework initialization
message("Parameter Reliability Study Framework")
message("Authors: Kenny Yu & Niels Vanhasbroeck")
message("")
message("Quick start:")
message("1. main('test')          # Test framework")
message("2. main('compensatory')  # Run compensatory analysis")
message("")
message("For help: main('help')")