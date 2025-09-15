PARAMETER RELIABILITY
===================

Authors: Kenny Yu & Niels Vanhasbroeck
Focus: Parameter reliability with misspecified models


PROJECT OVERVIEW
================

This project examines how parameter estimates compensate when statistical 
models are misspecified, and whether reliability metrics can detect such 
misspecification. Kenny focuses on compensatory mechanisms, while Niels 
focuses on test-retest reliability.


CORE RESEARCH QUESTION
======================

When you fit the wrong model to data, how do the remaining parameters 
"compensate" for missing components? Can reliability degradation serve 
as a diagnostic tool for model misspecification?


STUDY DESIGN
============

Model Comparisons:

Under-parameterization (compensatory mechanisms):
- Quadratic vs Linear: Missing curvature (cx²) compensation
- Interaction vs Main Effects: Missing interaction (dxz) compensation  
- Cubic vs Quadratic: Missing higher-order curvature (dx³) compensation

Over-parameterization (redundancy effects):
- Linear vs Quadratic: Redundant quadratic parameter reliability
- Main vs Interaction: Redundant interaction parameter reliability
- Quadratic vs Cubic: Redundant cubic parameter reliability

Parameter Space:
- Intercepts: 7 values in [-10, 10]
- Slopes: 7 values in [-5, 5]
- Higher-order effects: 5 non-zero values in [-1.5, 1.5]


SCRIPTS OVERVIEW
================

Core Framework:
--------------
model.R         - S4 model classes (Linear, Quadratic, Cubic, Interaction, AR1)
simulate.R      - Data generation methods for each model type
fitting.R       - Model fitting functions with error handling
reliability.R   - Reliability metrics (CV, SNR, bias, coverage, test-retest)

Analysis Modules:
----------------
compensatory.R  - Kenny's compensatory mechanism analysis
                  generate_compensatory_grid() - Parameter combinations
                  run_compensatory_condition() - Single condition analysis
                  run_compensatory_study() - Full study execution
                  
testretest.R    - Niels' test-retest analysis (to be implemented)

Support:
--------
utils.R         - Framework management, result processing, progress monitoring
visualization.R - Comprehensive plotting functions
execution.R     - Main interface and study orchestration


QUICK START
===========

Load framework:
source("execution.R")

Test everything works:
main('test')

Run pilot study:
pilot_results <- main('compensatory')

Run full study:
full_results <- run_complete_study(focus = "compensatory", study_size = "full")

Analyze results:
analysis <- analyze_results(full_results$output_file)


KEY FUNCTIONS
=============

Function                          Purpose
--------                          -------
main()                           Main interface (help, test, run studies)
run_compensatory_study()         Execute compensatory analysis
calculate_reliability()          Compute reliability metrics
compare_reliability()            Compare correct vs misspecified models
calculate_diagnostic_performance() Assess misspecification detection
create_study_visualizations()    Generate publication plots


OUTPUT
======

Results:   .RData files with complete analysis
Summaries: .csv files for large studies  
Plots:     Diagnostic power, compensatory heatmaps, parameter analysis
Metrics:   Bias, SNR degradation, coverage changes


STUDY SIZES
===========

Parameter Grid Structure:
- Model Comparisons: 6 types (3 under-parameterized + 3 over-parameterized)
- Parameter Values: Intercepts × Slopes × Higher-order effects × Sample Sizes × Error SDs
- Datasets per Condition: Multiple datasets for stable reliability estimates

Size      Parameter Grid                 Sample Sizes      Error SDs      Datasets/Condition    Total Conditions    Total Simulations   
----      --------------                 ------------      ---------      ------------------    ----------------    ------------------    
Pilot     3×3×3 parameters × 6 models    1 (N=100)        1 (σ=1.0)      50                    ~540                27,000         
Medium    5×5×4 parameters × 6 models    2 (50,100)       2 (1.0,2.0)    100                   ~4,800              480,000       
Full      7×7×5 parameters × 6 models    3 (50,100,200)   3 (0.5,1.0,2.0) 200                  ~16,200             3,240,000   

Model-Specific Parameter Counts:
Under-parameterization:
- Quadratic vs Linear: 7 × 7 × 5 = 245 parameter sets
- Interaction vs Main: 7 × 7 × 7 × 5 = 1,715 parameter sets  
- Cubic vs Quadratic: 7 × 7 × 5 × 5 = 1,225 parameter sets

Over-parameterization:
- Linear vs Quadratic: 7 × 7 = 49 parameter sets
- Main vs Interaction: 7 × 7 × 7 = 343 parameter sets
- Quadratic vs Cubic: 7 × 7 × 5 = 245 parameter sets

Total parameter sets: 3,822 (before applying sample size and error variance conditions)

Calculation Example (Full Study):
- 3,822 parameter sets × 3 sample sizes × 3 error SDs = 34,398 combinations
- After optimization: ~16,200 final conditions
- 16,200 conditions × 200 datasets each = 3,240,000 simulations

Notes:
- Each simulation: generate data from true model → fit correct model → fit misspecified model → calculate metrics
- Under-parameterization tests compensatory mechanisms (parameters absorbing missing effects)
- Over-parameterization tests redundancy effects (unnecessary parameters reducing reliability)
- AR(1) model retained for Niels' test-retest analysis but not used in compensatory study
- AR(1) vs Linear comparisons conceptually differ from compensatory mechanisms: they involve missing temporal error correlation rather than missing substantive predictor effects that remaining parameters can absorb