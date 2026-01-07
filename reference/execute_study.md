# Set-up Study

Given a particular simulation model, estimate a same or other
statistical model and compute the summary statistics of interest. In our
simulation, we assume that a same participant has a same set of
parameters that underlie their responses throughout the full study.
During the study, the participant encounters the same string of values
for the independent variable(s) multiple times throughout the study,
allowing for the assessment of test-retest reliability.

## Usage

``` r
execute_study(
  sim_model,
  est_model,
  n_participants = 100,
  n_outcomes = 20,
  n_bins = 5,
  parameter_sd = 1,
  ICC = 1,
  R2 = NA,
  save_results = FALSE,
  path = file.path("results"),
  filename = "results",
  statistics = list(descriptives = descriptives, signal = signal, icc = icc, accuracy =
    accuracy),
  ...
)
```

## Arguments

- sim_model:

  Object of the
  [`model-class`](https://github.com/ndpvh/parameter-reliability/reference/model-class.md)
  containing parameters around which participant-specific parameters
  will be generated using the
  [`generate_parameters`](https://github.com/ndpvh/parameter-reliability/reference/generate_parameters.md)
  function. This model will be used to simulate data.

- est_model:

  Object of the
  [`model-class`](https://github.com/ndpvh/parameter-reliability/reference/model-class.md)
  that will be fitted to the data simulated by `sim_model`

- n_participants:

  Integer denoting the number of participants to include in the study

- n_outcomes:

  Integer denoting the number of outcomes that exists within each
  string. Together with `n_bins`, the total number of observations per
  participant then becomes `N = n_bins * n_outcomes`. Defaults to `20`.

- n_bins:

  Integer denoting the number of repeated strings of outcomes to include
  in the study. Together with `n_outcomes`, the total number of
  observations per participant then becomes `N = n_bins * n_outcomes`.
  Defaults to `5`.

- parameter_sd:

  (Positive) numeric denoting the population standard deviation of the
  parameters, therefore indicating how many individual differences that
  exist on each parameter of the simulating model `sim_model`. Defaults
  to `1`

- ICC:

  Numeric between 0 and 1 denoting the value of the ICC that you want to
  simulate in the data. If specified, it will change the values of each
  of the parameters in the model across bins, emulating variation across
  bins and therefore the consistency with which they can be recovered.
  Defaults to `1`, meaning that parameters don't change across bins.

- R2:

  Numeric between 0 and 1 denoting the \\R^2\\ of the model on the
  within-person level. If specified, it will compute a residual standard
  deviation that allows for the determinstic part of the model to have
  an \\R^2\\ as specified. Defaults to `NA`, triggering the use of the
  model-specified residual standard deviation. Passed on to
  [`simulate`](https://github.com/ndpvh/parameter-reliability/reference/simulate-method.md)

- save_results:

  Logical denoting whether to save the results. Defaults to `FALSE`

- path:

  Path to the folder where you want to save the different results.
  Defaults to a folder `"results"` in the current working directory

- filename:

  Character denoting the delineating name to be given to the files that
  contain the different results. Defaults to `"results"`

- statistics:

  Named list containing single-argument functions that take in the
  estimated and generating values of the parameters and perform analyses
  on these values. Defaults to all functions defined in
  \_statistics.R\_.

- ...:

  Additional arguments passed on to
  [`simulate_x`](https://github.com/ndpvh/parameter-reliability/reference/simulate_x-method.md)

## Value

Test-retest reliability results
