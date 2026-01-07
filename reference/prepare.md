# Prepare True and Estimated Parameters for Analysis

Prepare True and Estimated Parameters for Analysis

## Usage

``` r
prepare(estimated, simulated)
```

## Arguments

- estimated:

  Dataframe containing estimated parameters per participant and bin,
  allowing for a decomposition analysis.

- simulated:

  Dataframe containing the simulated values for the parameters per
  participant.

## Value

Named list returning parameter names (`"parameters"`) and adjusted
data.frames for the estimated and true parameter values (`"estimated"`,
`"simulated"`) so that they can be readily compared to each other.
