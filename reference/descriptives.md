# Compute the Descriptive Statistics for the Parameters

Descriptive statistics include the mean, standard deviation, median, and
95 confidence interval across participants. Statistics are computed per
bin-type.

## Usage

``` r
descriptives(data)
```

## Arguments

- data:

  Named list containing the parameter names under `"parameters"`, the
  true parameter values under `"simulated"`, and the estimated parameter
  values under `"estimated"`. Typically the result of
  [`prepare`](https://github.com/ndpvh/parameter-reliability/reference/prepare.md)

## Value

Dataframe containing the parameter and bin (`"parameter"`, `"bin"`), as
well as the summary statistics for each.
