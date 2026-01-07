# Compute the Bias and MSE of the Estimates

This function depends on the assumption that the order of the estimated
parameters is the same as for the generating parameters. This is
automatically true in the
[`execute_study`](https://github.com/ndpvh/parameter-reliability/reference/execute_study.md)
function.

## Usage

``` r
accuracy(data)
```

## Arguments

- data:

  Named list containing the parameter names under `"parameters"`, the
  true parameter values under `"simulated"`, and the estimated parameter
  values under `"estimated"`. Typically the result of
  [`prepare`](https://github.com/ndpvh/parameter-reliability/reference/prepare.md)

## Value

Dataframe containing the parameter (`"parameter"`), the bias, and the
MSE
