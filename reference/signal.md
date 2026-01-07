# Compute the Signal-based Metrics on Standard Errors

Computes the coefficient of variation and the signal-to-noise ratio. The
coefficient of variation is defined as:

\$\$CV = \frac{\sigma}{\mu}\$\$

where \\\sigma\\ is the standard deviation and \\\mu\\ is the mean of
the estimated parameters. This statistic examines the relationship
between the observed variation and the average value of the parameters.
Note that the coefficient of variation is only interpretable if 0 is
interpretable on the scale. In the case of models, 0 is interpretable on
a ratio scale, and so the coefficient of variation is interpretable as
well. However, negative values of the parameters are allowed, which is
something the coefficient of variation does not expect, we therefore
adjust the formula so that:

\$\$CV = \frac{\sigma}{\|\mu\|}\$\$

communicating the relationship between parameter value and standard
deviations in a directionless way.

For random variables, the signal to noise ratio is defined as:

\$\$SNR = \frac{E\[S^2\]}{E\[N^2\]}\$\$

where \\S\\ represents the signal and \\N\\ represents the noise.

## Usage

``` r
signal(data)
```

## Arguments

- data:

  Named list containing the parameter names under `"parameters"`, the
  true parameter values under `"simulated"`, and the estimated parameter
  values under `"estimated"`. Typically the result of
  [`prepare`](https://github.com/ndpvh/parameter-reliability/reference/prepare.md)

## Value

Dataframe containing the parameter (`"parameter"`), the coefficient of
variation, and the signal-to-noise ratio
