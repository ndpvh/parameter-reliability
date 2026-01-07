# Compute the \\ICC(A, 1)\\ for Participant and Bin.

In our study, we define the \\ICC(A, 1)\\ as:

\$\$ICC(A, 1) = \frac{\sigma\_\text{participants}}^2\$\$

where \\\sigma^2\\ denotes the variance. This \\ICC\\ puts the
systematic variance – that is, the interindividual differences in the
values of the parameters – against the total observed variance in the
parameters, checking whether individual differences can be reliably
picked up on.

## Usage

``` r
icc(data)
```

## Arguments

- data:

  Named list containing the parameter names under `"parameters"`, the
  true parameter values under `"simulated"`, and the estimated parameter
  values under `"estimated"`. Typically the result of
  [`prepare`](https://github.com/ndpvh/parameter-reliability/reference/prepare.md)

## Value

Dataframe containing the parameter (`"parameter"`), systematic and
unsystematic variances (`"systematic"`, `"unsystematic"`), and the
estimated \\ICC(A, 1)\\ for each parameter (`"icc"`)
