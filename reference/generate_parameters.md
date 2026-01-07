# Generate a Random Parameter Set

Generate a Random Parameter Set

## Usage

``` r
generate_parameters(n, mean, sd)
```

## Arguments

- n:

  Integer denoting the number of parameters to generate

- mean:

  Numeric vector containing the means of the parameters

- sd:

  Numeric denoting the (shared) standard deviation of the parameters

## Value

Numeric matrix of size n x k, where n is the number of randomly
generated parameter sets and k the number of parameters of the model
