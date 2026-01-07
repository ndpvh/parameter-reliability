# Generate parameter grid

Use the bounds of the parameters of a model and create a grid of
parameter values. Each parameter value is taken at an equal distance
across the interval of the parameter.

## Usage

``` r
parameter_grid(model, n = NA, n_int = 5, n_slope = 5, ...)
```

## Arguments

- model:

  Object of one of the different model classes (e.g., linear,
  quadratic,...)

- n:

  Integer denoting the number of parameter sets to generate. Overwrites
  `n_int` and `n_slope` when specific. Defaults to `NA`, meaning that
  `n_int` and `n_slope` are used by default.

- n_int:

  Integer denoting the number of intercepts to draw. Needs to be larger
  than or equal to 2. Defaults to `5`.

- n_slope:

  Integer denoting the number of slopes to draw. Note that the
  autoregressive parameter is also taken to be a slope. Needs to be
  larger than or equal to 2. Defaults to `5`.

- ...:

  Additional arguments passed on to
  [`bounds`](https://github.com/ndpvh/parameter-reliability/reference/bounds-method.md)

## Value

Matrix with dimensions N x k, where k is the number of parameters of the
model and N the total number of combinations possible with these
parameters
