# Simulate Values for the Independent Variable for a Model

Simulate Values for the Independent Variable for a Model

## Usage

``` r
simulate_x(model, ...)

# S4 method for class 'linear'
simulate_x(model, Xfun = function(x) runif(x, -2, 2), N = 100)

# S4 method for class 'quadratic'
simulate_x(model, Xfun = function(x) runif(x, -2, 2), N = 100)

# S4 method for class 'cubic'
simulate_x(model, Xfun = function(x) runif(x, -2, 2), N = 100)

# S4 method for class 'main_effect'
simulate_x(
  model,
  Xfun = list(function(x) runif(x, -2, 2), function(x) runif(x, -2, 2)),
  N = 100
)

# S4 method for class 'interaction'
simulate_x(
  model,
  Xfun = list(function(x) runif(x, -2, 2), function(x) runif(x, -2, 2)),
  N = 100
)

# S4 method for class 'ar1'
simulate_x(model, Xfun = function(x) runif(x, -2, 2), N = 100)

# S4 method for class 'arx'
simulate_x(model, Xfun = function(x) runif(x, -2, 2), N = 100)
```

## Arguments

- model:

  Object of one of the different model classes (e.g., linear,
  quadratic,...)

- ...:

  Arguments passed on to lower functions. Contains `Xfun` and `N` in the
  generic of `simulate_x`

- Xfun:

  One or more functions with which to simulate values for the
  independent variable(s). Should take in only a single argument, namely
  `N`, specifying how many values for `X` to simulate. Defaults to a
  uniform distribution bounded between -2 and 2.

- N:

  Integer denoting the number of values that should be simulated.
  Defaults to `100`.

## Value

Matrix containing the values of the independent variables

## Examples

``` r
# Create a linear model
model <- linear(
  parameters = c(0, 10),
  sd = 1
)

# Simulate values for X for this model
simulate_x(
  model,
  Xfun = \(x) rnorm(x, mean = 0, sd = 1),
  N = 10
)
#>              [,1]
#>  [1,] -1.43127078
#>  [2,]  1.38291086
#>  [3,]  0.00312594
#>  [4,] -0.07788682
#>  [5,]  0.44142823
#>  [6,]  0.12892290
#>  [7,] -0.83021426
#>  [8,] -0.50359291
#>  [9,] -1.19364118
#> [10,] -0.75172332
```
