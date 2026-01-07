# Get the Bounds of the Model

Get the Bounds of the Model

## Usage

``` r
bounds(model, ...)

# S4 method for class 'linear'
bounds(model, intercept = c(-10, 10), slope = c(-5, 5))

# S4 method for class 'quadratic'
bounds(model, intercept = c(-10, 10), slope = c(-5, 5))

# S4 method for class 'cubic'
bounds(model, intercept = c(-10, 10), slope = c(-5, 5))

# S4 method for class 'main_effect'
bounds(model, intercept = c(-10, 10), slope = c(-5, 5))

# S4 method for class 'interaction'
bounds(model, intercept = c(-10, 10), slope = c(-5, 5))

# S4 method for class 'ar1'
bounds(model, intercept = c(-10, 10), slope = c(-5, 5))

# S4 method for class 'arx'
bounds(model, intercept = c(-10, 10), slope = c(-5, 5))
```

## Arguments

- model:

  Object of one of the different model classes (e.g., linear,
  quadratic,...)

- ...:

  Additional arguments provided to the model of choice.

- intercept:

  Numeric vector with two values denoting the lower and upper bound of
  the intercept parameter. Defaults to `c(-10, 10)`.

- slope:

  Numeric vector with two values denoting the lower and upper bound of
  all slope parameters for the covariates. Defaults to `c(-5, 5)`.

## Value

Matrix where the left/right column specifies the lower/upper bounds of
the parameters in the order specified in the description of the models

## Examples

``` r
# Example given for the linear model, but also possible for the other models
bounds(linear())
#>      [,1] [,2]
#> [1,]  -10   10
#> [2,]   -5    5

bounds(
  linear(),
  intercept = c(-10, 10),
  slope = c(0, 10)
)
#>      [,1] [,2]
#> [1,]  -10   10
#> [2,]    0   10
```
