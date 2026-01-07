# Simulate Data from a Model

Method for the different models that allows the simulation of data for
these models.

## Usage

``` r
simulate(model, ...)

# S4 method for class 'linear'
simulate(model, X = NULL, Xfun = function(x) runif(x, -2, 2), N = 100, R2 = NA)

# S4 method for class 'quadratic'
simulate(model, X = NULL, Xfun = function(x) runif(x, -2, 2), N = 100, R2 = NA)

# S4 method for class 'cubic'
simulate(model, X = NULL, Xfun = function(x) runif(x, -2, 2), N = 100, R2 = NA)

# S4 method for class 'main_effect'
simulate(
  model,
  X = NULL,
  Xfun = list(function(x) runif(x, -2, 2), function(x) runif(x, -2, 2)),
  N = 100,
  R2 = NA
)

# S4 method for class 'interaction'
simulate(
  model,
  X = NULL,
  Xfun = list(function(x) runif(x, -2, 2), function(x) runif(x, -2, 2)),
  N = 100,
  R2 = NA
)

# S4 method for class 'ar1'
simulate(model, X = NULL, Xfun = function(x) runif(x, -2, 2), N = 100)

# S4 method for class 'arx'
simulate(model, X = NULL, Xfun = function(x) runif(x, -2, 2), N = 100)
```

## Arguments

- model:

  Object of one of the different model classes (e.g., linear,
  quadratic,...)

- ...:

  Arguments passed on to lower functions. Contains all other arguments
  explained within this function.

- X:

  Numeric vector or matrix containing the values of the independent
  variables. For
  [`linear-class`](https://github.com/ndpvh/parameter-reliability/reference/linear-class.md),
  [`quadratic-class`](https://github.com/ndpvh/parameter-reliability/reference/quadratic-class.md),
  [`arx-class`](https://github.com/ndpvh/parameter-reliability/reference/arx-class.md),
  a numeric vector suffices, as only one independent variable is
  required. For
  [`main_effect-class`](https://github.com/ndpvh/parameter-reliability/reference/main_effect-class.md)
  and
  [`interaction-class`](https://github.com/ndpvh/parameter-reliability/reference/interaction-class.md),
  a matrix is needed instead, where the first column is taken to be
  \\x\\ (paired with parameter \\b\\) and the second column is taken to
  be \\z\\ (paired with parameter \\c\\). For the
  [`ar1-class`](https://github.com/ndpvh/parameter-reliability/reference/ar1-class.md),
  this argument is ignored. Defaults to `NULL`, forcing the user to
  specify its value for all models except for the
  [`ar1-class`](https://github.com/ndpvh/parameter-reliability/reference/ar1-class.md),
  or alternatively to specify a function with which to generate values
  of `X` through the `Xfun` argument.

- Xfun:

  Function with which to simulate values for `X`. Should take in only a
  single argument, namely `N`, specifying how many values for `X` to
  simulate. Defaults to a uniform distribution between `-2` and `2`.
  Note that the output of `Xfun` is used as an alternative to `X`,
  meaning that it should conform to distinction between numeric vector
  and numeric matrix specified for the `X` argument.

- N:

  Integer denoting the number of values that should be simulated.
  Ignored when `X` is defined. Defaults to `100`.

- R2:

  Numeric between 0 and 1 denoting the \\R^2\\ of the model. If
  specified, it will compute a residual standard deviation that allows
  for the determinstic part of the model to have an \\R^2\\ as
  specified. Defaults to `NA`, triggering the use of the model-specified
  residual standard deviation

## Value

Dataframe containing the values of the variables (\\y\\, \\x\\, and if
applicable \\z\\, named as such) and time (`time`)

## Examples

``` r
# Example given with the linear model, but extends to other models as well.
model <- linear(
  parameters = c(0, 10),
  sd = 1
)

# Simulate for the linear model described above with values for the predictor
# drawn from a standard normal distribution
simulate(
  model, 
  X = rnorm(10, mean = 0, sd = 1)
)
#>    time           y           x z
#> 1     1  -3.9920500 -0.27215368 0
#> 2     2 -23.9246587 -2.44668003 0
#> 3     3   0.7299723  0.06548664 0
#> 4     4 -10.4265746 -1.09850890 0
#> 5     5  -5.9163754 -0.63317818 0
#> 6     6 -22.0888443 -2.06365445 0
#> 7     7  27.4305264  2.64893203 0
#> 8     8 -11.8729197 -1.15339839 0
#> 9     9  -3.4819530 -0.34063788 0
#> 10   10   7.9038302  0.78636258 0

# You can also provide your own function to generate the predictor X to the 
# function. This is especially useful if this predictor is subject to more 
# complicated computations, such as the generation of gambling stimuli
simulate(
  model,
  Xfun = \(x) rnorm(x, mean = 0, sd = 1),
  N = 10
)
#>    time           y          x z
#> 1     1  -0.3195078  0.1243011 0
#> 2     2  -9.9132721 -0.9984326 0
#> 3     3  11.6943659  1.2333901 0
#> 4     4   2.5590491  0.3404245 0
#> 5     5  -4.0517801 -0.4727025 0
#> 6     6   8.2409064  0.7087531 0
#> 7     7 -16.9760919 -1.5289587 0
#> 8     8   1.4714385  0.2374253 0
#> 9     9 -11.8105088 -1.3128142 0
#> 10   10   8.5704756  0.7470286 0
```
