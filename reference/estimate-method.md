# Estimate a Model from Data

Estimation of the different models from a particular data set. Uses the
`lm` function under the hood.

## Usage

``` r
estimate(model, data)

# S4 method for class 'linear'
estimate(model, data)

# S4 method for class 'quadratic'
estimate(model, data)

# S4 method for class 'cubic'
estimate(model, data)

# S4 method for class 'main_effect'
estimate(model, data)

# S4 method for class 'interaction'
estimate(model, data)

# S4 method for class 'ar1'
estimate(model, data)

# S4 method for class 'arx'
estimate(model, data)
```

## Arguments

- model:

  Object representing the model to be used. Note that this may just be
  the class name and does not need any particular parameters to be
  specified.

- data:

  Data.frame containing the relevant variables, specifically `"y"`,
  `"x"`, `"z"`, and `"time"`. Ideally, all variables are present in the
  data.frame, even when they are not applicable to a particular model.

## Value

List containing the result of `lm` under `"fit"` and the model with
estimated parameters under `"model"`

## Examples

``` r
# Example given for the linear model, but also possible for other models
x <- rnorm(100)
data <- data.frame(
  x = x, 
  y = 3 + 10 * x + rnorm(100)
)

estimate(
  linear(),
  data
)
#> $fit
#> 
#> Call:
#> lm(formula = y ~ x, data = data)
#> 
#> Coefficients:
#> (Intercept)            x  
#>       3.139        9.961  
#> 
#> 
#> $model
#> An object of class "linear"
#> Slot "parameters":
#> [1] 3.138644 9.961004
#> 
#> Slot "sd":
#> [1] 1.017146
#> 
#> 
```
