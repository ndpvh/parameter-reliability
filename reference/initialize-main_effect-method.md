# Constructor for the [`main_effect-class`](https://github.com/ndpvh/parameter-reliability/reference/main_effect-class.md)

Constructor for the
[`main_effect-class`](https://github.com/ndpvh/parameter-reliability/reference/main_effect-class.md)

## Usage

``` r
# S4 method for class 'main_effect'
initialize(.Object, parameters = c(0, 0, 0), sd = 1)
```

## Arguments

- .Object:

  Model to initialize

- parameters:

  Numeric vector containing the values of the parameters of the model,
  namely \\a\\, \\b\\, and \\c\\ in this order. If left unspecified, the
  model will default to `c(0, 0, 0)`.

- sd:

  Numeric defining the error around the deterministic part defined by
  the argument `parameters`. If left unspecified, will default to `1`
