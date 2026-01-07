# Constructor for the [`linear-class`](https://github.com/ndpvh/parameter-reliability/reference/linear-class.md)

Constructor for the
[`linear-class`](https://github.com/ndpvh/parameter-reliability/reference/linear-class.md)

## Usage

``` r
# S4 method for class 'linear'
initialize(.Object, parameters = c(0, 0), sd = 1)
```

## Arguments

- .Object:

  Model to initialize

- parameters:

  Numeric vector containing the values of the parameters of the model,
  namely \\a\\ and \\b\\ in this order. If left unspecified, the
  parameters will default to `c(0, 0)`.

- sd:

  Numeric defining the error around the deterministic part defined by
  the argument `parameters`. If left unspecified, will default to `1`
