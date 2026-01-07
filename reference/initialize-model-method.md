# Constructor for the [`model-class`](https://github.com/ndpvh/parameter-reliability/reference/model-class.md)

Constructor for the
[`model-class`](https://github.com/ndpvh/parameter-reliability/reference/model-class.md)

## Usage

``` r
# S4 method for class 'model'
initialize(.Object, parameters = numeric(0), sd = 1)
```

## Arguments

- .Object:

  Model to initialize

- parameters:

  Numeric vector containing the values of the parameters of the model.
  If left unspecified, an empty numeric vector.

- sd:

  Numeric defining the error around the deterministic part defined by
  the argument `parameters`. If left unspecified, will default to `1`
