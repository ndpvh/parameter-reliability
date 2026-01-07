# An S4 Class to Represent the ar1 Model.

An S4 Class to Represent the ar1 Model.

## Details

Defines the `ar1` class, which states that for a given dependent
variable \\y\\, the previous values of this variable in part determine
current values of the variable:

\$\$y\_{t} = a + by\_{t - 1}\$\$

This class is solely defined by its parameters \\a\\ and \\b\\, which
should all be provided through the construction of this class.

## Slots

- `parameters`:

  Numeric vector containing the values of the parameters of the model,
  namely \\a\\ and \\b\\ in this order. If left unspecified, the model
  will default to `c(0, 0)`.

- `sd`:

  Numeric defining the error around the deterministic part defined by
  the slot `parameters`. If left unspecified, will default to `1`
