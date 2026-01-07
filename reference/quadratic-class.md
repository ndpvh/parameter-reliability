# An S4 Class to Represent the Quadratic Model.

An S4 Class to Represent the Quadratic Model.

## Details

Defines the `quadratic` class, which states that for a given input
\\x\\, the relationship between \\x\\ and the dependent variable \\y\\
is quadratic:

\$\$y = a + bx + cx^2\$\$

This class is solely defined by its parameters \\a\\, \\b\\, and \\c\\,
which should be provided through the construction of this class.

## Slots

- `parameters`:

  Numeric vector containing the values of the parameters of the model,
  namely \\a\\, \\b\\, and \\c\\ in this order. If left unspecified, the
  model will default to `c(0, 0, 0)`.

- `sd`:

  Numeric defining the error around the deterministic part defined by
  the slot `parameters`. If left unspecified, will default to `1`
