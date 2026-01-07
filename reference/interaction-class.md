# An S4 Class to Represent the Interaction Model.

An S4 Class to Represent the Interaction Model.

## Details

Defines the `interaction` class, which states that for two given input
variables \\x\\ and \\z\\, the relationship between these variables and
the dependent variable \\y\\ is one with a main_effect and interaction
effect:

\$\$y = a + bx + cz + dxz\$\$

This class is solely defined by its parameters \\a\\, \\b\\, \\c\\, and
\\d\\, which should all be provided through the construction of this
class.

## Slots

- `parameters`:

  Numeric vector containing the values of the parameters of the model,
  namely \\a\\, \\b\\, \\c\\, and \\d\\ in this order. If left
  unspecified, the model will default to `c(0, 0, 0, 0)`.

- `sd`:

  Numeric defining the error around the deterministic part defined by
  the slot `parameters`. If left unspecified, will default to `1`
