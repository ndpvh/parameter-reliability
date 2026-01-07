# An S4 Class to Represent the main_effect Effect Model.

An S4 Class to Represent the main_effect Effect Model.

## Details

Defines the `main_effect` class, which states that for two given input
variables \\x\\ and \\z\\, the relationship between these variables and
the dependent variable \\y\\ is one with only a main_effect effect:

\$\$y = a + bx + cz\$\$

This class is solely defined by its parameters \\a\\, \\b\\, and \\c\\,
which should all be provided through the construction of this class.

## Slots

- `parameters`:

  Numeric vector containing the values of the parameters of the model,
  namely \\a\\, \\b\\, and \\c\\ in this order. If left unspecified, the
  model will default to `c(0, 0, 0)`.

- `sd`:

  Numeric defining the error around the deterministic part defined by
  the slot `parameters`. If left unspecified, will default to `1`
