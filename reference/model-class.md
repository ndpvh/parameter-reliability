# An S4 Class to Represent all Models.

An S4 Class to Represent all Models.

## Details

Defines the `model` class, to which all other models belong. This class
is solely defined by its parameters and the error around it.

## Slots

- `parameters`:

  Numeric vector containing the values of the parameters of the model.

- `sd`:

  Numeric defining the error around the deterministic part defined by
  the slot `parameters`
