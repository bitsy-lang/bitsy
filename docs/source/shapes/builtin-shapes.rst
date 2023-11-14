Builtin Shapes
==============
Bitsy includes the following built-in shapes:

`Bit`
-----
A `Bit` is a 1-bit boolean value.

`Bit`\s are used to indicate signals which may be asserted or deasserted.
`Bit` only two values: `true` and `false`.

`Word<n>`
---------
A `Word<n>` is an n-bit number.
It is nominally interpreted to be unsigned.
However, operators and functions may re-interpret it as a signed number or as a bitvector.

`Vec<n, S>`
-----------
A `Vec<n, S>` is an n-element vector of elements of a shape `S`.

It's values are written as `[v1, v2, ..., vn]`.
For example, `[1, 2, 3]` is a value for `Vec<3, Word<8>>`.

`Valid<S>`
----------
A `Valid<S>` is an optional value of shape `S`.
It is used when a value may or may not be available.

It's values are `@Invalid`, when data isn't available.
In this case, it has no payload.
The other values have the form `@Valid(s)` when there is data available and the payload is the value `s` with shape `S`.

To work with values of type `Valid<S>`, we use the `match` statement:

.. code-block::

  match v {
    @Invaid => 0;   // evaluates to 0 when there is no data available
    @Valid(s) => s; // evaluates to s when there is data available
  }

`Tuple<S, T>`
-------------
A `Tuple<S, T>` is a pair `tuple(s, t)` with `s` having shape `S` and `t` having shape `T`.
