Builtin Shapes
==============
Bitsy includes the following built-in shapes:

`Bit`
-----
A `Bit` is a 1-bit boolean value.

`Word<n>`
---------
A `Word<n>` is an n-bit number.
It is nominally interpreted to be unsigned.

`Vec<n, S>`
-----------
A `Vec<n, S>` is an n-element vector of elements of a shape `S`.

`Valid<S>`
----------
A `Valid<S>` is an optional value of shape `S`.
It is used when a value may or may not be available.
It's values are `@Valid(s)` when there is data available, and `@Invalid` otherwise.

`Tuple<S, T>`
-------------
A `Tuple<S, T>` is a pair `tuple(s, t)` with `s` having shape `S` and `t` having shape `T`.
