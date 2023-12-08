Builtin Types
=============
Bitsy includes the following built-in types:

`Word<n>`
---------
A `Word<n>` is an n-bit number.
It is nominally interpreted to be unsigned.
However, operators and functions may re-interpret it as a signed number or as a bitvector.

`Vec<T, n>`
-----------
A `Vec<T, n>` is an n-element vector of elements of a type `S`.

It's values are written as `[v1, v2, ..., vn]`.
For example, `[1, 2, 3]` is a value for `Vec<Word<8>, 3>`.
