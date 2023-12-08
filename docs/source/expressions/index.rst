Expressions
===========
Expressions represent combinational logic.

**Literals**

To represent integers of arbitrary bitwdith, we annotate constants with their bitwidth.
The literal `42w16` is a 16-bit integer with the value 42.

**References**

You may reference ports, registers, and nodes, accessing their current value.

References must be visible to the moduel they appear in.
That means they may be:

* the name of a `node`, `reg`, or `incoming` in the same module
* the name of a submodule dotted with a `node` or `outgoing` of that submodule.
* the name of an external module dotted with one of its `outgoing` ports.

When using a register in this way, it references the *current* value of the register,
regardless of whatever the latching wire (`<=`) is going to do next cycle.

**Operations**

Some basic operations are supported:

* `||` or
* `&&` and
* `!` not
* `^` xor
* `==` equals
* `!=` different
* `<` less than
* `+` sum (wrapping)
* `-` difference (wrapping)

**Concatenation, Indexing, and Slicing**

You can concatenate words with the syntax `cat(w1, w2)`.
If `w1` is `Word<n>` and `w2` is `Word<m>`, the result is `Word<n + m>`.
The bits of `w1` become the high-order bits and `w2` become the lower bits.

You can index into a `Word<n>` with the syntax `w[0]`.
Note that we use a plain old literal and not `0w8` here.
This creates a static indexing of the word.
It has type `Word<1>`.

If you do use `0w8` or any value `i` of shape `Word<k>`,
you end up with dynamic indexing.
An out of bounds dynamic index results in `XXX`.

You can also slice a word with the syntax `w[8..4]`.
This will give you a `Word<4>` with the same result as if you had written
`cat(w[7], w[6], w[5], w[4])`.
Pay attention!
This might surprise you coming from Verilog, since `w[8]` is *not* included in the result.
However, experience from programming languages such as Python and Rust shows
that this is a very natural way to handle slicing
(albeit the ordering is reversed to align with the way we write out bitstrings).

**if statements**

`if` expressions create muxes.

All `if` expressions must have an `else` branch.

**Undefined Values**

The is the undefined value is used for terminals which aren't being driven or
invalid values are driven (eg, when `Valid` where the invalid bit is set but a payload is given).

The undefined value is written `XXX`.

Any operation involving `XXX` will result in `XXX`.
This includes `if` expressions when the condition or any branch is `XXX`.
