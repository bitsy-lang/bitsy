Nettle
======
Nettle is a circuit simulator intended for use with Bitsy.

Hello Nettle
------------
Tradition dictates that the first program that is written in any programming language should be Hello, World!
In that same spirit, let's look at the "hello world" of Nettle: `Passthrough`:

.. literalinclude:: ../examples/passthrough.ntl
   :language: nettle
   :linenos:

We see a module declaration, `mod Passthrough`.
Inside, we see two ports, one `incoming` port named `out` and one `outgoing` port named `in`.
Both have type `Word<8>`, meaning that an 8-bit value passes across them.
The line `out := in` connects the input port `in` to the output port `out`.
The notation `:=` is called a wire.

Blinky Nettle
-------------
A classic circuit used as the introduction to working with FPGAs is blinky:

.. literalinclude:: ../examples/blink.ntl
   :language: nettle
   :linenos:

Registers are the stateful components in a Nettle circuit.
They are declared with the `reg` keyword.

Here, we see that we declare a register `r`.
We see that `r` has type `Word<1>`, which is a single bit.
This register also given a reset value, `0w1`.
This is the value which the register takes on when the circuit is reset.
The notation `0w1` means "the number `0` represented with bitwidth of `1` bit.
(The `w` stands for "width").

The next two statements are wires.
But if you look closely, they use two different operators
The difference between them is whether the left hand side is updated immediately or on the next clock cycle.

* `:=` is called a **direct** wire.

  * Updates the left hand side immediately.
  * Works with `incoming` and `outgoing` ports and other non-`reg` components.

* `<=` is called a **latched** wire.

  * Updates the left hand side on the next clock cycle.
  * Only works with `reg`.

The right hand side of a wire may be an expression.
In `r <= !r;`, we latch the value of the expression `!r`, the logical negation of `r`.
This means `r`, in this example, will toggle between `0w1` (a logical false) and `1w1` (a logical true).

Finally, the result of `r` is forwarded to the `outgoing` port `out` through the second connect statement, `out := r;`.

Submodules
----------
When we want to organize our circuit, we can nest modules:

.. literalinclude:: ../examples/tutorial_submods.ntl
   :language: nettle
   :linenos:

In this example, lines 20 through 29 declare a submodule named `Sort`.
This submodule has two `incoming` ports and two `outgoing` ports.

The body of `sort` is simply two wires, each connecting the result of an `if` expression to one of the ports.

Because this submodule has no `reg` keyword and doesn't use any latching connects (`<=`), we can see it is **combinational**.
That is, its outputs update immediately when the inputs change and do not wait for the next clock cycle.

The `Sort` module has the effect of sorting the two incoming values `a` and `b`, placing the smaller one on `min` and the larger one on `max`.

We see on line 8 that the `Top` module instantiates `Sort` as a submodule.
We can interact through its ports, as we see on lines 13, 14, 16, and 17.

External Modules
----------------
An external module is a submodule whose implementation is not giving in the design itself.
Instead, the implementation is linked in by the simulator.
This is especially useful for software-controlled virtual hardware.

.. literalinclude:: ../examples/tutorial_ext.ntl
   :language: nettle
   :linenos:

An external module is introduced with the `ext` keyword.
It may only contain `incoming` and `outgoing` declarations.
It may not contain any `reg`\s or wire statements.

Here, a counter named `counter` counts up by 2 every cycle,
and its current value is sent to the external module `monitor`.
The exact behavior is determined by what it is linked against,
but the intent in this example is that the monitor is logging the values that `counter` takes on over time.
Perhaps the monitor will check to ensure that the value is never an *odd* number,
and it will alert us to when this important invariant is violated.

Expressions
-----------
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

Ports
-----
Ports allow a module to communicate with its parent.
Values for the top-level input ports must be provided by the testbench during simulation.

Ports may also appear in external modules and must match the portlist for the implementation.

Only `outgoing` ports in current module or `incoming` ports of a submodule may be the target of a wire statement.
This is reversed for reference expressions: only `outgoing` ports of a submodule
or `incoming` ports of the current module may be referenced.

Nodes
-----
Nodes represent a intermediate value.
They are useful for naming common subexpressions in a circuit.

Nodes are declared using the `node` keyword.
They work similarly to ports, but they cannot be accessed from outside of the module they are defined in.
Nodes must be connected to with a direct wire (`:=`).

Registers
---------
Registers are stateful components.

All `reg`\s start off with an `XXX` value.
They may optionally have a reset value supplied using the syntax `reg r reset 0w8;`.

Registers may only be referenced in the module they are declared in.

Combinational Loops
-------------------
Nettle checks for combinational loops before simulation.
Circuits with loops are rejected.

Note that if `r` is a `reg`, the wire `r <= r + 1w8` is not a combinatorial loop,
since the `r` on the left hand side implies the `set` terminal while the `r` on the right hand side implies the current value terminal.
