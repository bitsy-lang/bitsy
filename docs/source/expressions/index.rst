Expressions
===========
Expressions represent combinational logic.

**Literals**

To represent integers of arbitrary bitwdith, we annotate constants with their bitwidth.
The literal `42w16` is a 16-bit integer with the value 42.

We may also elide the width in any context where the type can be inferred.
For example, if we define a register:

.. code-block:: bitsy

    reg counter of Word[4] reset 0;

The reset value, `0` is inferred to have type `Word[4]`, and is the same as if we had written `0w4`.

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
* `+%` sum (carrying)
* `-` difference (wrapping)

**Concatenation, Indexing, and Slicing**

You can concatenate words with the syntax `cat(w1, w2)`.
If `w1` is `Word[n]` and `w2` is `Word[m]`, the result is `Word[n + m]`.
The bits of `w1` become the high-order bits and `w2` become the lower bits.

You can index into a `Word[n]` with the syntax `w[0]`.
Note that we use a plain old literal and not `0w8` here.
This creates a static indexing of the word.
It has type `Word[1]`.

You can also slice a word with the syntax `w[8..4]`.
This will give you a `Word[4]` with the same result as if you had written
`cat(w[7], w[6], w[5], w[4])`.
Pay attention!
This might surprise you coming from Verilog, since `w[8]` is *not* included in the result.
However, experience from programming languages such as Python and Rust shows
that this is a very natural way to handle slicing
(albeit the ordering is reversed to align with the way we write out bitstrings).

**mux and if expressions**

`mux` is used to create a simple multiplexer.
The syntax is `mux(s, a, b)`, and evaluates to `a` when `s` is asserted and `b` when `s` is asserted.

`if` expressions be used to create mux trees with more than one condition.
All `if` expressions must have an `else` branch.

**let statements**

`let` statements allow you to name subexpressions.

An example is `let x = foo + bar; x + 1`.
This defines a new variable `x` with the value `foo + bar`.
This value is only in scope for the remainder of the expression.

**match statements**

`match` statements allow you to select an expression based on a result.
Think of it like a fancy `if` statement that works well with enum types and alt types.

Each `match` statement has a subject, which is used to determine which match arm is used.

.. code-block:: bitsy

    node typ of InstrType;
    typ := match opcode {
        @OP => InstrType::R;
        @OP_IMM => InstrType::I;
        @LOAD => InstrType::I;
        @STORE => InstrType::S;
        @JAL => InstrType::J;
        @BRANCH => InstrType::B;
        @LUI => InstrType::U;
        @AUIPC => InstrType::U;
        @JALR => InstrType::I;
    };

**sext and zext**

You can extend a word to a larger word by using `sext` (sign-extend) and `zext` (zero-extend).
The width of the result is automatically inferred from context.
You cannot use `sext` on a `Word[0]`.

**word and trycast**

Given an enum type, you can cast it to its underlying numeric value using the `word` builtin.
For example, given the enum type:

.. code-block:: bitsy

    enum type OpFunct7 {
        ADD  = 0b0000000w7;
        SUB  = 0b0100000w7;
    }

The expression `word(OpFunct7::ADD)` evaluates to `0b0000000w7` and
`word(OpFunct7::SUB)` evaluates to `0b0100000w7`.

You can't cast from a word back to an enum,
since the value may not be a valid value in that enum type.
However, you can use `trycast` to get a `Valid` for that type.

In other words, `trycast(0b0000000w7)` evaluates to `@Valid(OpFunct7::ADD)`,
while `trycast(0b1111111w7)` evaluates to `@Invalid`.

**User-defined functions**

You can define your own functions in Bitsy:

.. code-block:: bitsy

    fn inc(x of Word[8]) -> Word[8] {
        x + 1
    }

You can then use these functions in expressions:

.. code-block:: bitsy

    pub mod Top {
        reg counter of Word[8] reset 0;
        counter <= inc(counter);
    }

**Holes**

A hole is an undefined expression.
They are handy for when you want to get an unfinished program to typecheck.

We write holes as `?` for an unnamed hole or `?foo` for a hole with a name (here, `foo`).

A circuit with a hole is unfinished.
However, a hole-aware evaluator may still be able to simulate in their presence.
