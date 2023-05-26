# Bitsy
Bitsy is a modern hardware description language.

## Module Definitions
You declare modules with `mod`.
Modules can be marked public with `pub`.
Non-`pub` modules are free to be inlined by the compiler.
Module blocks are closed with `end`.

At the top of a module, you declare your ports.
A port is a collection of related wires (called pins for now).
The advantage of ports is that they can be connected together in a single statement.
(To be implemented later).
In modules with only a few number of wires, you should name the port `io`.

Each port consists of a number of pins.
Each pin is marked as `incoming` or `outgoing`.
Each pin also has a name and a shape (such as `Bit` or `Word<8>`).

In the future, once multiple clock domain support is added,
ports will become the unit of clocking as well.
(In other words, all pins on the same port are clocked the same).

Below the port definitions are the component declarations.
There are different kinds of components:

* `mod` for submodules
* `reg` for registers
* `gate` for gates
* `const` for constant sources

Every component is given a name.
Components can be marked public with `pub`.
These are guaranteed to be preserved during compilation and to have a stable name.

For submodules, you declare it with the name of the module definition.

For registers, you declare them with a shape.
Optionally, you may include an `init` value.
This is the reset value for the register.

For gates, you declare the name of the gate primitive to be used.
These work just like modules.

For consts, you supply the value and its shape.
Consts simply drive the constant value to its output.

Every component defined in a module will expose its pins as terminals.
You must drive every sink terminal, or the module is considered incomplete.
Any source terminal you fail to sink will (someday) result in a warning.

To drive a sink from a source, you use the `wire` keyword.
Again, `pub` will make this wire public and prevent optimizations.
The wire will be guaranteed to have a stable name during simulation.

The source may be a terminal, created from the placement of some component,
or it may be an expression.
Unlike in Verilog, you may not wire sinks together.

Every module has an implicit clock and reset.

```
pub mod Top
    port io
        incoming in  of Word<8>
        outgoing out of Word<8>

    pub reg b of Bit
    pub reg w1 of Word<1> init 0
    pub reg w2 of Word<1> init 1

    pub mod adder of Adder
    pub gate and of And

    pub const v = false of Bit

    pub reg b2 of Bit

    pub reg state of State
        init @Idle

    pub wire b.set <= w1.val == w2.val
    pub wire state.set <= @Running(tuple(15, ${ bar = 0, baz = @Valid(false) }))
    pub wire io.out <= 255
end
```

## Tuples

Tuples are useful for when you want to stuff a bunch of data into a register or an enum variant (see below).

A tuple shape is defined as `Tuple<Bit, Word<8>>` to create a pair of a bit and a byte.
To construct a value of this shape, you use `tuple(false, 0)` (with a lowercase t).

## Structs

One advantage Bitsy offers over Verilog is proper support for user-defined types.
The first of these is `struct`s.

```
struct shape Foo
    field bar of Bit
    field baz of Word<8>
end
```

This defines a new shape (which is a type for hardware values).
We declare each field with the `field` keyword and give the name and shape of the field.

To construct values of this new shape, we use the struct literal syntax: `${ far = true, baz = 0 }`.

To access a field in a value of a struct shape, you use the familiar dot operator: `foo.bar`.
(Embarrassingly not impemented yet).

## Enums

The second sort of user-defined shape is the `enum`.
These are like tagged unions in Verilog.

```
enum shape Valid<S of Shape>
    @Invalid
    @Valid of S
end
```

Here, we have a shape with two variants, `@Invalid` is a value for when there "is no data",
and `@Valid(s)` for when there is a value.

Each variant may carry an optional payload.
Here, `@Valid` has a payload of shape `S`.
(The `S` is a parameter, given in the declaration.
Structs may also have parameters).

To make use of enum values, you use the `match` statement.

Here is an example of using a match to set the value of `io.out` based on the input `io.in`.
Notice that `io.out` will "default" to `false` whenever `io.in` is invalid:

```
    wire io.out <= match io.in {
        @Invalid => false;
        @Valid(bit) => bit;
    }
```

## Typechecking

The real advantage of Bitsy (I hope someday) is that the type system prevents you from engaging in nonsense.
In particular, there will never be any data loss from implicit truncation (unlike Verilog).
There will also never be automatic extension, so you always have to choose between sign- and zero-extension.
Adding values is always done with the peculiar-looking `+&` and +%` operators to remind you that words overflow.
And so we use `+&` for when we capture the carry bit and `+%` when we want to roll over.

Indexing is another major point of unsafety in Verilog.
To prevent this, all indexing with `x[i]` must be guaranteed to be in-bounds statically.
For indexing which may not be in-bounds, the checked version, `x[i]?` is used,
which returns a value of shape `Valid<S>` rather than `S`.

(Also not implemented yet).


## Shapes and Operations

### `Bit`

`Bit` is the type of single bits.
They have only two values: `true` for a logical "1" and `false` for a logical 0.

These support the standard boolean operators:

* `~b` logical NOT
* `a && b` logical AND
* `a || b` logical OR
* `a ^ b` logical XOR

To make use of bits, you can use `if` expressions:

```
if b {
    v1
} else if c {
    v2
} else {
    v3
}
```

Bitsy's `if` expressions work more like Verilog's teriary conditional expression than Verilog `if` statements.
This means `if b { a } else {c}` would be the equivalent of `b ? a : c`.
This also mean `if` statements are required to have an `else` branch.
Finally, all branches of an `if` expression must be expressions themselves, and cannot contain assignment statements.


### `Word<n>`

`Word<n>` is the type of n-bit integers.
Nominally, words are unsigned except in operations which explicitly interpret them in a signed manner.

They following operations are supported between two words of the same width:

* `a +& b` add the values `a` and `b` and extend the result by one bit for a carry.
* `a +% b` add the values `a` and `b` and wrap around on overflow.

* `a < b`
* `a <= b`
* `a > b`
* `a >= b`

You can change the width of a word with `w.trunc(n)`.
Here, `n` must be a constant equal or less than the width of `w`.
Similarly, you can zero-extend a word with `w.zext(n)`, and similarly,
`n` must be a constant greater than or more than the width of `w`.
You can also sign-extend a word with `w.sext(n)`.
To avoid confusion, `sext` is not allowed for the case of `Word<0>`.

### `Vec<n, S>`

For any number `n` and any shape `S`, we can produce the shape of fixed-length vectors of that shape, `Vec<n, S>`.

To construct a value of `Vec<n, S>`, we use `vec(s1, s2, ..., sn)` where each of `s1` `s2` through `sn` has shape `S`.

We can index into a vector using the try-index operator.
If `v` is of shape `Vec<n, S>` and `i` is `Word<m>`, then `v[i]?` will be a value of shape `Valid<S>`.
The reason for returning a `Valid<S>` is that if the index `i` is not in bounds, `@Invalid` is returned.
If `i` is in-bounds, `@Valid(s)` is returned.

When we have a value `v` of shape `Vec<n, S>` and n is a power of 2, we can index into it with a word of the appropriate size:
`v[n]` returns a value of shape `S`.

For constants `i` and `j`, we can index or slice:

* `a[i]` returns a value of shape `S`
* `a[i:j]` returns a `Vec<m, S>` where m is the value of j - i.

Note that the order for slicing is the same as in Python, with the larger index coming second.
The two indices are allowed to be the same, returning the empty type `Vec<0, S>`.

You can convert a `Word<n>` to a `Vec<n, Bit>` with `w.to\_vec()` and vice versa with `v.to\_word()`.


### Equality Testing

The two operations `==` and `!=` are supported for all shapes.
They return a `Bit`.
