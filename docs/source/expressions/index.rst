Expressions
===========
Bitsy is an expression-oriented hardware language.
This means that building combinational logic is as simple as writing out the logic as an expression.

Bitsy's expressions are as rich as any programming language.


Literal expressions
-------------------
The most basic kind of expression is the literal.

The literals for `Bit` are `true` and `false`.

The literals for `Word<n>` are numbers, such as `0`, `1`, `2`, etc.
These literals can be inferred to have shape `Word<n>` as long as the final result can be represented as `n` bits.
You can also express the literal in hexadecimal or binary.


Constructor expressions
-----------------------
For aggregate types, there is syntax for constructing values from other values.

You can construct a `Vec<n, S>` with the syntax `[v1, v2, ..., vn]`, where each of `v1`, `v2`, ..., `vn` are values of shape `S`.

You can construct a `Tuple<S, T>` with the syntax `tuple(s, t)`, where `s` is a value of shape `S` and `t` is a value of shape `T`.

You can construct an enum with its constructors.
For example, `Valid<S>` can be constructed with `@Invalid` or `@Valid(s)`, given a value `s` of shape `S`.

You can construct a struct with the syntax `${f1 = v1, f2 = v2, ..., fn = vn}`.


`if` expressions
----------------
Like any good programming language, Bitsy has `if` expressions.
As hardware, these act like multiplexers.

Note that these are *expressions* and not *statements*.
Every branch must supply a value and the values of each branch must be the same.
Moreover, there must always be an `else` clause.

The conditions are `Bit`-valued, since those are the booleans of Bitsy.

This example shows an expression that is always one greater than the value of `n`,
but loops back to 0 upon reaching 100:

.. code-block::

    if n == 100 {
        0
    } else {
        n + 1
    }

Naturally, `if` expressions can have `else if` clauses as well:

.. code-block::

    // allow us to play fizz buzz with a Word<4>
    if i == 3 {
        @Fizz
    } else if i == 5 {
        @Buzz
    } else if i == 15 {
        @FizzBuz
    } else {
        @Number(i)
    }


`match` expressions
-------------------
Enum shapes are an important part of Bitsy's design.
They allow you to express a value which can take on a fixed number of variants.
They are very useful for representing the state space of state machines,
especially when different states need to keep track of different pieces of data.

A `match` expression inspects an enum value and branches based on its tag.

.. code-block::

  // assign a numeric value to each of the stop light colors
  match state.val {
    @Red => 0;
    @Yellow => 10;
    @Green => 50;
  }

If the variants of an enum shape have a payload, the `match` expression will bring that payload into scope.
The following example shows how we can supply a default value of 0 for an `Valid<Word<8>>`:

.. code-block::

  match io.enqueue {
    @Invalid => 0;   // if io.enqueue has no data, supply a default value of 0
    @Valid(n) => n;  // else, just supply the data
  }


`let` expressions
-----------------
One of the most important features a language can provide is the ability to name meaningful subexpressions.
The `let` expression does just that.
It creates a new variable with a given definition in the remainder of the expression:

.. code-block::

    // define a new variable named sum, then see if it is above or below average
    let sum = a + b;
    if sum > 7 {
        @AboveAverage
    } else {
        @BelowAverage
    }

A `let` expression can also be used to tuples:

.. code-block::

    // find the different between a and b
    let tuple(min, max) = sort(a, b);
    max - min


Function calls
--------------
Bitsy allows you to package up and name small pieces of combinational logic using functions.
Functions are defined using the `fn` keyword:

.. code-block::

  fn next_color(color of Color) -> Color
      match state.val {
        @Red => @Green;
        @Yellow => @Red;
        @Green => @Yellow;
      }
  end

Once defined, you can call a function:

.. code-block::

    next_color(state.color)


Operators
---------
Bitsy defines a number of operators you're surely familiar with:

* `+` addition with carry
* `+%` addition with overflow
* `-` subtraction with underflow
* `<` less than comparison
* `==` equal comparison
* `!=` unequal comparison


Reference expressions
---------------------
Values can depend on the hardware available to you in the current module instance.
Given a hardware component, you can access its terminals by dotting into it.

For example, if you have a module defined like this:

.. code-block::

    mod Foo
        port io
            incoming in  of Word<8>
            outgoing out of Word<16>

        reg state of Word<8> init 0

        // ...
    end

You could create references to:

* `io.in`
* `io.out`
* `state.val`


Holes
-----
Hole expressions, written `?` or `?{comment here}`, are placeholders for expressions you have yet to write.
They are useful for when you want to build your design interactively or simulate an imcomplete circuit.

Holes always typecheck, regardless of what type is needed.

When a simulator tries to evaluate a hole, it will halt the simulation,
and you will be able to inspect the variables in scope at that point.


.. toctree::
  :maxdepth: 1
