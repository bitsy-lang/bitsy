User-defined Types
===================
There are two classes of user-defined types in Bitsy.

Enums
-----
You can define your own enumerations with `enum type`.

For example, in a RISC-V core, you might find the following definition
to capture the list of 7-bit instruction opcodes:

.. code-block:: bitsy

    enum type Opcode {
        OP      = 0b0110011w7;
        OP_IMM  = 0b0010011w7;
        LOAD    = 0b0000011w7;
        STORE   = 0b0100011w7;
        JAL     = 0b1101111w7;
        BRANCH  = 0b1100011w7;
        LUI     = 0b0110111w7;
        AUIPC   = 0b0010111w7;
        JALR    = 0b1100111w7;
    }

You can create a constant literal for any `enum` with the syntax `Opcode::OP_IMM`.
You can convert an enum value to a `Word` of the appropriate size with `word`.
For example, `word(Opcode::OP_IMM)` would equal `0b0010011w7`.

Structs
-------
You can define your own struct types with `struct type`.

For example, if you were writing hardware which made heavy use of 24-bit RGB color values,
you could define `Color` as:

.. code-block:: bitsy

    struct type Color {
        red   of Word[8];
        green of Word[8];
        blue  of Word[8];
    }

You can construct values of a struct type with the syntax
`{red = 0, green = 0, blue = 0}`.
You can take a value with a struct type and refer to its components with the syntax
`color->red`, etc.

Alts
----
An `alt type` is what's sometimes known as an algebraic data type.
Some programming languages also call them `enum` types.

Alt types are defined with a set of constructors.
Each constructor takes zero or more arguments.
The type of each argument is given when defining the alt type.

Here is an example of a definition for an alt type:

.. code-block:: bitsy

    alt type State {
        Idle();
        Running(Word[32], Word[32]);
        Done(Word[32]);
    }

We define `State` to have three constructors: `Idle`, `Running`, and `Done`.

You construct values of an alt type by calling the constructors.
Prepend the constructors with an `@` symbol to distinguish them from functions:

.. code-block:: bitsy

   mod StateMachine {
        reg state of State reset @Idle();
        // ...
   }

To make use of an alt type, you can match on it:

.. code-block:: bitsy

   mod StateMachine {
        reg state of State reset @Idle();
        state <= match state {
            @Idle() => ?idle_next_state;
            @Running(x, y) => ?running_next_state;
            @Done(x) => ?done_next_state;
        };
   }
